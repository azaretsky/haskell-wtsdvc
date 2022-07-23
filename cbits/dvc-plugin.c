#include "HsFFI.h"
#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <windows.h>
#include "wts-plugin-api.h"

extern int wts_hs_initialize(void);
extern int wts_hs_new_channel_connection(HsStablePtr, IWTSVirtualChannel *, HsStablePtr *);
extern int wts_hs_data_received(HsStablePtr, const void *, ULONG);
extern int wts_hs_closed(HsStablePtr);

static
void setup_std_handles(void)
{
    SECURITY_ATTRIBUTES sa = {
        .nLength = sizeof(sa),
        .lpSecurityDescriptor = NULL,
        .bInheritHandle = TRUE
    };
    HANDLE h;
    DWORD path_length;
    int fd;
    char path[256];
    path_length = ExpandEnvironmentStrings("%USERPROFILE%\\dvc-plugin.log", path, sizeof(path));
    if (path_length > sizeof(path)) {
        fprintf(stderr, "log file path is too long: %lu\n", path_length);
        return;
    }
    if (path_length == 0) {
        fprintf(stderr, "ExpandEnvironmentStrings %lu\n", GetLastError());
        return;
    }
    h = CreateFile(
        path,
        GENERIC_WRITE,
        FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
        &sa,
        OPEN_ALWAYS,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );
    if (h == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "CreateFile(%s) %lu\n", path, GetLastError());
        return;
    }
    if (!SetStdHandle(STD_OUTPUT_HANDLE, h) || !SetStdHandle(STD_ERROR_HANDLE, h)) {
        fprintf(stderr, "SetStdHandle %lu\n", GetLastError());
        CloseHandle(h);
        return;
    }
    fd = _open_osfhandle((intptr_t) h, _O_APPEND);
    if (fd == -1) {
        fprintf(stderr, "_open_osfhandle %s (%d)\n", strerror(errno), errno);
        CloseHandle(h);
        return;
    }
    if (_dup2(fd, 1) == -1 || _dup2(fd, 2) == -1) {
        fprintf(stderr, "_dup2 %s (%d)\n", strerror(errno), errno);
    } else {
        stdout->_file = 1;
        stderr->_file = 2;
    }
    _close(fd);
}

__attribute__ ((format (printf, 1, 2)))
static
void log_message(const char *format, ...)
{
    SYSTEMTIME local_time;
    va_list v;
    GetLocalTime(&local_time);
    fprintf(stderr, "%04d-%02d-%02d %02d:%02d:%02d.%03d [pid %lu, tid %lu] ",
        local_time.wYear, local_time.wMonth, local_time.wDay, local_time.wHour,
        local_time.wMinute, local_time.wSecond, local_time.wMilliseconds,
        GetCurrentProcessId(), GetCurrentThreadId());
    va_start(v, format);
    vfprintf(stderr, format, v);
    va_end(v);
    fprintf(stderr, "\n");
    fflush(stderr);
}

#define IID_STRING_BUF_SIZE 39

static
void iid_to_string(const REFIID iid, char out[IID_STRING_BUF_SIZE])
{
    sprintf(out, "{%08lx-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x}",
        iid->Data1,
        iid->Data2,
        iid->Data3,
        iid->Data4[0], iid->Data4[1],
        iid->Data4[2], iid->Data4[3], iid->Data4[4], iid->Data4[5], iid->Data4[6], iid->Data4[7]
    );
}

#define QUERY_INTERFACE_IMP(iface, name) \
    static \
    STDMETHODIMP name(iface *This, REFIID riid, void **ppvObject) \
    { \
        char iid[IID_STRING_BUF_SIZE] = {0}; \
        if (riid == NULL) { \
            log_message(#name " %p riid is NULL", This); \
            return E_INVALIDARG; \
        } \
        iid_to_string(riid, iid); \
        if (!IsEqualIID(riid, &IID_##iface) && !IsEqualIID(riid, &IID_IUnknown)) { \
            log_message(#name " %p unknown iid %s", This, iid); \
            return E_NOINTERFACE; \
        } \
        if (ppvObject == NULL) { \
            log_message(#name " %p %s ppvObject is NULL", This, iid); \
            return E_POINTER; \
        } \
        log_message(#name " %p %s", This, iid); \
        This->lpVtbl->AddRef(This); \
        *ppvObject = This; \
        return S_OK; \
    }

QUERY_INTERFACE_IMP(IWTSPlugin, plugin_query_interface)

static
STDMETHODIMP_(ULONG) plugin_add_ref(IWTSPlugin *This)
{
    log_message("plugin_add_ref %p", This);
    return 1;
}

static
STDMETHODIMP_(ULONG) plugin_release(IWTSPlugin *This)
{
    log_message("plugin_release %p", This);
    return 0;
}

static IWTSVirtualChannelManager *channel_manager = NULL;

static
STDMETHODIMP plugin_initialize(IWTSPlugin *This, IWTSVirtualChannelManager *pChannelMgr)
{
    ULONG refs;
    int argc = 1;
    char *argv[] = {"wts-dvc-plugin", NULL};
    char **args = argv;
    refs = pChannelMgr->lpVtbl->AddRef(pChannelMgr);
    channel_manager = pChannelMgr;
    log_message("plugin_initialize %p channel manager %p references %lu", This, pChannelMgr, refs);
    hs_init(&argc, &args);
    if (wts_hs_initialize() < 0) {
        This->lpVtbl->Terminated(This);
        return E_UNEXPECTED;
    }
    return S_OK;
}

static
STDMETHODIMP plugin_connected(IWTSPlugin *This)
{
    log_message("plugin_connected %p", This);
    return S_OK;
}

static
STDMETHODIMP plugin_disconnected(IWTSPlugin *This, DWORD dwDisconnectCode)
{
    log_message("plugin_disconnected %p dwDisconnectCode=0x%08lx", This, dwDisconnectCode);
    return S_OK;
}

static
STDMETHODIMP plugin_terminated(IWTSPlugin *This)
{
    IWTSVirtualChannelManager *cm;
    ULONG refs;
    hs_exit();
    cm = channel_manager;
    channel_manager = NULL;
    refs = cm->lpVtbl->Release(cm);
    log_message("plugin_terminated %p channel manager %p references %lu", This, cm, refs);
    return S_OK;
}

static CONST_VTBL IWTSPluginVtbl plugin_vtbl = {
    .QueryInterface = plugin_query_interface,
    .AddRef = plugin_add_ref,
    .Release = plugin_release,
    .Initialize = plugin_initialize,
    .Connected = plugin_connected,
    .Disconnected = plugin_disconnected,
    .Terminated = plugin_terminated
};

static IWTSPlugin plugin = {
    .lpVtbl = &plugin_vtbl
};

struct channel_callback {
    IWTSVirtualChannelCallback iface;
    LONG volatile refs;
    HsStablePtr channel_callback;
};

QUERY_INTERFACE_IMP(IWTSVirtualChannelCallback, ccb_query_interface)

static
STDMETHODIMP_(ULONG) ccb_add_ref(IWTSVirtualChannelCallback *This)
{
    struct channel_callback *ccb = (struct channel_callback *) This;
    LONG refs = InterlockedIncrement(&ccb->refs);
    log_message("ccb_add_ref %p %ld", ccb, refs);
    return refs;
}

static
STDMETHODIMP_(ULONG) ccb_release(IWTSVirtualChannelCallback *This)
{
    struct channel_callback *ccb = (struct channel_callback *) This;
    LONG refs = InterlockedDecrement(&ccb->refs);
    log_message("ccb_release %p %ld", ccb, refs);
    if (refs == 0) {
        if (ccb->channel_callback != NULL)
            hs_free_stable_ptr(ccb->channel_callback);
        free(ccb);
    }
    return refs;
}

static
STDMETHODIMP ccb_on_data_received(IWTSVirtualChannelCallback *This, ULONG cbSize, BYTE *pBuffer)
{
    struct channel_callback *ccb = (struct channel_callback *) This;
    log_message("ccb_on_data_received %p cbSize=%lu pBuffer=%p", ccb, cbSize, pBuffer);
    if (wts_hs_data_received(ccb->channel_callback, pBuffer, cbSize) < 0)
        return E_UNEXPECTED;
    return S_OK;
}

static
STDMETHODIMP ccb_on_close(IWTSVirtualChannelCallback *This)
{
    struct channel_callback *ccb = (struct channel_callback *) This;
    log_message("ccb_on_close %p", ccb);
    if (wts_hs_closed(ccb->channel_callback) < 0)
        return E_UNEXPECTED;
    return S_OK;
}

static CONST_VTBL IWTSVirtualChannelCallbackVtbl channel_callback_vtbl = {
    .QueryInterface = ccb_query_interface,
    .AddRef = ccb_add_ref,
    .Release = ccb_release,
    .OnDataReceived = ccb_on_data_received,
    .OnClose = ccb_on_close
};

struct listener_callback {
    IWTSListenerCallback iface;
    LONG volatile refs;
    HsStablePtr listener;
};

QUERY_INTERFACE_IMP(IWTSListenerCallback, lcb_query_interface)

static
STDMETHODIMP_(ULONG) lcb_add_ref(IWTSListenerCallback *This)
{
    struct listener_callback *lcb = (struct listener_callback *) This;
    LONG refs = InterlockedIncrement(&lcb->refs);
    log_message("lcb_add_ref %p %ld", lcb, refs);
    return refs;
}

static
STDMETHODIMP_(ULONG) lcb_release(IWTSListenerCallback *This)
{
    struct listener_callback *lcb = (struct listener_callback *) This;
    LONG refs = InterlockedDecrement(&lcb->refs);
    log_message("lcb_release %p %ld", lcb, refs);
    if (refs == 0) {
        hs_free_stable_ptr(lcb->listener);
        free(lcb);
    }
    return refs;
}

static
STDMETHODIMP lcb_on_new_channel_connection(
    IWTSListenerCallback *This,
    IWTSVirtualChannel *pChannel,
    BSTR data,
    BOOL *pbAccept,
    IWTSVirtualChannelCallback **ppCallback
  )
{
    struct listener_callback *lcb;
    struct channel_callback *ccb;
    HsStablePtr channel_callback;
    lcb = (struct listener_callback *) This;
    log_message("lcb_on_new_channel_connection %p pChannel=%p", lcb, pChannel);
    ccb = malloc(sizeof(struct channel_callback));
    if (ccb == NULL) {
        log_message("lcb_on_new_channel_connection %p %p: malloc failed", lcb, pChannel);
        return E_OUTOFMEMORY;
    }
    log_message("lcb_on_new_channel_connection %p %p -> created %p", lcb, pChannel, ccb);
    ccb->iface.lpVtbl = &channel_callback_vtbl;
    ccb->refs = 1;
    ccb->channel_callback = NULL;
    if (wts_hs_new_channel_connection(lcb->listener, pChannel, &channel_callback) < 0) {
        ccb->iface.lpVtbl->Release(&ccb->iface);
        return E_UNEXPECTED;
    }
    if (channel_callback == NULL) {
        *pbAccept = FALSE;
        *ppCallback = NULL;
        ccb->iface.lpVtbl->Release(&ccb->iface);
        return S_OK;
    }
    ccb->channel_callback = channel_callback;
    *pbAccept = TRUE;
    *ppCallback = &ccb->iface;
    return S_OK;
}

static CONST_VTBL IWTSListenerCallbackVtbl listener_callback_vtbl = {
    .QueryInterface = lcb_query_interface,
    .AddRef = lcb_add_ref,
    .Release = lcb_release,
    .OnNewChannelConnection = lcb_on_new_channel_connection
};

int wts_create_listener(const char *channel_name, HsStablePtr listener)
{
    struct listener_callback *lcb;
    HRESULT hr;
    lcb = malloc(sizeof(struct listener_callback));
    if (lcb == NULL) {
        log_message("wts_create_listener %s: malloc failed", channel_name);
        hs_free_stable_ptr(listener);
        return -1;
    }
    log_message("wts_create_listener %s -> created %p", channel_name, lcb);
    lcb->iface.lpVtbl = &listener_callback_vtbl;
    lcb->refs = 1;
    lcb->listener = listener;
    hr = channel_manager->lpVtbl->CreateListener(channel_manager, channel_name, 0, &lcb->iface, NULL);
    lcb->iface.lpVtbl->Release(&lcb->iface);
    if (hr != S_OK) {
        log_message("wts_create_listener %s: channel manager failed for %p error 0x%08lx", channel_name, lcb, hr);
        return -1;
    }
    return 0;
}

void wts_ref_channel(IWTSVirtualChannel *channel)
{
    ULONG refs = channel->lpVtbl->AddRef(channel);
    log_message("wts_ref_channel %p refs %lu", channel, refs);
}

void wts_unref_channel(IWTSVirtualChannel *channel)
{
    ULONG refs = channel->lpVtbl->Release(channel);
    log_message("wts_unref_channel %p refs %lu", channel, refs);
}

int wts_write_channel(IWTSVirtualChannel *channel, void *bytes, ULONG len)
{
    HRESULT hr = channel->lpVtbl->Write(channel, len, bytes, NULL);
    if (hr != S_OK) {
        log_message("wts_write_channel %p error 0x%08lx", channel, hr);
        return -1;
    }
    return 0;
}

int wts_close_channel(IWTSVirtualChannel *channel)
{
    HRESULT hr = channel->lpVtbl->Close(channel);
    if (hr != S_OK) {
        log_message("wts_close_channel %p error 0x%08lx", channel, hr);
        return -1;
    }
    return 0;
}

STDAPI VirtualChannelGetInstance(REFIID refiid, ULONG *pNumObjs, VOID **ppObjArray)
{
    setup_std_handles();
    if (refiid == NULL) {
        log_message("VirtualChannelGetInstance refiid is NULL");
        return E_INVALIDARG;
    }
    if (!IsEqualIID(refiid, &IID_IWTSPlugin)) {
        char iid[IID_STRING_BUF_SIZE] = {0};
        iid_to_string(refiid, iid);
        log_message("VirtualChannelGetInstance unknown iid %s", iid);
        return E_NOINTERFACE;
    }
    if (pNumObjs == NULL) {
        log_message("VirtualChannelGetInstance pNumObjs is NULL");
        return E_POINTER;
    }
    if (ppObjArray != NULL) {
        if (*pNumObjs < 1) {
            log_message("VirtualChannelGetInstance *pNumObjs=%lu is too small", *pNumObjs);
            return E_INVALIDARG;
        }
        log_message("VirtualChannelGetInstance get plugin instance %p", &plugin);
        ppObjArray[0] = &plugin;
    } else
        log_message("VirtualChannelGetInstance get number of instances");
    *pNumObjs = 1;
    return S_OK;
}
