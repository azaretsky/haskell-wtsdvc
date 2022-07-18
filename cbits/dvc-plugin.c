#include "HsFFI.h"
#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <windows.h>
#include "wts-plugin-api.h"

extern int wts_hs_initialize(void);

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

static
STDMETHODIMP plugin_query_interface(IWTSPlugin *This, REFIID riid, void **ppvObject)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    if (riid == NULL) {
        log_message("plugin_query_interface riid is NULL");
        return E_INVALIDARG;
    }
    iid_to_string(riid, iid);
    log_message("plugin_query_interface %s", iid);
    if (!IsEqualIID(riid, &IID_IWTSPlugin) && !IsEqualIID(riid, &IID_IUnknown))
        return E_NOINTERFACE;
    if (ppvObject == NULL)
        return E_POINTER;
    *ppvObject = This;
    return S_OK;
}

static
STDMETHODIMP_(ULONG) plugin_add_ref(IWTSPlugin *This)
{
    log_message("plugin_add_ref");
    return 1;
}

static
STDMETHODIMP_(ULONG) plugin_release(IWTSPlugin *This)
{
    log_message("plugin_release");
    return 0;
}

static IWTSVirtualChannelManager *channel_manager = NULL;

static
STDMETHODIMP plugin_initialize(IWTSPlugin *This, IWTSVirtualChannelManager *pChannelMgr)
{
    ULONG refs;
    int argc;
    char *argv[] = {"wts-dvc-plugin", NULL};
    char **args = argv;
    refs = pChannelMgr->lpVtbl->AddRef(pChannelMgr);
    channel_manager = pChannelMgr;
    log_message("plugin_initialize channel manager references %lu", refs);
    hs_init(&argc, &args);
    if (!wts_hs_initialize()) {
        This->lpVtbl->Terminated(This);
        return E_UNEXPECTED;
    }
    return S_OK;
}

static
STDMETHODIMP plugin_connected(IWTSPlugin *This)
{
    log_message("plugin_connected");
    return S_OK;
}

static
STDMETHODIMP plugin_disconnected(IWTSPlugin *This, DWORD dwDisconnectCode)
{
    log_message("plugin_disconnected 0x%08lx", dwDisconnectCode);
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
    log_message("plugin_terminated channel manager references %lu", refs);
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

struct listener_callback {
    IWTSListenerCallback iface;
    LONG volatile refs;
    HsStablePtr listener;
};

static
STDMETHODIMP lcb_query_interface(IWTSListenerCallback *This, REFIID riid, void **ppvObject)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    if (riid == NULL) {
        log_message("lcb_query_interface %p riid is NULL", This);
        return E_INVALIDARG;
    }
    iid_to_string(riid, iid);
    log_message("lcb_query_interface %p %s", This, iid);
    if (!IsEqualIID(riid, &IID_IWTSListenerCallback) && !IsEqualIID(riid, &IID_IUnknown)) {
        log_message("lcb_query_interface %p: unknown interface", This);
        return E_NOINTERFACE;
    }
    if (ppvObject == NULL)
        return E_POINTER;
    This->lpVtbl->AddRef(This);
    *ppvObject = This;
    return S_OK;
}

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
    struct listener_callback *lcb = (struct listener_callback *) This;
    log_message("lcb_on_new_channel_connection %p pChannel=%p data=%p", lcb, pChannel, data);
    *pbAccept = FALSE;
    *ppCallback = NULL;
    return S_OK;
}


static CONST_VTBL IWTSListenerCallbackVtbl listener_callback_vtbl = {
    .QueryInterface = lcb_query_interface,
    .AddRef = lcb_add_ref,
    .Release = lcb_release,
    .OnNewChannelConnection = lcb_on_new_channel_connection
};

HsInt32 wts_create_listener(const char *channel_name, HsStablePtr listener)
{
    struct listener_callback *lcb;
    HRESULT hr;
    lcb = malloc(sizeof(struct listener_callback));
    if (lcb == NULL) {
        log_message("wts_create_listener \"%s\": malloc failed", channel_name);
        hs_free_stable_ptr(listener);
        return E_OUTOFMEMORY;
    }
    log_message("wts_create_listener \"%s\" -> created %p", channel_name, lcb);
    lcb->iface.lpVtbl = &listener_callback_vtbl;
    lcb->refs = 1;
    lcb->listener = listener;
    hr = channel_manager->lpVtbl->CreateListener(channel_manager, channel_name, 0, &lcb->iface, NULL);
    lcb->iface.lpVtbl->Release(&lcb->iface);
    return hr;
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
        log_message("VirtualChannelGetInstance get plugin instance");
        ppObjArray[0] = &plugin;
    } else
        log_message("VirtualChannelGetInstance get number of instances");
    *pNumObjs = 1;
    return S_OK;
}
