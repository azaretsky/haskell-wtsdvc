#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <windows.h>
#include "wts-plugin-api.h"

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

static
void iid_to_string(const REFIID iid, char out[39])
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
    char iid[39] = {0};
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
    refs = pChannelMgr->lpVtbl->AddRef(pChannelMgr);
    channel_manager = pChannelMgr;
    log_message("plugin_initialize channel manager references %lu", refs);
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

STDAPI VirtualChannelGetInstance(REFIID refiid, ULONG *pNumObjs, VOID **ppObjArray)
{
    setup_std_handles();
    if (refiid == NULL) {
        log_message("VirtualChannelGetInstance refiid is NULL");
        return E_INVALIDARG;
    }
    if (!IsEqualIID(refiid, &IID_IWTSPlugin)) {
        char iid[39] = {0};
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
