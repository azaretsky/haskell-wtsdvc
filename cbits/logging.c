#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <windows.h>

static
void tweak_std_handle(DWORD nStdHandle, int desc, FILE *stream, int flags)
{
    int fd;
    fd = _open_osfhandle((intptr_t) GetStdHandle(nStdHandle), flags);
    if (fd == -1)
        return;
    if (_dup2(fd, desc) != -1)
        stream->_file = desc;
    _close(fd);
}

static
void setup_std_handles(void)
{
    if (!AttachConsole(ATTACH_PARENT_PROCESS)) {
        return;
    }
    tweak_std_handle(STD_INPUT_HANDLE, 0, stdin, _O_RDONLY);
    tweak_std_handle(STD_OUTPUT_HANDLE, 1, stdout, _O_APPEND);
    tweak_std_handle(STD_ERROR_HANDLE, 2, stderr, _O_APPEND);
}

BOOL WINAPI DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID lpvReserved)
{
    switch (fdwReason) {
    case DLL_PROCESS_ATTACH:
        setup_std_handles();
        break;
    case DLL_THREAD_ATTACH:
        break;
    case DLL_THREAD_DETACH:
        break;
    case DLL_PROCESS_DETACH:
        break;
    }
    return TRUE;
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

void log_qi_null_riid(const char *iface, void *This)
{
    log_message("%s::QueryInterface %p riid is NULL", iface, This);
}

void log_qi_unknown_iid(const char *iface, void *This, REFIID riid)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    iid_to_string(riid, iid);
    log_message("%s::QueryInterface %p unknown iid %s", iface, This, iid);
}

void log_qi_null_ppvobject(const char *iface, void *This, REFIID riid)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    iid_to_string(riid, iid);
    log_message("%s::QueryInterface %p %s ppvObject is NULL", iface, This, iid);
}

void log_qi(const char *iface, void *This, REFIID riid)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    iid_to_string(riid, iid);
    log_message("%s::QueryInterface %p %s", iface, This, iid);
}

void log_addref(const char *iface, void *This, LONG refs)
{
    log_message("%s::AddRef %p %ld", iface, This, refs);
}

void log_release(const char *iface, void *This, LONG refs)
{
    log_message("%s::Release %p %ld", iface, This, refs);
}

void log_plugin_initialize(void *This, void *pChannelMgr, ULONG refs)
{
    log_message("IWTSPlugin::Initialize %p channel manager %p references %lu", This, pChannelMgr, refs);
}

void log_plugin_connected(void *This)
{
    log_message("IWTSPlugin::Connected %p", This);
}

void log_plugin_disconnected(void *This, DWORD dwDisconnectCode)
{
    log_message("IWTSPlugin::Disconnected %p dwDisconnectCode=0x%08lx", This, dwDisconnectCode);
}

void log_plugin_terminated(void *This, void *cm, ULONG refs)
{
    log_message("IWTSPlugin::Terminated %p channel manager %p references %lu", This, cm, refs);
}

void log_data_received(void *This, ULONG cbSize, BYTE *pBuffer)
{
    log_message("IWTSVirtualChannelCallback::OnDataReceived %p cbSize=%lu pBuffer=%p", This, cbSize, pBuffer);
}

void log_closed(void *This)
{
    log_message("IWTSVirtualChannelCallback::OnClose %p", This);
}

void log_new_connection_malloc_failed(void *This, void *pChannel)
{
    log_message("IWTSListenerCallback::OnNewChannelConnection %p pChannel=%p: malloc failed", This, pChannel);
}

void log_new_connection(void *This, void *pChannel, void *ccb)
{
    log_message("IWTSListenerCallback::OnNewChannelConnection %p pChannel=%p -> %p", This, pChannel, ccb);
}

void log_create_listener_malloc_failed(const char *channel_name, void *listener)
{
    log_message("wts_create_listener %s listener=%p: malloc failed", channel_name, listener);
}

void log_create_listener(const char *channel_name, void *listener, void *lcb)
{
    log_message("wts_create_listener %s listener=%p -> %p", channel_name, listener, lcb);
}

void log_create_listener_channel_manager_error(const char *channel_name, void *lcb, HRESULT hr)
{
    log_message("wts_create_listener %s (%p): channel manager error 0x%08lx", channel_name, lcb, hr);
}

void log_write_channel(void *channel, BYTE *bytes, ULONG len, HRESULT hr)
{
    log_message("wts_write_channel %p bytes=%p len=%lu -> 0x%08lx", channel, bytes, len, hr);
}

void log_close_channel(void *channel, HRESULT hr)
{
    log_message("wts_close_channel %p -> 0x%08lx", channel, hr);
}

void log_vcgi_null_refiid(void)
{
    log_message("VirtualChannelGetInstance refiid is NULL");
}

void log_vcgi_unknown_iid(REFIID refiid)
{
    char iid[IID_STRING_BUF_SIZE] = {0};
    iid_to_string(refiid, iid);
    log_message("VirtualChannelGetInstance unknown iid %s", iid);
}

void log_vcgi_null_pnumobjs(void)
{
    log_message("VirtualChannelGetInstance pNumObjs is NULL");
}

void log_vcgi_too_small_numobjs(ULONG num)
{
    log_message("VirtualChannelGetInstance *pNumObjs=%lu is too small", num);
}

void log_vcgi_get(void *inst)
{
    log_message("VirtualChannelGetInstance get plugin instance %p", inst);
}

void log_vcgi_count(void)
{
    log_message("VirtualChannelGetInstance get number of instances");
}
