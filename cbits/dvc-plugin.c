#include "HsFFI.h"
#include <windows.h>
#include "logging.h"
#include "wts-plugin-api.h"

extern int wts_hs_initialize(void);
extern int wts_hs_new_channel_connection(HsStablePtr, IWTSVirtualChannel *, int *, HsStablePtr *);
extern int wts_hs_data_received(HsStablePtr, const void *, ULONG);
extern int wts_hs_closed(HsStablePtr);

#define INVALID_STABLE_PTR ((HsStablePtr) -1)

#define QUERY_INTERFACE_IMP(iface, name) \
    static \
    STDMETHODIMP name(iface *This, REFIID riid, void **ppvObject) \
    { \
        if (riid == NULL) { \
            log_qi_null_riid(#iface, This); \
            return E_INVALIDARG; \
        } \
        if (!IsEqualIID(riid, &IID_##iface) && !IsEqualIID(riid, &IID_IUnknown)) { \
            log_qi_unknown_iid(#iface, This, riid); \
            return E_NOINTERFACE; \
        } \
        if (ppvObject == NULL) { \
            log_qi_null_ppvobject(#iface, This, riid); \
            return E_POINTER; \
        } \
        log_qi(#iface, This, riid); \
        This->lpVtbl->AddRef(This); \
        *ppvObject = This; \
        return S_OK; \
    }

QUERY_INTERFACE_IMP(IWTSPlugin, plugin_query_interface)

static LONG volatile plugin_refs = 1;

static
STDMETHODIMP_(ULONG) plugin_add_ref(IWTSPlugin *This)
{
    LONG refs = InterlockedIncrement(&plugin_refs);
    log_addref("IWTSPlugin", This, refs);
    return 1;
}

static
STDMETHODIMP_(ULONG) plugin_release(IWTSPlugin *This)
{
    LONG refs = InterlockedDecrement(&plugin_refs);
    log_release("IWTSPlugin", This, refs);
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
    log_plugin_initialize(This, pChannelMgr, refs);
    hs_init(&argc, &args);
    if (wts_hs_initialize() < 0)
        return E_UNEXPECTED;
    return S_OK;
}

static
STDMETHODIMP plugin_connected(IWTSPlugin *This)
{
    log_plugin_connected(This);
    return S_OK;
}

static
STDMETHODIMP plugin_disconnected(IWTSPlugin *This, DWORD dwDisconnectCode)
{
    log_plugin_disconnected(This, dwDisconnectCode);
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
    log_plugin_terminated(This, cm, refs);
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
    log_addref("IWTSVirtualChannelCallback", ccb, refs);
    return refs;
}

static
STDMETHODIMP_(ULONG) ccb_release(IWTSVirtualChannelCallback *This)
{
    struct channel_callback *ccb = (struct channel_callback *) This;
    LONG refs = InterlockedDecrement(&ccb->refs);
    log_release("IWTSVirtualChannelCallback", ccb, refs);
    if (refs == 0) {
        if (ccb->channel_callback != INVALID_STABLE_PTR)
            hs_free_stable_ptr(ccb->channel_callback);
        free(ccb);
    }
    return refs;
}

static
STDMETHODIMP ccb_on_data_received(IWTSVirtualChannelCallback *This, ULONG cbSize, BYTE *pBuffer)
{
    log_data_received(This, cbSize, pBuffer);
    if (wts_hs_data_received(((struct channel_callback *) This)->channel_callback, pBuffer, cbSize) < 0)
        return E_UNEXPECTED;
    return S_OK;
}

static
STDMETHODIMP ccb_on_close(IWTSVirtualChannelCallback *This)
{
    log_closed(This);
    if (wts_hs_closed(((struct channel_callback *) This)->channel_callback) < 0)
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
    log_addref("IWTSListenerCallback", lcb, refs);
    return refs;
}

static
STDMETHODIMP_(ULONG) lcb_release(IWTSListenerCallback *This)
{
    struct listener_callback *lcb = (struct listener_callback *) This;
    LONG refs = InterlockedDecrement(&lcb->refs);
    log_release("IWTSListenerCallback", lcb, refs);
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
    int accept;
    lcb = (struct listener_callback *) This;
    ccb = malloc(sizeof(struct channel_callback));
    if (ccb == NULL) {
        log_new_connection_malloc_failed(lcb, pChannel);
        return E_OUTOFMEMORY;
    }
    log_new_connection(lcb, pChannel, ccb);
    ccb->iface.lpVtbl = &channel_callback_vtbl;
    ccb->refs = 1;
    ccb->channel_callback = INVALID_STABLE_PTR;
    if (wts_hs_new_channel_connection(lcb->listener, pChannel, &accept, &channel_callback) < 0) {
        ccb->iface.lpVtbl->Release(&ccb->iface);
        return E_UNEXPECTED;
    }
    if (!accept) {
        ccb->iface.lpVtbl->Release(&ccb->iface);
        *pbAccept = FALSE;
        *ppCallback = NULL;
    } else {
        ccb->channel_callback = channel_callback;
        *pbAccept = TRUE;
        *ppCallback = &ccb->iface;
    }
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
        log_create_listener_malloc_failed(channel_name, listener);
        hs_free_stable_ptr(listener);
        return -1;
    }
    log_create_listener(channel_name, listener, lcb);
    lcb->iface.lpVtbl = &listener_callback_vtbl;
    lcb->refs = 1;
    lcb->listener = listener;
    hr = channel_manager->lpVtbl->CreateListener(channel_manager, channel_name, 0, &lcb->iface, NULL);
    lcb->iface.lpVtbl->Release(&lcb->iface);
    if (hr != S_OK) {
        log_create_listener_channel_manager_error(channel_name, lcb, hr);
        return -1;
    }
    return 0;
}

void wts_ref_channel(IWTSVirtualChannel *channel)
{
    ULONG refs = channel->lpVtbl->AddRef(channel);
    log_addref("IWTSVirtualChannel", channel, refs);
}

void wts_unref_channel(IWTSVirtualChannel *channel)
{
    ULONG refs = channel->lpVtbl->Release(channel);
    log_release("IWTSVirtualChannel", channel, refs);
}

int wts_write_channel(IWTSVirtualChannel *channel, void *bytes, ULONG len)
{
    HRESULT hr = channel->lpVtbl->Write(channel, len, bytes, NULL);
    log_write_channel(channel, bytes, len, hr);
    return (hr == S_OK) ? 0 : -1;
}

int wts_close_channel(IWTSVirtualChannel *channel)
{
    HRESULT hr = channel->lpVtbl->Close(channel);
    log_close_channel(channel, hr);
    return (hr == S_OK) ? 0 : -1;
}

STDAPI VirtualChannelGetInstance(REFIID refiid, ULONG *pNumObjs, VOID **ppObjArray)
{
    if (refiid == NULL) {
        log_vcgi_null_refiid();
        return E_INVALIDARG;
    }
    if (!IsEqualIID(refiid, &IID_IWTSPlugin)) {
        log_vcgi_unknown_iid(refiid);
        return E_NOINTERFACE;
    }
    if (pNumObjs == NULL) {
        log_vcgi_null_pnumobjs();
        return E_POINTER;
    }
    if (ppObjArray != NULL) {
        ULONG num = *pNumObjs;
        if (num < 1) {
            log_vcgi_too_small_numobjs(num);
            return E_INVALIDARG;
        }
        log_vcgi_get(&plugin);
        ppObjArray[0] = &plugin;
    } else
        log_vcgi_count();
    *pNumObjs = 1;
    return S_OK;
}
