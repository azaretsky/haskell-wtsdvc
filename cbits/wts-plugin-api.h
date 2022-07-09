#define IUNKNOWN_METHODS \
    STDMETHOD(QueryInterface)(THIS_ REFIID riid, void **ppvObject) PURE; \
    STDMETHOD_(ULONG, AddRef)(THIS) PURE; \
    STDMETHOD_(ULONG, Release)(THIS) PURE;

#undef INTERFACE
#define INTERFACE IWTSVirtualChannel
DECLARE_INTERFACE(IWTSVirtualChannel) {
    IUNKNOWN_METHODS
    STDMETHOD(Write)(THIS_ ULONG cbSize, BYTE *pBuffer, IUnknown *pReserved) PURE;
    STDMETHOD(Close)(THIS) PURE;
};

static const IID IID_IWTSVirtualChannelCallback = {0xa1230204, 0xd6a7, 0x11d8, {0xb9, 0xfd, 0x00, 0x0b, 0xdb, 0xd1, 0xf1, 0x98}};

#undef INTERFACE
#define INTERFACE IWTSVirtualChannelCallback
DECLARE_INTERFACE(IWTSVirtualChannelCallback) {
    IUNKNOWN_METHODS
    STDMETHOD(OnDataReceived)(THIS_ ULONG cbSize, BYTE *pBuffer) PURE;
    STDMETHOD(OnClose)(THIS) PURE;
};

static const IID IID_IWTSListenerCallback = {0xa1230203, 0xd6a7, 0x11d8, {0xb9, 0xfd, 0x00, 0x0b, 0xdb, 0xd1, 0xf1, 0x98}};

#undef INTERFACE
#define INTERFACE IWTSListenerCallback
DECLARE_INTERFACE(IWTSListenerCallback) {
    IUNKNOWN_METHODS
    STDMETHOD(OnNewChannelConnection)(THIS_ IWTSVirtualChannel *pChannel, BSTR data, BOOL *pbAccept, IWTSVirtualChannelCallback **ppCallback) PURE;
};

#undef INTERFACE
#define INTERFACE IWTSListener
DECLARE_INTERFACE(IWTSListener) {
    IUNKNOWN_METHODS
    STDMETHOD(GetConfiguration)(THIS_ IPropertyBag **ppPropertyBag) PURE;
};

#undef INTERFACE
#define INTERFACE IWTSVirtualChannelManager
DECLARE_INTERFACE(IWTSVirtualChannelManager) {
    IUNKNOWN_METHODS
    STDMETHOD(CreateListener)(THIS_ const char *pszChannelName, ULONG uFlags, IWTSListenerCallback *pListenerCallback, IWTSListener **ppListener) PURE;
};

static const IID IID_IWTSPlugin = {0xa1230201, 0x1439, 0x4e62, {0xa4, 0x14, 0x19, 0x0d, 0x0a, 0xc3, 0xd4, 0x0e}};

#undef INTERFACE
#define INTERFACE IWTSPlugin
DECLARE_INTERFACE(IWTSPlugin) {
    IUNKNOWN_METHODS
    STDMETHOD(Initialize)(THIS_ IWTSVirtualChannelManager *pChannelMgr) PURE;
    STDMETHOD(Connected)(THIS) PURE;
    STDMETHOD(Disconnected)(THIS_ DWORD dwDisconnectCode) PURE;
    STDMETHOD(Terminated)(THIS) PURE;
};

__declspec(dllexport) STDAPI VirtualChannelGetInstance(REFIID refiid, ULONG *pNumObjs, VOID **ppObjArray);
