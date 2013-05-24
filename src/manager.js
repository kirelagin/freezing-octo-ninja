importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');


IntelliFactory.Runtime.Define(Dalvik, {
    ThreadWorker: {
        requestResource: function(r, cont) {
                            setTimeout(function() { Dalvik.Manager.processRequest(r, cont); }, 0);
                         }
    }
});


IntelliFactory.Runtime.Start();

var interactive = self;
// First message is from the interactivity manager
self.onmessage = function(e) {
    self.onmessage = undefined;
    Dalvik.Manager.init()(e.data);

    Dalvik.Native.init();
    r = Dalvik.Native.genSignature("LMain;", "main", "V", ["[Ljava/lang/String;"])
    c = Dalvik.Manager.library().get_Item(r.$0)
    impl = Dalvik.Runtime.getMethodImpl(c.$0, true, r)

    chan = new MessageChannel()

    chan.port1.onmessage = function(e) {
        Dalvik.Manager.processRequest(e.data, function(reply) {
            chan.port1.postMessage(reply);
        });
    };

    self.postMessage(['spawn', [r.$0, r, impl.$0]], [chan.port2])
};
