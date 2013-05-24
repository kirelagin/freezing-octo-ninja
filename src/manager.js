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

var interactive;
self.onconnect = function(e) {
    // First connection is from the interactivity manager
    interactive = e.ports[0];
    interactive.onmessage = function(e) {
        interactive.onmessage = undefined;
        Dalvik.Manager.init()(e.data);
        self.onconnect = function(e) {
            var port = e.ports[0];
            port.onmessage = function(e) {
                Dalvik.Manager.processRequest(e.data, function(reply) {
                    port.postMessage(reply);
                });
            };
            interactive.postMessage(["log", "memory manager connection"]);
        };
        interactive.postMessage(["log", "memory manager ready"]);
    };
};
