importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');


IntelliFactory.Runtime.Define(Dalvik, {
    ThreadWorker: {
        requestResource: function(r, cont) {
                            cont(Dalvik.Manager.processRequest(r));
                         }
    }
});


IntelliFactory.Runtime.Start();


self.onconnect = function(e) {
    var port = e.ports[0];
    port.onmessage = function(e) {
        Dalvik.Manager.init(e.data);
        port.onmessage = function(e) {
            Dalvik.Manager.processRequest(e.data, function(reply) {
                port.postMessage(reply);
            });
        }
    }
}
