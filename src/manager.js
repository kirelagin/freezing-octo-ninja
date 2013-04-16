importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');

IntelliFactory.Runtime.Start();


self.onconnect = function(e) {
    var port = e.ports[0];
    port.onmessage = function(e) {
        Dalvik.Manager.init(e.data);
        port.onmessage = function(e) {
            var reply = Dalvik.Manager.processRequest(e.data);
            port.postMessage(reply);
        }
    }
}
