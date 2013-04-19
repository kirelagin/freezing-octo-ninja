importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');

var manager;

function unexpected(e) {
    throw "Unexpected message from manager";
}

IntelliFactory.Runtime.Define(Dalvik, {
    ThreadWorker: {
        requestResource: function(r, cont) {
                            manager.onmessage = function(m) {
                                manager.onmessage = unexpected;
                                cont(m.data);
                            }
                            manager.postMessage(r);
                         }
    }
});


IntelliFactory.Runtime.Start();


//self.onmessage = function(e) {
//    manager = e.ports[0];
//    manager.onmessage = unexpected;
//}
manager = self; // not literally `self`, but parent
manager.onmessage = function(e) {
    manager.onmessage = unexpected;
    Dalvik.ThreadWorker.init(e.data, []);
}
