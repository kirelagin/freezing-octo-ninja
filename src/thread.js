importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');

var manager;
var interactive;

function unexpected(e) {
    throw "Unexpected message from manager";
}

IntelliFactory.Runtime.Define(Dalvik, {
    ThreadWorker: {
        requestResource:    function(r, cont) {
                               manager.onmessage = function(m) {
                                   manager.onmessage = unexpected;
                                   cont(m.data);
                               }
                               manager.postMessage(r);
                            },
        requestInteraction: function(r, cont) {
                                interactive.postMessage(r);
                                setTimeout(cont, 0);
                            }
    }
});


IntelliFactory.Runtime.Start();

Dalvik.Native.init();


interactive = self; // not literally `self`, but parent
interactive.onmessage = function(e) {
    manager = e.data[0];
    manager.start();
    manager.onmessage = unexpected;
    Dalvik.ThreadWorker.init(e.data[1], []);
}
