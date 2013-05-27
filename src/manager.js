importScripts('./WebSharper/WebSharper.js', './WebSharper/IntelliFactory.WebSharper.dll.js', './WebSharper/IntelliFactory.WebSharper.Collections.dll.js');
importScripts('gLong.js');
importScripts('output.js');


IntelliFactory.Runtime.Define(Dalvik, {
    ThreadWorker: {
        requestResource: function(r, cont) {
                            setTimeout(function() { Dalvik.Manager.processRequest(r, undefined, cont); }, 0);
                         }
    }
});


IntelliFactory.Runtime.Start();
Dalvik.Native.init();

var interactive = self;
// First message is from the interactivity manager
self.onmessage = function(e) {
    var tag = e.data[0];
    var d = e.data[1];

    if (tag == 'load') {
        Dalvik.Manager.init()(d);
    } else if (tag == 'main') {
        exec(d['class'], "main", "V", ["[Ljava/lang/String;"], [])
    } else if (tag == 'exec') {
        exec(d['class'], "exec", "V", d['sigargs'], d['args'])
    };
};

function exec(cls, mname, sigret, sigargs, args) {
    var r = Dalvik.Native.genSignature("L" + cls + ";", mname, sigret, sigargs);
    var c = Dalvik.Manager.library().get_Item(r.$0);
    var impl = Dalvik.Runtime.getMethodImpl(c.$0, true, r);

    var chan = new MessageChannel();

    chan.port1.onmessage = (function(c) {
        return function(e) {
            Dalvik.Manager.processRequest(e.data, c, function(reply) {
                c.port1.postMessage(reply);
        })}
    })(chan);

    self.postMessage(['spawn', [[r.$0, r, impl.$0], args]], [chan.port2])
}
