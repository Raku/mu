
if (Perl6 == undefined)       var Perl6           = {};
if (Perl6.MetaClass == undefined) Perl6.MetaClass = function () {};

Perl6.MetaClass.Dispatcher = function (metaclass, order) {
    if (order == undefined)    order = ':canonical';
    if (order == ':canonical') order = ':ascendant';
    var dispatcher;
    if (order == ':ascendant') {
        dispatcher = _make_ascendant_dispatcher(metaclass);
    }
    else {
        throw 'Unsupported dispatch order';
    }
    this.i = dispatcher;
}

Perl6.MetaClass.Dispatcher.prototype.next = function () { return this.i(); }

function _make_iterator (values) {
    var counter = 0;
    return function () {
        if (counter >= values.length) return undefined;
        return values[counter++];
    }
}

function _make_ascendant_dispatcher (metaclass) {
    return _make_iterator(metaclass.MRO());
}