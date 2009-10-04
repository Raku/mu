function init_type(type) {
    type.prototype.DISPATCH = DISPATCH_from_methods;
    type.prototype.FETCH = FETCH;
}
function DISPATCH_from_methods(interpreter,identifier,capture) {
    if (this[identifier.value]) {
        this[identifier.value](interpreter,capture);
    } else {
        throw 'no method '+identifier.value; 
    }
}
function FETCH(interpreter,capture) {
    setr(interpreter,this);
}
function setr(interpreter,val) {
        interpreter._continuation.DISPATCH(interpreter,new P6Str("setr"),new P6capture([interpreter._continuation,val],[]));
}
function P6Frame(mold) {
    this.mold = mold;
    this.reg = [];
    for (i in this.mold.constants) {
        this.reg[i] = this.mold.constants[i];
    }
    this.pc = 0;
}
P6Frame.prototype.DISPATCH = DISPATCH_from_methods;
P6Frame.prototype.eval = function(interpreter,capture) {
//    print("in eval");
    this.mold.code(interpreter,this)
    if (this.pc == -1) interpreter._continuation = undefined;
}
P6Frame.prototype.setr = function(interpreter,capture) {
    this.reg[this.ret] = capture.positional[1];
}
P6Frame.prototype.set_back = function(interpreter,capture) {
    this._back = capture.positional[1];
}
P6Frame.prototype.set_control = function(interpreter,capture) {
    this._control = capture.positional[1];
}
P6Frame.prototype.back = function(interpreter,capture) {
    setr(interpreter,this._back);
}

function P6Mold(regs,constants,code) {
    this.constants = constants;
    this.code = code;
}
P6Mold.prototype.DISPATCH = DISPATCH_from_methods;

function P6capture(positional,named) {
    this.positional = positional;
    var i=0;
    this.named = {};
    for (var i=0;i < named.length;i += 2) {
        this.named[named[i].value] = named[i+1];
    }
}
init_type(P6capture);
P6capture.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6capture(capture.positional.slice(1),capture.named));
}

function P6Str(str) {
    this.value = str;
}
init_type(P6Str);

function P6Int(i) {
    this.value = i;
}
init_type(P6Int);

function P6LexPad() {
    this.entries = {};
}
init_type(P6LexPad);
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture.positional[1].value;
    if (this.entries[key]) {
        setr(interpreter,this.entries[key]);
        //print("found ",key);
    } else {
        throw "Could not find variable "+key+" in the lexical scope.";
    }
}
P6LexPad.prototype['postcircumfix:{ }'] = function(interpreter,capture) {
    var key = capture.positional[1].value;
    setr(interpreter,new P6BValue(this,key));

}
function P6BValue(lexpad,key) {
    this.lexpad = lexpad;
    this.key = key
}
init_type(P6BValue);
P6BValue.prototype.BIND = function(interpreter,capture) {
    this.lexpad.entries[this.key] = capture.positional[1];
    setr(interpreter,this);
}
P6BValue.prototype.FETCH = function(interpreter,capture) {
    if (!this.lexpad.entries[this.key]) this.lexpad.entries[this.key] = new P6Scalar;
    var entry = this.lexpad.entries[this.key];
    print("FETCH from:",entry.ID);
    entry.DISPATCH(interpreter,new P6Str("FETCH"),new P6capture([entry],[]));
}
P6BValue.prototype.STORE = function(interpreter,capture) {
    if (!this.lexpad.entries[this.key]) this.lexpad.entries[this.key] = new P6Scalar;
    var entry = this.lexpad.entries[this.key];
    entry.DISPATCH(interpreter,new P6Str("STORE"),new P6capture([entry,capture.positional[1]],[]));
}


function set_reg(frame,i,value) {
    frame.reg[frame.mold.constants.length+i] = value;
}
function P6Interpreter () {
    this._continuation = undefined;
}
P6Interpreter.prototype.DISPATCH = DISPATCH_from_methods;
P6Interpreter.prototype.loop = function(interpreter,capture) {
    while (this._continuation) this._continuation.DISPATCH(this,new P6Str("eval"),new P6capture([this._continuation],[]));
}
P6Interpreter.prototype['goto'] = function(interpreter,capture) {
    this._continuation = capture.positional[1];
}
P6Interpreter.prototype.continuation = function(interpreter,capture) {
    setr(interpreter,this._continuation);
}


function P6Code(mold) {
    this.mold = mold
}
init_type(P6Code);
P6Code.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6Code(capture.named.mold));
}
P6Code.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    var frame = new P6Frame(this.mold);
    set_reg(frame,0,interpreter);
    set_reg(frame,1,SMOP__S1P__LexicalPrelude);
    frame._back = interpreter._continuation;
    interpreter.DISPATCH(interpreter,new P6Str("goto"),new P6capture([interpreter,frame],[]));
}


function JSFunction(func) {
    this.func = func;
}
init_type(JSFunction);

JSFunction.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    this.func(interpreter,capture.positional[1]);
}

var SMOP__S1P__LexicalPrelude = new P6LexPad();
;
SMOP__S1P__LexicalPrelude.entries.Code = new P6Code;
SMOP__S1P__LexicalPrelude.entries.capture = new P6capture([],[]);

function builtin(name,func) {
    SMOP__S1P__LexicalPrelude.entries[name] = new JSFunction(func);
}
builtin('&say',function(interpreter,capture) {
    var str = '';
    for (var i in capture.positional) str += capture.positional[i].value;
    print(str);
});
builtin('&infix:~',function(interpreter,capture) {
    setr(interpreter,new P6Str(capture.positional[0].value + capture.positional[1].value));
});
function define_type(name,methods) {
    var proto = function() {
    }
    proto.prototype.ID = name + ' protobject';
    init_type(proto);
    proto.prototype['new'] = methods['new'];

    var constructor = function() {
    }
    init_type(constructor);
    constructor.prototype.ID = name;
    for (var m in methods) {
        constructor.prototype[m] = methods[m];
    }
    SMOP__S1P__LexicalPrelude.entries[name] = new proto;
    return constructor;
}

var P6AdhocSignature = define_type('AdhocSignature',{
    'new': function(interpreter,capture) {
        setr(interpreter,new P6AdhocSignature);
    }
});
var P6Scalar = define_type('Scalar',{
    'new': function(interpreter,capture) {
        setr(interpreter,new P6Scalar);
    },
    FETCH: function(interpreter,capture) {
        print("FETCH:",this.container.value);
        setr(interpreter,this.container);
    },
    STORE: function(interpreter,capture) {
        this.container = capture.positional[1];
    }
});

