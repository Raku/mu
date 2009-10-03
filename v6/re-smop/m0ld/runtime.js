function init_type(type) {
    type.prototype.DISPATCH = DISPATCH_from_methods;
    type.prototype.FETCH = FETCH;
}
function DISPATCH_from_methods(interpreter,identifier,capture) {
    this[identifier.value](interpreter,capture);
}
function FETCH(interpreter,capture) {
    setr(interpreter,this);
}
function setr(interpreter,val) {
        interpreter.continuation.DISPATCH(interpreter,new P6Str("setr"),new P6capture([interpreter.continuation,val],[]));
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
    print("in eval");
    this.mold.code(interpreter,this)
}
P6Frame.prototype.setr = function(interpreter,capture) {
    this.reg[this.ret] = capture.positional[1];
}

function P6Mold(regs,constants,code) {
    this.constants = constants;
    this.code = code;
}
P6Mold.prototype.DISPATCH = DISPATCH_from_methods;

function P6capture(positional,named) {
    this.positional = positional;
    this.named = named;
}
init_type(P6capture);
P6capture.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6capture([],[]));
}

function P6Str(str) {
    this.value = str;
}
P6Str.prototype.DISPATCH = DISPATCH_from_methods;


function P6LexPad() {
    this.entries = {};
}
P6LexPad.prototype.DISPATCH = DISPATCH_from_methods;
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture.positional[1].value;
    if (this.entries[key]) {
        setr(interpreter,this.entries[key]);
        print("found ",key);
    } else {
        print("not found",key);
    }
}
function P6Interpreter () {
    this.continuation = undefined;
}
P6Interpreter.prototype.DISPATCH = DISPATCH_from_methods;
P6Interpreter.prototype.loop = function(interpreter,capture) {
    while (this.continuation) this.continuation.DISPATCH(this,new P6Str("eval"),new P6capture([this.continuation],[]));
}
P6Interpreter.prototype['goto'] = function(interpreter,capture) {                                            this.continuation = capture.positional[1];
}

function P6AdhocSignature() {
}
init_type(P6AdhocSignature);
P6AdhocSignature.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6AdhocSignature);
}

function P6Code() {
}
init_type(P6Code);
P6Code.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6Code);
}
P6Code.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    print("postcircumfix:( )");
}



var SMOP__S1P__LexicalPrelude = new P6LexPad();
SMOP__S1P__LexicalPrelude.entries.AdhocSignature = new P6AdhocSignature;
SMOP__S1P__LexicalPrelude.entries.Code = new P6Code;
SMOP__S1P__LexicalPrelude.entries.capture = new P6capture;
