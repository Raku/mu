function DISPATCH_from_methods(interpreter,identifier,capture) {
    this[identifier.value](interpreter,capture);
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

function P6Mold(regs,constants,code) {
    this.constants = constants;
    this.code = code;
}
P6Mold.prototype.DISPATCH = DISPATCH_from_methods;

function P6capture(positional,named) {
    this.positional = positional;
    this.named = named;
}
P6capture.prototype.DISPATCH = DISPATCH_from_methods;

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
        interpreter.continuation.DISPATCH(interpreter,new P6Str("setr"),new P6capture([interpreter.continuation,this.entries[key]],[]));
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

var SMOP__S1P__LexicalPrelude = new P6LexPad();
