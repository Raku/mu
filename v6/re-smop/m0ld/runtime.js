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
        print("found ",key);
    } else {
        print("not found",key);
    }
}
var SMOP__S1P__LexicalPrelude = new P6LexPad();
