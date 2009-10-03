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
P6Str.prototype.DISPATCH = DISPATCH_from_methods;


function P6LexPad() {
    this.entries = {};
}
P6LexPad.prototype.DISPATCH = DISPATCH_from_methods;
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture.positional[1].value;
    if (this.entries[key]) {
        setr(interpreter,this.entries[key]);
        //print("found ",key);
    } else {
        print("not found",key);
    }
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

function P6AdhocSignature() {
}
init_type(P6AdhocSignature);
P6AdhocSignature.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6AdhocSignature);
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

function P6say() {
}
init_type(P6say);
P6say.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    print(capture.positional[1].positional[0].value);
}

var SMOP__S1P__LexicalPrelude = new P6LexPad();
SMOP__S1P__LexicalPrelude.entries.AdhocSignature = new P6AdhocSignature;
SMOP__S1P__LexicalPrelude.entries.Code = new P6Code;
SMOP__S1P__LexicalPrelude.entries.capture = new P6capture([],[]);
SMOP__S1P__LexicalPrelude.entries['&say'] = new P6say;
