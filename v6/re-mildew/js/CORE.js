var P6Scalar;
var lexical_prelude = {};
function init_type(type) {
    type.prototype.DISPATCH = DISPATCH_from_methods;
    type.prototype.FETCH = FETCH;
}
function DISPATCH_from_methods(interpreter,identifier,capture) {
//    print(this.ID+'.'+identifier.value,"\n");
    if (this[identifier.value]) {
        this[identifier.value](interpreter,capture);
    } else {
        throw 'no method '+identifier.value+' in '+this.ID; 
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
    this.reg[this.ret] = capture._positional[1];
}
P6Frame.prototype.set_back = function(interpreter,capture) {
    this._back = capture._positional[1];
}
P6Frame.prototype.set_lexical = function(interpreter,capture) {
    this._lexical = capture._positional[1];
}
P6Frame.prototype.set_control = function(interpreter,capture) {
    this._control = capture._positional[1];
}
P6Frame.prototype.set_regs = function(interpreter,capture) {
    for (var i=1;i < capture._positional.length;i++) {
        set_reg(this,i-1,capture._positional[i]);
    }
}
P6Frame.prototype.back = function(interpreter,capture) {
    setr(interpreter,this._back);
}

function P6Mold(regs,constants,code) {
    this.constants = constants;
    this.code = code;
}
P6Mold.prototype.DISPATCH = DISPATCH_from_methods;

P6Mold.prototype.create = function(interpreter,capture) {
    setr(interpreter,new P6Frame(this));
}

function P6capture(positional,named) {
    this._positional = positional;
    var i=0;
    this._named = {};
    for (var i=0;i < named.length;i += 2) {
        this._named[named[i].value] = named[i+1];
    }
}
init_type(P6capture);
P6capture.prototype['new'] = function(interpreter,capture) {
    setr(interpreter,new P6capture(capture._positional.slice(1),capture._named));
}
P6capture.prototype['positional'] = function(interpreter,capture) {
    setr(interpreter,this._positional[capture._positional[1].value]);
}
P6capture.prototype['named'] = function(interpreter,capture) {
    var val = this._named[capture._positional[1].value];
    setr(interpreter,val ? val : SMOP__NATIVE__bool_false);
}

function P6Str(str) {
    this.value = str;
}
init_type(P6Str);

function P6Int(i) {
    this.value = i;
}
init_type(P6Int);
P6Int.prototype['true'] = function(interpreter,capture) {
    setr(interpreter,boolify(this.value));
}

function P6LexPad() {
    this.entries = {};
    this._outer = new P6Scalar;
}
init_type(P6LexPad);
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture._positional[1].value;
    if (this.entries[key]) {
        setr(interpreter,this.entries[key]);
        //print("found ",key);
    } else if (this._outer.container) {
        /*XXX*/
        this._outer.container.lookup(interpreter,capture);
    } else {
        throw "Could not find variable "+key+" in the lexical scope.";
    }
}
P6LexPad.prototype['postcircumfix:{ }'] = function(interpreter,capture) {
    var key = capture._positional[1].value;
    setr(interpreter,new P6BValue(this,key));
}
P6LexPad.prototype['new'] = function(interpreter,capture) {
    var scope = new P6LexPad;
    setr(interpreter,scope);
}
P6LexPad.prototype.outer = function(interpreter,capture) {
    setr(interpreter,this._outer);
}


function P6BValue(lexpad,key) {
    this.lexpad = lexpad;
    this.key = key
}
init_type(P6BValue);
P6BValue.prototype.BIND = function(interpreter,capture) {
    this.lexpad.entries[this.key] = capture._positional[1];
    setr(interpreter,this);
}
P6BValue.prototype.FETCH = function(interpreter,capture) {
    if (!this.lexpad.entries[this.key]) this.lexpad.entries[this.key] = new P6Scalar;
    var entry = this.lexpad.entries[this.key];
    entry.DISPATCH(interpreter,new P6Str("FETCH"),new P6capture([entry],[]));
}
P6BValue.prototype.STORE = function(interpreter,capture) {
    if (!this.lexpad.entries[this.key]) this.lexpad.entries[this.key] = new P6Scalar;
    var entry = this.lexpad.entries[this.key];
    entry.DISPATCH(interpreter,new P6Str("STORE"),new P6capture([entry,capture._positional[1]],[]));
}


function set_reg(frame,i,value) {
    frame.reg[frame.mold.constants.length+i] = value;
}

var P6Interpreter = define_type('Interpreter',{
    'loop': function(interpreter,capture) {
        while (this._continuation) this._continuation.DISPATCH(this,new P6Str("eval"),new P6capture([this._continuation],[]));
    },
    'goto': function(interpreter,capture) {
        this._continuation = capture._positional[1];
    },
    'continuation': function(interpreter,capture) {
        setr(interpreter,this._continuation);
    }
});


var P6Code = define_type('Code',{
    'new': function(interpreter,capture) {
    var code = new P6Code();
    code._mold = capture._named.mold;
    code._outer = capture._named.outer;
    code._signature = capture._named.signature;
    setr(interpreter,code);
    },
    'postcircumfix:( )': function(interpreter,capture) {
        var frame = new P6Frame(code_mold);
        set_reg(frame,0,interpreter);
        set_reg(frame,1,capture);
        set_reg(frame,2,interpreter._continuation);
        set_reg(frame,3,capture._positional[0]);
        set_reg(frame,4,this._outer);
        set_reg(frame,5,this._signature);
        set_reg(frame,6,this._mold);
        interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame],[]));
    }
});


function JSFunction(func) {
    this.func = func;
}
init_type(JSFunction);

JSFunction.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    this.func(interpreter,capture._positional[1]);
}


function builtin(name,func) {
    lexical_prelude[name] = new JSFunction(func);
}


builtin('&say',FETCH_all(function(interpreter,capture) {
    var str = '';
    for (var i in capture._positional) str += capture._positional[i].value;
    print(str,"\n");
}));
builtin('&print',function(interpreter,capture) {
    var str = '';
    for (var i in capture._positional) str += capture._positional[i].value;
    print(str);
});
builtin('&infix:+:(int,int)',function(interpreter,capture) {
    setr(interpreter,new P6Int(capture._positional[0].value + capture._positional[1].value));
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
    lexical_prelude[name] = new proto;
    return constructor;
}

var P6AdhocSignature = define_type('AdhocSignature',{
    'new': function(interpreter,capture) {
        var sig = new P6AdhocSignature;
        sig._BIND = capture._named.BIND;
        setr(interpreter,sig);
    },
    BIND: function(interpreter,capture) {
        var frame = new P6Frame(this._BIND);
        set_reg(frame,0,interpreter);
        set_reg(frame,1,capture._positional[2]);
        set_reg(frame,2,capture._positional[1]);
        frame._back = interpreter._continuation;
        interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame],[]));
    }

});
P6Scalar = define_type('Scalar',{
    'new': function(interpreter,capture) {
        setr(interpreter,new P6Scalar);
    },
    FETCH: function(interpreter,capture) {
        setr(interpreter,this.container);
    },
    STORE: function(interpreter,capture) {
        this.container = capture._positional[1];
    }
});
var P6False = define_type('False',{
    'true': function(interpreter,capture) {
        setr(interpreter,this);
    },
});
var P6True = define_type('True',{
    'true': function(interpreter,capture) {
        setr(interpreter,this);
    },
});
var P6Array = define_type('Array',{
    'new': function(interpreter,capture) {
        var _array = new P6Array;
        _array._array = [];
        setr(interpreter,_array);
    },
    push: function(interpreter,capture) {
        this._array.push(capture._positional[1]);
    },
    unshift: function(interpreter,capture) {
        this._array.unshift(capture._positional[1]);
    },
    shift: function(interpreter,capture) {
        /*TODO exception*/
        var val = this._array.shift()
        setr(interpreter,val ? val : SMOP__NATIVE__bool_false);
    },
    'postcircumfix:[ ]': function(interpreter,capture) {
        var index = capture._positional[1].value;
        setr(interpreter,new P6ArrayProxy(this,index));
    },
    elems: function(interpreter,capture) {
        setr(interpreter,new P6Int(this._array.length));
    }
});

function P6ArrayProxy(array,index) {
    this._array = array;
    this.index = index
}
init_type(P6ArrayProxy);
P6ArrayProxy.prototype.BIND = function(interpreter,capture) {
    this._array._array[this.index] = capture._positional[1];
    setr(interpreter,this);
}
P6ArrayProxy.prototype.FETCH = function(interpreter,capture) {
    if (!this._array._array[this.index]) this._array._array[this.index] = new P6Scalar;
    var entry = this._array._array[this.index];
    entry.DISPATCH(interpreter,new P6Str("FETCH"),new P6capture([entry],[]));
}
P6ArrayProxy.prototype.STORE = function(interpreter,capture) {
    if (!this._array._array[this.index]) this._array._array[this.index] = new P6Scalar;
    var entry = this._array._array[this.index];
    entry.DISPATCH(interpreter,new P6Str("STORE"),new P6capture([entry,capture._positional[1]],[]));
}

SMOP__NATIVE__bool_false = new P6False;
SMOP__NATIVE__bool_true = new P6True;


var SMOP__S1P__LexicalScope = new P6LexPad;
var SMOP__S1P__LexicalPrelude = new P6LexPad();
SMOP__S1P__LexicalPrelude.entries = lexical_prelude;
SMOP__S1P__LexicalPrelude.entries.Code = new P6Code;
SMOP__S1P__LexicalPrelude.entries.capture = new P6capture([],[]);
SMOP__S1P__LexicalPrelude.entries.True = SMOP__NATIVE__bool_true;
SMOP__S1P__LexicalPrelude.entries.False = SMOP__NATIVE__bool_false;
SMOP__S1P__LexicalPrelude.entries['$LexicalPrelude'] = SMOP__S1P__LexicalPrelude;
SMOP__S1P__LexicalPrelude.entries['$?PACKAGE'] = new P6LexPad();


var PRIMITIVES = new P6LexPad();
function primitive(name,func) {
    PRIMITIVES.entries[name] = new JSFunction(func);
}
function boolify(x) {
    return x ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;
}
var FETCH_all_mold = new P6Mold(1,[],function(interpreter,frame) {
    switch (frame.pc) {
        case 0:
            if (frame.__capture._positional.length <= frame.__index) {
                interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame._back],[]));
                frame.__f(interpreter,frame.__capture);
                break;
            }
            var obj = frame.__capture._positional[frame.__index];
            frame.pc = 1;
            frame.ret = 0;
            obj.DISPATCH(interpreter,new P6Str("FETCH"),new P6capture([obj],[]));
            break;
        case 1:
            frame.__capture._positional[frame.__index] = frame.reg[0];
            frame.__index++;
            frame.pc = 0;
            break;
    }
});
function FETCH_all(f) {
    return function(interpreter,capture) {
        var frame = new P6Frame(FETCH_all_mold);
        frame._back = interpreter._continuation;
        frame.__capture = capture;
        frame.__index = 0;
        frame.__f = f;
        interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame],[]));
    };
}

primitive('&concat',function(interpreter,capture) {
    setr(interpreter,new P6Str(capture._positional[0].value + capture._positional[1].value));
});
primitive('&eq',function(interpreter,capture) {
    setr(interpreter,boolify(capture._positional[0].value == capture._positional[1].value));
});

SMOP__S1P__LexicalPrelude.entries['PRIMITIVES::'] = PRIMITIVES;


/* TODO autogenerated - move out */
var code_mold = new P6Mold(14,[new P6Int(1),new P6Str("BIND"),new P6Str("FETCH"),new P6Str("STORE"),new P6Str("cc"),new P6Str("create"),new P6Str("goto"),new P6Str("named"),new P6Str("new"),new P6Str("outer"),new P6Str("positional"),new P6Str("set_back"),new P6Str("set_lexical"),new P6Str("set_regs"),new P6Str("true"),SMOP__S1P__LexicalScope],function(interpreter,frame) {
  switch (frame.pc) {
    case 0:
      frame.pc = 1;
      frame.ret = 24;
      frame.reg[15].DISPATCH(interpreter,frame.reg[8],new P6capture([frame.reg[15]],[]));break;
    case 1:
      frame.pc = 2;
      frame.ret = 25;
      frame.reg[24].DISPATCH(interpreter,frame.reg[9],new P6capture([frame.reg[24]],[]));break;
    case 2:
      frame.pc = 3;
      frame.ret = 23;
      frame.reg[25].DISPATCH(interpreter,frame.reg[3],new P6capture([frame.reg[25],frame.reg[20]],[]));break;
    case 3:
      frame.pc = 4;
      frame.ret = 26;
      frame.reg[17].DISPATCH(interpreter,frame.reg[10],new P6capture([frame.reg[17],frame.reg[0]],[]));break;
    case 4:
      frame.pc = 5;
      frame.ret = 23;
      frame.reg[21].DISPATCH(interpreter,frame.reg[1],new P6capture([frame.reg[21],frame.reg[26],frame.reg[24]],[]));break;
    case 5:
      frame.pc = 6;
      frame.ret = 27;
      frame.reg[17].DISPATCH(interpreter,frame.reg[7],new P6capture([frame.reg[17],frame.reg[4]],[]));break;
    case 6:
      frame.pc = 7;
      frame.ret = 28;
      frame.reg[27].DISPATCH(interpreter,frame.reg[14],new P6capture([frame.reg[27]],[]));break;
    case 7:
      frame.pc = frame.reg[28] == SMOP__NATIVE__bool_false ? 9 : 8;
      break;
    case 8:
      frame.pc = 9;
      frame.ret = 18;
      frame.reg[27].DISPATCH(interpreter,frame.reg[2],new P6capture([frame.reg[27]],[]));break;
    case 9:
      frame.pc = 10;
      frame.ret = 29;
      frame.reg[22].DISPATCH(interpreter,frame.reg[5],new P6capture([frame.reg[22]],[]));break;
    case 10:
      frame.pc = 11;
      frame.ret = 23;
      frame.reg[29].DISPATCH(interpreter,frame.reg[13],new P6capture([frame.reg[29],frame.reg[16],frame.reg[24]],[]));break;
    case 11:
      frame.pc = 12;
      frame.ret = 23;
      frame.reg[29].DISPATCH(interpreter,frame.reg[12],new P6capture([frame.reg[29],frame.reg[24]],[]));break;
    case 12:
      frame.pc = 13;
      frame.ret = 23;
      frame.reg[29].DISPATCH(interpreter,frame.reg[11],new P6capture([frame.reg[29],frame.reg[18]],[]));break;
    case 13:
      frame.pc = 14;
      frame.ret = 23;
      frame.reg[16].DISPATCH(interpreter,frame.reg[6],new P6capture([frame.reg[16],frame.reg[29]],[]));break;
case 14:frame.pc = -1;
  }
}
)
