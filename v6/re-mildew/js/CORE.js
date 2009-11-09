var P6Scalar;
var lexical_prelude = {};
var SMOP__NATIVE__bool_false;
var SMOP__NATIVE__bool_true;
function always_true(interpreter,capture) {
    setr(interpreter,SMOP__NATIVE__bool_true);
}
function init_type(ID,type) {
    type.prototype.DISPATCH = DISPATCH_from_methods;
    type.prototype.FETCH = FETCH;
    type.prototype['true'] = always_true;
    type.prototype.ID = ID;
}
function DISPATCH_from_methods(interpreter,identifier,capture) {
    //print(this.ID+'.'+identifier.value,"\n");
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
    this._control = SMOP__NATIVE__bool_false;
    this._back = SMOP__NATIVE__bool_false;
    this.reg = [];
    for (i in this.mold.constants) {
        this.reg[i] = this.mold.constants[i];
    }
    this.pc = 0;
}
init_type('frame',P6Frame);
P6Frame.prototype.eval = function(interpreter,capture) {
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
P6Frame.prototype.control = function(interpreter,capture) {
    setr(interpreter,this._control);
}
P6Frame.prototype.lexical = function(interpreter,capture) {
    setr(interpreter,this._lexical);
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
init_type('capture',P6capture);
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
P6capture.prototype['elems'] = function(interpreter,capture) {
    setr(interpreter,new P6Int(this._positional.length));
}
P6capture.prototype['FETCH'] = function(interpreter,capture) {
    if (this._positional.length == 1) {
        var content = this._positional[0];
        content.DISPATCH(interpreter,new P6Str('FETCH'),new P6capture([content],[]));
    } else {
        setr(interpreter,this);
    }
}

function P6Str(str) {
    this.value = str;
}
P6Int.prototype['true'] = function(interpreter,capture) {
    setr(interpreter,boolify(this.value));
}
init_type('Str',P6Str);
P6Str.prototype['true'] = function(interpreter,capture) {
    setr(interpreter,boolify(this.value && this.value != '0'));
}
P6Str.prototype['perl'] = function(interpreter,capture) {
    setr(interpreter,new P6Str("'" + this.value.replace(/([\\\'])/g,"\\$1") + "'"));
}
P6Str.prototype['Str'] = function(interpreter,capture) {
    setr(interpreter,this);
}

function P6Int(i) {
    this.value = i;
}
init_type('Int',P6Int);
P6Int.prototype['true'] = function(interpreter,capture) {
    setr(interpreter,boolify(this.value));
}
P6Int.prototype['Str'] = function(interpreter,capture) {
    setr(interpreter,new P6Str(this.value));
}

function P6LexPad() {
    this.entries = {};
    this._outer = new P6Scalar;
}
init_type('LexPad',P6LexPad);
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture._positional[1].value;
    if (this.entries[key]) {
        setr(interpreter,new P6BValue(this,key));
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
init_type('BValue',P6BValue);
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

var P6Interpreter = define_typex('Interpreter',{
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


var P6Code = define_typex('Code',{
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
    },
});


function JSFunction(func) {
    this.func = func;
}
init_type('JS function',JSFunction);

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

function define_typex(name,methods) {
    return define_type({name:name,methods:methods,nonew:1});
}
function define_type(options) {
    var methods = {};
    var name = "?";
    var attrs = [];
    if (options.name) name = options.name;
    if (options.methods) methods = options.methods;
    if (options.attributes) attrs = options.attributes;


    var proto = function() {
    }
    init_type(name + ' protobject',proto);
    proto.prototype['^!CREATE'] = methods['^!CREATE'];

    var constructor = function() {
    }
    init_type(name,constructor);

    if (!options.nonew && !methods['new']) { 
        methods['new'] = function(interpreter,capture) {
            var obj = new constructor;
            for (var i in attrs) {
                var storage = "_"+attrs[i];
                obj[storage] = new P6Scalar;
                obj[storage].container = SMOP__NATIVE__bool_false;
            }
            setr(interpreter,obj);
        }
    }
    for (var i in attrs) {
        (function() {
            var storage = "_"+attrs[i];
            methods[attrs[i]] = function(interpreter,capture) {
                setr(interpreter,this[storage]);
            }
        })();
    }

    proto.prototype['new'] = methods['new'];

    for (var m in methods) {
        constructor.prototype[m] = methods[m];
    }
    lexical_prelude[name] = new proto;
    return constructor;
}

var P6AdhocSignature = define_typex('AdhocSignature',{
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
P6Scalar = define_typex('Scalar',{
    'new': function(interpreter,capture) {
        var scalar = new P6Scalar;
        scalar.container = capture._positional[1];
        if (!scalar.container) scalar.container = SMOP__NATIVE__bool_false;
        setr(interpreter,scalar);
    },
    FETCH: function(interpreter,capture) {
        setr(interpreter,this.container);
    },
    STORE: function(interpreter,capture) {
        this.container = capture._positional[1];
    }
});
var P6False = define_typex('False',{
    'true': function(interpreter,capture) {
        setr(interpreter,this);
    },
});
var P6True = define_typex('True',{
    'true': function(interpreter,capture) {
        setr(interpreter,this);
    },
});
var P6Array = define_typex('Array',{
    'new': function(interpreter,capture) {
        var _content = new P6Array;
        _content._content = [];
        setr(interpreter,_content);
    },
    push: function(interpreter,capture) {
        this._content.push(capture._positional[1]);
    },
    unshift: function(interpreter,capture) {
        this._content.unshift(capture._positional[1]);
    },
    shift: function(interpreter,capture) {
        /*TODO exception*/
        var val = this._content.shift()
        setr(interpreter,val ? val : SMOP__NATIVE__bool_false);
    },
    'postcircumfix:[ ]': function(interpreter,capture) {
        var index = capture._positional[1].value;
        setr(interpreter,new P6ContainerProxy(this,index));
    },
    elems: function(interpreter,capture) {
        setr(interpreter,new P6Int(this._content.length));
    }
});

var P6Hash = define_typex('Hash',{
    'new': function(interpreter,capture) {
        var _content = new P6Hash;
        _content._content = {};
        setr(interpreter,_content);
    },
    'postcircumfix:{ }': function(interpreter,capture) {
        var index = capture._positional[1].value;
        setr(interpreter,new P6ContainerProxy(this,index));
    },
    keys: function(interpreter,capture) {
        var keys = [];
        for (i in this._content) {
            keys.push(new P6Str(i));
        }
        var ret = new P6Array;
        ret._content = keys;
        setr(interpreter,ret);
    },
    exists:function(interpreter,capture) { 
        var key = capture._positional[1].value;
        setr(interpreter,boolify(this._content.hasOwnProperty(key)));
    }
});

var P6Package = define_typex('Package',{
    'new': function(interpreter,capture) {
        var pkg = new P6Package;
        pkg._name = new P6Scalar;
        pkg._content = {};
        setr(interpreter,pkg);
    },
    'name': function(interpreter,capture) {
        setr(interpreter,this._name);
    },
    'postcircumfix:{ }': function(interpreter,capture) {
        var key = capture._positional[1].value;
        setr(interpreter,new P6ContainerProxy(this,key));
    },
});

var P6opaque = define_typex('p6opaque',{
    '^!CREATE': function(interpreter,capture) {
        var obj = new P6opaque;
        obj._how = new P6Scalar;
        obj._who = new P6Scalar;
        obj._methods = new P6Hash;
        obj._methods._content = {};

        obj._attributes = new P6Hash;
        obj._attributes._content = {};

        obj._instance_storage = new P6Hash;
        obj._instance_storage._content = {};

        setr(interpreter,obj);
    },
    '^!how': function(interpreter,capture) {
        setr(interpreter,this._how);
    },
    '^!who': function(interpreter,capture) {
        setr(interpreter,this._who);
    },
    '^!methods': function(interpreter,capture) {
        setr(interpreter,this._methods);
    },
    '^!attributes': function(interpreter,capture) {
        setr(interpreter,this._attributes);
    },
    '^!instance_storage': function(interpreter,capture) {
        setr(interpreter,this._instance_storage);
    }
});
P6opaque.prototype.DISPATCH = function (interpreter,identifier,capture) {
    var how = this._how.container;
    if (this[identifier.value]) this[identifier.value](interpreter,capture);
    else how.DISPATCH(interpreter,new P6Str("dispatch"),new P6capture([how,capture._positional[0],identifier,capture],[]));
}

var P6PrototypeHOW = define_typex('PrototypeHOW',{
    'add_method': function(interpreter,capture) {
        //XXX repr violation
        capture._positional[1]._methods._content[capture._positional[2].value] = capture._positional[3];
    },
    'add_attribute': function(interpreter,capture) {
        //XXX repr violation
        capture._positional[1]._attributes._content[capture._positional[2].value] = capture._positional[3];
    },
    'dispatch': function(interpreter,capture) {
        var frame = new P6Frame(pureprototypehow_dispatch_mold);
        set_reg(frame,0,interpreter);
        set_reg(frame,1,capture._positional[1]);
        set_reg(frame,2,capture._positional[2]);
        set_reg(frame,3,capture._positional[3]);
        set_reg(frame,4,capture._positional[3]._positional[0]);
        frame._back = interpreter._continuation;
        interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame],[]));
    },
    'lookup_fail': function(interpreter,capture) {
        throw "lookup_fail:"+capture._positional[2].value;
    }
    
});
var P6FlattenedScope = define_typex('FlattenedScope',{
    'new': function(interpreter,capture) {
        var scope = new P6FlattenedScope;
        scope._scope = capture._positional[1];
        setr(interpreter,scope);
    },
    'postcircumfix:{ }': function(interpreter,capture) {
        this._scope.DISPATCH(interpreter,new P6Str("lookup"),new P6capture([this._scope,capture._positional[1]],[]));
    }
});
var P6ControlExceptionReturn = define_typex('ControlExceptionReturn',{
    'new': function(interpreter,capture) {
        var exception = new P6ControlExceptionReturn;
        exception._handled = new P6Scalar;
        exception._routine = new P6Scalar;
        exception._capture = new P6Scalar;
        setr(interpreter,exception);
    },
    'handled': function(interpreter,capture) {
        setr(interpreter,this._handled);
    },
    'routine': function(interpreter,capture) {
        setr(interpreter,this._routine);
    },
    'capture': function(interpreter,capture) {
        setr(interpreter,this._capture);
    },
    'throw': function(interpreter,capture) {
        var frame = new P6Frame(throw_mold);
        set_reg(frame,0,interpreter);
        set_reg(frame,1,this);
        set_reg(frame,2,new P6capture([this],[]));
        frame._back = interpreter._continuation;
        interpreter.DISPATCH(interpreter,new P6Str('goto'),new P6capture([interpreter,frame],[]));
    },
    'handle_return': function(interpreter,capture) {
        var exception = capture._positional[1];
        if (capture._positional[2] == exception._routine.container) {
            setr(interpreter,exception._capture.container);
        } else {
            exception.DISPATCH(interpreter,new P6Str('throw'),new P6capture([exception],[]));
        }
    }
});

define_type({
    name: 'Attribute',
    attributes: ['name','private_name','container_type']
});

function P6ContainerProxy(array,index) {
    this._content = array;
    this.index = index
}
init_type('Container Proxy',P6ContainerProxy);
P6ContainerProxy.prototype.BIND = function(interpreter,capture) {
    this._content._content[this.index] = capture._positional[1];
    setr(interpreter,this);
}
P6ContainerProxy.prototype.FETCH = function(interpreter,capture) {
    if (!this._content._content[this.index]) this._content._content[this.index] = new P6Scalar;
    var entry = this._content._content[this.index];
    entry.DISPATCH(interpreter,new P6Str("FETCH"),new P6capture([entry],[]));
}
P6ContainerProxy.prototype.STORE = function(interpreter,capture) {
    if (!this._content._content[this.index]) this._content._content[this.index] = new P6Scalar;
    var entry = this._content._content[this.index];
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

SMOP__S1P__LexicalPrelude.entries.PrototypeHOW = new P6PrototypeHOW;


SMOP__S1P__LexicalPrelude.entries['$LexicalPrelude'] = SMOP__S1P__LexicalPrelude;
SMOP__S1P__LexicalPrelude.entries['$?PACKAGE'] = new P6LexPad();

var SMOP__S1P__FlattenedScope = SMOP__S1P__LexicalPrelude.entries.FlattenedScope;

function onmoldload() {
    SMOP__S1P__LexicalPrelude.entries['$DefaultMethodSignature'] = new P6AdhocSignature();
    SMOP__S1P__LexicalPrelude.entries['$DefaultMethodSignature']._BIND = default_method_signature_BIND_mold;
    SMOP__S1P__LexicalPrelude.entries['$DefaultBlockSignature'] = new P6AdhocSignature();
    SMOP__S1P__LexicalPrelude.entries['$DefaultBlockSignature']._BIND = default_block_signature_BIND_mold;

}


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
primitive('&int_add',function(interpreter,capture) {
    setr(interpreter,new P6Int(capture._positional[0].value + capture._positional[1].value));
});
primitive('&int_substract',function(interpreter,capture) {
    setr(interpreter,new P6Int(capture._positional[0].value - capture._positional[1].value));
});
primitive('&int_less',function(interpreter,capture) {
    setr(interpreter,boolify(capture._positional[0].value < capture._positional[1].value));
});
primitive('&get_interpreter',function(interpreter,capture) {
    setr(interpreter,interpreter);
});
var UID = 0;
primitive('&storage_name',function(interpreter,capture) {
    if (!capture._positional[0].UID) {
        capture._positional[0].UID = ++UID;
    }
    setr(interpreter,new P6Str(capture._positional[0].UID+capture._positional[1].value));
});

SMOP__S1P__LexicalPrelude.entries['PRIMITIVES::'] = PRIMITIVES;
