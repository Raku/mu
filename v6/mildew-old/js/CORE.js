var P6Scalar;
var lexical_prelude = {};
var SMOP__NATIVE__bool_false;
var SMOP__NATIVE__bool_true;
function always_true(interpreter,capture) {
    return SMOP__NATIVE__bool_true;
}
function init_type(ID,type) {
    type.prototype.DISPATCH = DISPATCH_from_methods;
    type.prototype.FETCH = FETCH;
    type.prototype['true'] = always_true;
    type.prototype.ID = ID;
}
function DISPATCH_from_methods(interpreter,identifier,capture) {
    //print(this.ID+'.'+identifier,"\n");
    if (this[identifier]) {
        return this[identifier](interpreter,capture);
    } else {
        throw 'no method '+identifier+' in '+this.ID; 
    }
}
function FETCH(interpreter,capture) {
    return this;
}
function P6Frame(mold) {
    this.mold = mold;
    this._control = SMOP__NATIVE__bool_false;
    this._catch = SMOP__NATIVE__bool_false;
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
P6Frame.prototype.set_catch = function(interpreter,capture) {
    this._catch = capture._positional[1];
}
P6Frame.prototype.set_regs = function(interpreter,capture) {
    for (var i=1;i < capture._positional.length;i++) {
        set_reg(this,i-1,capture._positional[i]);
    }
}
P6Frame.prototype.back = function(interpreter,capture) {
    return this._back;
}
P6Frame.prototype.control = function(interpreter,capture) {
    return this._control;
}
P6Frame.prototype['catch'] = function(interpreter,capture) {
    return this._catch;
}
P6Frame.prototype.lexical = function(interpreter,capture) {
    return this._lexical;
}

function P6Mold(regs,constants,code) {
    this.constants = constants;
    this.code = code;
}
P6Mold.prototype.DISPATCH = DISPATCH_from_methods;

P6Mold.prototype.create = function(interpreter,capture) {
    return new P6Frame(this);
}
init_type('Mold',P6Mold);

function P6capture(positional,named) {
    this._positional = positional;
    var i=0;
    this._named = {};
    for (var i=0;i < named.length;i += 2) {
        this._named[named[i]] = named[i+1];
    }
}
init_type('capture',P6capture);
P6capture.prototype['new'] = function(interpreter,capture) {
    var c = new P6capture(capture._positional.slice(1),[]);
    c._named = capture._named;
    return c;
}
P6capture.prototype['delegate'] = function(interpreter,capture) {
    var pos = [];
    pos[0] = capture._positional[1];
    for (var i=1;i<this._positional.length;i++) pos[i] = this._positional[i];

    var c = new P6capture(pos,[]);
    c._named = this._named;
    return c;
}
P6capture.prototype['positional'] = function(interpreter,capture) {
    return this._positional[capture._positional[1]];
}
P6capture.prototype['named'] = function(interpreter,capture) {
    var val = this._named[capture._positional[1]];
    return val ? val : SMOP__NATIVE__bool_false;
}
P6capture.prototype['elems'] = function(interpreter,capture) {
    return this._positional.length;
}
P6capture.prototype['named_count'] = function(interpreter,capture) {
    var count = 0;
    for (i in this._named) count++;
    return count;
}
P6capture.prototype['FETCH'] = function(interpreter,capture) {
    if (this._positional.length == 1) {
        var content = this._positional[0];
        return content.DISPATCH(interpreter,('FETCH'),new P6capture([content],[]));
    } else {
        return this;
    }
}


define_type({
    name: 'Str',
    c: String,
    methods: {
        Str: function(interpreter,capture) {
            return String(this);
        },
        "true": function(interpreter,capture) {
            return boolify(this != 0 && this != '0');
        },
        perl: function(interpreter,capture) {
            return "'" + this.replace(/([\\\'])/g,"\\$1") + "'";
        },
        FETCH: function(interpreter,capture) {
            return String(this);
        }
    }
});

define_type({
    name: 'Int',
    c: Number,
    methods: {
        "true": function(interpreter,capture) {
            return boolify(this != 0);
        },
        Str: function(interpreter,capture) {
            return this + "";
        },
        FETCH: function(interpreter,capture) {
            return Number(this+0);
        }
    }
});

function P6LexPad() {
    this.entries = {};
    this._outer = new P6Scalar;
}
init_type('LexPad',P6LexPad);
P6LexPad.prototype.lookup = function(interpreter,capture) {
    var key = capture._positional[1];
    if (this.entries.hasOwnProperty(key)) {
        return new P6BValue(this,key);
    } else if (this._outer.container != SMOP__NATIVE__bool_false) {
        /*XXX*/
        return this._outer.container.lookup(interpreter,capture);
    } else {
        throw "Could not find variable "+key+" in the lexical scope.";
    }
}
P6LexPad.prototype['postcircumfix:{ }'] = function(interpreter,capture) {
    var key = capture._positional[1];
    return new P6BValue(this,key);
}
P6LexPad.prototype['new'] = function(interpreter,capture) {
    var scope = new P6LexPad;
    return scope;
}
P6LexPad.prototype.outer = function(interpreter,capture) {
    return this._outer;
}
P6LexPad.prototype.exists = function(interpreter,capture) {
    var key = capture._positional[1];
    return boolify(this.entries.hasOwnProperty(key));
}


function P6BValue(lexpad,key) {
    this.lexpad = lexpad;
    this.key = key;
}
init_type('BValue',P6BValue);
P6BValue.prototype.BIND = function(interpreter,capture) {
    this.lexpad.entries[this.key] = capture._positional[1];
    return this;
}
P6BValue.prototype.FETCH = function(interpreter,capture) {
    if (!this.lexpad.entries.hasOwnProperty(this.key)) this.lexpad.entries[this.key] = new P6Scalar;
    var entry = this.lexpad.entries[this.key];
    return entry.DISPATCH(interpreter,"FETCH",new P6capture([entry],[]));
}
P6BValue.prototype.STORE = function(interpreter,capture) {
    if (!this.lexpad.entries.hasOwnProperty(this.key)) {
        this.lexpad.entries[this.key] = new P6Scalar;
    } else {
    }
    var entry = this.lexpad.entries[this.key];
    return entry.DISPATCH(interpreter,"STORE",new P6capture([entry,capture._positional[1]],[]));
}


function set_reg(frame,i,value) {
    frame.reg[frame.mold.constants.length+i] = value;
}


var P6Interpreter = define_type({
    name:'Interpreter',
    methods: {
        'loop': function(interpreter,capture) {
            while (this._continuation) this._continuation.DISPATCH(this,"eval",new P6capture([this._continuation],[]));
        },
        'goto': function(interpreter,capture) {
            this._continuation = capture._positional[1];
        },
        'continuation': function(interpreter,capture) {
            return this._continuation;
        }
    }
});


var P6Code = define_type({
    name: 'Code',
    methods: {
        'new': function(interpreter,capture) {
            var code = new P6Code();
            code._mold = capture._named.mold;
            code._outer = capture._named.outer;
            code._signature = capture._named.signature;
            return code;
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
            return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
        },
        'signature': function(interpreter,capture) {
            return this._signature;
        },
        'outer': function(interpreter,capture) {
            return this._outer;
        },
        'mold': function(interpreter,capture) {
            return this._mold;
        }
    }
});


function JSFunction(func) {
    this.func = func;
}
init_type('JS function',JSFunction);

JSFunction.prototype['postcircumfix:( )'] = function(interpreter,capture) {
    return this.func(interpreter,capture._positional[1]);
}


function builtin(name,func) {
    lexical_prelude[name] = new JSFunction(func);
}


builtin('&say',FETCH_all(function(interpreter,capture) {
    var str = '';
    for (var i in capture._positional) str += capture._positional[i];
    print(str,"\n");
    return SMOP__NATIVE__bool_true;
}));
builtin('&print',FETCH_all(function(interpreter,capture) {
    var str = '';
    for (var i in capture._positional) str += capture._positional[i];
    print(str);
    return SMOP__NATIVE__bool_true;
}));
builtin('&not',FETCH_all(function(interpreter,capture) {
    return boolify(capture._positional[0] == SMOP__NATIVE__bool_false);
}));
builtin('&eval',FETCH_all(function(interpreter,capture) {
   var mold = eval(p6_to_js(capture._positional[0]));
   var frame = new P6Frame(mold);
   frame._back = interpreter._continuation;

   set_reg(frame,0,interpreter);
   set_reg(frame,1,interpreter._continuation._lexical);
   set_reg(frame,2,interpreter._continuation);

   return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
}));

function define_type(options) {
    var methods = {};
    var name = "?";
    var attrs = [];
    var c = function() {
    }
    if (options.name) name = options.name;
    if (options.methods) methods = options.methods;
    if (options.attributes) attrs = options.attributes;
    if (options.c) c = options.c;


    var proto = function() {
    }
    init_type(name + ' protobject',proto);
    proto.prototype['^!CREATE'] = methods['^!CREATE'];

    init_type(name,c);

    if (!options.nonew && !methods['new']) { 
        methods['new'] = function(interpreter,capture) {
            var obj = new c;
            for (var i in attrs) {
                var storage = "_"+attrs[i];
                obj[storage] = new P6Scalar;
            }
            return obj;
        }
    }
    for (var i in attrs) {
        (function() {
            var storage = "_"+attrs[i];
            methods[attrs[i]] = function(interpreter,capture) {
                return interpreter,this[storage];
            }
        })();
    }

    proto.prototype['new'] = methods['new'];

    for (var m in methods) {
        c.prototype[m] = methods[m];
    }
    lexical_prelude[name] = new proto;
    return c;
}

var P6AdhocSignature = define_type({
    name: 'AdhocSignature',
    methods: {
        'new': function(interpreter,capture) {
            var sig = new P6AdhocSignature;
            sig._BIND = capture._named.BIND;
            return sig;
        },
        BIND: function(interpreter,capture) {
            var frame = new P6Frame(this._BIND);
            set_reg(frame,0,interpreter);
            set_reg(frame,1,capture._positional[2]);
            set_reg(frame,2,capture._positional[1]);
            frame._back = interpreter._continuation;
            return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
        }
    }
});
P6Scalar = define_type({
    name: 'Scalar',
    c: function(content) {
        if (content !== undefined) this.container = content;
        else this.container = SMOP__NATIVE__bool_false;
    },
    methods: {
        'new': function(interpreter,capture) {
            var scalar = new P6Scalar;
            return new P6Scalar(capture._positional[1]);
        },
        FETCH: function(interpreter,capture) {
            return this.container;
        },
        STORE: function(interpreter,capture) {
            this.container = capture._positional[1];
            return this.container;
        }
    }
});
var P6False = define_type({
    name:'False',
    methods: {
        'true': function(interpreter,capture) {
            return this;
        },
    }
});
var P6True = define_type({
    name:'True',
    methods: {
        'true': function(interpreter,capture) {
            return this;
        },
    }
});
var P6Array = define_type({
    name:'Array',
    methods: {
        'new': function(interpreter,capture) {
            var _content = new P6Array;
            _content._content = [];
            return _content;
        },
        push: function(interpreter,capture) {
            this._content.push(capture._positional[1]);
            return SMOP__NATIVE__bool_false;
        },
        unshift: function(interpreter,capture) {
            this._content.unshift(capture._positional[1]);
        },
        shift: function(interpreter,capture) {
            /*TODO exception*/
            var val = this._content.shift()
            return val ? val : SMOP__NATIVE__bool_false;
        },
        'postcircumfix:[ ]': function(interpreter,capture) {
            var index = capture._positional[1];
            return new P6ContainerProxy(this,index);
        },
        elems: function(interpreter,capture) {
            return this._content.length;
        }
    }
});

var P6Hash = define_type({
    name:'Hash',
    methods: {
        'new': function(interpreter,capture) {
            var _content = new P6Hash;
            _content._content = {};
            return _content;
        },
        'postcircumfix:{ }': function(interpreter,capture) {
            var index = capture._positional[1];
            return new P6ContainerProxy(this,index);
        },
        keys: function(interpreter,capture) {
            var keys = [];
            for (i in this._content) {
                keys.push(i);
            }
            var ret = new P6Array;
            ret._content = keys;
            return ret;
        },
        exists:function(interpreter,capture) { 
            var key = capture._positional[1];
            return boolify(this._content.hasOwnProperty(key));
        }
    }
});

var P6Package = define_type({
    name: 'Package',
    methods: {
        'new': function(interpreter,capture) {
            var pkg = new P6Package;
            pkg._name = new P6Scalar;
            pkg._content = {};
            return pkg;
        },
        'name': function(interpreter,capture) {
            return this._name;
        },
        'postcircumfix:{ }': function(interpreter,capture) {
            var key = capture._positional[1];
    //        print('looking up '+key+'\n');
            return new P6ContainerProxy(this,key);
        },
    }
});

var P6opaque = define_type({
    name:'p6opaque',
    nonew: 1,
    methods: {
        '^!CREATE': function(interpreter,capture) {
            var obj = new P6opaque;
            obj.created = 1;
            obj._is_container = new P6Scalar;
            obj._how = new P6Scalar;
            obj._who = new P6Scalar;
            obj._methods = new P6Hash;
            obj._methods._content = {};
    
            obj._attributes = new P6Hash;
            obj._attributes._content = {};
    
            obj._instance_storage = new P6Hash;
            obj._instance_storage._content = {};
    
            obj._does = new P6Array;
            obj._does._content = [];
    
            return obj;
        },
        '^!how': function(interpreter,capture) {
            return this._how;
        },
        '^!who': function(interpreter,capture) {
            return this._who;
        },
        '^!methods': function(interpreter,capture) {
            return this._methods;
        },
        '^!attributes': function(interpreter,capture) {
            return this._attributes;
        },
        '^!instance_storage': function(interpreter,capture) {
            return this._instance_storage;
        },
        '^!does': function(interpreter,capture) {
            return this._does;
        },
        '^!is_container': function(interpreter,capture) {
            return this._is_container;
        }
    }
});
P6opaque.prototype.DISPATCH = function (interpreter,identifier,capture) {
    var how = this._how.container;
    if (!this[identifier] || (identifier == 'FETCH' && this._is_container.container != SMOP__NATIVE__bool_false)) return how.DISPATCH(interpreter,"dispatch",new P6capture([how,capture._positional[0],identifier,capture],[]));
    else return this[identifier](interpreter,capture);
}

var P6PrototypeHOW = define_type({
    name: 'PrototypeHOW',
    methods: {
        'add_method': function(interpreter,capture) {
            //XXX repr violation
            capture._positional[1]._methods._content[capture._positional[2]] = capture._positional[3];
        },
        'add_attribute': function(interpreter,capture) {
            //XXX repr violation
            capture._positional[1]._attributes._content[capture._positional[2]] = capture._positional[3];
        },
        'dispatch': function(interpreter,capture) {
            var frame = new P6Frame(pureprototypehow_dispatch_mold);
            set_reg(frame,0,interpreter);
            set_reg(frame,1,capture._positional[1]);
            set_reg(frame,2,capture._positional[2]);
            set_reg(frame,3,capture._positional[3]);
            set_reg(frame,4,capture._positional[3]._positional[0]);
            frame._back = interpreter._continuation;
    //        print("gotoing...",capture._positional[3]._positional[0].ID,"\n");
            return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
        },
        'lookup_fail': function(interpreter,capture) {
            var methods = [];
            for (i in capture._positional[1]._methods._content) methods.push(i);
    
            throw "Could not find method "+capture._positional[2]+" in "+capture._positional[1]._who.container._name.container+". Available methods: "+methods.join(',');
        }
    }
});
var P6FlattenedScope = define_type({
    name:'FlattenedScope',
    methods: {
        'new': function(interpreter,capture) {
            var scope = new P6FlattenedScope;
            scope._scope = capture._positional[1];
            return scope;
        },
        'postcircumfix:{ }': function(interpreter,capture) {
            return this._scope.DISPATCH(interpreter,"lookup",new P6capture([this._scope,capture._positional[1]],[]));
        }
    }
});
var P6ControlExceptionReturn = define_type({
    name:'ControlExceptionReturn',
    methods: {
        'new': function(interpreter,capture) {
            var exception = new P6ControlExceptionReturn;
            exception._handled = new P6Scalar;
            exception._routine = new P6Scalar;
            exception._capture = new P6Scalar;
            return exception;
        },
        'handled': function(interpreter,capture) {
            return this._handled;
        },
        'routine': function(interpreter,capture) {
            return this._routine;
        },
        'capture': function(interpreter,capture) {
            return this._capture;
        },
        'throw': function(interpreter,capture) {
            var frame = new P6Frame(throw_mold);
            set_reg(frame,0,interpreter);
            set_reg(frame,1,this);
            set_reg(frame,2,new P6capture([this],[]));
            frame._back = interpreter._continuation;
            return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
        },
        'handle_return': function(interpreter,capture) {
            var exception = capture._positional[1];
            if (capture._positional[2] == exception._routine.container) {
                return exception._capture.container;
            } else {
                return exception.DISPATCH(interpreter,'throw',new P6capture([exception],[]));
            }
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
    return this;
}
P6ContainerProxy.prototype.FETCH = function(interpreter,capture) {
    if (!this._content._content.hasOwnProperty(this.index)) this._content._content[this.index] = new P6Scalar;
    var entry = this._content._content[this.index];
    return entry.DISPATCH(interpreter,"FETCH",new P6capture([entry],[]));
}
P6ContainerProxy.prototype.STORE = function(interpreter,capture) {
    if (!this._content._content.hasOwnProperty(this.index)) this._content._content[this.index] = new P6Scalar;
    var entry = this._content._content[this.index];
    return entry.DISPATCH(interpreter,"STORE",new P6capture([entry,capture._positional[1]],[]));
}

define_type({
    name: 'MildewSOLoader',
    methods: {
        load: function(interpreter,capture) {
            var path = capture._positional[1].replace(/\.so$/,'\.js');
            var mold = eval(slurp(path));
            var frame = new P6Frame(mold);
            frame._back = interpreter._continuation;

            set_reg(frame,0,interpreter);
            set_reg(frame,1,capture._positional[2]);
            set_reg(frame,2,interpreter._continuation);

            return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
        }
    }
});

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


var PRIMITIVES = new P6Package();
PRIMITIVES._content = {};
function primitive(name,func) {
    PRIMITIVES._content[name] = new JSFunction(func);
}
function boolify(x) {
    return x ? SMOP__NATIVE__bool_true : SMOP__NATIVE__bool_false;
}
var FETCH_all_mold = new P6Mold(1,[],function(interpreter,frame) {
    switch (frame.pc) {
        case 0:
            if (frame.__capture._positional.length <= frame.__index) {
                interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame._back],[]));
                var tmp = frame.__f(interpreter,frame.__capture);
                if (tmp) interpreter._continuation.DISPATCH(interpreter,"setr",new P6capture([interpreter._continuation,tmp],[]));
                break;
            }
            var obj = frame.__capture._positional[frame.__index];
            frame.pc = 1;
            frame.ret = 0;
            var tmp = obj.DISPATCH(interpreter,"FETCH",new P6capture([obj],[]));
            if (tmp != undefined) frame.reg[0] = tmp; else break;
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
        return interpreter.DISPATCH(interpreter,'goto',new P6capture([interpreter,frame],[]));
    };
}

primitive('&concat',function(interpreter,capture) {
    return capture._positional[0] + capture._positional[1];
});
primitive('&eq',function(interpreter,capture) {
    return boolify(capture._positional[0] == capture._positional[1]);
});
primitive('&pointer_equal',function(interpreter,capture) {
    return boolify(capture._positional[0] == capture._positional[1]);
});
primitive('&SMOP_RI',function(interpreter,capture) {
    return capture._positional[0].ID;
});
primitive('&ritest',function(interpreter,capture) {
    return boolify(capture._positional[0].ID == capture._positional[1]);
});
primitive('&idconst_eq',function(interpreter,capture) {
    return boolify(capture._positional[0] == capture._positional[1]);
});
primitive('&int_add',function(interpreter,capture) {
    return capture._positional[0] + capture._positional[1];
});
primitive('&int_substract',function(interpreter,capture) {
    return capture._positional[0] - capture._positional[1];
});
primitive('&int_less',function(interpreter,capture) {
    return boolify(capture._positional[0] < capture._positional[1]);
});
primitive('&get_interpreter',function(interpreter,capture) {
    return interpreter;
});
var UID = 0;
primitive('&storage_name',function(interpreter,capture) {
    if (!capture._positional[0].UID) {
        capture._positional[0].UID = ++UID;
    }
    return capture._positional[0].UID+capture._positional[1];
});

SMOP__S1P__LexicalPrelude.entries['PRIMITIVES::'] = PRIMITIVES;
