// This is the part of the Prelude for JavaScript which is written in
// JavaScript. See lib6/Prelude/JS.pm for the part written in Perl 6.

// Create our namespace.
if(PIL2JS == undefined) var PIL2JS = {};

// IE doesn't care about standards and only interprets "\r" as linefeed.
PIL2JS.LF =
  typeof document     != "undefined" &&
  typeof document.all != "undefined" &&
  navigator.userAgent.indexOf("Konqueror") == -1
    ? "\r"
    : "\n";

PIL2JS.cur_uid = 1;
PIL2JS.new_uid = function () {
  return PIL2JS.cur_uid++;
};

// Hash class. As with the various exception classes above, we don't need to
// declare any methods. This class only exists so code can do if(foo instanceof
// PIL2JS.Hash) {...}.
PIL2JS.Hash = function () { this.entries = {} };
PIL2JS.Hash.prototype = {
  add_pair: function (pair) {
    this.entries[pair.key.toNative()] = pair;
  },
  exists:   function (key) {
    var ikey = key.toNative();
    return ikey != "toPIL2JSBox" && this.entries[ikey] != undefined;
  },
  delete_key: function (key) {
    var old = this.get_value(key);
    delete this.entries[key.toNative()];
    return old;
  },
  pairs:    function () {
    var pairs = [];
    for(var internal_key in this.entries) {
      if(internal_key != "toPIL2JSBox") {
        pairs.push(this.entries[internal_key]);
      }
    }
    return pairs;
  },
  keys:     function () {
    var keys  = [];
    var pairs = this.pairs();
    for(var i = 0; i < pairs.length; i++) {
      keys.push(pairs[i].key);
    }
    return keys;
  },
  get_value: function (key) {
    return this.exists(key) ? this.entries[key.toNative()].value : undefined;
  },
  toString: function () { return "<PIL2JS.Hash>" }
};

// class Pair { has $.key; has $.value }
PIL2JS.Pair = function (key, value) {
  this.key   = key;
  this.value = value;
};
PIL2JS.Pair.prototype.toString = function () { return "<PIL2JS.Pair>" };

// Ref class.
PIL2JS.Ref = function (referencee) {
  this.referencee = referencee;
  return this;
};
PIL2JS.Ref.prototype.toString = function () { return "<PIL2JS.Ref>" };

// This is necessary to emulate pass by ref, needed for is rw and is ref.
// See section "DESIGN" in README.
// GET    :: SomeNativeType
// STORE  :: SomeBox -> TheSameBox
// BINDTO :: SomeBox -> TheSameBox
// PIL2JS.Box is a plain normal variable container, which can be assigned to
// and rebound.
PIL2JS.Box = function (value) {
  this.GET   = function ()  { return value };
  this.STORE = function (n) {
    var new_val   = n.GET();
    var my_ctype  = this.container_type;
    var new_ctype = n.container_type;

    // Rewrite rules (kind of context simulation)
    // my @a = "hi" --> my @a = ("hi",)
    if(
      my_ctype  == PIL2JS.ContainerType.Array &&
      new_ctype == PIL2JS.ContainerType.Scalar
    ) {
      new_val = _26main_3a_3ainfix_3a_2c.GET()([PIL2JS.Context.SlurpyAny, n]).GET();
    // my @a = %h
    } else if(
      my_ctype  == PIL2JS.ContainerType.Array &&
      new_ctype == PIL2JS.ContainerType.Hash
    ) {
      var pairs = new_val.pairs();
      for(var i = 0; i < pairs.length; i++) {
        pairs[i] = new PIL2JS.Box.Constant(pairs[i]);
      }
      new_val =
       _26main_3a_3ainfix_3a_2c.GET()([PIL2JS.Context.SlurpyAny].concat(pairs)).GET();
    // my @a = @b (copy @b, don't bind)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Array &&
      new_ctype == PIL2JS.ContainerType.Array
    ) {
      new_val =
       _26main_3a_3ainfix_3a_2c.GET()([PIL2JS.Context.SlurpyAny].concat(new_val)).GET();

    // my %a = (a => 1, b => 2) (or generally my %a = @a) --> my %a = hash(a => 1, b => 2)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Hash &&
      new_ctype == PIL2JS.ContainerType.Array) {
      new_val = _26main_3a_3ahash.GET()([PIL2JS.Context.SlurpyAny, n]).GET();
    // my %a = %b (copy %b, don't bind)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Hash &&
      new_ctype == PIL2JS.ContainerType.Hash
    ) {
      var pairs = new_val.pairs();
      for(var i = 0; i < pairs.length; i++) {
        pairs[i] = new PIL2JS.Box.Constant(pairs[i]);
      }
      // &hash takes care of the copying.
      new_val =
        _26main_3a_3ahash.GET()([PIL2JS.Context.SlurpyAny].concat(pairs)).GET();
    // my %a = (a => 1) or my %a = 10
    } else if(
      my_ctype  == PIL2JS.ContainerType.Hash &&
      new_ctype == PIL2JS.ContainerType.Scalar
    ) {
      new_val = _26main_3a_3ahash.GET()([PIL2JS.Context.SlurpyAny, n]).GET();

    // my $scalar = @array or my $scalar = %hash (should auto-ref)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Scalar &&
      new_ctype != PIL2JS.ContainerType.Scalar
    ) {
      new_val =
        _26main_3a_3aprefix_3a_5c.GET()([PIL2JS.Context.ItemAny, n]).GET();
    }

    value = new_val;
    return this;
  };
  this.uid   = PIL2JS.new_uid();
  this.container_type = PIL2JS.container_type(value);
};

PIL2JS.Box.prototype = {
  BINDTO: function (other) {
    if(
      (this.uid != undefined && other.uid != undefined && this.uid == other.uid) ||
      (this.uid == undefined && other.uid == undefined && this.GET() == other.GET())
    ) {
      PIL2JS.die("Binding would create a bind cycle!");
    }

    var get   = other.GET,   my_ctype    = this.container_type,
        store = other.STORE, other_ctype = other.container_type;

    // No problem: $foo := $bar, @foo := @bar, %foo := %bar
    if(my_ctype == other_ctype) {
      this.GET   = get;
      this.STORE = function (n) { store(n); return this };
      this.uid   = other.uid;
    // Problematic: $foo := @array, $foo := %hash
    // See http://www.nntp.perl.org/group/perl.perl6.language/22541:
    // Right, so I guess what really happens is ref autogeneration in that
    // case, and there's no difference between
    //
    //    $x = [at]array;
    //    $x := [at]array;
    //
    // Hey, who said anything about consistency?  :-)
    } else if(my_ctype == PIL2JS.ContainerType.Scalar) {
      var val    = get();
      this.GET   = function ()  { return new PIL2JS.Ref(new PIL2JS.Box.Constant(val)) };
      this.STORE = function (n) { val = n.GET(); return this };
      this.uid   = other.uid;
    } else if(my_ctype == PIL2JS.ContainerType.Array && other_ctype == PIL2JS.ContainerType.Scalar) {
      var other_val = get();
      if(other_val instanceof PIL2JS.Ref && other_val.referencee.GET() instanceof Array) {
        // Ok.
        var other_box = other_val.referencee;
        this.GET   = other_box.GET;
        this.STORE = other_box.STORE;
        this.uid   = other.uid;
      } else {
        PIL2JS.die("Can't use \"" + this.GET() + "\" as an array of array reference!");
      }
    } else if(my_ctype == PIL2JS.ContainerType.Hash && other_ctype == PIL2JS.ContainerType.Scalar) {
      var other_val = get();
      if(other_val instanceof PIL2JS.Ref && other_val.referencee.GET() instanceof PIL2JS.Hash) {
        // Ok.
        var other_box = other_val.referencee;
        this.GET   = other_box.GET;
        this.STORE = other_box.STORE;
        this.uid   = other.uid;
      } else {
        PIL2JS.die("Can't use \"" + this.GET() + "\" as a hash or hash reference!");
      }
    // Impossible (confirmed by Larry:
    // http://www.nntp.perl.org/group/perl.perl6.language/22535)
    // @foo := %bar, %bar := @foo
    } else {
      PIL2JS.die("Can't bind arrays to hashes or hashes to arrays!");
    }

    return this;
  },

  // Needed for "is copy".
  copy: function () {
    return new PIL2JS.Box(this.GET());
  },

  // Return us as a native JavaScript object.
  toNative: function () {
    var unboxed = this.GET();

    // Special magic for Array: Call .toNative() for each element.
    if(unboxed instanceof Array) {
      var arr = [];
      for(var i = 0; i < unboxed.length; i++) {
        arr.push(unboxed[i] == undefined ? undefined : unboxed[i].toNative());
      }
      return arr;

    // Special magic for Function: Create a wrapper function which wraps all
    // arguments in PIL2JS.Boxes and unwraps the results.
    // real_func    :: BoxedArgs  -> BoxedResults
    // wrapper_func :: NativeArgs -> NativeResult
    } else if(unboxed instanceof Function) {
      return function () {
        var args = []; //PIL2JS.Context.ItemAny.GET()].concat(arguments);
        for(var i = 0; i < arguments.length; i++)
          args[i] = new PIL2JS.Box.Constant(arguments[i]);
        return unboxed([PIL2JS.Context.ItemAny].concat(args)).toNative();
      };

    } else if(unboxed instanceof Boolean) {
      return unboxed == true ? true : false;

    } else if(unboxed instanceof PIL2JS.Ref) {
      return unboxed.referencee.toNative();

    // Special magic for string: Work aroung IE bug.
    } else if(typeof unboxed == "string") {
      // Convert "\n"s (IE...)
      return unboxed.replace(/\n/, PIL2JS.LF);

    // Else: simply return the unboxed thing.
    } else {
      return unboxed;
    }
  },

  // This is an all-PIL2JS.Box-objects global hash mapping method name to
  // (boxed) method Code object. Of course, we'll have to check the class of
  // the invocant later, but for now that's ok.
  perl_methods: {}

  /*
    toString: function () {
      _26main_3a_3aprefix_3a_7e.GET()([this]);
    },
  */
};

// PIL2JS.Box.Proxy is the equivalent of Perl's Proxy class.
PIL2JS.Box.Proxy = function (get, store) {
  this.GET   = get;
  this.STORE = store;
  this.uid   = PIL2JS.new_uid();
  this.container_type = PIL2JS.container_type(get());
};

// new PIL2JS.Box.Readonly(some_existing_box) returns a new box which cannot be
// assigned to. Necessary for sub params without "is rw".
PIL2JS.Box.ReadOnly = function (box) {
  this.GET   = function ()  { return box.GET() };
  this.STORE = function (n) { PIL2JS.die("Can't modify constant item!"); return n };
  this.uid   = box.uid;
  this.container_type = box.container_type;
};

// Returns a new box wrapping a constant value.
// Assignment and rebinding will, of course, not work.
PIL2JS.Box.Constant = function (value) {
  this.GET    = function ()  { return value };
  this.STORE  = function (n) { PIL2JS.die("Can't modify constant item!") };
  this.BINDTO = function (o) { PIL2JS.die("Can't rebind constant item!") };
  this.uid    = undefined;
  this.container_type = PIL2JS.container_type(value);
};

// Returns a stub box -- all calls will die.
PIL2JS.Box.Stub = function (value) {
  this.GET    = function ()  { PIL2JS.die(".GET() of a PIL2JS.Box.Stub called!") }
  this.STORE  = function (n) { PIL2JS.die(".STORE() of a PIL2JS.Box.Stub called!") };
  this.BINDTO = function (o) { PIL2JS.die(".BINDTO() of a PIL2JS.Box.Stub called!") };
  this.uid    = function ()  { PIL2JS.die(".uid() of a PIL2JS.Box.Stub called!") };
  this.container_type = PIL2JS.ContainerType.Scalar;
};

// Inheritance.
PIL2JS.Box.Proxy.prototype    = PIL2JS.Box.prototype;
PIL2JS.Box.ReadOnly.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Constant.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Stub.prototype     = PIL2JS.Box.prototype;

PIL2JS.Box.constant_func = function (arity, f) {
  f.pil2js_arity = arity;
  return new PIL2JS.Box.Constant(f);
};

PIL2JS.ContainerType = {
  Scalar: { toString: function () { return "<PIL2JS.ContainerType.Scalar>" } },
  Array:  { toString: function () { return "<PIL2JS.ContainerType.Array>"  } },
  Hash:   { toString: function () { return "<PIL2JS.ContainerType.Hash>"   } }
};

PIL2JS.container_type = function (thing) {
  if(thing == undefined) {
    return PIL2JS.ContainerType.Scalar;
  } else if(thing instanceof Array) {
    return PIL2JS.ContainerType.Array;
  } else if(thing instanceof PIL2JS.Hash) {
    return PIL2JS.ContainerType.Hash;
  } else {
    return PIL2JS.ContainerType.Scalar;
  }
}

Object.prototype.toPIL2JSBox = function () { return new PIL2JS.Box.Constant(this) };
Array.prototype.toPIL2JSBox  = function () {
  var ret = [];
  for(var i = 0; i < this.length; i++) {
    ret.push(PIL2JS.box_native_result(this[i]));
  }
  return new PIL2JS.Box.Constant(ret);
};

PIL2JS.box_native_result = function (res) {
  if(res == undefined) {
    return new PIL2JS.Box.Constant(res);
  } else {
    return res.toPIL2JSBox();
  }
};

// Call (possibly native sub) sub with args
PIL2JS.call = function (inv, sub, args) {
  if(sub == undefined)
    PIL2JS.die("Use of uninitialized value in subroutine entry!");

  // It's a plain sub (i.e. not method) call.
  if(inv == undefined) {
    // It's a boxed (and therefore Perl 6) sub.
    if(sub.GET) {
      var ret = sub.GET()(args);
      if(ret == undefined)
        PIL2JS.die("Internal error: Boxed sub returned unboxed undefined!");
      return ret;
    } else {
      var code = "PIL2JS.box_native_result(sub(";
      // We start from 1 instead of 0 here because we exclude the cxt.
      for(var i = 1; i < args.length; i++) {
        code += "args[" + i + "].toNative(),";
      }
      if(args.length > 1) code = code.slice(0, -1);
      code += "))";
      return eval(code);
    }
  } else {
    if(inv.GET) {
      if(inv.perl_methods[sub]) {
        return PIL2JS.call(undefined, inv.perl_methods[sub], [args[0], inv].concat(args.slice(1)));
      } else {
        PIL2JS.die("No such method: \"" + sub + "\"");
      }
    } else {
      return PIL2JS.call(undefined, inv[sub], args);
    }
  }
};

// Flatten inp_arr (one level):
// [1,2,[3,4,5],[6,[7,8]]] -> [1,2,3,4,5,6,[7,8]]
PIL2JS.make_slurpy_array = function (inp_arr) {
  var out_arr = [];

  for(var i = 0; i < inp_arr.length; i++) {
    if(inp_arr[i].GET() instanceof Array) {
      add_arr = [];
      for(var j = 0; j < inp_arr[i].GET().length; j++) {
        add_arr.push(
          inp_arr[i].GET()[j] == undefined
          ? new PIL2JS.Box.Constant(undefined)
          : inp_arr[i].GET()[j]
        );
      }
      out_arr = out_arr.concat(add_arr);
    } else if(inp_arr[i].GET() instanceof PIL2JS.Hash) {
      var pairs = inp_arr[i].GET().pairs();
      for(var i = 0; i < pairs.length; i++) {
        pairs[i] = new PIL2JS.Box.Constant(pairs[i]);
      }
      out_arr = out_arr.concat(pairs);
    } else {
      out_arr.push(inp_arr[i]);
    }
  }

  return out_arr;
};

// StubIO class.
PIL2JS.StubIO = function () {};
PIL2JS.StubIO.prototype = {
  print: new PIL2JS.Box.Constant(function (args) {
    return _26main_3a_3aprint.GET()(args);
  }),
  say: new PIL2JS.Box.Constant(function (args) {
    return _26main_3a_3asay.GET()(args);
  })
};

// Magical variables: $?POSITION and $!.
var _24main_3a_3a_3fPOSITION = new PIL2JS.Box("<unknown>");
var _24main_3a_3a_21         = new PIL2JS.Box(undefined);
var _25main_3a_3aENV         = new PIL2JS.Box(new PIL2JS.Hash);
var _40main_3a_3a_2aEND      = new PIL2JS.Box([]);
var _24main_3a_3a_2aERR      = new PIL2JS.StubIO;
var _24main_3a_3aERR         = _24main_3a_3a_2aERR;
var _40main_3a_3a_2aINC      = new PIL2JS.Box([]);
var _26main_3a_3a_3fBLOCK    = new PIL2JS.Box(undefined);
var _26main_3a_3a_3fSUB      = new PIL2JS.Box(undefined);
var _24main_3a_3a_2aOS       = new PIL2JS.Box.Constant("browser");
var _24main_3a_3aOS          = _24main_3a_3a_2aOS;
var _24main_3a_3a_           = new PIL2JS.Box(undefined);
// Stub for $?CALLER::CALLER::POSITION, so Test.pm doesn't die on a failed
// test.
var _24_3fCALLER_3a_3aCALLER_3a_3aCALLER_3a_3aPOSITION =
  new PIL2JS.Box("<$?CALLER::CALLER::POSITION not yet implemented>");
var _24_3fCALLER_3a_3aCALLER_3a_3aSUBNAME =
  new PIL2JS.Box("<$?CALLER::CALLER::SUBNAME not yet implemented>");

// Prettyprint an error msg.
PIL2JS.new_error = function (msg) {
  return new Error(msg.slice(-1, 1) == "\n"
    ? msg
    : msg + " at " + _24main_3a_3a_3fPOSITION.toNative()
  );
};

// &warn and &die.
PIL2JS.warn = function (msg) { alert(PIL2JS.new_error(msg)) };
PIL2JS.die = function (msg) {
  var error = PIL2JS.new_error(msg);
  throw(error);
};

// &return, &leave, &last, &next are all implemented using faked escape
// continuations, i.e. exceptions.
PIL2JS.ControlException       = {};
PIL2JS.ControlException.last  = function () {};
PIL2JS.ControlException.last.prototype.toString =
  function () { return "Can't \"last\" outside a loop block!" };
PIL2JS.ControlException.next  = function () {};
PIL2JS.ControlException.next.prototype.toString =
  function () { return "Can't \"next\" outside a loop block!" };
PIL2JS.ControlException.redo  = function () {};
PIL2JS.ControlException.redo.prototype.toString =
  function () { return "Can't \"redo\" outside a loop block!" };
PIL2JS.ControlException.ret   = function (level, retval) {
  // The sublevel (SUBROUTINE, SUBBLOCK, etc.) the &return/&leave/whatever is
  // destined to.
  this.level        = level;
  // The (boxed) return value.
  this.return_value = retval;

  // Slightly hacky: Display an error if the user return()s outside a sub.
  this.toString     = function () {
    var msg = 
      "Can't return outside a " + level + "-routine. at " +
      _24main_3a_3a_3fPOSITION.toNative();
    alert(msg);
    return msg;
  }
};

// &PIL2JS::Internals::generic_return -- generates a function, which, when
// invoked, will cause a return of the given level by throwing an appropriate
// exception.
var _26PIL2JS_3a_3aInternals_3a_3ageneric_return =
  new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var level = args[0].toNative();
    return new PIL2JS.Box.Constant(function (args) {
      var cxt_ = args.shift();
      args     = PIL2JS.make_slurpy_array(args);
      var ret  =
        args.length >  1 ? new PIL2JS.Box.Constant(args) :
        args.length == 1 ? args[0] :
        new PIL2JS.Box.Constant(undefined);
      throw(new PIL2JS.ControlException.ret(level, ret));
    });
  });

PIL2JS.call_chain = [];

// Greps args for PIL2JS.Pairs and returns them as a hash.
PIL2JS.grep_for_pairs = function (args) {
  var pairs = {};

  for(var i = 0; i < args.length; i++) {
    if(args[i].GET() instanceof PIL2JS.Pair) {
      pairs[args[i].GET().key.toNative()] = args[i].GET().value;
    }
  }

  return pairs;
};

// *@foo sets @foo's .flatten_me property to true.
// Here, we expand these flattened arrays.
PIL2JS.possibly_flatten = function (args) {
  var ret = [];

  for(var i = 0; i < args.length; i++) {
    if(args[i].GET() instanceof Array && args[i].GET().flatten_me) {
      var add_arr = [];
      for(var j = 0; j < args[i].GET().length; j++) {
        add_arr.push(
          args[i].GET()[j] == undefined
          ? new PIL2JS.Box.Constant(undefined)
          : args[i].GET()[j]
        );
      }
      ret = ret.concat(add_arr);
    } else if(args[i].GET() instanceof PIL2JS.Hash && args[i].GET().flatten_me) {
      var pairs = args[i].GET().pairs();
      for(var j = 0; j < pairs.length; j++) {
        pairs[j] = new PjL2JS.Box.Constant(pairs[j]);
      }
      ret = ret.concat(pairs);
    } else {
      ret.push(args[i]);
    }
  }

  return ret;
};

// Searches args for pairs and deletes the pairs where .key eq name.
PIL2JS.delete_pair_from_args = function (args, name) {
  var n = [];

  for(var i = 0; i < args.length; i++) {
    if(!(args[i].GET() instanceof PIL2JS.Pair && args[i].GET().key.toNative() == name)) {
      n.push(args[i]);
    }
  }

  return n;
};

// Context class.
PIL2JS.Context = function (cxt) {
  this["main"] = cxt["main"];
  this["type"] = cxt["type"];
  return this;
};
PIL2JS.Context.prototype = {
  toString: function () {
    return "Context.new(:main[" + this["main"] + "], :type[" + this["type"] + "])";
  }
};

PIL2JS.Context.Void      = new PIL2JS.Box.Constant(new PIL2JS.Context({ main: "void" }));
PIL2JS.Context.ItemAny   = new PIL2JS.Box.Constant(new PIL2JS.Context({ main: "item", type: "Any" }));
PIL2JS.Context.SlurpyAny = new PIL2JS.Box.Constant(new PIL2JS.Context({ main: "slurpy", type: "Any" }));

// Doesn't work yet -- load a JSAN mod.
PIL2JS.use_jsan = function (mod) {
  if(JSAN == undefined)
    PIL2JS.die("JSAN library not loaded!");

  mod = mod.replace(/::/, ".");
  JSAN.prototype.use.apply(JSAN.prototype, [mod]);
};

PIL2JS.catch_all_exceptions = function (code) {
  try { code() } catch(err) {
    alert(err);
  }
};

// &*ref.
var _26main_3a_3aref = PIL2JS.Box.constant_func(1, function (args) {
  var thing = args[1].GET();
  if(typeof(thing) == "string" || thing instanceof String) {
    return new PIL2JS.Box.Constant("Str");
  } else if(typeof(thing) == "boolean" || thing instanceof Boolean) {
    return new PIL2JS.Box.Constant("Bool");
  } else if(typeof(thing) == "number" || thing instanceof Number) {
    return new PIL2JS.Box.Constant("Num");
  } else if(thing instanceof Array) {
    return new PIL2JS.Box.Constant("Array");
  } else if(thing instanceof PIL2JS.Ref) {
    return new PIL2JS.Box.Constant("Ref");
  } else if(thing instanceof PIL2JS.Hash) {
    return new PIL2JS.Box.Constant("Hash");
  } else if(thing instanceof PIL2JS.Pair) {
    return new PIL2JS.Box.Constant("Pair");
  } else if(thing instanceof Function) {
    return new PIL2JS.Box.Constant("Code");
  } else {
    PIL2JS.die(
      "Internal error: .ref() not yet implemented for " +
      typeof(thing) +
      "\n"
    );
  }
});
_26main_3a_3aref.perl_name = "&main::ref";
PIL2JS.Box.prototype.perl_methods["ref"] = _26main_3a_3aref;

var _26main_3a_3aisa = PIL2JS.Box.constant_func(1, function (args) {
  var self = args[1], cmptype = args[2].GET(), ref = _26main_3a_3aref;
  var type = ref.GET()([PIL2JS.Context.ItemAny, self]).GET();
  return new PIL2JS.Box.Constant(type == cmptype);
});
_26main_3a_3aisa.perl_name = "&main::isa";
PIL2JS.Box.prototype.perl_methods["isa"] = _26main_3a_3aisa;

var _26main_3a_3aprefix_3a_5c = PIL2JS.Box.constant_func(1, function (args) {
  var thing = args[1];
  return new PIL2JS.Box.Constant(new PIL2JS.Ref(thing));
});
_26main_3a_3aprefix_3a_5c.perl_name = "&main::prefix:\\";
