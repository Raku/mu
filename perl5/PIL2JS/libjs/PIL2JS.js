// This is the part of the Prelude for JavaScript which is written in
// JavaScript. See lib6/Prelude/JS.pm for the part written in Perl 6.

// Ensure the MetaModel is loaded.
try { Perl6.Object } catch(err) {
  var error = new Error("Perl6.MetaModel not loaded; aborting.");
  alert(error);
  throw(error);
}

// Create our namespace.
if(PIL2JS == undefined) var PIL2JS = {};
// Trick to speed up PIL2JS, by putter:
// We'll lexicalize PIL2JS in all functions by "var PIL2JS = AlsoPIL2JS".
// This gives a ~~10% speedup, as PIL2JS is then a nice local variable instead
// of a global one.
var AlsoPIL2JS_SpeedupHack = PIL2JS;

// Optimization, needed by P5 Prelude::JS
PIL2JS.if_undefined = function (thing, otherwise) {
  return thing == undefined ? otherwise : thing;
}

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
PIL2JS.Hash = function () { this.entries = {}; this.num_of_entries = 0 };
PIL2JS.Hash.prototype.add_pair = function (pair) {
  var key = pair.key.toNative();
  if(!this.entries[key]) this.num_of_entries++;
  this.entries[key] = pair;
};
PIL2JS.Hash.prototype.exists = function (key) {
  var ikey = key.toNative();
  return this.entries[ikey] != undefined;
};
PIL2JS.Hash.prototype.delete_key = function (key) {
  var old = this.get_value(key),
      key = key.toNative();
  if(this.entries[key]) this.num_of_entries--;
  delete this.entries[key];
  return old;
};
PIL2JS.Hash.prototype.pairs = function () {
  var pairs = [];
  for(var internal_key in this.entries) {
    pairs.push(this.entries[internal_key]);
  }
  return pairs;
};
PIL2JS.Hash.prototype.keys = function () {
  var keys  = [];
  var pairs = this.pairs();
  for(var i = 0; i < pairs.length; i++) {
    keys.push(pairs[i].key);
  }
  return keys;
};
PIL2JS.Hash.prototype.get_value = function (key) {
  return this.exists(key) ? this.entries[key.toNative()].value : undefined;
};
PIL2JS.Hash.prototype.toString = function () { return "<PIL2JS.Hash>" };

// class Pair { has $.key; has $.value }
PIL2JS.Pair = function (key, value) {
  this.key   = key;
  this.value = value;
};
PIL2JS.Pair.prototype.toString = function () { return "<PIL2JS.Pair>" };

// Ref class.
PIL2JS.Ref = function (referencee) {
  this.referencee = referencee;
  this.autoderef  = referencee.FETCH() instanceof Array
                 || referencee.FETCH() instanceof PIL2JS.Hash;
  // || referencee instanceof PIL2JS.OwnObject, see
  // http://www.nntp.perl.org/group/perl.perl6.language/22532.
  return this;
};
PIL2JS.Ref.prototype.toString = function () { return "<PIL2JS.Ref>" };

// This is necessary to emulate pass by ref, needed for is rw and is ref.
// See section "DESIGN" in README.
// FETCH  :: SomeNativeType
// STORE  :: SomeBox -> TheSameBox
// BINDTO :: SomeBox -> TheSameBox
// PIL2JS.Box is a plain normal variable container, which can be assigned to
// and rebound.
PIL2JS.Box = function (value) {
  this.FETCH = function ()  { return value };
  this.STORE = function (n) {
    var new_val   = n.FETCH();
    var my_ctype  = this.container_type;
    var new_ctype = n.container_type;

    // Rewrite rules (kind of context simulation)
    // my @a = "hi" --> my @a = ("hi",)
    if(
      my_ctype  == PIL2JS.ContainerType.Array &&
      new_ctype == PIL2JS.ContainerType.Scalar
    ) {
      new_val = PIL2JS.cps2normal(_26main_3a_3ainfix_3a_2c.FETCH(), [PIL2JS.Context.SlurpyAny, n]).FETCH();
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
        PIL2JS.cps2normal(
          _26main_3a_3ainfix_3a_2c.FETCH(), [PIL2JS.Context.SlurpyAny].concat(pairs)
        ).FETCH();
    // my @a = @b (copy @b, don't bind)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Array &&
      new_ctype == PIL2JS.ContainerType.Array
    ) {
      new_val =
        PIL2JS.cps2normal(
          _26main_3a_3ainfix_3a_2c.FETCH(), [PIL2JS.Context.SlurpyAny].concat(new_val)
        ).FETCH();

    // my %a = (a => 1, b => 2) (or generally my %a = @a) --> my %a = hash(a => 1, b => 2)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Hash &&
      new_ctype == PIL2JS.ContainerType.Array) {
      new_val = PIL2JS.cps2normal(_26main_3a_3ahash.FETCH(), [PIL2JS.Context.SlurpyAny, n]).FETCH();
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
        PIL2JS.cps2normal(
          _26main_3a_3ahash.FETCH(), [PIL2JS.Context.SlurpyAny].concat(pairs)
        ).FETCH();
    // my %a = (a => 1) or my %a = 10
    } else if(
      my_ctype  == PIL2JS.ContainerType.Hash &&
      new_ctype == PIL2JS.ContainerType.Scalar
    ) {
      new_val = PIL2JS.cps2normal(_26main_3a_3ahash.FETCH(), [PIL2JS.Context.SlurpyAny, n]).FETCH();

    // my $scalar = @array or my $scalar = %hash (should auto-ref)
    } else if(
      my_ctype  == PIL2JS.ContainerType.Scalar &&
      new_ctype != PIL2JS.ContainerType.Scalar
    ) {
      new_val =
        PIL2JS.cps2normal(
         _26main_3a_3aprefix_3a_5c.FETCH(), [PIL2JS.Context.ItemAny, n]
        ).FETCH();
    }

    value = new_val;
    return this;
  };
  this.uid            = PIL2JS.new_uid();
  this.container_type = PIL2JS.container_type(value);
};

PIL2JS.Box.prototype = {
  BINDTO: function (other) {
    if(
      (this.uid != undefined && other.uid != undefined && this.uid == other.uid) ||
      (this.uid == undefined && other.uid == undefined && this.FETCH() == other.FETCH())
    ) {
      // PIL2JS.die("Binding would create a bind cycle!");
      // Bind cycles are actually legal.
    }

    var fetch = other.FETCH, my_ctype    = this.container_type,
        store = other.STORE, other_ctype = other.container_type;

    // No problem: $foo := $bar, @foo := @bar, %foo := %bar
    if(my_ctype == other_ctype) {
      this.FETCH      = fetch;
      this.STORE      = function (n) { store(n); return this };
      this.uid        = other.uid;
      this.isConstant = other.isConstant;
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
      var val         = fetch();
      this.FETCH      = function ()  { return new PIL2JS.Ref(new PIL2JS.Box.Constant(val)) };
      this.STORE      = function (n) { val = n.FETCH(); return this };
      this.uid        = other.uid;
      this.isConstant = other.isConstant;
    } else if(my_ctype == PIL2JS.ContainerType.Array && other_ctype == PIL2JS.ContainerType.Scalar) {
      var other_val = fetch();
      if(other_val instanceof PIL2JS.Ref && other_val.referencee.FETCH() instanceof Array) {
        // Ok.
        var other_box   = other_val.referencee;
        this.FETCH      = other_box.FETCH;
        this.STORE      = other_box.STORE;
        this.uid        = other.uid;
        this.isConstant = other.isConstant;
      } else {
        PIL2JS.die("Can't use object of type \"" + PIL2JS.cps2normal(_26main_3a_3aref.FETCH(), [PIL2JS.Context.ItemAny, other]).toNative() + "\" as an array or array reference!");
      }
    } else if(my_ctype == PIL2JS.ContainerType.Hash && other_ctype == PIL2JS.ContainerType.Scalar) {
      var other_val = fetch();
      if(other_val instanceof PIL2JS.Ref && other_val.referencee.FETCH() instanceof PIL2JS.Hash) {
        // Ok.
        var other_box   = other_val.referencee;
        this.FETCH      = other_box.FETCH;
        this.STORE      = other_box.STORE;
        this.uid        = other.uid;
        this.isConstant = other.isConstant;
      } else {
        PIL2JS.die("Can't use object of type \"" + PIL2JS.cps2normal(_26main_3a_3aref.FETCH(), [PIL2JS.Context.ItemAny, other]).toNative() + "\" as a hash or hash reference!");
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
    return new PIL2JS.Box(this.FETCH());
  },

  // Return us as a native JavaScript object.
  toNative: function () {
    var unboxed = this.FETCH();

    // Special magic for Array: Call .toNative() for each element.
    if(unboxed instanceof Array) {
      var arr = [];
      for(var i = 0; i < unboxed.length; i++) {
        arr.push(unboxed[i] == undefined ? undefined : unboxed[i].toNative());
      }
      if(unboxed.flatten_me) arr.flatten_me = unboxed.flatten_me;
      return arr;

    } else if(unboxed instanceof PIL2JS.Hash) {
      var hash = {};
      for(var key in unboxed.entries) {
        hash[key] = unboxed.entries[key].value.toNative();
      }
      return hash;

    } else if(unboxed instanceof PIL2JS.Pair) {
      var hash = {};
      hash[unboxed.key.toNative()] = unboxed.value.toNative();

    // Special magic for Function: Create a wrapper function which wraps all
    // arguments in PIL2JS.Boxes and unwraps the results.
    // real_func    :: BoxedArgs  -> BoxedResults
    // wrapper_func :: NativeArgs -> NativeResult
    } else if(unboxed instanceof Function) {
      return function () {
        var args = []; //PIL2JS.Context.ItemAny.FETCH()].concat(arguments);
        for(var i = 0; i < arguments.length; i++)
          args[i] = new PIL2JS.Box.Constant(arguments[i]);

        // Of course, this will break if unboxed does call/cc magic.
        var retval = PIL2JS.cps2normal(unboxed, [PIL2JS.Context.ItemAny].concat(args));
        if(retval == undefined)
          retval = new PIL2JS.Box.Constant(undefined);
        // PIL2JS.die("Continuation wasn't called!");
        return retval.toNative();
      };

    } else if(unboxed instanceof Boolean) {
      return unboxed == true ? true : false;

    } else if(unboxed instanceof PIL2JS.Ref) {
      return unboxed.referencee.toNative();

    // Special magic for string: Work around IE bug.
    } else if(typeof unboxed == "string") {
      // Convert "\n"s (IE...)
      return unboxed.replace(/\n/, PIL2JS.LF);

    // Else: simply return the unboxed thing.
    } else {
      return unboxed;
    }
  }
};

// PIL2JS.Box.Proxy is the equivalent of Perl's Proxy class.
PIL2JS.Box.Proxy = function (fetch, store) {
  this.FETCH = fetch;
  this.STORE = store;
  this.uid   = PIL2JS.new_uid();
  this.container_type = PIL2JS.container_type(fetch());
};

// new PIL2JS.Box.Readonly(some_existing_box) returns a new box which cannot be
// assigned to. Necessary for sub params without "is rw".
PIL2JS.Box.ReadOnly = function (box) {
  this.FETCH = function ()  { return box.FETCH() };
  this.STORE = function (n) { PIL2JS.die("Can't modify readonly item!"); return n };
  this.uid   = box.uid;
  this.container_type = box.container_type;
};

// Returns a new box wrapping a constant value.
// Assignment and rebinding will, of course, not work.
PIL2JS.Box.Constant = function (value) {
  this.FETCH  = function ()  { return value };
  this.STORE  = function (n) { PIL2JS.die("Can't modify constant item!") };
  this.BINDTO = function (o) { PIL2JS.die("Can't rebind constant item!") };
  this.uid    = undefined;
  this.container_type = PIL2JS.container_type(value);
  this.isConstant     = true;
};

// Returns a stub box -- all calls will die.
PIL2JS.Box.Stub = function (value) {
  this.FETCH  = function ()  { PIL2JS.die(".FETCH() of a PIL2JS.Box.Stub called!") }
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

PIL2JS.toPIL2JSBox = function (thing) {
  // I'd do this as Object.prototype.toPIL2JSBox, but this causes severe
  // problems, is a hack, and causes the MetaModel to not work, as it depends
  // on for(var k in some_object) to not return "toPIL2JSBox".
  if(thing instanceof Array) {
    var ret = [];
    for(var i = 0; i < thing.length; i++) {
      ret.push(PIL2JS.box_native_result(thing[i]));
    }
    if(thing.flatten_me) ret.flatten_me = thing.flatten_me;
    return new PIL2JS.Box.Constant(ret);
  } else if(thing instanceof Function) {
    return PIL2JS.Box.constant_func(thing.arity, function (args) {
      PIL2JS.call(undefined, thing, args);
    });
  } else {
    return new PIL2JS.Box.Constant(thing);
  }
}

PIL2JS.box_native_result = function (res) {
  if(res == undefined) {
    return new PIL2JS.Box.Constant(undefined);
  } else {
    return PIL2JS.toPIL2JSBox(res);
  }
};

// Hack to work around JS not providing .tailcall... :(
PIL2JS.runloop = function (f) {
  var was_in_errhandler = true;
  while(was_in_errhandler) {
    was_in_errhandler = false;
    try { f() } catch(err) {
      if(err instanceof Function) {
        f = err;
        was_in_errhandler = true;
      } else {
        throw err;
      }
    }
  }
};

// Call (possibly native sub) sub with args
PIL2JS.call = function (inv, sub, args) {
  if(sub == undefined)
    PIL2JS.die("Use of uninitialized value in subroutine entry!");

  // It's a plain sub (i.e. not method) call.
  if(inv == undefined) {
    // It's a boxed (and therefore Perl 6) sub.
    if(sub.FETCH) {
      sub.FETCH()(args);
    } else {
      var cxt  = args.shift();
      var cc   = args.pop();
      var code = "PIL2JS.box_native_result(sub(";
      for(var i = 0; i < args.length; i++) {
        code += "args[" + i + "].toNative(),";
      }
      if(args.length > 0) code = code.slice(0, -1);
      code += "))";
      cc(eval(code));
    }

  // It's a method call.
  } else {
    if(inv.FETCH) {
      var val    = inv.FETCH();
      // Possibly autoderef Refs.
      if(val instanceof PIL2JS.Ref && val.autoderef) val = val.referencee.FETCH();

      // Check if we're calling a real object part of the MetaModel or
      // something other.
      var isreal = val instanceof Perl6.Instance
                || val instanceof Perl6.MetaClass
                || val instanceof Perl6.Method
                || val instanceof Perl6.Class;

      // We're calling a method on a native JS object (Array, PIL2JS.Hash,
      // etc.)?
      if(!isreal) {
        // So create an instance of the corresponding Perl 6 class (which is
        // part of the metamodel).
        var realclass = PIL2JS.nativeclass2realclass(
          val == undefined
            ? val
            : val.constructor
        );
        if(!realclass) realclass = _3amain_3a_3aItem;
        val    = new Perl6.Instance(realclass.FETCH());
        isreal = true;
      }

      // All classes we create inherit from __PIL2JS. This is so we can detect
      // that our classes are really our's.
      var isour  = isreal && val.isa("__PIL2JS");

      // It is a real object, but it doesn't belong to us: Cut off the context
      // and inv info and call the method the official way.
      if(isreal && !isour) {
        var cc = args.pop();
        cc(PIL2JS.Box.Constant(call_method.apply([val, sub].concat(args.slice(2)))));

      // It is a real object and it belongs to us: Retrieve the original boxed
      // sub object and call it.
      } else if(isreal && isour && val.can(sub)) {
        var boxedsub = val.can(sub).call("__i_am_pil2js");
        boxedsub.FETCH()([args[0], inv].concat(args.slice(1)));

      // Sorry.
      } else {
        PIL2JS.die("No such method: \"" + sub + "\"");
      }

    // It is a native JS object. Retrieve a Function reference and re-call
    // PIL2JS.call, as a native JS method may return a boxed object.
    } else {
      return PIL2JS.call(undefined, inv[sub], args);
    }
  }
};

PIL2JS.__PIL2JSClass   = new Perl6.Class("__PIL2JS", { "class": { "methods": {} } });
PIL2JS.new_empty_class = function (name, superclass) {
  return new PIL2JS.Box.Constant(
    new Perl6.Class(name, {
      "is": [superclass.FETCH()],
      "class": { "methods": {} }
    })
  );
}

var _3amain_3a_3aAny       = PIL2JS.new_empty_class("Any",       new PIL2JS.Box.Constant(PIL2JS.__PIL2JSClass));
var _3amain_3a_3aItem      = PIL2JS.new_empty_class("Item",      _3amain_3a_3aAny);
var _3amain_3a_3aArray     = PIL2JS.new_empty_class("Array",     _3amain_3a_3aItem);
var _3amain_3a_3aHash      = PIL2JS.new_empty_class("Hash",      _3amain_3a_3aItem);
var _3amain_3a_3aPair      = PIL2JS.new_empty_class("Pair",      _3amain_3a_3aItem);
var _3amain_3a_3aStr       = PIL2JS.new_empty_class("Str",       _3amain_3a_3aItem);
var _3amain_3a_3aNum       = PIL2JS.new_empty_class("Num",       _3amain_3a_3aItem);
var _3amain_3a_3aInt       = PIL2JS.new_empty_class("Int",       _3amain_3a_3aNum);
var _3amain_3a_3aRat       = PIL2JS.new_empty_class("Rat",       _3amain_3a_3aNum);
var _3amain_3a_3aBool      = PIL2JS.new_empty_class("Bool",      _3amain_3a_3aItem);
var _3amain_3a_3aCode      = PIL2JS.new_empty_class("Code",      _3amain_3a_3aItem);
var _3amain_3a_3aBlock     = PIL2JS.new_empty_class("Block",     _3amain_3a_3aCode);
var _3amain_3a_3aRoutine   = PIL2JS.new_empty_class("Routine",   _3amain_3a_3aCode);
var _3amain_3a_3aSub       = PIL2JS.new_empty_class("Sub",       _3amain_3a_3aRoutine);
var _3amain_3a_3aMethod    = PIL2JS.new_empty_class("Method",    _3amain_3a_3aRoutine);
var _3amain_3a_3aSubmethod = PIL2JS.new_empty_class("Submethod", _3amain_3a_3aRoutine);
var _3amain_3a_3aMulti     = PIL2JS.new_empty_class("Multi",     _3amain_3a_3aRoutine);
var _3amain_3a_3aRule      = PIL2JS.new_empty_class("Rule",      _3amain_3a_3aRoutine);
var _3amain_3a_3aMacro     = PIL2JS.new_empty_class("Macro",     _3amain_3a_3aRoutine);
var _3amain_3a_3aRef       = PIL2JS.new_empty_class("Ref",       _3amain_3a_3aItem);
var _3amain_3a_3aJunction  = PIL2JS.new_empty_class("Junction",  _3amain_3a_3aAny);

// Returns, given a native JS object, the corresponding boxed class object.
PIL2JS.nativeclass2realclass = function (constr) {
  if(constr == Array) {
    return _3amain_3a_3aArray;
  } else if(constr == PIL2JS.Hash) {
    return _3amain_3a_3aHash;
  } else if(constr == PIL2JS.Pair) {
    return _3amain_3a_3aPair;
  } else if(constr == String) {
    return _3amain_3a_3aStr;
  } else if(constr == Number) {
    return _3amain_3a_3aNum;
  } else if(constr == Function) {
    return _3amain_3a_3aCode;
  } else if(constr == PIL2JS.Junction.Any || constr == PIL2JS.Junction.All || constr == PIL2JS.Junction.One || constr == PIL2JS.Junction.None) {
    return _3amain_3a_3aJunction;
  } else if(constr == PIL2JS.Ref) {
    return _3amain_3a_3aRef;
  }
};

// Adds a method to a boxed class object.
PIL2JS.addmethod = function (cls, name, sub) {
  if(cls == undefined) PIL2JS.die("PIL2JS.addmethod called with undefined class!");
  cls = cls.FETCH();
  if(!(cls instanceof Perl6.Class))
    PIL2JS.die("PIL2JS.addmethod called with a weird class: \"" + cls + "\"!");
  cls.meta().add_method(name, new Perl6.Method(cls.meta(), function (check) {
    if(check != "__i_am_pil2js")
      PIL2JS.die("PIL2JS method called from outside of PIL2JS!");
    return sub;
  })); //, sub.FETCH().pil2js_arity);
};

// Flatten inp_arr (one level):
// [1,2,[3,4,5],[6,[7,8]]] -> [1,2,3,4,5,6,[7,8]]
PIL2JS.make_slurpy_array = function (inp_arr) {
  var out_arr = [];

  for(var i = 0; i < inp_arr.length; i++) {
    if(inp_arr[i].FETCH() instanceof Array) {
      add_arr = [];
      for(var j = 0; j < inp_arr[i].FETCH().length; j++) {
        add_arr.push(
          inp_arr[i].FETCH()[j] == undefined
          ? new PIL2JS.Box.Constant(undefined)
          : inp_arr[i].FETCH()[j]
        );
      }
      out_arr = out_arr.concat(add_arr);
    } else if(inp_arr[i].FETCH() instanceof PIL2JS.Hash) {
      var pairs = inp_arr[i].FETCH().pairs();
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
PIL2JS.StubIO.prototype.print = new PIL2JS.Box.Constant(function (args) {
  _26main_3a_3aprint.FETCH()(args);
});
PIL2JS.StubIO.prototype.say = new PIL2JS.Box.Constant(function (args) {
  _26main_3a_3asay.FETCH()(args);
});

// Magical variables: $?POSITION, $!, etc.
var _24main_3a_3a_3fPOSITION = new PIL2JS.Box("<unknown>");
var _24main_3a_3a_3fSUBNAME  = new PIL2JS.Box(undefined);
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
// HACKS. Needs prober integration of Perl6.MetaModel.

// Prettyprint an error msg.
PIL2JS.new_error = function (msg) {
  if(!(msg instanceof PIL2JS.Box)) msg = new PIL2JS.Box.Constant(msg);
  var errmsg = typeof(msg.FETCH()) == "string" || msg.FETCH() instanceof String
    ? msg.FETCH()
    : "<obj>";
  var err = Error(errmsg.slice(-1, 1) == "\n"
    ? errmsg
    : errmsg + " at " + _24main_3a_3a_3fPOSITION.toNative()
  );
  err.pil2js_orig_msg = msg;
  err.pil2js_pos      = _24main_3a_3a_3fPOSITION.toNative();
  return err;
};

// &warn and &die.
PIL2JS.warn = function (msg) { PIL2JS.print_exception(PIL2JS.new_error(msg)) };
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
PIL2JS.ControlException.end   = function () {};

// PIL2JS.generic_return -- generates a function, which, when invoked, will
// cause a return of the given level by throwing an appropriate exception.
PIL2JS.generic_return = function (returncc) {
  return new PIL2JS.Box.Constant(function (args) {
    var cxt = args.shift(), cc = args.pop();

    // args     = PIL2JS.make_slurpy_array(args);
    var ret  =
      args.length >  1 ? new PIL2JS.Box.Constant(args) :
      args.length == 1 ? args[0] :
      new PIL2JS.Box.Constant(undefined);

    returncc(ret, cc);
  });
};

// Entrypoints for currently active coroutines
PIL2JS.coro_entrypoints = [];

PIL2JS.already_exited = false;
var _26main_3a_3aexit = PIL2JS.Box.constant_func(1, function (args) {
  if(PIL2JS.already_exited) return;
  PIL2JS.already_exited = true;

  // Run all END blocks.
  var blocks = _40main_3a_3a_2aEND.FETCH();
  for(var i = 0; i < blocks.length; i++) {
    PIL2JS.cps2normal(blocks[i].FETCH(), [PIL2JS.Context.Void]);
  }

  /* We've finished, so we don't call the cc. */

  throw new PIL2JS.ControlException.end;
});

// Array of boxed subs we're currently in.
var PIL2JS_callchain = [];
// Array of pads we're currently in, used for $CALLER::.
// Note that the pads change at runtime, i.e.:
//   # No my $a yet
//   # PIL2JS_subpads[-1]["$a"] undefined
//   my $a;
//   # PIL2JS_subpads[-1]["$a"] is defined now.
var PIL2JS_subpads   = [];
PIL2JS.resolve_callervar = function (delta, name) {
  // delta == 0: current pad
  // delta == 1: pad of calling sub ($CALLER::foo)
  // delta == 2: pad of the sub calling the calling sub ($CALLER::CALLER::foo)
  // etc.

  if(PIL2JS_subpads.length >= delta + 1) {
    var pad = PIL2JS_subpads[PIL2JS_subpads.length - delta - 1];
    if(pad[name]) {
      return pad[name];
    } else {
      PIL2JS.die("No variable named \"" + name + "\" in caller[" + delta + "]!");
    }
  } else {
    PIL2JS.die("No caller[" + delta + "] found!");
  }
};

// Greps args for PIL2JS.Pairs and returns them as a hash.
PIL2JS.grep_for_pairs = function (args) {
  var pairs = {};

  for(var i = 0; i < args.length; i++) {
    if(args[i].FETCH() instanceof PIL2JS.Pair) {
      pairs[args[i].FETCH().key.toNative()] = args[i].FETCH().value;
    }
  }

  return pairs;
};

PIL2JS.get_and_remove_all_pairs = function (args) {
  var hash     = new PIL2JS.Hash;
  var new_args = [];

  for(var i = 0; i < args.length; i++) {
    if(args[i].FETCH() instanceof PIL2JS.Pair) {
      hash.add_pair(args[i].FETCH());
    } else {
      new_args.push(args[i]);
    }
  }

  return { args: new_args, hash: hash };
}

// *@foo sets @foo's .flatten_me property to true.
// Here, we expand these flattened arrays.
PIL2JS.possibly_flatten = function (args) {
  var ret = [];

  for(var i = 0; i < args.length; i++) {
    if(args[i].FETCH() instanceof Array && args[i].FETCH().flatten_me) {
      var add_arr = [];
      for(var j = 0; j < args[i].FETCH().length; j++) {
        add_arr.push(
          args[i].FETCH()[j] == undefined
          ? new PIL2JS.Box.Constant(undefined)
          : args[i].FETCH()[j]
        );
      }
      ret = ret.concat(add_arr);
    } else if(args[i].FETCH() instanceof PIL2JS.Hash && args[i].FETCH().flatten_me) {
      var pairs = args[i].FETCH().pairs();
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
    if(!(args[i].FETCH() instanceof PIL2JS.Pair && args[i].FETCH().key.toNative() == name)) {
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
PIL2JS.Context.prototype.toString = function () {
  return "Context.new(:main[" + this["main"] + "], :type[" + this["type"] + "])";
};

PIL2JS.Context.Void      = new PIL2JS.Context({ main: "void" });
PIL2JS.Context.ItemAny   = new PIL2JS.Context({ main: "item", type: "Any" });
PIL2JS.Context.SlurpyAny = new PIL2JS.Context({ main: "slurpy", type: "Any" });

PIL2JS.print_exception = function (err) {
  PIL2JS.cps2normal(
    _26main_3a_3asay.FETCH(), [
      PIL2JS.Context.Void,
      PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [
        PIL2JS.Context.ItemAny,
        err.pil2js_orig_msg
          ? err.pil2js_orig_msg
          : new PIL2JS.Box.Constant(err.toString())
      ])
    ]
  );
};
PIL2JS.catch_all_exceptions = function (code) {
  try { code() } catch(err) {
    PIL2JS.print_exception(err);
  }
};
PIL2JS.catch_end_exception = function (code) {
  try { code() } catch(err) {
    if(err instanceof PIL2JS.ControlException.end) {
      return;
    } else {
      throw err;
    }
  }
};

// Will, of course, break if call/cc magic is done.
PIL2JS.cps2normal = function (f, args) {
  var ret = undefined;
  PIL2JS.runloop(function () { f(args.concat(function (r) { ret = r })) });
  //if(ret == undefined) PIL2JS.die("Continuation wasn't called[" + f + "]!");
  return ret;
};

// &*ref.
var _26main_3a_3aref = PIL2JS.Box.constant_func(1, function (args) {
  var thing = args[1].FETCH();
  var cc    = args.pop();

  if(thing == undefined) {
    cc(new PIL2JS.Box.Constant("Scalar")); // XXX?
  } else if(typeof(thing) == "string" || thing instanceof String) {
    cc(new PIL2JS.Box.Constant("Str"));
  } else if(typeof(thing) == "boolean" || thing instanceof Boolean) {
    cc(new PIL2JS.Box.Constant("Bool"));
  } else if(typeof(thing) == "number" || thing instanceof Number) {
    cc(new PIL2JS.Box.Constant("Num"));
  } else if(thing instanceof Array) {
    cc(new PIL2JS.Box.Constant("Array"));
  } else if(thing instanceof PIL2JS.Hash) {
    cc(new PIL2JS.Box.Constant("Hash"));
  } else if(thing instanceof PIL2JS.Pair) {
    cc(new PIL2JS.Box.Constant("Pair"));
  } else if(thing instanceof Function) {
    cc(new PIL2JS.Box.Constant("Code"));
  } else if(thing instanceof PIL2JS.Ref && thing.autoderef) {
    _26main_3a_3aref.FETCH()([PIL2JS.Context.ItemAny, thing.referencee, cc]);
  } else if(thing instanceof PIL2JS.Ref) {
    cc(new PIL2JS.Box.Constant("Ref"));
  } else if(thing instanceof PIL2JS.Junction.Any || thing instanceof PIL2JS.Junction.All || thing instanceof PIL2JS.Junction.One || thing instanceof PIL2JS.Junction.None) {
    cc(new PIL2JS.Box.Constant("Junction"));
  } else if(thing instanceof Perl6.Class) {
    cc(new PIL2JS.Box.Constant("Class")); // XXX
  } else {
    PIL2JS.die(
      "Internal error: .ref() not yet implemented for " +
      typeof(thing) +
      "\n"
    );
  }
});
_26main_3a_3aref.perl_name = "&main::ref";
PIL2JS.addmethod(_3amain_3a_3aAny, "ref", _26main_3a_3aref);

// &*isa. hack.
var _26main_3a_3aisa = PIL2JS.Box.constant_func(1, function (args) {
  var self = args[1], cmptype = args[2].FETCH(), cc = args.pop(), ref = _26main_3a_3aref;

  ref.FETCH()([PIL2JS.Context.ItemAny, self, function (type) {
    type = type.FETCH();
    cc(new PIL2JS.Box.Constant(
      type == cmptype                      ||
      type == "Array" && cmptype == "List" ||
      type == "Any"
    ));
  }]);
});
_26main_3a_3aisa.perl_name = "&main::isa";
PIL2JS.addmethod(_3amain_3a_3aAny, "isa", _26main_3a_3aisa);

// &prefix:<\>
var _26main_3a_3aprefix_3a_5c = PIL2JS.Box.constant_func(1, function (args) {
  var thing = args[1], cc = args.pop();
  cc(new PIL2JS.Box.Constant(new PIL2JS.Ref(thing)));
});
_26main_3a_3aprefix_3a_5c.perl_name = "&main::prefix:\\";

// &prefix:<~>. Written in JS instead of P6 for speed, as &prefix:<~> gets
// called often.
var _26main_3a_3aprefix_3a_7e = PIL2JS.Box.constant_func(1, function (args) {
  var thing = args[1].FETCH(), cc = args.pop();

  if(thing == undefined) {
    cc(new PIL2JS.Box.Constant(""));
  } else {
    _26main_3a_3aref.FETCH()([PIL2JS.Context.ItemAny, args[1], function (ref) {
      ref = ref.FETCH();

      if(ref == "Str") {
        cc(new PIL2JS.Box.Constant(String(thing).toString()));
      } else if(ref == "Array") {
        if(thing.referencee) thing = thing.referencee.FETCH();
        var res = "";
        for(var i = 0; i < thing.length; i++) {
          if(thing[i] != undefined) {
            res += PIL2JS.cps2normal(
              _26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, thing[i]]
            ).FETCH();
          }
          res += " ";
        }
        if(thing.length > 0) res = res.slice(0, -1);
        cc(new PIL2JS.Box.Constant(res));
      } else if(ref == "Hash") {
        if(thing.referencee) thing = thing.referencee.FETCH();
        var res   = "";
        var pairs = thing.pairs();
        for(var i = 0; i < pairs.length; i++) {
          res += "" +
            PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, pairs[i].key]).FETCH() +
            "\t" +
            PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, pairs[i].value]).FETCH() +
            "\n";
        }
        if(pairs.length > 0) res = res.slice(0, -1);
        cc(new PIL2JS.Box.Constant(res));
      } else if(ref == "Pair") {
        cc(new PIL2JS.Box.Constant(
          "" +
          PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, thing.key]).FETCH() +
          "\t" +
          PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, thing.value]).FETCH()
        ));
      } else if(ref == "Bool") {
        cc(new PIL2JS.Box.Constant(
          PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f.FETCH(), [PIL2JS.Context.ItemAny, args[1]]).FETCH()
            ? "1"
            : ""
        ));
      } else if(ref == "Num") {
        cc(new PIL2JS.Box.Constant(Number(thing).toString()));
      } else if(ref == "Junction") {
        var res = "", values = thing.values;
        for(var i = 0; i < values.length; i++) {
          res += PIL2JS.cps2normal(_26main_3a_3aprefix_3a_7e.FETCH(), [PIL2JS.Context.ItemAny, values[i]]).FETCH();
          res += thing.op;
        }
        if(values.length > 0) res = res.slice(0, -thing.op.length);
        cc(new PIL2JS.Box.Constant(res));
      } else if(ref == "Ref") {
        PIL2JS.die("Can't stringify non-array or hash references!");
      } else if(ref == "Class") { // XXX
        cc(new PIL2JS.Box.Constant("<class>"));
      } else {
        PIL2JS.die(
          "Stringification for objects of class " +
          ref +
          " not yet implemented!"
        );
      }
    }]);
  }
});

// &prefix:<+>. Written in JS instead of P6 for speed, as it gets called often.
var _26main_3a_3aprefix_3a_2b = PIL2JS.Box.constant_func(1, function (args) {
  var cc = args.pop();
  PIL2JS.possibly_autothread([args[1]], [true], cc, function (cc, thing) {
    var ref = PIL2JS.cps2normal(_26main_3a_3aref.FETCH(), [PIL2JS.Context.ItemAny, thing]).FETCH();

    thing = thing.FETCH();
    if(thing == undefined) return cc(new PIL2JS.Box.Constant(0));

    if(ref == "Str") {
      cc(new PIL2JS.Box.Constant(Number(thing)));
    } else if(ref == "Array") {
      if(thing.referencee) thing = thing.referencee.FETCH();
      cc(new PIL2JS.Box.Constant(thing.length));
    } else if(ref == "Hash") {
      if(thing.referencee) thing = thing.referencee.FETCH();
      cc(new PIL2JS.Box.Constant(thing.num_of_entries));
    } else if(ref == "Bool") {
      cc(new PIL2JS.Box.Constant(
        PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f.FETCH(), [PIL2JS.Context.ItemAny, thing]).FETCH()
          ? 1
          : 0
      ));
    } else if(ref == "Num") {
      cc(new PIL2JS.Box.Constant(Number(thing)));
    } else if(ref == "Ref") {
      PIL2JS.die("Can't numfiy non-array or hash references!");
    } else {
      PIL2JS.die(
        "Numification for objects of class "+
        ref +
        " not yet implemented!"
      );
    }
  });
});

// &prefix:<?>. Written in JS instead of P6 for speed, as it gets called very
// often.
var _26main_3a_3aprefix_3a_3f = PIL2JS.Box.constant_func(1, function (args) {
  var a = args[1].FETCH(), cc = args.pop();
  if(a instanceof PIL2JS.Ref && a.autoderef) a = a.referencee.FETCH();

  if(a instanceof Array) {
    cc(new PIL2JS.Box.Constant(a.length > 0));
  } else if(a instanceof PIL2JS.Hash) {
    cc(new PIL2JS.Box.Constant(a.num_of_entries > 0));
  } else if(a instanceof PIL2JS.Ref) {
    cc(new PIL2JS.Box.Constant(1 == 1));
  } else if(a instanceof PIL2JS.Junction.All) {
    for(var i = 0; i < a.values.length; i++) {
      if(!PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f.FETCH(), [PIL2JS.Context.ItemAny, a.values[i]]).FETCH()) {
        return cc(new PIL2JS.Box.Constant(0 == 1));
      }
    }
    cc(new PIL2JS.Box.Constant(1 == 1));
  } else if(a instanceof PIL2JS.Junction.Any) {
    for(var i = 0; i < a.values.length; i++) {
      if(PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f.FETCH(), [PIL2JS.Context.ItemAny, a.values[i]]).FETCH()) {
        return cc(new PIL2JS.Box.Constant(1 == 1));
      }
    }
    cc(new PIL2JS.Box.Constant(0 == 1));
  } else if(a instanceof PIL2JS.Junction.One) {
    var ret = false;
    for(var i = 0; i < a.values.length; i++) {
      if(PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f, [PIL2JS.Context.ItemAny, a.values[i]]).FETCH()) {
        if(ret) {
          return cc(new PIL2JS.Box.Constant(0 == 1));
        } else {
          ret = true;
        }
      }
    }
    cc(new PIL2JS.Box.Constant(ret));
  } else if(a instanceof PIL2JS.Junction.None) {
    for(var i = 0; i < a.values.length; i++) {
      if(PIL2JS.cps2normal(_26main_3a_3aprefix_3a_3f.FETCH(), [PIL2JS.Context.ItemAny, a.values[i]]).FETCH()) {
        return cc(new PIL2JS.Box.Constant(0 == 1));
      }
    }
    cc(new PIL2JS.Box.Constant(1 == 1));
  } else {
    cc(new PIL2JS.Box.Constant(a != undefined && a != "" && a != "0" && a != 0));
  }
});

/*PIL2JS.bind_params = function (pdefs, args, sub) {
  var cxt   = args.shift();
  args      = PIL2JS.possibly_flatten(args);
  var pairs = PIL2JS.grep_for_pairs(args);

  // Phase 1: Possibly remove and extract named args.
  for(var i = 0; i < pdefs.length; i++) {
    if(pairs[pdefs[i].name] != undefined) {
      pdefs[i].result = pdefs[i].undef.BINDTO(pairs[pdefs[i].name]);
      args = PIL2JS.delete_pair_from_args(args, pdefs[i].name);
    }
  }
}*/

PIL2JS.Junction = {};
PIL2JS.Junction.All  = function (values) { this.values = values; this.op = "&" };
PIL2JS.Junction.Any  = function (values) { this.values = values; this.op = "|" };
PIL2JS.Junction.None = function (values) { this.values = values; this.op = "none" };
PIL2JS.Junction.One  = function (values) { this.values = values; this.op = "^" };

PIL2JS.possibly_autothread = function (args, bools, origcc, sub) {
  args = [].concat(args);

  // First pass:  Autothread all and none.
  // Second pass: Autothread any and one.
  for(var pass = 0; pass <= 1; pass++) {
    for(var i = 0; i < args.length; i++) {
      if(args[i] != undefined && bools[i]) {
        var junc = args[i].FETCH();
        var autothread = pass == 0
          ? junc instanceof PIL2JS.Junction.All || junc instanceof PIL2JS.Junction.None
          : junc instanceof PIL2JS.Junction.Any || junc instanceof PIL2JS.Junction.One;
        if(autothread) {
          var values  = [].concat(junc.values);
          var results = [];
          var j       = 0;
          var cc      = function (ret) {
            if(ret != undefined) results.push(ret);
            if(j < values.length) {
              args[i] = values[j];
              j++;
              PIL2JS.possibly_autothread(args, bools, cc, sub);
            } else {
              origcc(new PIL2JS.Box.Constant(new junc.constructor(results)));
            }
          };
          return cc();
        }
      }
    }
  }

  sub.apply(null, [origcc].concat(args));
};
