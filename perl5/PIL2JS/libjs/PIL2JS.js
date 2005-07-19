// This is the part of the Prelude for JavaScript which is written in
// JavaScript. See lib6/Prelude/JS.pm for the part written in Perl 6.

if(PIL2JS == undefined) var PIL2JS = {};

PIL2JS.LF = typeof document != "undefined" && typeof document.all != "undefined"
  ? "\r"
  : "\n";

// This is necessary to emulate pass by ref, needed for is rw and is ref.
PIL2JS.Box = function (get, store) {
  this.GET   = get;
  this.STORE = store;
};

PIL2JS.Box.prototype = {
  BINDTO: function (other) {
    this.GET   = other.GET;
    this.STORE = other.STORE;
    return this;
  },
  clone: function () {
    return new PIL2JS.Box.Proxy(this.GET());
  },
  toNative: function () {
    var unboxed = this.GET();
    if(unboxed instanceof Array) {
      var arr = [];
      for(var i = 0; i < unboxed.length; i++) {
        arr.push(unboxed[i].toNative());
      }
      return arr;
    } else if(unboxed instanceof Function) {
      return function () {
        var args = arguments;
        for(var i = 0; i < args.length; i++)
          args[i] = new PIL2JS.Box.Constant(args[i]);
        return unboxed(args).toNative();
      };
    } else if(typeof unboxed == "string") {
      // Convert "\n"s (IE...)
      return unboxed.replace(/\n/, PIL2JS.LF);
    } else {
      return unboxed;
    }
  },
  perl_methods: {}
  /*
    toString: function () {
      _26main_3a_3aprefix_3a_7e.GET()([this]);
    },
  */
};

PIL2JS.Box.Proxy = function (value) {
  this.GET   = function ()  { return value };
  this.STORE = function (n) { value = n.GET(); return n };
};
PIL2JS.Box.ReadOnly = function (box) {
  this.GET   = function ()  { return box.GET() };
  this.STORE = function (n) { PIL2JS.die("Can't modify constant item!\n"); return n };
};
PIL2JS.Box.Constant = function (value) {
  this.GET   = function ()  { return value };
  this.STORE = function (n) { PIL2JS.die("Can't modify constant item!\n"); return n };
};
PIL2JS.Box.Stub = function (value) {
  this.GET   = function ()  { PIL2JS.die(".GET() of a PIL2JS.Box.Stub called!\n") }
  this.STORE = function (n) { PIL2JS.die(".STORE() of a PIL2JS.Box.Stub called!\n"); return n };
};

PIL2JS.Box.Proxy.prototype    = PIL2JS.Box.prototype;
PIL2JS.Box.ReadOnly.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Constant.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Stub.prototype     = PIL2JS.Box.prototype;

// Call (possibly native sub) sub with args
PIL2JS.call = function (inv, sub, args) {
  if(sub == undefined)
    PIL2JS.die("Use of uninitialized value in subroutine entry!\n");

  // It's a plain sub (i.e. not method) call.
  if(inv == undefined) {
    // It's a boxed (and therefore Perl 6) sub.
    if(sub.GET) {
      var ret = sub.GET()(args);
      if(ret == undefined)
        PIL2JS.die("Internal error: Boxed sub returned unboxed undefined!");
      return ret;
    } else {
      var code = "new PIL2JS.Box.Constant(sub(";
      for(var i = 0; i < args.length; i++) {
        code += "args[" + i + "].toNative(),";
      }
      if(args.length > 0) code = code.slice(0, -1);
      code += "))";
      return eval(code);
    }
  } else {
    if(inv.GET) {
      if(inv.perl_methods[sub]) {
        return PIL2JS.call(undefined, inv.perl_methods[sub], [inv].concat(args));
      } else {
        PIL2JS.die("No such method: \"" + sub + "\"");
      }
    } else {
      PIL2JS.die("Internal error: PIL2JS.call not implemented for invocation of native methods\n");
    }
  }
};

PIL2JS.make_slurpy_array = function (inp_arr) {
  var out_arr = [];

  for(var i = 0; i < inp_arr.length; i++) {
    if(inp_arr[i].GET() instanceof Array) {
      out_arr = out_arr.concat(inp_arr[i].GET());
    } else {
      out_arr.push(inp_arr[i]);
    }
  }

  return out_arr;
};

var _24main_3a_3a_3fPOSITION = new PIL2JS.Box.Proxy("<unknown>");

PIL2JS.new_error = function (msg) {
  return new Error(msg.substr(-1, 1) == "\n"
    ? msg
    : msg + " at " + _24main_3a_3a_3fPOSITION.toNative()
  );
};
PIL2JS.warn = function (msg) { alert(PIL2JS.new_error(msg)) };
PIL2JS.die = function (msg) {
  var error = PIL2JS.new_error(msg);
  alert(error);
  throw(error);
};

PIL2JS.Exception = {};
PIL2JS.Exception.last  = function () {};
PIL2JS.Exception.next  = function () {};
PIL2JS.Exception.ret   = function (level, retval) {
  this.level        = level;
  this.return_value = retval;
  this.toString     = function () {
    var msg = 
      "Can't return outside a " + level + "-routine. at " +
      _24main_3a_3a_3fPOSITION.toNative();
    alert(msg);
    return msg;
  }
};

var _26PIL2JS_3a_3aInternals_3a_3ageneric_return =
  new PIL2JS.Box.Constant(function (args) {
    var level = args[0].toNative();
    return new PIL2JS.Box.Constant(function (args) {
      args = PIL2JS.make_slurpy_array(args);
      var ret =
        args.length >  1 ? new PIL2JS.Box.Constant(args) :
        args.length == 1 ? args[0] :
        new PIL2J2.Box.Constant(undefined);
      throw(new PIL2JS.Exception.ret(level, ret));
    });
  });

PIL2JS.Pair = function (key, value) {
  this.key   = key;
  this.value = value;
};

PIL2JS.part_pairs = function (args) {
  var normal_args = [];
  var pairs       = {};

  for(var i = 0; i < args.length; i++) {
    if(args[i].GET() instanceof PIL2JS.Pair) {
      pairs[args[i].GET().key.toNative()] = args[i].GET().value;
    } else {
      normal_args.push(args[i]);
    }
  }

  return [normal_args, pairs];
};

PIL2JS.use_jsan = function (mod) {
  if(JSAN == undefined)
    PIL2JS.die("JSAN library not loaded!");

  mod = mod.replace(/::/, ".");
  JSAN.prototype.use.apply(JSAN.prototype, [mod]);
};
