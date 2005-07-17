#!/usr/bin/perl
# This is the frontend for the modules PIL::Parser and PIL::Nodes.
# See sub &usage at the bottom for usage information.

use warnings;
use strict;
use lib "lib";

use Getopt::Long;
use PIL::Parser;
use PIL::Nodes;

GetOptions(
  "verbose" => \my $verbose,
  "html"    => \my $html,
  "help"    => \&usage,
) or usage();

local $/;
my $pil  = <>;
my $tree = PIL::Parser->parse($pil);

if($verbose) {
  print STDERR "Parse tree:\n";
  require YAML;
  print STDERR YAML::Dump($tree);
}

# This is the part of the JS Prelude which needs to be written in JS.
local $_;
my $js = <<'EOF' . join "\n", map { $_->as_js } @{ $tree->{"pilGlob"} }, $tree->{pilMain};
if(PIL2JS == undefined) var PIL2JS = {};

// __pil2js_box boxes its input object.
// This is necessary to emulate pass by ref, needed for is rw and is ref.
PIL2JS.Box = function (value) {
  this.GET   = function ()  { return value };
  this.STORE = function (n) { value = n; return n };
};

PIL2JS.Box.prototype = {
  clone: function () {
    return new PIL2JS.Box(this.GET());
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
    } else {
      return unboxed;
    }
  },
  perl_methods: {},
  /*
    toString: function () {
      _26main_3a_3aprefix_3a_7e.GET()([this]);
    },
  */
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

PIL2JS.Box.ReadOnly.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Constant.prototype = PIL2JS.Box.prototype;
PIL2JS.Box.Stub.prototype     = PIL2JS.Box.prototype;

// Call (possibly native sub) sub with args
PIL2JS.call = function (inv, sub, args) {
  if(sub == undefined)
    PIL2JS.die("Use of uninitialized value in subroutine entry!\n");

  // It's a boxed (and therefore Perl 6) sub.
  if(inv == undefined) {
    if(sub.GET) {
      return sub.GET()(args);
    } else {
      var code = "new PIL2JS.Box.Constant(sub(";
      for(var i = 0; i < args.length; i++) {
        code += "args[" + i + "].toNative(),";
      }
      if(code.substr(-1, 1) == ",") code = code.slice(0, -1);
      code += "))";
      return eval(code);
    }
  } else {
    if(inv.GET) {
      if(inv.perl_methods[sub]) {
        return inv.perl_methods[sub].GET()([inv].concat(args));
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

PIL2JS.die = function (msg) {
  var error = new Error(msg);
  alert(error);
  throw(error);
};

EOF

unless($html) {
  print $js;
} else {
  # Output a standard HTML skeleton.
  printf <<EOF, join "\n", map { " " x 6 . $_ } split "\n", $js;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>PIL2JS</title>
  </head>

  <body>
    <pre id="__pil2js_tty"></pre>

    <script type="text/javascript">//<![CDATA[
%s
      //]]
    </script>
  </body>
</html>
EOF
}

sub usage { print STDERR <<USAGE; exit }
Usage:
  \$ cd perl5/PIL2JS
  \$ pugs -CPIL -Ilib6 -MPrelude::JS -we 'say 2 + 3' | \
    ./pil2js.pl [-v] [--html] > test.js

pil2js.pl compiles PIL code given in STDIN to JavaScript code, outputted to
STDOUT.

USAGE
