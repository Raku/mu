module Prelude::JS {}
# XXX pugs can't emit PIL for module Foo {...}, where ... is non-empty.

# JS::Root is the * of the JavaScript code later.
# Why not simply use * here too? Because we don't want to overwrite core subs
# (&defined, &time, operators, etc.) with calls to &JS::inline, as then modules
# (which are called at compile-time) won't work.

# &JS::inline("...") is a pseudo sub: &JS::inline's first param is directly
# inlined (with no escaping) into the resulting JavaScript code.

# new PIL2JS.Box(...) boxes a value. That is, it is packed in an Object with
# the property .GET() holding the original value. This is necessary to emulate
# pass by ref (needed for is rw and is ref).

sub JS::Root::return(*@args) is primitive {
  PIL2JS::Internals::generic_return(5)(@args);
}

sub JS::Root::leave(*@args) is primitive {
  PIL2JS::Internals::generic_return(3)(@args);
}

sub statement_control:<loop>($pre, Code $cond, Code $body, Code $post) is primitive {
  JS::inline('
    function (pre, cond, body, post) {
      try {
        for(pre; cond(); post()) {
          try {
            body();
          } catch(err) {
            if(err instanceof PIL2JS.Exception.next) {
              // Ok;
            } else {
              throw err;
            }
          }
        }
      } catch(err) {
        if(err instanceof PIL2JS.Exception.last) {
          return undefined;
        } else {
          throw err;
        }
      }
      return undefined;
    }
  ').($pre, $cond, $body, $post);
}

sub JS::Root::last() is primitive { JS::inline "throw(new PIL2JS.Exception.last())"; 1 }
sub JS::Root::next() is primitive { JS::inline "throw(new PIL2JS.Exception.next())"; 1 }

sub statement_control:<while>(Code $cond, Code $body) is primitive {
  JS::inline('
    function (cond, body) {
      var ret = undefined;
      while(ret = cond()) {
        body();
      }
      return ret;
    }
  ').($cond, $body);
}

sub statement_control:<until>(Code $cond, Code $body) is primitive {
  JS::inline('
    function (cond, body) {
      var ret = undefined;
      while(!(ret = cond())) {
        body();
      }
      return ret;
    }
  ').($cond, $body);
}

sub statement_control:<if>(Bool $cond, Code $true, Code $false) is primitive {
  JS::inline('
    function (cond, t, f) {
      return cond ? t() : f();
    }
  ').($cond, $true, $false);
}

sub statement_control:<unless>(Bool $cond, Code $true, Code $false) is primitive {
  statement_control:<if>(!$cond, $true, $false);
}

sub JS::Root::defined($a) is primitive {
  JS::inline('
    function (val) {
      return typeof(val) != "undefined";
    }
  ').($a);
}

sub JS::Root::time() is primitive {
  JS::inline "new PIL2JS.Box.Constant((new Date()).getTime() / 1000 - 946684800)";
}

method JS::Root::join(Array $self: Str $sep) {
  JS::inline('
    function (arr, sep) {
      return arr.join(sep);
    }
  ')($self, $sep);
}

method JS::Root::elems(Array $self:) {
  JS::inline('function (arr) { return arr.length }')($self);
}

sub JS::Root::substr(Str $str, Int $a, Int $b) is primitive {
  JS::inline('function (str, a, b) { return str.substr(a, b) }')($str, $a, $b);
}

method JS::Root::ref($self is rw:) { JS::inline('
  function (thing) {
    if(typeof(thing) == "string") {
      return "Str";
    } else if(typeof(thing) == "boolean") {
      return "Bool";
    } else if(typeof(thing) == "number") {
      return "Num";
    } else if(thing instanceof Array) {
      return "Array";
    } else {
      PIL2JS.die(
        "Internal error: .ref() not yet implemented for " +
        typeof(thing) +
        "\n"
      );
    }
  }
')($self) }

method JS::Root::isa($self is rw: $other is rw) { $self.ref eq $other }

sub JS::Root::say(Str *@text) is primitive {
  print @text.join("") ~ "\n";
}
sub JS::Root::print(Str $text) is primitive {
  JS::inline('
    function (msg) {
      // Copied from http://openjsan.org/doc/t/th/theory/Test/Simple/0.11/lib/Test/Builder.html
      // I\'m sure that there must be a more efficient way to do this,
      // but if I store the node in a variable outside of this function
      // and refer to it via the closure, then things don\'t work right
      // --the order of output can become all screwed up (see
      // buffer.html).  I have no idea why this is.
      var node = document.getElementById("__pil2js_tty");
      if (node) {
          // This approach is neater, but causes buffering problems when
          // mixed with document.write. See tests/buffer.html.
          //node.appendChild(document.createTextNode(msg));
          //return;
          for (var i = 0; i < node.childNodes.length; i++) {
              if (node.childNodes[i].nodeType == 3 /* Text Node */) {
                  // Append to the node and scroll down.
                  node.childNodes[i].appendData(msg);
                  window.scrollTo(0, document.body.offsetHeight
                                  || document.body.scrollHeight);
                  return;
              }
          }

          // If there was no text node, add one.
          node.appendChild(document.createTextNode(msg));
          window.scrollTo(0, document.body.offsetHeight
                          || document.body.scrollHeight);
          return;
      }

      // Default to the normal write and scroll down...
      document.write(msg);
      window.scrollTo(0, document.body.offsetHeight
                      || document.body.scrollHeight);
    }
  ').($text);
  ?1;
}

# Standard operators
my @subs = (
  "infix:«<»",    "Number(a)  < Number(b)",
  "infix:«>»",    "Number(a)  > Number(b)",
  "infix:«<=»",   "Number(a) <= Number(b)",
  "infix:«>=»",   "Number(a) >= Number(b)",
  "infix:«==»",   "Number(a) == Number(b)",
  "infix:«!=»",   "Number(a) != Number(b)",
  "infix:«lt»",   "String(a)  < String(b)",
  "infix:«gt»",   "String(a)  > String(b)",
  "infix:«le»",   "String(a) <= String(b)",
  "infix:«ge»",   "String(a) >= String(b)",
  "infix:«eq»",   "String(a) == String(b)",
  "infix:«ne»",   "String(a) != String(b)",
  "infix:«+»",    "Number(a)  + Number(b)",
  "infix:«-»",    "Number(a)  - Number(b)",
  "infix:«*»",    "Number(a)  * Number(b)",
  "infix:«/»",    "Number(a)  / Number(b)",
  "infix:«%»",    "Number(a)  % Number(b)",
  "infix:«~»",    "String(a)  + String(b)",
  "prefix:«?»",   "a ? true : false",
  "prefix:«!»",   "a ? false : true",
  "prefix:«-»",   "-a",
);

# First, we generate the code to eval later.
# Why don't eval the sub declarations immediately?
#   Because then we can't use them anymore. E.g.:
#     sub infix:<~> ($a, $b) { JS::inline(...) }
#     my $foo = $bar ~ $baz; # won't work!
# Ok, so why don't you use JS::Root::infix:<~> then?
#   Because the following doesn't parse currently:
#     sub JS::Root::infix:<~> ($a, $b) {...}
my $eval;
for @subs -> $name, $body {
  my $arity  = $name ~~ rx:P5/^infix:/ ?? 2 :: 1;
  my $jsbody = "function ({$arity == 1 ?? "a" :: "a, b"}) \{
    return($body);
  \}";

  my $args = $arity == 1 ?? '$a' :: '$a, $b';
  $eval ~= "
    sub $name ($args) is primitive \{
      JS::inline('$jsbody').($args);
    \}
  ";
}

# From here on, most normal things won't work any longer, as all the standard
# operators are overloaded with calls to JS::inline.
Pugs::Internals::eval $eval;
die $! if $!;

sub infix:<//>   ($a, Code $b) is primitive { defined($a) ?? $a :: $b() }
sub infix:<||>   ($a, Code $b) is primitive { $a ?? $a :: $b() }
sub infix:<&&>   ($a, Code $b) is primitive { $a ?? $b() :: $a }
sub infix:<err>  ($a, Code $b) is primitive { infix:<//>($a, $b()) } # XXX! hack
sub infix:<or>   ($a, Code $b) is primitive { infix:<||>($a, $b()) } # XXX! hack
sub infix:<and>  ($a, Code $b) is primitive { infix:<&&>($a, $b()) } # XXX! hack
sub prefix:<++>  ($a is rw)    is primitive { $a = $a + 1 }
sub postfix:<++> ($a is rw)    is primitive { my $cur = $a; $a = $a + 1; $cur }
sub prefix:<-->  ($a is rw)    is primitive { $a = $a - 1 }
sub postfix:<--> ($a is rw)    is primitive { my $cur = $a; $a = $a - 1; $cur }

sub infix:<,>(*@xs)            is primitive { @xs }
sub circumfix:<[]>(*@xs)       is primitive { @xs }
method postcircumfix:<[]>(Array $self: Int $idx is copy) {
  $idx = +$self + $idx if $idx < 0;
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var ret = args[0].GET()[args[1].toNative()];
    return ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret;
  })')($self, $idx);
}

sub infix:<=:=>($a is rw, $b is rw) is primitive { JS::inline('new PIL2JS.Box.Constant(
  function (args) {
    return new PIL2JS.Box.Constant(args[0] == args[1]);
  }
)')($a, $b) }

# Pending support for multi subs.
sub prefix:<~>($thing) is primitive {
  if($thing.isa("Str")) {
    JS::inline('function (thing) { return String(thing).toString() }')($thing);
  } elsif($thing.isa("Array")) {
    $thing.join(" ");
  } elsif($thing.isa("Bool")) {
    $thing ?? "bool::true" :: "bool::false";
  } elsif($thing.isa("Num")) {
    JS::inline('function (thing) { return Number(thing).toString() }')($thing);
  } else {
    die "Stringification for objects of class {$other.ref} not yet implemented!\n";
  }
}

sub prefix:<+>($thing) is primitive {
  if($thing.isa("Str")) {
    JS::inline('function (thing) { return Number(thing) }')($thing);
  } elsif($thing.isa("Array")) {
    $thing.elems;
  } elsif($thing.isa("Bool")) {
    $thing ?? 1 :: 0;
  } elsif($thing.isa("Num")) {
    JS::inline('function (thing) { return Number(thing) }')($thing);
  } else {
    die "Numification for objects of class {$other.ref} not yet implemented!\n";
  }
}

sub JS::Root::warn(Str *@msg) is primitive { $JS::PIL2JS.warn(@msg.join("")) }
sub JS::Root::die(Str *@msg)  is primitive { $JS::PIL2JS.die.(@msg.join("")) }
