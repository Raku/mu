method key (Pair $self:) is rw {
  die ".key only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(args[1].FETCH().key);
  })')($self);
}

method value (Pair $self:) is rw {
  die ".value only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(args[1].FETCH().value);
  })')($self);
}

sub infix:«=>»($key is copy, $value is rw) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt = args.shift(), cc = args.pop();
    var key = args[0], value = args[1];
    var new_val_container = value.copy();

    // lvalue pair assignment, see S06 and
    // http://www.nntp.perl.org/group/perl.perl6.language/19425.
    cc(new PIL2JS.Box.Proxy(
      function ()  { return new PIL2JS.Pair(key, new_val_container) },
      function (n) {
        value.STORE(n);
        return this;
      }
    ));
  })')($key, $value);
}

# Needs PIL2 and MMD to be done without hacks
sub PIL2JS::Internals::Hacks::postcircumfix_for_pair_objects (
  Pair $pair, Any $key
) is primitive is rw {
  if $pair.key eqv $key {
    $pair.value;
  } else {
    undef;
  }
}

sub PIL2JS::Internals::Hacks::init_pair_postcircumfix_method () is primitive {
  JS::inline('(function () {
    PIL2JS.addmethod(
      _3amain_3a_3aPair,
      "postcircumfix:{}",
      _26PIL2JS_3a_3aInternals_3a_3aHacks_3a_3apostcircumfix_for_pair_objects
    );
  })')();
}
