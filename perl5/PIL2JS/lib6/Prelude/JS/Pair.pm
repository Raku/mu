method key (Pair $self:) {
  die ".key only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    return args[1].GET().key;
  })')($self);
}

method value (Pair $self:) {
  die ".value only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    return args[1].GET().value;
  })')($self);
}

sub infix:«=>»($key, $value is rw) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt = args.shift();
    var key = args[0], value = args[1];

    // lvalue pair assignment, see S06 and
    // http://www.nntp.perl.org/group/perl.perl6.language/19425.
    return new PIL2JS.Box.Proxy(
      function ()  { return new PIL2JS.Pair(key, value) },
      function (n) {
        value.STORE(n);
        return this;
      }
    );
  })')($key, $value);
}
