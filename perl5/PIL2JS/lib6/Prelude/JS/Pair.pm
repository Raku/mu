method key (Pair $self:) {
  die ".key only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(args[1].FETCH().key);
  })')($self);
}

method value (Pair $self:) {
  die ".value only works on objects of type Pair!" unless $self.isa("Pair");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    args.pop()(args[1].FETCH().value);
  })')($self);
}

sub infix:«=>»($key, $value is rw) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt = args.shift(), cc = args.pop();
    var key = args[0], value = args[1];

    // lvalue pair assignment, see S06 and
    // http://www.nntp.perl.org/group/perl.perl6.language/19425.
    cc(new PIL2JS.Box.Proxy(
      function ()  { return new PIL2JS.Pair(key, value) },
      function (n) {
        value.STORE(n);
        return this;
      }
    ));
  })')($key, $value);
}
