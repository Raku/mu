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
