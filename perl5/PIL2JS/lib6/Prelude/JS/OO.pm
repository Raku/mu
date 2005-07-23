method JS::Root::ref($self is rw:) { JS::inline('new PIL2JS.Box.Constant(
  function (args) {
    var thing = args[1].GET();
    if(typeof(thing) == "string") {
      return new PIL2JS.Box.Constant("Str");
    } else if(typeof(thing) == "boolean") {
      return new PIL2JS.Box.Constant("Bool");
    } else if(typeof(thing) == "number") {
      return new PIL2JS.Box.Constant("Num");
    } else if(thing instanceof Array) {
      return new PIL2JS.Box.Constant("Array");
    } else if(thing instanceof PIL2JS.Ref) {
      return new PIL2JS.Box.Constant("Ref");
    } else {
      PIL2JS.die(
        "Internal error: .ref() not yet implemented for " +
        typeof(thing) +
        "\n"
      );
    }
  }
)')($self) }

method JS::Root::isa($self is rw: $other is rw) { $self.ref eq $other }
