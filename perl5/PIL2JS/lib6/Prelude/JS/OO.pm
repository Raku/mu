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


