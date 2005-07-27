# Important: As this method participates in getting &prefix:<~> and &prefix:<+>
# working, it *has to be* unboxed!
method JS::Root::ref($self is rw:) { JS::inline('new PIL2JS.Box.Constant(
  function (args) {
    var thing = args[1].GET();
    if(typeof(thing) == "string" || thing instanceof String) {
      return new PIL2JS.Box.Constant("Str");
    } else if(typeof(thing) == "boolean" || thing instanceof Boolean) {
      return new PIL2JS.Box.Constant("Bool");
    } else if(typeof(thing) == "number" || thing instanceof Number) {
      return new PIL2JS.Box.Constant("Num");
    } else if(thing instanceof Array) {
      return new PIL2JS.Box.Constant("Array");
    } else if(thing instanceof PIL2JS.Ref) {
      return new PIL2JS.Box.Constant("Ref");
    } else if(thing instanceof PIL2JS.Hash) {
      return new PIL2JS.Box.Constant("Hash");
    } else if(thing instanceof PIL2JS.Pair) {
      return new PIL2JS.Box.Constant("Pair");
    } else if(thing instanceof Function) {
      return new PIL2JS.Box.Constant("Code");
    } else {
      PIL2JS.die(
        "Internal error: .ref() not yet implemented for " +
        typeof(thing) +
        "\n"
      );
    }
  }
)')($self) }

# Important: As this method participates in getting &prefix:<~> and &prefix:<+>
# working, it *has to be* unboxed!
method JS::Root::isa($self is rw: $other is rw) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var self = args[1], cmptype = args[2].GET(), ref = args[3];
    var type = ref.GET()([PIL2JS.Context.ItemAny, self]).GET();
    return new PIL2JS.Box.Constant(type == cmptype);
  })')($self, $other, &ref);
}
