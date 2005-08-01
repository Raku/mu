sub PIL2JS::Internals::generic_deref($thing) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var thing = args[1].FETCH();
    if(!(thing instanceof PIL2JS.Ref)) {
      PIL2JS.die("Can\'t use \"" + thing + "\" as a generic reference!");
    }

    // Relay .FETCH and .STORE to the referencee.
    var ret = new PIL2JS.Box.Proxy(
      function () { return thing.referencee.FETCH() },
      function (n) {
        thing.referencee.STORE(n);
        return thing.referencee;
      }
    );

    ret.uid = thing.referencee.uid;
    return ret;
  })')($thing);
}

sub PIL2JS::Internals::autoderef(Ref $ref) returns Bool is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var thing = args[1].FETCH();

    if(!(thing instanceof PIL2JS.Ref)) {
      PIL2JS.die("\"" + thing + "\" is not a reference!");
    }

    return new PIL2JS.Box.Constant(thing.autoderef);
  })')($ref);
}

method tied(Ref $self:) returns Ref {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var ref = args[1].FETCH();

    if(!(ref instanceof PIL2JS.Ref)) {
      PIL2JS.die("\"" + ref + "\" is not a reference!");
    }

    var new_ref       = new PIL2JS.Ref(ref.referencee);
    new_ref.autoderef = false;

    return new PIL2JS.Box.Constant(new_ref);
  })')($self);
}

sub circumfix:<${}>($thing) is primitive is rw { PIL2JS::Internals::generic_deref($thing) }
sub circumfix:<@{}>($thing) is primitive is rw { PIL2JS::Internals::generic_deref($thing) }
sub circumfix:<%{}>($thing) is primitive is rw { PIL2JS::Internals::generic_deref($thing) }
sub circumfix:<&{}>($thing) is primitive is rw { PIL2JS::Internals::generic_deref($thing) }
