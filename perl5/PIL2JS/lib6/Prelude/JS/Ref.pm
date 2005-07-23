sub prefix:<\>($thing is rw) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var thing = args[1];
    return new PIL2JS.Box.Constant(new PIL2JS.Ref(thing));
  })')($thing);
}

sub PIL2JS::Internals::generic_deref($thing) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var thing = args[1].GET();
    if(!(thing instanceof PIL2JS.Ref))
      PIL2JS.die("Can\'t use \"" + thing + "\" as a generic reference!");

    // Relay .GET and .STORE to the referencee.
    var ret = new PIL2JS.Box.Proxy(
      function () { return thing.referencee.GET() },
      function (n) {
        thing.referencee.STORE(n);
        return thing.referencee;
      }
    );

    ret.uid = thing.referencee.uid;
    return ret;
  })')($thing);
}
