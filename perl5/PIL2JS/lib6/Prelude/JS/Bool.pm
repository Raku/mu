method JS::Root::defined($a:) {
  JS::inline('
    function (val) {
      return typeof(val) != "undefined";
    }
  ')($a);
}

method JS::Root::not($a:)  { !$a }
method JS::Root::true($a:) { ?$a }

sub prefix:<?>($a) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var a = args[1].GET();
    if(a instanceof PIL2JS.Ref && a.referencee.GET() instanceof Array) {
      return new PIL2JS.Box.Constant(a.referencee.GET().length > 0);
    } else if(a instanceof PIL2JS.Ref && a.referencee.GET() instanceof PIL2JS.Hash) {
      return new PIL2JS.Box.Constant(a.referencee.GET().pairs().length > 0);
    } else {
      return new PIL2JS.Box.Constant(a != undefined && a != "" && a != "0" && a != 0);
    }
  })')($a);
}

sub prefix:<!>($a) is primitive { $a ?? ?0 :: ?1 }
