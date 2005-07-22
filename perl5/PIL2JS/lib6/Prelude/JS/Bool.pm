method JS::Root::defined($a:) {
  JS::inline('
    function (val) {
      return typeof(val) != "undefined";
    }
  ')($a);
}

method JS::Root::not($a:)  { !$a }
method JS::Root::true($a:) { ?$a }
