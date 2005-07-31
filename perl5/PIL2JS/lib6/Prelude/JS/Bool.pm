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
    if(a instanceof PIL2JS.Ref && a.autoderef) {
      if(a.referencee.GET() instanceof Array) {
        return new PIL2JS.Box.Constant(a.referencee.GET().length > 0);
      } else if(a.referencee.GET() instanceof PIL2JS.Hash) {
        return new PIL2JS.Box.Constant(a.referencee.GET().pairs().length > 0);
      } else {
        return _26main_3a_3aprefix_3a_3f.GET()([PIL2JS.Context.ItemAny, a.referencee]);
      }
    } else if(a instanceof PIL2JS.Ref) {
      return new PIL2JS.Box.Constant(1 == 1);
    } else {
      return new PIL2JS.Box.Constant(a != undefined && a != "" && a != "0" && a != 0);
    }
  })')($a);
}

sub prefix:<!>($a) is primitive { $a ?? ?0 :: ?1 }

sub infix:<^^>   ($a, $b) is primitive {
     if  $a and  $b { ?0 }
  elsif  $a and !$b { $a }
  elsif !$a and  $b { $b }
  else              { ?0 }
}
our &infix:<xor> = &infix:<^^>;

sub infix:<?|>   ($a, $b)      is primitive { ?($a || $b) }

sub infix:<//>   ($a, Code $b) is primitive { defined($a) ?? $a :: $b() }
sub infix:<||>   ($a, Code $b) is primitive { $a ?? $a :: $b() }
sub infix:<&&>   ($a, Code $b) is primitive { $a ?? $b() :: $a }
sub infix:<err>  ($a, Code $b) is primitive { infix:<//>($a, $b()) } # XXX! hack
sub infix:<or>   ($a, Code $b) is primitive { infix:<||>($a, $b()) } # XXX! hack
sub infix:<and>  ($a, Code $b) is primitive { infix:<&&>($a, $b()) } # XXX! hack
