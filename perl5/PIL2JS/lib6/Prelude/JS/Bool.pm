method JS::Root::defined($a:) {
  JS::inline('(
    function (val) {
      return typeof(val) != "undefined";
    }
  )')($a);
}

method JS::Root::not(*@a:) { @a.elems == 0 ?? undef !! !@a[0] } # XXX correct?
method JS::Root::true($a:) { ?$a }

sub prefix:<!>($a) is primitive { $a ?? ?0 !! ?1 }

sub infix:<^^>   ($a, $b) is primitive {
     if  $a and  $b { ?0 }
  elsif  $a and !$b { $a }
  elsif !$a and  $b { $b }
  else              { ?0 }
}
our &infix:<xor> = &infix:<^^>;

sub infix:<?|>   ($a, $b)      is primitive { ?($a || $b) }

sub infix:<//>   ($a, Code $b) is primitive { defined($a) ?? $a !! $b() }
sub infix:<||>   ($a, Code $b) is primitive { $a ?? $a !! $b() }
sub infix:<&&>   ($a, Code $b) is primitive { $a ?? $b() !! $a }
sub infix:<err>  ($a, Code $b) is primitive { infix:<//>($a, $b()) }
sub infix:<or>   ($a, Code $b) is primitive { infix:<||>($a, $b()) }
sub infix:<and>  ($a, Code $b) is primitive { infix:<&&>($a, $b()) }
# XXX shouldn't need to call $b here
