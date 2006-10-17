method JS::Root::defined($a:) {
  JS::inline('(
    function (val) {
      return typeof(val) != "undefined";
    }
  )')($a);
}

method JS::Root::not(*@a:) { @a.elems == 0 ?? () # should be: want.Scalar ?? undef !! ()
                                           !! !@a[0] } # XXX correct?
method JS::Root::true($a:) { ?$a }

sub Bool::True is primitive { ?1 }
sub Bool::False is primitive { ?0 }

sub True is primitive { ?1 }
sub False is primitive { ?0 }

sub prefix:<!>($a) is primitive { $a ?? ?0 !! ?1 }

sub infix:<^^>   ($a, $b) is primitive {
     if  $a and  $b { ?0 }
  elsif  $a and !$b { $a }
  elsif !$a and  $b { $b }
  else              { ?0 }
}
our &infix:<xor> ::= &infix:<^^>;

sub infix:<?|>   ($a, $b)      is primitive { ?($a || $b) }

sub infix:<//>   ($a, Code $b) is primitive { defined($a) ?? $a !! $b() }
sub infix:<||>   ($a, Code $b999999) is primitive { $a ?? $a !! $b999999() }
sub infix:<&&>   ($a, Code $b) is primitive { $a ?? $b() !! $a }
sub infix:<err>  ($a, Code $b) is primitive { infix:<//>($a, $b()) }
sub infix:<or>   ($a, Code $b) is primitive { infix:<||>($a, $b()) }
sub infix:<and>  ($a, Code $b) is primitive { infix:<&&>($a, $b()) }
# XXX shouldn't need to call $b here
