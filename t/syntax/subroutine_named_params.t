use v6;

require Test;




plan 9;


=kwid

= DESCRIPITION

These tests test named parmaeters.

# 23:48 <autrijus> is the canonical example
# 23:48 <autrijus> +$x makes it addressable only by name
# 23:48 <autrijus> positional args can also optionally be addressed by name.
# 23:49 <autrijus> named args are optional by default.
# 23:49 <autrijus> but if you use ++$x
# 23:49 <autrijus> then its named and required.
# 23:49 <autrijus> sub foo (+$x = 3) { $x }
# 23:49 <autrijus> # defaults to 3
# 23:49 <autrijus> sub foo ($x, +$y = $x) { } # this even works.
# 23:50 <autrijus> I _think_ t/syntax
# 23:50 <autrijus> but I need to take a nap now
# 23:50 <autrijus> see details in http://dev.perl.org/perl6/synopsis/S06.html
# 23:50 <autrijus> also do some TODO for bareword quoting
# 23:50 <autrijus> and :name<value> syntax
#


=cut

sub foo (+$x = 3) { $x }

is(foo(), 3, "not specifying named params that aren't mandatory works");
is(foo(4), 4, "using a named as a positional works");


sub assign_based_on_positional ($x, +$y = $x) { $y } 


is(assign_based_on_named_positional(5), 5, " When we dont' explicitly specify, we get the original value");
is(assign_based_on_named_positional(5,  y => 2), 2, " When we explicitly specify, we get our value");


sub mandatory (++$param) {
    return $param;
}


is (mandatory(param => 5) , 5);

is (mandatory(), undef, "not specifying a mandatory parameter fails");


# From S06

sub formalize($text, +$case, +$justify)  returns List {
   return($text,$case,$justify); 
}

=kwid

($text,$case,$justify)  = formalize('title', case=>'upper');
is($text,'title');
is($justify, undef);
is($case, 'upper');


($text,$case,$justify)   = formalize('title', justify=>'left');
is($text,'title');
is($justify, 'left');
is($case, undef);

 ($text,$case,$justify)   = formalize('title', :justify<right>, :case<title>);

todo_is($text,'title');
todo_is($justify, 'right');
todo_is($case, 'title');


=kwid

= AUTHOR

Jesse Vincent <jesse@bestpractical.com>

=cut
