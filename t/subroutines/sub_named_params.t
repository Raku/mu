use v6;

require Test;




plan 16;


=kwid

= DESCRIPITION

These tests test named parmaeters.
# 23:48 <autrijus> sub foo (+$x) { $x }  foo( 'x' => 4 )
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
# 23:50 <autrijus> see details in http://dev.perl.org/perl6/synopsis/S06.html
# 23:50 <autrijus> also do some TODO for bareword quoting
# 23:50 <autrijus> and :name<value> syntax
#


=cut


sub simple_pos_params (+$x) { $x } 

todo_is(simple_pos_params( 'x' => 4 ), 4, "simple named param");


sub foo (+$x = 3) { $x }

is(foo(), 3, "not specifying named params that aren't mandatory works");
is(foo(4), 4, "using a named as a positional works");

todo_is(foo( 'x' => 5),4, "naming named param also works");



sub assign_based_on_positional ($x, +$y = $x) { $y } 


todo_is(eval 'assign_based_on_named_positional(5)', 5, "When we don't explicitly specify, we get the original value");
todo_is(eval 'assign_based_on_named_positional(5,  "y"=> 2)', 2, "When we explicitly specify, we get our value");
todo_is(eval 'assign_based_on_named_positional(5,  y => 2)', 2, "When we explicitly specify, we get our value");


sub mandatory (++$param) {
    return $param;
}


is (mandatory('param' => 5) , 5, "named mandatory parameter is returned");

is (eval 'mandatory()', undef, "not specifying a mandatory parameter fails");


# From S06

sub formalize($text, +$case, +$justify)  returns List {
   return($text,$case,$justify); 
}

{
my ($text,$case,$justify)  = formalize('title', case=>'upper');
is($text,'title', "text param was positional");
todo_is($justify, undef, "justification param was not given");
todo_is($case, 'upper', "case param was named, and in justification param's position");
}


{
my ($text,$case,$justify)   = formalize('title', justify=>'left');
is($text,'title', "text param was positional");
is($justify, 'left', "justify param was named");
todo_is($case, undef, "case was not given at all");
}

{
fail("FIXME parsefail (3 tests)"); # currently fails compilation even in eval
#my  ($text,$case,$justify); # this doesn't even compile in an eval:   = eval 'formalize("title", :justify<right>, :case<title>)';

#todo_is($text,'title', "title param was positional");
#todo_is($justify, 'right', "justify param was named with funny syntax");
#todo_is($case, 'title', "case param was named with funny syntax");
}

=kwid

= AUTHOR

Jesse Vincent <jesse@bestpractical.com>

=cut
