#!/usr/bin/pugs

use v6;

require Test;

plan 22;

=kwid

= DESCRIPITION

These tests test named parmaeters. L<S06/"Named parameters">
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

sub simple_pos_param($x) { $x }
is simple_pos_param(x => 3), 3, "positional param may be addressed by name (1)";
is simple_pos_param(:x(3)),  3, "positional param may be addressed by name (2)";

# L<S06/"Named parameters" /marked by a \+/>
sub simple_pos_params (+$x) { $x }

is(simple_pos_params( 'x' => 4 ), 4, "simple named param");


sub foo (+$x = 3) { $x }

is(foo(), 3, "not specifying named params that aren't mandatory works");
is(foo(4), 4, "using a named as a positional works");

is(foo( 'x' => 5), 5, "naming named param also works");



sub assign_based_on_positional ($x, +$y = $x) { $y } 


todo_is(eval 'assign_based_on_named_positional(5)', 5, "When we don't explicitly specify, we get the original value");
todo_is(eval 'assign_based_on_named_positional(5,  "y"=> 2)', 2, "When we explicitly specify, we get our value");
todo_is(eval 'assign_based_on_named_positional(5,  y => 2)', 2, "When we explicitly specify, we get our value");
todo_is(eval 'my $var = "y"; assign_based_on_named_positional(5, $var => 2)', 2, "When we explicitly specify, we get our value");

# L<S06/"Named parameters" /a \+\+ prefix.*?required/>
sub mandatory (++$param) {
    return $param;
}


is (mandatory('param' => 5) , 5, "named mandatory parameter is returned");

is (eval 'mandatory()', undef, "not specifying a mandatory parameter fails");


# From L<S06/"Named parameters" /sub formalize/>
sub formalize($text, +$case, +$justify)  returns List {
   return($text,$case,$justify); 
}

{
my ($text,$case,$justify)  = formalize('title', case=>'upper');
is($text,'title', "text param was positional");
is($justify, undef, "justification param was not given");
is($case, 'upper', "case param was named, and in justification param's position");
}


{
my ($text,$case,$justify)   = formalize('title', justify=>'left');
is($text,'title', "text param was positional");
is($justify, 'left', "justify param was named");
is($case, undef, "case was not given at all");
}

{
my  ($text,$case,$justify) = formalize("title", :justify<right>, :case<title>);

is($text,'title', "title param was positional");
is($justify, 'right', "justify param was named with funny syntax");
is($case, 'title', "case param was named with funny syntax");
}

{
sub h($a,$b,$d) { $d ?? h($b,$a,$d-1) :: $a~$b }

is(h('a','b',1),'ba',"parameters don\'t bind incorrectly");
}

=kwid

= AUTHOR

Jesse Vincent <jesse@bestpractical.com>
Carl Masak <cmasak@gmail.com>

=cut
