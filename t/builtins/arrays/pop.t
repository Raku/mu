#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Pop tests

=cut

plan 26;

{ # pop() elements into variables
    my @pop = (1, 2, 3, 4);

    is(+@pop, 4, 'we have 4 elements in the list');
    my $a = pop(@pop);
    is($a, 4, 'pop(@pop) works');

    is(+@pop, 3, 'we have 3 elements in the list');
    my $a = pop @pop;
    is($a, 3, 'pop @pop works');

    is(+@pop, 2, 'we have 2 elements in the list');
    my $a = @pop.pop();
    is($a, 2, '@pop.pop() works');

    is(+@pop, 1, 'we have 1 element in the list');
    my $a = @pop.pop;
    is($a, 1, '@pop.pop works');

    is(+@pop, 0, 'we have no more element in the list');
    ok(!defined(pop(@pop)), 'after the list is exhausted pop() returns undef');
}

{ # pop() elements inline
    my @pop = (1, 2, 3, 4);

    is(+@pop, 4, 'we have 4 elements in the list');
    is(pop(@pop), 4, 'inline pop(@pop) works');

    is(+@pop, 3, 'we have 3 elements in the list');
    is(pop @pop, 3, 'inline pop @pop works');

    is(+@pop, 2, 'we have 2 elements in the list');
    is(@pop.pop(), 2, 'inline @pop.pop() works');

    is(+@pop, 1, 'we have 1 element in the list');
    is(@pop.pop, 1, 'inline @pop.pop works');

    is(+@pop, 0, 'we have no more element in the list');
    ok(!defined(pop(@pop)), 'after the list is exhausted pop() returns undef');
}

# invocant syntax with inline lists
{
    is([1, 2, 3].pop, 3, 'this will return 3');
    ok(!defined([].pop), 'this will return undef');    
}

# some edge cases

{
    my @pop;
    ok(!defined(@pop.pop()), 'pop on an un-initalized list returns undef');
}

# testing some error cases
{
    my @pop = 1 .. 5;
    dies_ok({ pop()         }, 'pop() requires arguments');    
    dies_ok({ pop(@pop, 10) }, 'pop() should not allow extra arguments');            
    dies_ok({ @pop.pop(10)  }, 'pop() should not allow extra arguments');    
}

# Pop with Inf lists (waiting on answers from perl6-compiler email)
#{
#    my @push = 1 .. Inf;
#    # best not to uncomment this it just go on forever
#    todo_throws_ok { 'pop @push' }, '?? what should this error message be ??', 'cannot push onto a Inf list';
#}

