#!/usr/bin/pugs

use v6;
require Test;

plan 40;

# split on an empty string

my @split1 = split "", "forty-two";
is +@split1, 9, "split created the correct number of elements";
is @split1[0], "f", 'the first value of the split array is ok';
is @split1[1], "o", 'the second value of the split array is ok';
is @split1[2], "r", 'the third value of the split array is ok';
is @split1[3], "t", 'the fourth value of the split array is ok';
is @split1[4], "y", 'the fifth value of the split array is ok';
is @split1[5], "-", 'the sixth value of the split array is ok';
is @split1[6], "t", 'the seventh value of the split array is ok';
is @split1[7], "w", 'the eighth value of the split array is ok';
is @split1[8], "o", 'the ninth value of the split array is ok';

# split on a space

my @split2 = split(' ', 'split this string');
is(+@split2, 3, 'got the right number of split elements');
is(@split2[0], 'split', 'the first element is right');
is(@split2[1], 'this', 'the second element is right');
is(@split2[2], 'string', 'the third element is right');

# split on a single character delimiter

my @split3 = split('$', 'try$this$string');
is(+@split3, 3, 'got the right number of split elements');
is(@split3[0], 'try', 'the first element is right');
is(@split3[1], 'this', 'the second element is right');
is(@split3[2], 'string', 'the third element is right');

# split on a multi-character delimiter

my @split4 = split(', ', "comma, seperated, values");
is(+@split4, 3, 'got the right number of split elements');
is(@split4[0], 'comma', 'the first element is right');
is(@split4[1], 'seperated', 'the second element is right');
is(@split4[2], 'values', 'the third element is right');

# split on a variable delimiter

my $delimiter = '::';
my @split5 = split($delimiter, "Perl6::Pugs::Test");
is(+@split5, 3, 'got the right number of split elements');
is(@split5[0], 'Perl6', 'the first element is right');
is(@split5[1], 'Pugs', 'the second element is right');
is(@split5[2], 'Test', 'the third element is right');

# split with a reg-exp

{ # your basic CSV split
    my @split = split(rx:perl5{,}, "split,me");
    is(+@split, 2, 'got the right number of values');
    is(@split[0], 'split', 'got the right first value');
    is(@split[1], 'me', 'got the right second value');
}

{ # split on multiple space characters
    my @split = split(rx:perl5{\s+}, "Hello World    Goodbye   Mars");
    is(+@split, 4, 'got the right number of values');
    is(@split[0], 'Hello', 'got the right first value');
    is(@split[1], 'World', 'got the right second value');
    is(@split[2], 'Goodbye', 'got the right third value');
    is(@split[3], 'Mars', 'got the right fourth value');     
}

{ # split with a capture
    my @split = split(rx:perl5{([A-Z]|<)}, "Hello < test");
    is(+@split, 5, 'got the right number of values');
    is(@split[0], '', 'got the right first value');
    is(@split[1], 'H', 'got the right second value');
    is(@split[2], 'ello ', 'got the right third value');
    is(@split[3], '<', 'got the right fourth value');
    is(@split[4], ' test', 'got the right fifth value');     
}