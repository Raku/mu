# use of control blocks as rvalues

use v6-alpha;

use Test;

plan 9;

# L<S04/Closure traits/"marked with a *" "used within" expression>

my $hist = '';

# Test INIT {} as rval:

my $init_val;
my $init = {
    $init_val = INIT { $hist ~= 'I' };
}

is $init(), 'BCI', 'INIT {} runs only once';
is $init_val, 'BCI', 'INIT {} as rval is its ret val';
is $init(), 'BCI', 'INIT {} runs only once';

# Test CHECK {} as rval:

my $check_val;
my $check = {
    $check_val = CHECK { $hist ~= 'C' };
}

is $check(), 'BC', 'CHECK {} runs only once';
is $check_val, 'BC', 'CHECK {} as rval is its ret val';
is $check(), 'BC', 'CHECK {} runs only once';

# Test BEGIN {} as rval:

my $begin_val;
my $begin = {
    $begin_val = BEGIN { $hist ~= 'B' };
}

is $begin(), 'B', 'BEGIN {} runs only once';
is $begin_val, 'B', 'BEGIN {} as rval is its ret val';
is $begin(), 'B', 'BEGIN {} runs only once';
