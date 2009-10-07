use v6;
use Test;
BEGIN { @*INC.push('t/spec/packages/') };
use Test::Util;

plan *;

my @get_out_tests = (
    {
        name => 'callable',
        code => 'rand.say',
        want => { status => 0, out => sub { $^a > 0 && $^a < 1 }, err => '' },
    },
    {
        name => 'say hello',
        code => 'say "hello"',
        want => { status => 0, out => "hello\n", err => '' },
    },
    {
        name => 'die',
        code => 'die "ugh\n"',
        want => { status => 256, out => '', err => rx/^ugh\n/ },
    },
    {
        name => 'no output',
        code => 'for 1 {}',
        want => { status => 0, out => '', err => '' },
    },
    {
        name => 'say hello and die',
        code => 'say "hello";die "ugh\n"',
        want => { status => 256, out => "hello\n", err => rx/^ugh\n/ },
    },
    {
        name => 'no newline',
        code => 'print "hello"',
        want => { status => 0, out => 'hello', err => '' },
    },
    {
        name => 'fail out eq',
        code => 'say "hello"',
        want => { status => 0, out => 'whoops', err => '' },
    },
    {
        name => 'fail out rx',
        code => 'say "hello"',
        want => { status => 0, out => rx/x/, err => '' },
    },
    {
        name => 'fail out call',
        code => 'say "hello"',
        want => { status => 0, out => sub { 0 }, err => '' },
    },
    {
        name => 'fail status eq',
        code => 'say "hello"',
        want => { status => 1, out => "hello\n", err => '' },
    },
    {
        name => 'fail err eq',
        code => 'say "hello"',
        want => { status => 0, out => "hello\n", err => 'whoops' },
    },
);

for @get_out_tests -> %t {
    todo( 'should fail' ) if %t<name> ~~ m/^fail/;
    is_run( %t<code>, %t<want>, %t<name> );
}

is_run( 'say $*IN.lines', 'GIGO', { out => "GIGO\n", err => '', status => 0 },
        'input comes back out' );

# Empty 'want' hash causes a skip
is_run( '1', {}, 'this and next test should skip' );
is_run( '1', {} );

# A test with no name still works
is_run( '1', { status => 0 } );

# This test is kind of evil and probably doesn't work outside Unix.
# is_run( 'run "rm getout-*"', { status => 0 }, 'expect skip from death' );

done_testing();
