#!/usr/bin/pugs

use v6;
use Test;

plan 5;

use Test::Builder;
use Test::Builder::TestPlan;

my $ok;
my $Test = Test::Builder.new();
is( $Test.ref, 'Test::Builder', 'new() should return a Test::Builder object' );

{
    my $Test2 = Test::Builder.new();
    ok( $Test =:= $Test2, '... Test::Builder is a singleton' );
}

my $custom_plan = Test::Builder::TestPlan.new();
my $Test3       = Test::Builder.create( plan => $custom_plan );
isnt( $Test3.id, $Test.id, 'create() should return non-singleton object' );
is( $Test3.testplan.id, $custom_plan.id, '... allowing plan setting' );

# now launch an external process to test DESTROY() the smart way

my $destroy_test = '#!/usr/bin/pugs

use Test::Builder;
use Test::Builder::TestPlan;

class Test::Builder::CustomPlan is Test::Builder::NullPlan
{
    method footer returns Str ( Int $run )
    {
        my $out = open(\'destroy.out\', :w) or fail "Could not open: $!";
        $out.say( \'custom plan output\' );
    }
}

my $custom_plan = Test::Builder::CustomPlan.new();
my $Test        = Test::Builder.new( plan => $custom_plan );
undefine $Test;';

my $out = open('destroy_test.p6', :w);
unless $out
{
    diag( "Could not write destroy_test.p6" );
    exit;
}

$out.say( $destroy_test );
$out.close;

my $pugs = '../../pugs';
$pugs ~= '.exe' if $*OS eq any<MSWin32 mingw msys cygwin>;

my $res  = system( $pugs, ( map { "-I$_" } @*INC ), 'destroy_test.p6' );
if $res 
{
    my $output = slurp 'destroy.out';
    is( $output, 'custom plan output',
        'DESTROY() should write plan footer, if it exists' );
}
else
{
    skip( 1, "Could not launch $pugs for destroy test" );
}

=cut

END
{
    if ! %*ENV<TEST_DEBUG_FILES>
    {
        unlink 'destroy.out';
        unlink 'destroy_test.p6';
    }
}

=cut
