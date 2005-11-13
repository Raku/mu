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
    ok( $Test === $Test2, '... Test::Builder is a singleton' );
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
        say( \'custom plan output\' );
    }
}

my $custom_plan = Test::Builder::CustomPlan.new();
my $Test        = Test::Builder.new( plan => $custom_plan );';

my $out = open('destroy_test.p6', :w);
unless $out
{
    diag( "Could not write destroy_test.p6" );
    exit;
}

$out.say( $destroy_test );
$out.close;

my ($pugs,$redir) = ( '../../pugs', '>' );

if($?OS eq any<MSWin32 mingw cygwin>)
{
    $pugs = '..\\..\\pugs.exe';
    $pugs = 'pugs.exe' if -e 'pugs.exe';
}
else
{
  $pugs = './pugs' if -e './pugs';
};

sub run_pugs (Str $filename)
{
    my $libs     = join(' ', map { "-I$_" } @*INC );
    my $tempfile = "temp-ex-output" ~ ".$*PID." ~ int rand 1000;
    my $command  = "$pugs $libs $filename $redir $tempfile";
    diag $command;
    system $command;
    my $res      = slurp $tempfile;
    unlink $tempfile;
    return $res;
}
  
my $output       = run_pugs( 'destroy_test.p6' );

is( $output, "custom plan output\n",
    'DESTROY() should write plan footer, if it exists' );

END
{
    unlink 'destroy_test.p6' unless %*ENV<TEST_DEBUG_FILES>;
}
