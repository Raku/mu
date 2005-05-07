class Test::Builder;

use Test::Builder::Test;
use Test::Builder::Output;
use Test::Builder::TestPlan;

my  Test::Builder           $:singleton;
has Test::Builder::Output   $.output handles 'diag';
has Test::Builder::TestPlan $.plan;
has                         @:results;

method new ( Test::Builder ::Class:, *@args )
{
    return $:singleton //= Class.create( @args );
}

method create ( Test::Builder ::Class, *@args )
{
    return Class.BUILD( @args );
}

submethod BUILD ( Test::Builder::Output ?$.output, ?$TestPlan )
{
    $.plan     = $TestPlan if $TestPlan;
    $.output //= Test::Builder::Output.new();
}

method plan ( Str ?$explanation, Int ?$tests )
{
    fail "Plan already set!" if $.plan;

    if $tests
    {
        $.plan = Test::Builder::TestPlan.new( expect => $tests );
    }
    elsif $explanation eq 'no_plan'
    {
        $.plan = Test::Builder::NullPlan.new();
    }
    else
    {
        fail "Unknown plan";
    }

    $.output.write( $.plan.header() );
}

method ok returns Bit ( Bit $passed, Str ?$description = '' )
{
    .report_test(
        Test::Builder::Test.create(
            number      => +$.results + 1,
            passed      =>  $passed,
            description =>  $description,
        )
    );

    return $passed;
}

method todo returns Bit ( Bit $passed, Str ?$description, Str ?$reason )
{
    .report_test(
        Test::Builder::Test.create(
            todo        => 1,
            number      => +$.results + 1,
            reason      =>  $reason,
            description =>  $description,
        )
    );

    return $passed;
}

method skip ( Int ?$num = 1, Str ?$reason = 'skipped' )
{
    for 1 .. $num
    {
        .report_test(
            Test::Builder::Test.create(
                skip   => 1,
                number => +$.results + 1,
                reason =>  $reason,
            )
        );
    }
}

method skip_all
{
    fail "Cannot skip_all with a plan" if $.plan;

    $.output.write( "1..0" );
    exit 0;
}

method BAILOUT ( Str $reason = '' )
{
    $.output.write( "Bail out!  $reason" );
    exit 255;
}

method report_test ( Test::Builder::Test $test )
{
    push $.results, $test;
    $.output.write( $test.report() );
}
