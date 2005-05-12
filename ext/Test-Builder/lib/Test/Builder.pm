class Test::Builder-0.0.1;

use Test::Builder::Test;
use Test::Builder::Output;
use Test::Builder::TestPlan;

my  Test::Builder           $:singleton;
has Test::Builder::Output   $.output handles 'diag';
has Test::Builder::TestPlan $.plan;
has                         @:results;

method new ( Test::Builder ::Class: *@args )
{
    return $:singleton //= Class.create( @args );
}

method create ( Test::Builder ::Class: *@args )
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

method BAILOUT ( Str ?$reason = '' )
{
    $.output.write( "Bail out!  $reason" );
    exit 255;
}

method report_test ( Test::Builder::Test $test )
{
    push $.results, $test;
    $.output.write( $test.report() );
}

=pod

=head1 NAME

Test::Builder - Backend for building test libraries

=head1 SYNOPSIS

  use Test::Builder;

=head1 DESCRIPTION

This is a perl6 port of the perl5 module Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<Test::Builder::Output $.output>

=item B<Test::Builder::TestPlan $.plan>

=back

=head1 METHODS

=over 4

=item B<new (*@args)>

This method actually creates the Test::Builder singleton.

=item B<create (*@args)>

This method actually creates the singleton instance. 

=item B<plan ( Str ?$explanation, Int ?$tests )>

=item B<ok returns Bit ( Bit $passed, Str ?$description = '' )>

=item B<todo returns Bit ( Bit $passed, Str ?$description, Str ?$reason )>

=item B<skip ( Int ?$num = 1, Str ?$reason = 'skipped' )>

=item B<skip_all>

=item B<BAILOUT ( Str $reason = '' )>

=item B<report_test ( Test::Builder::Test $test )>

=back

=head1 SEE ALSO

Perl5 Test::Builder

=head1 AUTHORS

code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
