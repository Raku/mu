class Test::Builder-0.2.1;

use Test::Builder::Test;
use Test::Builder::Output;
use Test::Builder::TestPlan;

my  Test::Builder           $:singleton;
has Test::Builder::Output   $.output handles 'diag';
has Test::Builder::TestPlan $.testplan;
has                         @:results;

method new ( Test::Builder $Class: ?$plan, ?$output )
{
    return $:singleton //= $Class.SUPER::new(
		testplan => $plan, output => $output
	);
}

method create ( Test::Builder $Class: ?$plan, ?$output )
{
    return $Class.new( testplan => $plan, output => $output );
}

submethod BUILD
(
    Test::Builder::TestPlan ?$.testplan,
    Test::Builder::Output   ?$.output = Test::Builder::Output.new()
)
{}

submethod DESTROY
{
	my $footer = $.testplan.footer( +@:results );
	$.output.write( $footer ) if $footer;
}

method plan ( Str ?$explanation, Int ?$tests )
{
    fail "Plan already set!" if $.testplan;

    if $tests
    {
        $.testplan = Test::Builder::TestPlan.new( expect => $tests );
    }
    elsif $explanation eq 'no_plan'
    {
        $.testplan = Test::Builder::NullPlan.new();
    }
    else
    {
        fail "Unknown plan";
    }

    $.output.write( $.testplan.header() );
}

method ok returns Bit ( $self: Bit $passed, Str ?$description = '' )
{
    $self.report_test(
        Test::Builder::Test.new(
            number      => +$.results + 1,
            passed      =>  $passed,
            description =>  $description,
        )
    );

    return $passed;
}

method diag ( Str ?$diagnostic = '' )
{
	$.output.diag( $diagnostic );
}

method todo returns Bit ( $self: Bit $passed, Str ?$description, Str ?$reason )
{
    $self.report_test(
        Test::Builder::Test.new(
            todo        => 1,
            number      => +$.results + 1,
            reason      =>  $reason,
            description =>  $description,
        )
    );

    return $passed;
}

method skip ( $self: Int ?$num = 1, Str ?$reason = 'skipped' )
{
    for 1 .. $num
    {
        $self.report_test(
            Test::Builder::Test.new(
                skip   => 1,
                number => +$.results + 1,
                reason =>  $reason,
            )
        );
    }
}

method skip_all
{
    fail "Cannot skip_all with a plan" if $.testplan;

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
    fail 'No plan set!' unless $.testplan;

    push $.results, $test;
    $.output.write( $test.report() );
}

=pod

=head1 NAME

Test::Builder - Backend for building test libraries

=head1 SYNOPSIS

  module My::Test::Module;

  use Test::Builder;
  use Test::Builder::Output;

  my Test::Builder $Test .= new(
        output => Test::Builder::Output.new(
	       error_output => open('my_error_log_file')
        )
    );

  sub plan (Str ?$explanation, Int ?$tests) is export {
	 $Test.testplan($explanation, $tests);
  }

  sub ok ($passed, ?$description, ?$todo) is export {
	 if $todo {
		$Test.todo($passed, $description, $todo) 
		    || $Test.diag("FAILED : $description");
	 }
	 else {
		$Test.ok($passed, $description)
		    || $Test.diag("FAILED : $description");
	 }
  }

  sub is ($got, $expected, ?$description, ?$todo) is export {
	 if $todo {
		$Test.todo($got eq $expected, $description, $todo)
		    || $Test.diag("FAILED : $description");
	 }
	 else {
		$Test.ok($got eq $expected, $description)
		    || $Test.diag("FAILED : $description");		
	 }
  }

  # then using our test module the test file themselves ...

  use My::Test::Module;

  plan('no_plan');
  # or
  plan :tests<20>;

  ok(2 == 2, '... 2 is equal to 2');
  is(2 + 2, 5, '... 2 plus 2 should be 5', :todo<bug>);

=head1 DESCRIPTION

This is a Perl 6 port of the Perl 5 module Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<Test::Builder::Output $.output>

=item B<Test::Builder::TestPlan $.testplan>

=back

=head1 METHODS

=over 4

=item B<new( *@args )>

This method actually returns a Test::Builder singleton, creating it if
necessary.  The optional named arguments are:

=over 4

=item C<output>

A Test::Builder::Output object.

=item C<plan>

A Test::Builder::TestPlan object.

=back

=item B<create( *@args )>

This method actually creates and returns a new Test::Builder instance.  It
takes the same optional named arguments as C<new>.

=item B<plan( Str ?$explanation, Int ?$tests )>

Sets the current test plan or throws an exception if there's already a plan in
place.  You have two options for the plan.  If you pass a pair such as C<tests
=Egt 10>, the plan is to run ten tests.  If you pass the string C<no_plan>,
there is no set number of tests to run.

Those are the only valid arguments.

You must have a plan set before you can record any tests.

=item B<ok returns Bit ( Bit $passed, Str ?$description = '' )>

Records that a test has passed or failed, depending on the value of C<$passed>,
recording C<$description> as an optional explanation.

=item B<todo returns Bit ( Bit $passed, Str ?$description, Str ?$reason )>

Records that a test has passed or failed, depending on C<$passed> with an
optional C<$description>, but marks it as a TODO test with an optional
C<$reason>.

=item B<skip( Int ?$num = 1, Str ?$reason = 'skipped' )>

Records the skipping of C<$num> tests (one by default), giving an optional
C<$reason> for skipping them.

=item B<skip_all()>

Skips all of the tests before running them.

Fails if there is a test plan set.

=item B<BAILOUT( Str $reason = '' )>

Aborts the entire test run.

=item B<report_test( Test::Builder::Test $test )>

Records a test.  Internal use only, probably.

=back

=head1 SEE ALSO

Perl 5 Test::Builder.

=head1 AUTHORS

code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=cut
