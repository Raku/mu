class Test::Builder:Test-0.0.1
{
    method new(
        $number,     
        $passed       = 1,
        ?$skip        = 0,
        ?$todo        = 0,
        ?$reason      = '', 
        ?$description = '',
    )
    {
        return Test::Builder::Test::TODO.new(
            description => $description, reason => $reason, passed => $passed
        ) if $todo;

        return Test::Builder::Test::Skip.new(
            description => $description, reason => $reason, passed => 1
        ) if $skip;

        return Test::Builder::Test::Pass.new(
            description => $description, passed => 1
        ) if $passed;

        return Test::Builder::Test::Fail.new(
            description => $description, passed => 0
        );
    }
}

role Test::Builder::Test::Base {
    has Bool $.passed;
    has Int  $.number;
    has Str  $.diagnostic;
    has Str  $.description;

    submethod BUILD (
        $.description,
        $.passed,
        ?$.number     =     0,
        ?$.diagnostic = '???',
    ) {}

    method status returns Hash
    {
        return
        {
            passed      => $.passed,
            description => $.description,
        };
    }

    method report returns Str
    {
        my $ok          = $.passed ?? 'ok ' :: 'not ok ';
        my $description = " - $.description";
        return join( ' ', $ok, $.number, $description );
    }

}

class Test::Builder::Test::Pass does Test::Builder::Test::Base {}
class Test::Builder::Test::Fail does Test::Builder::Test::Base {}

role Test::Builder::Test::WithReason does Test::Builder::Test::Base
{
    has Str $.reason;

    submethod BUILD ( $.reason ) {}

    method status returns Hash
    {
        my $status        = $self.*WALK[:super];
        $status{"skip"}   = 1;
        $status{"reason"} = $.reason;
        return $status;
    }
}

class Test::Builder::Test::Skip does Test::Builder::Test::WithReason
{
    method report returns Str
    {
        return "not ok $.number #skip $.reason";
    }

    method status returns Hash
    {
        my $status      = $self.*WALK[:super];
        $status{"skip"} = 1;
        return $status;
    }

}

class Test::Builder::Test::TODO does Test::Builder::Test::WithReason
{
    method report returns Str
    {
        my $ok          = $.really_passed ?? 'ok ' :: 'not ok ';
        my $description = " # TODO $.description";
        return join( ' ', $ok, $.number, $description );
    }

    method status returns Hash
    {
        my $status               = $self.*WALK[:super];
        $status{"TODO"}          = 1;
        $status{"passed"}        = 1;
        $status{"really_passed"} = $.passed;
        return $status;
    }
}

=pod

=head1 NAME

Test::Builder::Test

=head1 SYNOPSIS

  use Test::Builder::Test;

=head1 DESCRIPTION

The Test::Builder::Test class represents a single test within the
Test::Builder framework. The class itself is actually a factory for
classes which C<does> the Test::Builder::Test::Base role.

=head1 METHODS

=over 4

=item B<new>

This method is actually a Factory method to create a specific 
class instances which C<does> the Test::Builder::Test::Base role.

=over 4

=item C<$number>   

This is the test number. 

=item C<$passed = 1>

This parameter defaults to true, and so the factory returns a 
Test::Builder::Test::Pass instance, passing on the C<$description> 
parameter, and setting the passed parameter to a true value. 

If this parameter is set to false the factory will return a 
Test::Builder::Test::Fail instance, passing on the C<$description>
parameter, and setting the passed parameter to a false value. 

=item C<?$skip = 0>

This parameter defaults to false, but if it is true, the factory returns
a Test::Builder::Test::Skip instance, passing on the C<$description>, 
and C<$reason> parameters and a true value for the passed parameter.

=item C<?$todo = 0>

This parameter defaults to false, but if it is true, the factory returns
a Test::Builder::Test::TODO instance, passing on the C<$description>, 
C<$reason> and C<$passed> parameters. 

=item C<?$reason = ''> 

This parameter is only used for the Skip and TODO tests, and should contain
a string describing why the test was skipped or is still TODO.

=item C<?$description = ''>

This parameter is passed to all the classes, and contains a description of
the test itself.

=back

=back

=head1 ROLES

=over 4

=item B<Test::Builder::Test::Base>

This role contains a number of public attributes all of which can be 
set using named parameters in the constructor.

=over 4

=item B<Bool $.passed>

=item B<Int $.number>

=item B<Str $.diagnostic>

=item B<Str $.description>

=back 

This role contains two methods

=over 4

=item B<status returns Hash>

This returns a hash containing two keys C<passed> and C<description>.

=item B<report returns Str>

This returns a string which follows the TAP protocol.

  ok 1 - test description
  not ok 2 - test description

=back

=item B<Test::Builder::Test::WithReason>

This role actually extends the Test::Builder::Test::Base role. It adds a 
simple public attribute.

=over 4

=item B<Str $.reason>

=back

And overrides a single method

=over 4

=item B<status returns Hash>

This method actually just calls the superclass C<status> method and adds
a C<skip> and C<reason> attribute to the hash before returning it.

=back

=back

=head1 OTHER CLASSES

=over 4

These first two classes do nothing more than C<does> the 
Test::Builder::Test::Base role.

=item B<Test::Builder::Test::Pass>

=item B<Test::Builder::Test::Fail>

These next two classes both C<does> the Test::Builder::Test::WithReason
role, but they also override some methods to provide specific functionality.

=item B<Test::Builder::Test::TODO>

This overrides two methods.

=over 4

=item B<report returns Str>

This is overridden to handle the specific details TAP output for TODO tests.

=item B<status returns Hash>

This adds a C<TODO> key, as well as makes sure the C<passed> key is set to 
true, and adds a C<really_passed> key which contains the boolean representing
if the test truely did pass or not.

=back

=item B<Test::Builder::Test::Skip>

This also overrides two methods.

=over 4

=item B<report returns Str>

This is overridden to handle the specific details TAP output for Skip tests.

=item B<status returns Hash>

This just makes sure the C<skip> key is set to true in the hash.

=back

=back

=head1 SEE ALSO

Perl5 Test::Builder

=head1 AUTHORS

code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut