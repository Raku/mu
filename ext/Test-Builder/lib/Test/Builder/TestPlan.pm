class Test::Builder::TestPlan-0.0.1
{
    has Int $.expect;

    submethod BUILD ( ?$expect = 0 )
    {
        fail "Invalid or missing plan" unless $expect;
        $.expect = $expect;
    }

    method header returns Str
    {
        return "1..$.expect";
    }

    method footer returns Str (Int $run)
    {
        return '' if $run == $.expect;
        return "Expected $self.expect but ran $run";
    }
}

class Test::Builder::NullPlan is Test::Builder::TestPlan
{
    method header returns Str
    {
        return '';
    }

    method footer returns Str (Int $run)
    {
        return "1..$run";
    }
}

=pod

=head1 NAME

Test::Builder::TestPlan

=head1 SYNOPSIS

  use Test::Builder::TestPlan;

=head1 DESCRIPTION

This file contains both the Test::Builder::TestPlan object and 
a subclass, Test::Builder::NullPlan. 

Test::Builder::NullPlan is roughly equivalent to C<plan('no_plan')>
in the Perl 5 Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<Int $.expect>

The number of tests expected to run.

=back

=head1 METHODS

=over 4

=item B<header returns Str>

Returns a string containing the TAP header for this plan.

=item B<footer returns Str (Int $run)>

Returns a string containing the TAP footer for this plan.

=back

=head1 SEE ALSO

Perl 5 Test::Builder and Test::Harness::TAP.

=head1 AUTHORS

code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=cut
