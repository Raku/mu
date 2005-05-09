
use v6;
module Perl::MetaMethod-0.0.1;

use Hack::Instances;

sub Perl::MetaMethod::new(Code $sub) returns Str is export {
    my $id = make_instance("Perl::MetaMethod", { 
        'sub'    => $sub,
        'params' => [],
    });
    return $id;
}

# XXX - what are the params? method args? or something more?

sub methodParams(Str $inv: *@params) returns Array {
    my %self := get_instance($inv, "Perl::MetaMethod");
    if @params {
        my @inv_params = %self<params>;
        %self<params> = [ @inv_params, @params ];
    }
    return %self<params>;
}

sub methodInvoke(Str $inv: Array *@args) returns Any {
    my %self := get_instance($inv, "Perl::MetaMethod");
    return %self<sub>(@args);
}

=pod

=head1 NAME

Perl::MetaMethod - A meta-model for Perl Classes

=head1 SYNOPSIS

  use Perl::MetaMethod;

=head1 DESCRIPTION

=head1 FUNCTIONS

=over 4

=item B<methodParams ($inv: *@params)>

=item B<methodInvoke ($inv: *@args)>

=back

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

