
use v6;
module Hack::Instances-0.0.1;

my %INSTANCES;

sub make_instance($class, $obj is rw) returns Str is export {
    my $id;
    while ( !$id or %INSTANCES.exists($id) ) {
	$id = "OBJECT;$class;" ~ substr(rand() ~ "", 2, 15);
    }

    %INSTANCES{$id} = $obj;
    return $id;
}

sub get_instance(Str $inst) is export {
    my $self := %INSTANCES{$inst};
    return $self;
}

=pod

=head1 NAME

Hack::Instances - An abstraction of inside-out classes

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHORS

Sam Vilain

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
