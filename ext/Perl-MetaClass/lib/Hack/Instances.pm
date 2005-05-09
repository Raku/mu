
use v6;
module Hack::Instances-0.0.1;

my %INSTANCES;

sub make_instance($class, $obj) returns Str is export {
    my $id;
    while ( !$id or %INSTANCES.exists($id) ) {
	$id = "OBJECT;$class;" ~ substr(rand() ~ "", 2, 15);
    }

    %INSTANCES{$id} = $obj;
    say "Created { $obj } as $id";
    return $id;
}

sub get_instance(Str $inst) returns Hash is rw is export {
    my %self = %INSTANCES{$inst};
    say "Returning { %self } from $inst";
    return %self;
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
