
package Perl6::Role;

use strict;
use warnings;

use Perl6::Role::Method;

use Carp 'confess';

our %ROLES;

use Data::Dumper;
our $DEBUG = 0;
sub debug { return unless $DEBUG; print ">>> ", @_, "\n" }

sub add_role {
    my ($meta_role, $name, $role) = @_;
    $role->{name} = $name;
    $ROLES{$name} = $role;
}

sub flatten_roles_into {
    my ($meta_role, $class, @roles) = @_;
    debug "combining the roles (" . (join ", ", @roles) . ") into (" . $class->meta->name . ")";
    my $r = $meta_role->combine_roles($class, @roles);
    debug "flattened role is (" . $r->{name} . ")";
    foreach my $method (keys %{$r->{methods}}) {
        debug "adding the method ($method) into (" . $class->meta->name . ")";
        $class->meta->add_method($method => Perl6::Role::Method->new(
            $class->meta->name,
            $r->{methods}->{$method}
            )) unless $class->meta->has_method($method);
    }
    $class->meta->add_method('does' => Perl6::Role::Method->new(
        $class->meta->name, sub {
        my (undef, $role) = @_;
        return $role =~ /\b$r->{name}\b/ if $role;
        return split /\|/ => $r->{name};
    }));    
}

sub combine_roles {
    my ($meta_role, $class, @role_names) = @_;
    debug "combine-ing roles -> (" . (join ", ", @role_names) . ")";
    my @roles = $meta_role->collect_role_list(\@role_names);

    my $composite_role = {
        name    => (join "|", map { $_->{name} } @roles),
        attrs   => {},
        methods => {},
    };

    foreach my $role (@roles) {
        debug "processing the role (" . $role->{name} . ")";
        foreach my $method_name (keys %{$role->{methods}}) {
            debug "adding the method ($method_name) into the role (" . $role->{name} . ")";
            if (exists $composite_role->{methods}->{$method_name}) {
                unless ($class->meta->has_method($method_name)) {
                    confess "We have a method conflict on ($method_name) in (" . $role->{name} . ")";                
                }
            }
            else {
                $composite_role->{methods}->{$method_name} = $role->{methods}->{$method_name};            
            }
        }
    }

    debug "got our compostite role " . Dumper($composite_role);
    return $composite_role;
}

sub collect_role_list {
    my ($meta_role, $role_names, $seen) = @_;
    $seen ||= {};
    my @roles;
    foreach my $role_name (@{$role_names}) { 
        unless (exists $seen->{$role_name}) {
            debug "processing role: $role_name";        
            my $r = $ROLES{$role_name};
            push @roles => $r;
            push @roles => $meta_role->collect_role_list($r->{does}, $seen) if $r->{does};        
        }
    } 
    debug "seen these roles: " . Dumper $seen;
    return @roles;
}


1;

=pod

=head1 NAME

Perl6::Role - (meta) Role in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
