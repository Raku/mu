
package Perl6::Role;

use strict;
use warnings;

use Perl6::Method;

our %ROLES;

use Data::Dumper;
our $DEBUG = 0;
sub debug { return unless $DEBUG; print ">>> ", @_, "\n" }

sub add_role {
    my ($name, $role) = @_;
    $role->{name} = $name;
    $ROLES{$name} = $role;
}

sub flatten_roles_into {
    my ($class, @roles) = @_;
    debug "combining the roles (" . (join ", ", @roles) . ") into (" . $class->metaclass->name . ")";
    my $r = combine_roles($class, @roles);
    debug "flattened role is (" . $r->{name} . ")";
    foreach my $method (keys %{$r->{methods}}) {
        debug "adding the method ($method) into (" . $class->metaclass->name . ")";
        $class->metaclass->add_method($method => Perl6::Role::Method->new(
            $class->metaclass->name,
            $r->{methods}->{$method}
            )) unless $class->metaclass->has_method($method);
    }
    $class->metaclass->add_method('does' => Perl6::Role::Method->new(
        $class->metaclass->name, sub {
        my (undef, $role) = @_;
        return $role =~ /$r->{name}/ if $role;
        return split /\|/ => $r->{name};
    }));    
}

sub combine_roles {
    my ($class, @role_names) = @_;
    debug "combine-ing roles -> (" . (join ", ", @role_names) . ")";
    my @roles = collect_role_list(\@role_names);

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
                unless ($class->metaclass->has_method($method_name)) {
                    die "We have a method conflict on ($method_name) in (" . $role->{name} . ")";                
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
    my ($role_names, $seen) = @_;
    $seen ||= {};
    my @roles;
    foreach my $role_name (@{$role_names}) { 
        unless (exists $seen->{$role_name}) {
            debug "processing role: $role_name";        
            my $r = $ROLES{$role_name};
            push @roles => $r;
            push @roles => collect_role_list($r->{does}, $seen) if $r->{does};        
        }
    } 
    debug "seen these roles: " . Dumper $seen;
    return @roles;
}


1;