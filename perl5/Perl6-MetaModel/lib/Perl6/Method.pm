
package Perl6::Method;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

our @CURRENT_CLASS_STACK;
our @CURRENT_INVOCANT_STACK;

sub new {
    my ($class, $associated_with, $code) = @_;
    (defined $associated_with && defined $code) 
        || confess "Insufficient Arguments : You must provide a class this is associated with and code";    
    (ref($code) eq 'CODE') 
        || confess "Incorrect Object Type : The code arguments must be a CODE reference";
    bless {
        associated_with => $associated_with,
        code => sub {
            my ($self, @args) = @_;  
            push @CURRENT_CLASS_STACK => $associated_with;
            push @CURRENT_INVOCANT_STACK => $args[0] if blessed($args[0]);    
            my @rval = $code->(@args); 
            pop @CURRENT_INVOCANT_STACK if blessed($args[0]);
            pop @CURRENT_CLASS_STACK;
            return wantarray ? @rval : $rval[0];            
        },
    }, $class;
}

sub _create {
    my ($class, $associated_with, $code, $type) = @_;
    (defined $associated_with && defined $type && defined $code) 
        || confess "Insufficient Arguments : You must provide a class this is associated with and code";    
    (ref($code) eq 'CODE') 
        || confess "Incorrect Object Type : The code arguments must be a CODE reference";
    # normalize the type
    $type = lc($type);
    
    my $method = sub {
        push @CURRENT_CLASS_STACK => $associated_with;
        push @CURRENT_INVOCANT_STACK => $_[0] if blessed($_[0]);    
        my @rval = $code->(@_); 
        pop @CURRENT_INVOCANT_STACK if blessed($_[0]);
        pop @CURRENT_CLASS_STACK;
        return wantarray ? @rval : $rval[0];            
    };
    
    $method = bless($method, 'Perl6::Class::Method')    if $type eq 'class';
    $method = bless($method, 'Perl6::Instance::Method') if $type eq 'instance';  
    $method = bless($method, 'Perl6::Role::Method')     if $type eq 'role';        
    
    if ($type eq 'submethod') {
        my $old = $method;
        $method = bless sub { 
            return ::next_METHOD() if $_[0]->{class} ne $associated_with; 
            $old->(@_); 
        }, 'Perl6::SubMethod';
    }
    if ($type eq 'private') {
        my $old = $method;
        $method = bless sub {
            (::CLASS() eq $associated_with)
                || confess "Cannot call private method from different class";
            $old->(@_); 
        }, 'Perl6::PrivateMethod';
    }    
    
    return $method;
}

sub create_instance_method { (shift)->_create(@_, 'instance')  }
sub create_class_method    { (shift)->_create(@_, 'class')     }
sub create_role_method     { (shift)->_create(@_, 'role')      }
sub create_submethod       { (shift)->_create(@_, 'submethod') }
sub create_private_method  { (shift)->_create(@_, 'private')   }

sub do { 
    my ($self, @args) = @_;   
    if (UNIVERSAL::isa($self, 'CODE')) {
        return $self->(@args);         
    }
    else {
        return $self->{code}->($self, @args);         
    }
}

# XXX -
# this is the API from A12, but I think
# that this really should be some kind
# of proxy object which wraps the method.
# (see t/35_Method_introspection.t for more)
sub name      { (shift)->{name} }
sub signature { '*@_'           }
sub returns   { 'Any'           }
sub multi     { 0               }
sub associated_with { (shift)->{associated_with} }

{
    package Perl6::Class::Method;
    use base 'Perl6::Method';
    
    package Perl6::Instance::Method;
    use base 'Perl6::Method';
    
    package Perl6::Role::Method;
    use base 'Perl6::Instance::Method';
    
    package Perl6::PrivateMethod;
    use base 'Perl6::Method';
}

1;

__END__

=pod

=head1 NAME

Perl6::Method - Base class for Methods in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new ($associated_with, $code)>

=item B<do (@args)>

=item B<associated_with>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
