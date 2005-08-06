
package Perl6::Method;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

our @CURRENT_CLASS_STACK;
our @CURRENT_INVOCANT_STACK;

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
        push @CURRENT_INVOCANT_STACK => $_[0]->[0] if blessed($_[0]->[0]);    
        my @rval = $code->(@{$_[0]}); 
        pop @CURRENT_INVOCANT_STACK if blessed($_[0]->[0]);
        pop @CURRENT_CLASS_STACK;
        return wantarray ? @rval : $rval[0];            
    };
    
    $method = bless($method, 'Perl6::Class::Method')    if $type eq 'class';
    $method = bless($method, 'Perl6::Instance::Method') if $type eq 'instance';  
    $method = bless($method, 'Perl6::Role::Method')     if $type eq 'role';        
    
    if ($type eq 'submethod') {
        my $old = $method;
        $method = bless sub { 
            unless (ref($_[0]) eq 'FORCE') {
                return ::next_METHOD() 
                    ## XXX
                    # this should not be accessing either the 
                    # instance_data->name slot, but we cannot
                    # do anything about it for now ...
                    if ::get_P6opaque_instance_class($_[0]->[0]) ne $associated_with->{instance_data}->{name}; 
            }
            $old->($_[1]); 
        }, 'Perl6::SubMethod';
    }
    if ($type eq 'private') {
        my $old = $method;
        $method = bless sub {
            (::CLASS() eq $associated_with)
                || confess "Cannot call private method from different class";
            $old->($_[0]); 
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
    return $self->(\@args);         
}

sub force_call { 
    my ($self, @args) = @_;   
    return $self->(bless({} => 'FORCE'), [ @args ]);         
}

{
    package Perl6::Class::Method;
    use base 'Perl6::Method';
    
    package Perl6::Instance::Method;
    use base 'Perl6::Method';
    
    package Perl6::Role::Method;
    use base 'Perl6::Instance::Method';
    
    package Perl6::PrivateMethod;
    use base 'Perl6::Method';
    
    package Perl6::SubMethod;
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
