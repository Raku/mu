
package Perl6::Instance;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

sub _allocate_instance {
    my ($class, $klass, $attrs) = @_;
    bless {
        class         => $klass,
        instance_data => $attrs
    }, $class;
}

sub meta { (shift)->{class}->meta() }

sub isa {
    our $AUTOLOAD = 'isa';
    goto &AUTOLOAD;
}

sub can {
    our $AUTOLOAD = 'can';
    goto &AUTOLOAD;
}

sub AUTOLOAD {
    my @AUTOLOAD = split '::', our $AUTOLOAD;
    my $label = $AUTOLOAD[-1];
    my $self = shift;    
    # NOTE:
    # DESTROYALL is what should really be called
    # so we just deal with it like this :)
    if ($label =~ /DESTROY/) {
        # XXX - hack to avoid destorying Perl6::Class object
        # as that presents some issues for some reason
        return unless blessed($self) ne 'Perl6::Class';
        $label = 'DESTROYALL';        
    }
    my @return_value;
    # get the dispatcher instance ....
    my $dispatcher = $self->meta->dispatcher(':canonical');

    # just discard it if we are calling SUPER
    $dispatcher->next() if ($AUTOLOAD[0] eq 'SUPER');

    # this needs to be fully qualified for now
    my $method = ::WALKMETH($dispatcher, $label, (blessed($self) ? () : (for => 'Class')));
    (blessed($method) && $method->isa('Perl6::Method'))
        || confess "Method ($label) not found for instance ($self)";        

    push @Perl6::MetaModel::CURRENT_DISPATCHER => [ $dispatcher, $label, $self, @_ ];

    @return_value = $method->call($self, @_);     
       
    # we can dispose of this value, as it 
    # should never be called outside of 
    # a method invocation
    pop @Perl6::MetaModel::CURRENT_DISPATCHER;
    return wantarray ?
                @return_value
                :
                $return_value[0];
}

1;

__END__

=pod

=head1 NAME

Perl6::Instance - A class to represent instances in the Perl 6 Meta model

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 METHODS

=over 4

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
