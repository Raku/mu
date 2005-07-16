
package Perl6::MetaClass::Dispatcher;

use strict;
use warnings;

use Carp 'confess';

sub new {
    my ($class, $metaclass, $order) = @_;
    $order ||= ':preorder';
    my $dispatcher;
    if ($order eq ':preorder') {
        $dispatcher = _make_preorder_dispatcher($metaclass);
    }
    (ref($dispatcher) eq 'CODE')
        || confess "Unsupported dispatch order ($order)";
    bless { i => $dispatcher }, $class;
}

sub next : method { (shift)->{i}->() }

sub _make_iterator {
    my (@values) = @_;
    my $counter = 0;
    return sub {
        $values[$counter++]
    };
}

sub _make_preorder_dispatcher {
    my ($metaclass) = @_;
    my @stack = _make_iterator($metaclass);
    return sub {
        TOP:
            if (defined $stack[-1]) {
                # get the iterator on the top of the stack
                # get the current value out of the iterator
                my $current_metaclass = $stack[-1]->();
                # if current is null then ...
                if (not defined $current_metaclass) {
                    # that iterator is exhausted and we 
                    # need to pop it off the stack ...
                    pop @stack;
                    # now go back to the top and start over
                    goto TOP;
                }
                else {
                    push @stack => _make_iterator(@{$current_metaclass->superclasses})
                        if $current_metaclass->superclasses;
                }             
                return $current_metaclass;
            }
            return undef;
    };
}

1;


__END__

=pod

=head1 NAME

Perl6::MetaClass::Dispatcher - Dispatcher generator for the MetaClass

=head1 DESCRIPTION

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

