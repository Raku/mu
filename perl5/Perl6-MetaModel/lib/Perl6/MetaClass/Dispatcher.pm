
package Perl6::MetaClass::Dispatcher;

use strict;
use warnings;

use Carp 'confess';

sub new {
    my ($class, $metaclass, $order) = @_;
    $order = ':descendant' # C3 is the canonical order
        if not(defined($order)) || $order eq ':canonical';
    my $dispatcher;
    if ($order eq ':preorder') {
        $dispatcher = _make_preorder_dispatcher($metaclass);
    }
    elsif ($order eq ':breadth') {
        $dispatcher = _make_breadth_dispatcher($metaclass);
    }
    elsif ($order eq ':descendant') {
        $dispatcher = _make_descendant_dispatcher($metaclass);
    }    
    elsif ($order eq ':ascendant') {
        $dispatcher = _make_ascendant_dispatcher($metaclass);
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

sub _make_descendant_dispatcher {
    my ($metaclass) = @_;
    my @MRO = $metaclass->MRO;
    return _make_iterator(@MRO);
}

sub _make_ascendant_dispatcher {
    my ($metaclass) = @_;
    my @MRO = $metaclass->MRO;
    return _make_iterator(reverse @MRO);
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

sub _make_breadth_dispatcher {
    my ($metaclass) = @_;
    my @stack = _make_iterator($metaclass);
    return sub {
        TOP:
            if (scalar(@stack) != -0) {
                # get the iterator on the top of the stack
                # get the current value out of the iterator
                my $current_metaclass = $stack[0]->();
                # if current is null then ...
                if (not defined $current_metaclass) {
                    # that iterator is exhausted and we 
                    # need to pop it off the stack ...
                    shift @stack;
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

