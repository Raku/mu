
package Perl6::MM::C3;

use Perl6::Core::List;
use Perl6::Core::Bit;

package C3;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'symbol';

sub new {
    my ($class, $klass) = @_;
    (blessed($klass) && $klass->isa('opaque'))
        || confess "C3 only accepts classes";
    bless { class  => $klass } => $class;
}

sub linearize {
    my $self = shift;
    
    my @supers = $self->{class}->send('superclasses')->to_native;
    my @seqs = (
        list->new($self->{class}),          # the class we are linearizing
        (map { $_->send('MRO') } @supers ), # the MRO of all the superclasses
        list->new(@supers)                  # a list of all the superclasses        
    );

    my @res;
    while (1) {
        # remove all empty seqences
        my @nonemptyseqs = (map { (($_->is_empty == $bit::FALSE) ? $_ : ()) } @seqs);
        # return the list if we have no more no-empty sequences
        return list->new(@res) if not @nonemptyseqs; 
        my $cand; # a canidate ..
        foreach my $seq (@nonemptyseqs) {
            $cand = $seq->fetch(num->new(0)); # get the head of the list
            my $nothead;            
            foreach my $sub_seq (@nonemptyseqs) {
                # XXX - this is instead of the python "in"
                my %in_tail = (map { $_ => 1 } $sub_seq->tail->to_native);
                # NOTE:
                # jump out as soon as we find one matching
                # there is no reason not too. However, if 
                # we find one, then just remove the '&& last'
                $nothead++ && last if exists $in_tail{$cand};      
            }
            last unless $nothead; # leave the loop with our canidate ...
            $cand = undef;        # otherwise, reject it ...
        }
        confess "Inconsistent hierarchy" if not $cand;
        push @res => $cand;
        # now loop through our non-empties and pop 
        # off the head if it matches our canidate
        foreach my $seq (@nonemptyseqs) {
            $seq->shift if $seq->fetch(num->new(0)) == $cand;
        }
    }       
}

1;

__END__

=pod

=head1 NAME

C3 - the core C3 type

=cut
