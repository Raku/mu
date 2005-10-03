=head1 NAME

Cipher::TestGuts - test cipher

=head1 DESCRIPTION

This fake cipher is used in testing the Cipher API.  No user servicable parts 
inside.

=head1 SECURITY

This cipher doesn't even use the plaintext.  I suppose that means it's a 
perfect cipher--it's impossible to retrieve the plaintext from...

=cut

#'

use Cipher;
class Cipher::TestGuts is Cipher {
    has int $.num_invocations;

    method _head() { return map {ord} 'head'.split('') }
    method _tail() { return map {ord} 'tail'.split('') }
    method _cipher($self: Array $data) {
        $.num_invocations++;
        return map {
            ($self.mode eq 'enciphering' ?? ord 'E' !! ord 'D'), 
            ord $.num_invocations
        } *$data;
    }
    
    submethod BUILD() { .zeroize() }
    method zeroize() { $.num_invocations = 0 }
}
