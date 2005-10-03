=head1 NAME

Cipher::Caesar - Simple modular-addition cipher

=head1 SYNOPSIS

    my &rot13 := Cipher::Caesar.encipherer;     # Defaults to rot13
    my $julius = Cipher::Caesar.new(:shift(3));
    print rot13("Insert secret here");
    print $julius.cipher("Attack at dawn");

=head1 DESCRIPTION

Cipher::Caesar implements simple byte-level modular addition ciphers, including 
rot13, the Caesar cipher (add or subtract values from all letters), and an 
unnamed similar cipher where all bytes are changed, not just letters.

Cipher::Caesar conforms to the standard Cipher API; see L<Cipher/Using the 
Cipher API> for general information.

=head2 Constructor options

With no options, Cipher::Caesar creates a rot13 encipherer/decipherer (rot13 
draws no distinction between them.)  The C<:shift(N)> option can be used to 
select a different number of letters to shift (e.g. Julius Caesar used 3; 
Augustus Caesar used 1).  If a shift is specified, you should also specify 
either C<< :mode<enciphering> >> or C<< :mode<deciphering> >>.  The C<:all> 
option will cause the module to shift across all byte values, not just the ones 
corresponding to letters.

=head1 SECURITY

Caesar ciphers provide extremely trivial security; a cryptanalyst need only 
graph the letter frequency to crack the cipher, and even if the message is 
contrived to hide the letter frequency, the keyspace is only 26.  You would 
have to be insane to use this for anything more than obscuring text from 
accidental viewing.  (rot13 in particular is often used for obscuring spoilers, 
answers, and punchlines.)

=head1 SEE ALSO

L<Cipher>

David Kahn, I<The Codebreakers - The Story of Secret Writing>, 1967. ISBN 0684831309.

=cut

use Cipher;
class Cipher::Caesar-0.01 is Cipher;

my @upper is constant = map {ord} "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split('');
my @lower is constant = map {ord} "abcdefghijklmnopqrstuvwxyz".split('');

has byte %.table;

submethod BUILD(int ?$shift = 13, ?$.all, ?$mode) {
    if $.all {
        for 0..255 {
            %.table{$_} = ($_ + $shift) % 256;
        }
    }
    else {
        for 0..26 {
            %.table{@upper[$_]} = @upper[($_ + $shift) % 26];
            %.table{@lower[$_]} = @lower[($_ + $shift) % 26];
        }
    }
    if $mode ~~ m:P5:i/^de/ {
        my %tt;
        for %.table.kv { %tt{$^value} = $^key }
        %.table = %tt;
    }    
}

method zeroize() {
    for %.table.values -> $x is rw {
        $x = 0;
    }
}

method _cipher(Array $data) {
    map { %.table{$_} // $_ } *$data;
}
