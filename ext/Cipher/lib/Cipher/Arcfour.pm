=head1 NAME

Cipher::Arcfour - Arcfour (RC4-compatible) stream cipher

=head1 SYNOPSIS

    $ciphertext = Cipher::Arcfour.encipher($plaintext, :key($key));

    &cipher := Cipher::Arcfour.encipherer(:key($key));
    print cipher($_) for =$IN;
    print cipher();

    my $cipher = Cipher::Arcfour.new(:key($key));
    @output = gather { take $cipher.cipher($_) for @input };

=head1 DESCRIPTION

Arcfour is a very simple (less than fifteen readable lines) but surprisingly 
secure stream cipher.  It is believed to be compatible with RSA Security's 
RC4(tm) cipher, in that keys should produce the same keystream and thus text 
should be enciphered and deciphered the same way.  RC4 and Arcfour are used in 
standards such as SSH, SSL and TLS, WEP, and WPA, as well as many non-standard 
cryptosystems.

Arcfour operates on whole bytes, XORing elements of a constantly-transforming 
state table with each byte of plaintext or ciphertext.  (It doesn't distinguish 
between enciphering and deciphering operations.)

Although it is secure when used properly, using it incorrectly can lead to 
serious insecurity; see the L</SECURITY> section for details.  (In particular, 
note that the serious insecurities plaguing WEP were caused by improper use of 
RC4.)

=head2 Interface

The interface for Cipher::Arcfour is largely defined by L<Cipher::Stream> and 
its parent module, L<Cipher>; for example, the methods used for actual 
cryptography are there.  See those modules for details.

The C<new> constructor takes only one option, C<:key>, containing the key.  
Arcfour keys can be any random number between 40 and 128 bits long, rounded to 
the nearest byte (but see L</SECURITY> for important caveats).

Cipher::Arcfour also includes several attributes which might be interesting to 
people interested in the algorithm itself.  The attributes in question are 
C<i>, C<j> and C<state>.  To understand how to interpret them, see the 
Wikipedia article on RC4, I<Applied Cryptography> by Bruce Schneier, or most 
cryptography resources written in the last ten years or so.

=head1 SECURITY

Although Arcfour is perfectly secure when used properly, it has several known 
problems and weaknesses.

=over 4

=item Initial portions of Arcfour's keystreams are weak.

Basically, the first few bytes of data aren't encrypted well.  If your 
application allows, skip over a kilobyte or so by calling 
C<generate_keystream> with a number of bytes and throwing the return value away.
Encryptions after this point will be using stronger portions of the keystream.
(Just remember to do it at both ends!)

=item Encrypting with the exact same key twice can compromise both encryptions.

If two different data streams are enciphered with the same key, the encryption 
can be removed by XORing them together; you might not be able to derive the 
plaintext from the result, but any cryptographer worth the name can.  To avoid 
this, make sure you combine the key with a nonce or initialization vector, a 
one-time random number.  (It will need to be sent to the other end along with 
the ciphertext, but it's safe to send it in the clear.)

=item Arcfour is susceptible to related-key attacks.

This means that you should strive to use only truly random keys; Perl's C<rand> 
function is not good enough.  (You should be doing this anyway, though.)  It 
also means that the nonce or initialization vector should be hashed with the 
key, not merely appended to it; appending can create precisely the sort of weak 
keying that can be exploited by a cryptoanalyst.  Use 128 bits of a good 
cryptographic hashing/authentication algorithm like HMAC-SHA-256 for this.

=back 4

=head1 SEE ALSO

L<Cipher>, L<Cipher::Stream>

Bruce Schneier. I<Applied Cryptography, Second Edition>. 1995, published by 
John Wiley & Sons, Inc.

=head1 COPYRIGHT

Copyright (C) 2005 Brent Royal-Gordon <brent@brentdax.com>.

This code is free software, and may be used, distributed and/or modified under 
the same terms as Perl itself.

=cut

use Cipher::Stream;
class Cipher::Arcfour is Cipher::Stream;

has Byte @.state;
has Int $.i;
has Int $.j;

submethod BUILD(?$key) {
    my @key;
    if $key.isa(Array)  { @key = *$key }
    else { @key = map {ord} $key.split }
    
    .zeroize();
    
    # Initialize state table
    my $j = 0;
    for 0..255 -> $i {
        ($j += @.state[$i] + @key[$i % @key.elems]) %= 256;
        @.state[$i, $j] = @.state[$j, $i];
    }
}

method zeroize() {
    @.state[$_] = $_ for 0..255;
    $.i = $.j = 0;
}

method generate_keystream(Int $n) returns Array {
    return gather {
        for 1..$n {
            ++$.i;
            $.i %= 256;
            ($.j += @.state[$.i]) %= 256;
            @.state[$.i, $.j] = @.state[$.j, $.i];
            take @.state[ (@.state[$.i] + @.state[$.j]) % 256 ];
        }
    }
}
