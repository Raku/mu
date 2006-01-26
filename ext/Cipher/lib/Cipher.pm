=head1 NAME

Cipher.pm - Perl 6 Cipher API

=head1 SYNOPSIS

    class Cipher::Not is Cipher {
        method zeroize() {
            # No state to zeroize
        }
        method _cipher(byte @data) {
            return map { +^$_ } @data;
        }
    }
    #Later...
    my $encrypt = Cipher::Not.new(:mode<encrypt>);
    print $encrypt.cipher($_) for =$IN;     # Works with bytearrays and strings
    print $encrypt.finishstr();

=head1 DESCRIPTION

The Cipher API is a common interface for cryptographic ciphers.  Ciphers 
conforming to the Cipher API are interchangable, allowing a programmer to 
change ciphers simply by changing the class name used.

Ciphers used with the Cipher API must operate on bytes or multi-byte blocks; 
the API does not support enciphering individual bits.  Also note that the 
Cipher API operates at the byte level, not the character level, so different 
character encodings may be enciphered in different ways.

=head2 Using the API

The Cipher API has functional, procedural, and object-oriented interfaces.  The 
functional and object-oriented interfaces both follow the same basic steps, 
albeit with different interfaces:

=over 4

=item 1. Create the cipher.

=item 2. Give it data to encipher (or decipher), retrieving intermediate 
results along the way.

=item 3. Retrieve any final data and clear the cipher's state.

=back 4

The procedural interface does the same thing within a single call, hiding the 
full complexity.

=head3 Procedural interface

The Cipher API's procedural interface is good enough for many purposes.  
Although the interface is said to be procedural, it is invoked via two class 
methods.

=over 4

=item C<encipher>

    $ciphertext = Some::Cipher.encipher($plaintext, :opt<value>, :opt2<value2>);

Enciphers the plaintext string, returning the ciphertext version.  Most 
algorithms will have a key, which will be specified in the options; check your 
cipher's documentation for details.

=item C<decipher>

    $plaintext = Some::Cipher.decipher($ciphertext, :opt<value>, :opt2<value2>);

Does the same, but deciphers instead of enciphering.  (There may not be a 
distinction for some ciphers.)

=back 4

=head3 Functional interface

The Cipher API's functional interface is perhaps the easiest one that allows 
for continuous ciphering.

Both of these functions return a closure.  Call the closure with a block of 
data to have it enciphered or deciphered; when you're done, call the closure 
with no arguments to finish the ciphering and clear the cipher's state.  
Remember that ciphering steps may not return all (or any) of the data you 
passed in, and that the finishing step may return data.

An example:

    &cipher := Cipher::Arcfour.encipherer(:key($key));  #Create the cipher closure
    print cipher($text1);   # Encrypt some data
    print cipher($text2);   # Encrypt more data
    print cipher();         # Finish up

=over 4

=item C<encipherer>

    $encipher = Some::Cipher.encipherer(:opt1<value1>, :opt2<value2>);

Create an encipherer closure, which can be called repeatedly to encipher data.
The encipherer closure can be called with a string, and should be called at the 
end of enciphering with zero arguments.

=item C<decipherer>

    $decipher = Some::Cipher.decipherer(:opt1<value1>, :opt2<value2>);

The same, except that the closure deciphers instead of enciphering.

=back 4

=head3 Object-Oriented interface

The Cipher API is fundamentally object-oriented; the procedural and functional 
interfaces are layers on top of the object-oriented backend.

To use the Cipher API, you must first create an object of the appropriate 
class, passing the key, a mode (either "enciphering" or "deciphering"), and any 
other options to the constructor.  Then you call the cipher() method repeatedly 
with your data.  Finally, call the finish() method to return any leftover data 
and clear the cipher object.

The methods intended to be used by Cipher API users are:

=over 4

=item C<new>

    $cipher_obj = Some::Cipher.new(:opt1<value1>, :opt2<value2>, :mode<enciphering>);

This class method constructs a new cipher object.  The options passed to the 
C<new> constructor generally include some sort of key, and sometimes also 
parameters for block size and so on; see the cipher module's documentation for 
details.  The C<mode> value indicates the operation to be performed; the values 
C<encrypting>, C<encrypt> and C<encipher> are equivalent to C<enciphering>, 
while C<decrypting>, C<decrypt> and C<decipher> are equivalent to 
C<deciphering>.

=item C<cipher>

    push @output, $cipher_obj.cipher($data);

Enciphers or deciphers the given data.  May return data which is part of the 
ciphertext, but is not necessarily the ciphertext version of the data given.
The data may be either a string or an array of bytes; the array of bytes will 
be slightly faster.

=item C<finish>

    push @output, $cipher_obj.finish();

Completes ciphering of any leftover data and returns it as an array of bytes, 
then clears the object's internal state.  This should be called as the last 
step of enciphering.

=item C<finishstr>

    push @output, $cipher_obj.finishstr();

The same as C<finish>, but returns the data as a string instead of an array of 
bytes.

=item C<zeroize>

    $cipher_obj.zeroize();

Tells the cipher object to clear its internal state as completely as possible. 
This is automatically done as part of C<finish>, C<finishstr> and the 
destructor, but there may be situations in which explicitly zeroizing the 
cipher object would be desirable.

Note that calling any methods on a zeroed (and hence, also a finished) cipher 
object has undefined results.

=back 4

=head2 Writing for the API

Although the API seems large, most of it is implemented within the C<Cipher> 
class itself; in fact, the only method that truly must be implemented is the 
one that actually enciphers the data.  They may choose, however, to override 
any method for speed--although they should be careful to avoid changing the 
actual semantics.

Please note that most of the time, it will not be necessary to write these 
methods yourself; in particular, L<Cipher::Block> and L<Cipher::Stream> can 
reduce the implementation of most block and stream ciphers to a constructor, 
the cryptographic core, and a couple attributes.

=over 4

=item C<BEGIN>

Although not technically part of the API, most ciphers will want to implement 
a constructor with C<BEGIN>.

=item C<_cipher>

    C<method _cipher(Array of byte $data) returns Array of byte {...}>

Performs the actual ciphering.  This method should operate on a copy of the 
data, either by declaring C<$data> to be an C<is copy> variable or by making a 
copy internally, and should return the copy once it has been encrypted or 
decrypted.

=item C<_head>

    C<method _head() returns Array of byte {...}>

Returns any data the cipher needs to add at the beginning, before the 
ciphertext.  This will be prepended to the return value of the first call to 
C<_cipher>.  This is rarely used by the cipher itself; more often it is used by 
a block cipher mode role to add some information to the output.

A default implementation of this method is provided which returns nothing.

=item C<_tail>

    C<method _tail() returns Array of byte {...}>

Called as part of C<finish>; ciphers and returns any pending data.  Block 
ciphers should buffer input data until they have an entire block, then cipher 
the block and return the new ciphertext (or plaintext).  A call to C<_tail> 
indicates that there is no more incoming data, and the remaining data should be 
padded and ciphered.  (The L<Cipher::Block> class takes care of much of this.)

A default implementation of this method is provided which returns nothing.

=item C<zeroize>

    C<method zeroize() returns Void {...}>

Clears the cipher object's internal structures.

Cipher authors should assume that if this method is being called, the Secret 
Police are breaking down the door and Our Heroes need any sensitive data still 
in the cipher to be deleted immediately.  This method should be as thorough as 
possible; for example, it should not merely set arrays to C<()>, but loop 
through them setting all the elements to 0.  In reality, most calls will be for 
pedestrian events like object destruction, but let's not make too many 
assumptions.

=back 4

=head1 DISCLAIMER

This code has not been reviewed by a security expert, and may contain bugs and 
vulnerabilities.  B<Perform your own security analysis before using it in any 
sensitive application.>

B<I<The authors and copyright holders DO NOT take responsibility for any 
insecurity caused by bugs in the Cipher API or individual ciphers.>>

=head1 SEE ALSO

L<Cipher::Stream>, L<Cipher::Block>

L<Digest>

Bruce Schneier. I<Applied Cryptography, Second Edition>. 1995, published by 
John Wiley & Sons, Inc.

=head1 COPYRIGHT

Copyright (C) 2005 Brent Royal-Gordon <brent@brentdax.com>.

This code is free software, and may be used, distributed and/or modified under 
the same terms as Perl itself.

=cut

class Cipher-0.02;

#enum Cipher::Mode <enciphering deciphering>;
has $.mode;
has bool $!seen_head;

submethod BUILD($.mode = "enciphering") {
    $!seen_head = 0;
    given lc $.mode {
        when any <enciphering encipher encrypting encrypt> {
            $.mode = "enciphering";
        }
        when any <deciphering decipher decrypting decrypt> {
            $.mode = "deciphering";
        }
        default { die "Unrecognized mode $.mode" }
    }
}

# What they need to implement.
method zeroize()                {...}
method _cipher(Array $data) returns Array {...}
# Many, but not all, ciphers will need to override these
method _head() returns Array { return }
method _tail() returns Array { return }

# What we implement for them.
method finish(Cipher $self:) returns Array {
    my @tail=$self._tail();
    $self.zeroize();
    return @tail;
}
method finishstr(Cipher $self:) returns Str {
    return stringify($self.finish());
}

multi method cipher(Cipher $self: Array $data) returns Array {
    return gather {
        unless $!seen_head {
            take *$self._head();
            $!seen_head = 1;
        }
        take *$self._cipher($data);
    };
}
multi method cipher(Cipher $self: Str $data) {
    return stringify($self.cipher(byteify($data)));
}

method encipher(Class $class: Str $plaintext, *%options) {
    my $self = $class.new(*%options, :mode<enciphering>);
    return $self.cipher($plaintext) ~ $self.finishstr();
}
method decipher(Class $class: Str $ciphertext, *%options) {
    my $self = $class.new(*%options, :mode<deciphering>);
    return $self.cipher($ciphertext) ~ $self.finishstr();
}

method encipherer(Class $class: *%options) {
    my $self = $class.new(:mode<enciphering>, *%options);
    return sub(Str $plaintext?) {
        if defined $plaintext  { return $self.cipher($plaintext) }
        else                   { return $self.finishstr() }
    };
}
method decipherer(Class $class: *%options) {
    my $self = $class.new(*%options, :mode<deciphering>);
    return sub(Str $ciphertext?) {
        if defined $ciphertext { return $self.cipher($ciphertext) }
        else                   { return $self.finishstr() }
    };
}

submethod DESTROY() {
    .zeroize();
}

# utility subroutines

sub byteify(Str $string) returns Array of Int {
    return map {ord} $string.split('');
}
sub stringify(Array $array) returns Str {
    return [~] map {chr} *$array;
}
