use Cipher;
class Cipher::Block is Cipher;

has int @.buffer;

# Stuff the block cipher has to implement
method cipher_block(Array $data) returns Array {...}
method _zeroize()                              {...}

# Things roles can override
method pad() {
    my $bs = .block_size;
    @.buffer.push(128);
    @.buffer.push(0) while @.buffer < $bs;
    return *.cipher_block(@.buffer);
}

method _cipher(Array $data) {
    push @.buffer, *$data;
    my int $bs = .block_size;
    
    gather {
        while @.buffer >= $bs {
            my @data = @.buffer.splice(0, $bs);
            @.buffer.splice(0, $bs, []);
            take *.cipher_block(@data);
        }
    }
}

# Things we have to do
method zeroize() {
    ._zeroize();
    for @.buffer -> $x is rw { $x = 0 }
}

method _tail() {
    return .pad();
}
