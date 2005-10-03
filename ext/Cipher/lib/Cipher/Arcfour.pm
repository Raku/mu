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
            ++$.i %= 256;
            ($.j += @.state[$.i]) %= 256;
            @.state[$.i, $.j] = @.state[$.j, $.i];
            take @.state[ (@.state[$.i] + @.state[$.j]) % 256 ];
        }
    }
}
