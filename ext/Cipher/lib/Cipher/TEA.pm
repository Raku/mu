use Cipher::Block;
class Cipher::TEA is Cipher::Block;

has int @.key;
my  int $delta is readonly = 0x9e3779b9;

submethod BUILD($key) {
    if ($key.isa(Array)) {
        @.key = $?SELF!fusebytes($key);
    }
    else {
        @.key = $?SELF!fusebytes(map {ord} $key);
    }
}

method _zeroize() {
    for @.key -> $x is rw { $x = 0 }
}

method block_size() returns int { 8 }

method cipher_block(Array $block) returns Array {
    my int($v0, $v1) = $?SELF!fusebytes($block);
    
    if ($.mode eq 'enciphering') {
        my int $sum = 0;
        for 0..32 {
            $sum += $delta;
            $v0  += ($v1 +< 4) + @.key[0] +^ $v1 + $sum +^ ($v1 +> 5) + @.key[1];
            $v1  += ($v0 +< 4) + @.key[2] +^ $v0 + $sum +^ ($v0 +> 5) + @.key[3];
        }
    }
    else {
        my int $sum = 0xc6ef3720;
        for 0..32 {
            $v1  -= ($v0 +< 4) + @.key[2] +^ $v0 + $sum +^ ($v0 +> 5) + @.key[3];
            $v0  -= ($v1 +< 4) + @.key[0] +^ $v1 + $sum +^ ($v1 +> 5) + @.key[1];
            $sum -= $delta;
        }
    }
    
    return $?SELF!splitbytes($v0, $v1);
}

my method fusebytes(@bytes) {
    gather {
        for @bytes -> $b1, $b2, $b3, $b4 {
            my $quad = $b1 +< 24;
            $quad  +|= $b2 +< 16;
            $quad  +|= $b3 +<  8;
            $quad  +|= $b4;
            take $quad;
        }
    }
}

my method splitbytes(@bytes) {
    gather {
        for @bytes {
            take $_ +& 0xff000000 +> 24;
            take $_ +& 0x00ff0000 +> 16;
            take $_ +& 0x0000ff00 +>  8;
            take $_ +& 0x000000ff;
        }
    }
}
