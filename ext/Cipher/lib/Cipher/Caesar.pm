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
    #warn $_.perl();
    map { %.table{$_} // $_ } *$data;
}
