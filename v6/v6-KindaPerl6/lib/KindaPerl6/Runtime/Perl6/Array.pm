use v6-alpha;
class Array is Container {
    method perl {
        my $v;   # XXX kp6 ast processor bug
        my $s = '[ ';
        for @(self) -> $v { 
            $s = $s ~ $v.perl ~ ', ';
        };
        return $s ~ ' ]' 
    };
    method str {
        self.join( ' ' );
    };
    method true { self.elems != 0 };
    method int  { self.elems };
    method array { self };
}
