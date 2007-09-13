use v6-alpha;
class Array is Container {
    method perl {
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

    # belongs to List
    method grep(&test) {
        my @result;
        my $s;
        my $v;
        for @(self) -> $v { 
            if test($v) {
                @result.push($v);
            };
        };
        return @result;
    };

    # XXX waits for infix <
#    method min {
#        my $res;
#        $res = @(self)[0];
#        my $v;
#        for @(self) -> $v {
#            # XXX no infix c
#            if $v < $res {
#                $res = $v;
#            }
#        }
#        return $res;
#    };

}

# vim: sw=4 ts=4 expandtab syn=perl6
