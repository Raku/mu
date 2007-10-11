use v6-alpha;
class Array is Container {
    method perl {
        '[ ' ~ (self.map( sub { $_.perl } )).join(', ') ~ ' ]' 
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
        for @(self) -> $v { 
            $_ := $v;   # this should be automatic ???
            if test($v) { 
                @result.push($v);
            };
        };
        return @result;
    };

    method map(&proc) {
        my @result;
        for @(self) -> $v { 
            $_ := $v;   # this should be automatic ???
            @result.push(proc($v));
        };
        return @result;
    };

    method join($sep) {
        my $result = '';
        my $s = '';
        for @(self) -> $v { 
            $result = $result ~ $s ~ $v;
            $s = $sep;
        };
        return $result;
    };

    method uniq {
        my %h;
        my @res;
        for @(self) -> $v { 
            if !(%h{$v}) {
                @res.push($v);
            };
            %h{$v} = 1;
        };
        @res;
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
