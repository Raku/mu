use v6-alpha;
class Array is Container {
    method perl {
        '[ ' ~ (self.map( sub { $_.perl } )).join(', ') ~ ' ]' 
    };
    method Str {
        self.join( ' ' );
    };
    method true { self.elems != 0 };
    method Int  { self.elems };
    method array { self };

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

    method smartmatch ( $v ) {
        ( self.grep: sub { $v ~~ $_ } ).true
    };

    method min {
        my $res = self.[0];
        my $v;
        for @(self) -> $v {
            if $v < $res {
                $res = $v;
            }
        }
        return $res;
    };

    method max {
        my $res = self.[0];
        my $v;
        for @(self) -> $v {
            if $v > $res {
                $res = $v;
            }
        }
        return $res;
    };

}

# vim: sw=4 ts=4 expandtab syn=perl6
