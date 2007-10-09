use v6-alpha;
class Gather is Array {
    has $.code;
    has $.buf;
    has $.finished;
    method perl {
        '( gather ' ~ $.code.perl ~ ' )' 
    };
    method str {
        (self.eager).str;
    };
    method true {
        self._more;
        return $.buf.true; 
    };
    method eager  { 
        while !$.finished { self._more };
        self.buf;
    };
    method lazy {
        self
    };
    method elems  { 
        (self.eager).elems;
    };
    method hash {
        (self.eager).hash;
    };
    method array {
        self
    };
    method INDEX ($ix) {
        my $obj = self;
        my $i = $ix + 0;
        #say "index $ix on ",$obj.buf;
        while !$obj.finished { 
                if $i < ($obj.buf).elems 
                {
                    return ($obj.buf)[$i];
                };
                $obj._more; 
        };
        return ($obj.buf)[$i];
    };
    method map (&code) {
        my $obj = self;
        my $a;
        $a = Gather.new( sub {
            my $i = 0;
            while !$obj.finished { 
                    my $r = $obj[$i];
                    $r = code( $r );
                    #say "take $r";
                    $a._take( $r ); 
                    $i = $i + 1;
            };
            while $i < ($obj.buf).elems { 
                    my $r = $obj[$i];
                    $r = code( $r );
                    #say "take $r";
                    $a._take( $r ); 
                    $i = $i + 1;
            };
        } );
        return $a;
    };
}
