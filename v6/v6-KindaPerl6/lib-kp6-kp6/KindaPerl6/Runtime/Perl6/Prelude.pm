class GLOBAL {
    sub all {
        my $junc = Junction.new;
        $junc.things = @_;
        $junc.type = 'all';
        $junc;
    };
    sub any {
        my $junc = Junction.new;
        $junc.things = @_;
        $junc.type = 'any';
        $junc;
    };
    sub none {
        my $junc = Junction.new;
        $junc.things = @_;
        $junc.type = 'none';
        $junc;
    };
    sub one {
        my $junc = Junction.new;
        $junc.things = @_;
        $junc.type = 'one';
        $junc;
    };
    sub infix:<|>($a,$b) {
        any($a,$b);
    };
    sub infix:<&>($a,$b) {
        all($a,$b);
    };
    sub infix:<^>($a,$b) {
        one($a,$b);
    };
    sub Inf { Math.Inf };
    sub NaN { Math.NaN };
    sub mkdir { IO.mkdir( @_ ) };
    sub rmdir { IO.rmdir( @_ ) };
    sub p5token($regex) {
        #say 'p5token';
        sub ( $self, $str, $pos ) { 
            #say 'sub returned by p5token';
            if (!(defined($str))) { $str = $_; };
            my $MATCH;
            $MATCH = Match.new(); $MATCH.match_str = $str; $MATCH.from = $pos; $MATCH.to = $pos; $MATCH.bool = 1;
            $MATCH.bool = match_p5rx($str,$regex);
            return $MATCH;
        }
    }
}

