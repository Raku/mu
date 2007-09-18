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
    sub infix:<..>($a,$b) {
        my $r = Range.new(); $r.start = $a; $r.end = $b;
        $r;
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
            return match_p5rx($regex,$str,$pos);
        }
    }
}
