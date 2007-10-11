class GLOBAL {
    sub all {
        Junction.new( things => @_, type => 'all' );
    };
    sub any {
        Junction.new( things => @_, type => 'any' );
    };
    sub none {
        Junction.new( things => @_, type => 'none' );
    };
    sub one {
        Junction.new( things => @_, type => 'one' );
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
        Range.new( start => $a, end => $b );
    };
    sub Inf { Math.Inf };
    sub NaN { Math.NaN };
    sub mkdir { IO.mkdir( @_ ) };
    sub rmdir { IO.rmdir( @_ ) };
    
    sub p5token($regex) { # XXX - this belongs to Runtime::Perl5
        #say 'p5token';
        sub ( $self, $str, $pos ) { 
            #say 'sub returned by p5token';
            if (!(defined($str))) { $str = $_; };
            return match_p5rx($regex,$str,$pos);
        }
    }
}
