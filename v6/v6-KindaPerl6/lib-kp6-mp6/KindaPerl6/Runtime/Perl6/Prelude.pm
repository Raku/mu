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
}

