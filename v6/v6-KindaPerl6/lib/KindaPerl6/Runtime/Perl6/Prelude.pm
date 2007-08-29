class GLOBAL {
    sub all {
        my $junc = Junction.new;
        $junc.things = @_;
        $junc.type = 'all';
        $junc;
    };
}

