package t::TestFilterSimple2;
use Filter::Simple::Compile sub {
    s/y/k/g; # Y2K!
};

1;
