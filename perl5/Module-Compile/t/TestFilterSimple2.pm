package t::TestFilterSimple2;
use Filter::Simple::Cached sub {
    s/y/k/g; # Y2K!
};

1;
