package t::TestFilterSimple;
use Filter::Simple::Cached;

FILTER {
    s/y/k/g; # Y2K!
};

1;
