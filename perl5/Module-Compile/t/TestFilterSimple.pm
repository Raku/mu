package t::TestFilterSimple;
use Filter::Simple::Compile;

FILTER {
    s/y/k/g; # Y2K!
};

1;
