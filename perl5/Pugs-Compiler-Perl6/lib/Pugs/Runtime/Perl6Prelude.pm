use v6-alpha;

enum bool <False True>;

sub true {
    $_[0] ?? bool::True !! bool::False
}
