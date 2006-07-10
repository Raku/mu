use v6-alpha;

module main;

enum bool <False True>;

sub true is export {
    $_[0] ?? bool::True !! bool::False
}
