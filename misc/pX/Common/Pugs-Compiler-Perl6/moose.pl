use v6-pugs;
class Point;

has $.x is rw;  # instance attributes
has $.y;        # default "is readonly" 
method clear () {
    $.x = 0;  # accessible within the class
    $.y = 0;
}

# use v5;
# package Point;
# use Moose;
#
# has x => (is => 'rw');
# has y => (is => 'ro');
#
# sub clear {
#  my $self = shift;
# #  $self‐>{x} = 0;
#  $self‐>y(0);
# }
