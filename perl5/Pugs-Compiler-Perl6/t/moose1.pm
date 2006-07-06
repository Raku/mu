use v6-alpha;
class Point;

has $.x is rw;  # instance attributes
has $.y;        # default "is readonly" 
method clear () {
    $.x = 0;  # accessible within the class
    $.y = 0;
}

