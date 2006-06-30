use v6-pugs;
class Point;

has $.x is rw;  # instance attributes
has $.y;        # default "is readonly" 
method clear () {
    $.x = 0;  # accessible within the class
    $.y = 0;
}

