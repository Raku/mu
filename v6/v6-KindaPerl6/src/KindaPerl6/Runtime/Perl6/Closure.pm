use v6-alpha;
class Closure {
    has $.code;
    has $.scope;
    method perl {
        'sub { ... }' 
    };
    method str {
        'sub { ... }' 
    };
}
