use v6-alpha;
class Hash is Container {
    has $.pairs;
    method perl {
        '{ ... }' 
    };
    method str {
        '...' 
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    #method int  { ... };
}
