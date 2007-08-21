use v6-alpha;
class Hash is Container {
    #has $.pairs;
    method perl {
        my $pair;   # XXX kp6 ast processor bug
        my $s = '{ ';
        for self.pairs -> $pair { 
            $s = $s ~ $pair.key ~ ' => ' ~ $pair.value ~ ', ';
        };
        return $s ~ ' }' 
    };
    method str {
        '...' 
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    #method int  { ... };
}
