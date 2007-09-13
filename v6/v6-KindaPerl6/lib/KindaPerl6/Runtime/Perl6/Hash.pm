use v6-alpha;
class Hash is Container {
    method perl {
        my $pair;   # XXX kp6 ast processor bug
        my $s = '{ ';
        for self.pairs -> $pair { 
            $s = $s ~ ($pair.key).perl ~ ' => ' ~ ($pair.value).perl ~ ', ';
        };
        return $s ~ ' }' 
    };
    method str {
        ( ( self.pairs ).map(sub ($pair) { $pair.key ~ "\t" ~ $pair.value}) ).join( "\n" ); 
    };
    method keys {
        my $pairs = self.pairs;
        $pairs.map(sub ($pair) {$pair.key}); 
    };
    method values {
        my $pairs = self.pairs;
        $pairs.map(sub ($pair) {$pair.value}); 
    };
    method true { self.elems != 0 };
    method int  { self.elems };
    method hash { self };
}
