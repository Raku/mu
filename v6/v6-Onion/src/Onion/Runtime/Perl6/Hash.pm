use v6-alpha;
class Hash is Container {
    method perl {
        my $s = '{ ';
        for self.pairs -> $pair { 
            $s = $s ~ ($pair.key).perl ~ ' => ' ~ ($pair.value).perl ~ ', ';
        };
        return $s ~ ' }' 
    };
    method Str {
        ( ( self.pairs ).map( -> $pair { $pair.key ~ "\t" ~ $pair.value}) ).join( "\n" ); 
    };
    method keys {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.key}); 
    };
    method values {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.value}); 
    };
    method true { self.elems != 0 };
    method Int  { self.elems };
    method hash { self };
}
