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
        my $pair;   # XXX kp6 ast processor bug
        my $s = '';
        for self.pairs -> $pair { 
            # XXX no tabs or newlines yet
            $s = $s ~ $pair.key ~ '  ' ~ $pair.value ~ ', ';
        };
        return $s 
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
