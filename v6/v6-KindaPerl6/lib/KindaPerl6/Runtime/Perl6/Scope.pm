use v6-alpha;
class Scope is Container {

    has $.hash;
    has $.outer;  # static or dynamic scope
    
    method inner {
        my $inner = Scope.new();
        $inner.outer = self;
        $inner.hash  = {};
        return $inner;
    };

    method LOOKUP ( $key ) {
        if exists( $.hash{$key} ) {
            return $.hash{$key};
        };
        return (self.outer).LOOKUP( $key );
    };

    # TODO !!!
    
    method perl {
        my $s = '{ ';
        for self.pairs -> $pair { 
            $s = $s ~ ($pair.key).perl ~ ' => ' ~ ($pair.value).perl ~ ', ';
        };
        return $s ~ ' }' 
    };
    method str {
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
    method int  { self.elems };
    method hash { self };
}
