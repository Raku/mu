role MessageBus::Cacheable;

method fetch        (Str *@keys --> List of Pair)       { ... }
method store        (Str $key, Str $val, Num $time)     { ... }

method add_pub      (Str $chan, Str $pub)               { ... }
method remove_pub   (Str $chan, Str $pub)               { ... }

method get_idx      (Str $chan, Str $pub --> Int)       { ... }
method set_idx      (Str $chan, Str $pub, Int $idx)     { ... }

method list_pub_idx (Str $chan --> Hash of Int)         { ... }

method get ($chan, %orig, %curr) {
    @.fetch(
        %curr.kv.map: -> $pub, $idx {
            (%orig{$pub}+1 .. $idx).map:{
                [~] $chan, $pub, $_
            }
        }
    ).sort;
}

method put ($chan, $pub, $msg) {
    my $idx = 1 + $.get_idx($chan, $pub);
    $.store: [~]($chan, $pub, $idx), $msg, time;
    $.set_idx: $chan, $pub, $idx;
}

