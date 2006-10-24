role MessageBus::Cacheable;

method fetch                (Str *@keys --> List of Pair)                   { ... }
method store                (Str $key, Str $val, Num $time, Num $expiry)    { ... }

method add_publisher        (Str $chan, Str $pub)                           { ... }
method remove_publisher     (Str $chan, Str $pub)                           { ... }

method get_index            (Str $chan, Str $pub --> Int)                   { ... }
method set_index            (Str $chan, Str $pub, Int $index)               { ... }

method publisher_indices    (Str $chan --> Hash of Int)                     { ... }

method get ($chan, %orig, %curr) {
    @.fetch(
        %curr.kv.map: -> $pub, $index {
            (%orig{$pub}+1 .. $index).map:{
                [~] $chan, $pub, $_
            },
        },
    ).sort;
}

method put ($chan, $pub, $msg, $expiry = 0) {
    my $index = 1 + $.get_index($chan, $pub);
    $.store: [~]($chan, $pub, $index), $msg, time, $expiry;
    $.set_index: $chan, $pub, $index;
}
