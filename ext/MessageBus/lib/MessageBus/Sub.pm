class MessageBus::Sub;

has $.chan;     # Current channel key
has %!pubs;     # Hash from Pub ID to current index
has $!cache;    # Cache object

submethod BUILD ($.chan, $!cache) {
    # populate pub indices from Cache
    %!pubs = $!cache.list_pub_idx($.chan);
}

method get () {
    my %orig = %!pubs;
    %!pubs = $!cache.list_pub_idx($.chan);
    $!cache.get($.chan, %orig, %!pubs);
}
