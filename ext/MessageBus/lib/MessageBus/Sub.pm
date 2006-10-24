class MessageBus::Sub;

has $.chan;     # Current channel key
has %!pubs;     # Hash from Pub ID to last-refreshed index
has $!cache;    # Cache object

submethod BUILD ($.chan, $!cache) {
    # populate pub indices from Cache
    %!pubs = $!cache.publisher_indices($.chan);
}

method get () {
    my %orig = %!pubs;
    %!pubs = $!cache.publisher_indices($.chan);
    $!cache.get($.chan, %orig, %!pubs);
}
