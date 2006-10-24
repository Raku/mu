class MessageBus::Pub;

has $.chan;     # Channel key for this Pub
has $!uuid;     # UUID for this Pub
has $!cache;    # Cache object

submethod BUILD ($.chan, $!cache) {
    # use perl5:Data::UUID;
    $!uuid = rand; # Data::UUID.new.create_b64;
    $!cache.add_pub($.chan, $!uuid);
}

submethod DESTROY () {
    $!cache.remove_pub($.chan, $!uuid);
}

method put ($msg) {
    $!cache.put($.chan, $!uuid, $msg);
}
