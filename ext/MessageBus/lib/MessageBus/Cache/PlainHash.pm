class MessageBus::Cache::PlainHash;

use MessageBus::Cacheable;
does MessageBus::Cacheable;

state %cache;

method fetch (*@keys) {
    %cache{@keys};
}

method store ($key, $val, $time, $expiry) {
    %cache{$key} = ($time => $val);
}

method publisher_indices ($chan) {
    %cache{$chan};
}

method add_publisher ($chan, $pub) {
    %cache{$chan}{$pub} = 0;
}

method remove_publisher ($chan, $pub) {
    delete %cache{$chan}{$pub};
}

method get_index ($chan, $pub) {
    %cache{$chan}{$pub};
}

method set_index ($chan, $pub, $idx) {
    %cache{$chan}{$pub} = $idx;
}
