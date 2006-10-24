class MessageBus::Cache::PlainHash;

use MessageBus::Cacheable;
does MessageBus::Cacheable;

state %cache;

method fetch (*@keys) {
    %cache{@keys};
}

method store ($key, $val, $time) {
    %cache{$key} = ($time => $val);
}

method list_pub_idx ($chan) {
    %cache{$chan};
}

method add_pub ($chan, $pub) {
    %cache{$chan}{$pub} = 0;
}

method remove_pub ($chan, $pub) {
    delete %cache{$chan}{$pub};
}

method get_idx ($chan, $pub) {
    %cache{$chan}{$pub};
}

method set_idx ($chan, $pub, $idx) {
    %cache{$chan}{$pub} = $idx;
}
