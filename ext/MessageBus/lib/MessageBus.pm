class MessageBus;

use MessageBus::Pub;
use MessageBus::Sub;

has $!cache;

method new_pub ($chan = '') {
    MessageBus::Pub.new(:$chan, :$!cache);
}

method new_sub ($chan = '') {
    MessageBus::Sub.new(:$chan, :$!cache);
}
