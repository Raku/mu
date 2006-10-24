class MessageBus;

use MessageBus::Pub;
use MessageBus::Sub;

has $!cache;

method publish ($chan = '') {
    MessageBus::Pub.new(:$chan, :$!cache);
}

method subscribe ($chan = '') {
    MessageBus::Sub.new(:$chan, :$!cache);
}
