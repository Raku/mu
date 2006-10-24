use Test;
use MessageBus;
use MessageBus::Cache::PlainHash;

my @backends = (MessageBus::Cache::PlainHash);

plan @backends * 5;

for @backends {
    my $bus = MessageBus.new(cache => .new);

    my @sub[0] = $bus.new_sub;

    is_deeply(@sub[0].get.map:{.value}, [], 'get worked when there is no pubs');

    my $pub = $bus.new_pub;

    given $pub {
        .put: 'foo';

        @sub[1] = $bus.new_sub;

        .put: 'bar';
        .put: 'baz';
    }

    is_deeply(@sub[0].get.map:{.value}, [<foo bar baz>], 'get worked (1)');
    is_deeply(@sub[0].get, [], 'get emptied the cache (1)');

    is_deeply(@sub[1].get.map:{.value}, [<bar baz>], 'get worked (2)');
    is_deeply(@sub[1].get, [], 'get emptied the cache (2)');
}
