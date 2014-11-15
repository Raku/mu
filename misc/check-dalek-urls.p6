use v6;
use JSON::Tiny;
use HTTP::UserAgent;

my @confs := from-json slurp 'dalek-conf.json';

my $ua = HTTP::UserAgent.new(timeout => 10);

for @confs -> $c {
    next unless $c<url>;
    my $response = try $ua.get($c<url>);
    if $response && $response.is-success {
        say "OK: $c<url>";
    }
    else {
        say $c<url> , '    ', $response.?status-line // $!;
    }
}
