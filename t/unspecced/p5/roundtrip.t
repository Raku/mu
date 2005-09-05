#!/usr/bin/pugs

use v6;
use Test;

plan(6);

unless eval 'eval("1", :lang<perl5>)' {
    skip_rest;
    exit;
}

eval(q/
package Id;
sub new {
    my ($class, $ref) = @_;
    bless \$ref, $class;
}
sub identity {
    my $self = shift;
    return $$self;
}
/, :lang<perl5>);

my $japh = { "Just another $_ hacker" };
my $japh2 = -> $name { "Just another $name hacker" };
my $id   = eval("Id", :lang<perl5>);

is($id.new($japh).identity.('Pugs'), 'Just another Pugs hacker', "Closure roundtrips");
is($id.new($japh2).identity.('Pugs'), 'Just another Pugs hacker', "Closure roundtrips");

my $keys_p5 = eval('sub {warn join(",",@_); return keys %{$_[0]}}', :lang<perl5>);
my $tohash_p5 = eval('sub { return {map {$_ => 1} @_ } }', :lang<perl5>);
my %hash = (foo => 'bar', hate => 'software');
{
    lives_ok {
    my $foo = $tohash_p5.(keys %hash);
    cmp_ok($foo, &infix:<cmp>, %hash);
    cmp_ok($foo.keys, &infix:<cmp>, %hash.keys);
    }
}

skip_rest; # XXX - for release
exit;

{
    lives_ok { # is_deeply
        cmp_ok(%hash.keys, &infix:<cmp>, $keys_p5.(%hash));
    }
}
