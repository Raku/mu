#!/usr/bin/pugs

use v6;
use Test;

plan(6);

unless eval 'eval_perl5("1")' {
    skip_rest;
    exit;
}

eval_perl5(q/
package Id;
sub new {
    my ($class, $ref) = @_;
    bless \$ref, $class;
}
sub id {
    my $self = shift;
    return $$self;
}
/);

my $japh = { "Just another $_ hacker" };
my $japh2 = -> $name { "Just another $name hacker" };
my $id   = eval_perl5("Id");

is($id.new($japh).id.('Pugs'), 'Just another Pugs hacker', "Closure roundtrips");
is($id.new($japh2).id.('Pugs'), 'Just another Pugs hacker', "Closure roundtrips");

my $keys_p5 = eval_perl5('sub {warn join(",",@_); return keys %{$_[0]}}');
my $tohash_p5 = eval_perl5('sub { return {map {$_ => 1} @_ } }');
my %hash = (foo => 'bar', hate => 'software');
{
    lives_ok {
    my $foo = $tohash_p5.(keys %hash);
    cmp_ok($foo, &infix:<cmp>, %hash);
    cmp_ok($foo.keys, &infix:<cmp>, %hash.keys);
    }
}
{
    lives_ok { # is_deeply
	cmp_ok(%hash.keys, &infix:<cmp>, $keys_p5.(%hash));
    }
}
