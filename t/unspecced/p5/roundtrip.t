#!/usr/bin/pugs

use v6;
use Test;

plan(2);

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
