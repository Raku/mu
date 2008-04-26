#!/usrbin/perl -w
use Test::More tests => 3;
use strict;
use Data::Bind;

Data::Bind->sub_signature
    (\&formalize,
     { var => '$title', constraint => sub { length($_[0]) > 1 } },
     { var => '$subtitle' },
     { var => '$case', named_only => 1 },
     { var => '$justify', named_only => 1 });
sub formalize {
    my ($title, $subtitle, $case, $justify);
    Data::Bind->arg_bind(\@_);
    no warnings 'uninitialized';
    return join(':', $title, $case, $justify);
}

is(formalize([\'this is title', \'subtitle'], { case => \'blah'}),
   'this is title:blah:');

my $r = eval { formalize([\'', \'subtitle'], { case => \'blah'}); 1 };
my $e = $@;
is( $r, undef, "dies with bad value" );
like( $@, qr/constraint.*title/i, "error" );
