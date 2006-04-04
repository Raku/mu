#!/usrbin/perl -w
use Test::More tests => 1;
use strict;
use Data::Bind;

Data::Bind->sub_signature
    (\&formalize,
     { var => '$title' },
     { var => '$subtitle' },
     { var => '$case', named => 1 },
     { var => '$justify', named => 1 });
sub formalize {
    my ($title, $subtitle, $case, $justify);
    Data::Bind->arg_bind(\@_);
    no warnings 'uninitialized';
    return join(':', $title, $case, $justify);
}

is(formalize([\'this is title', \'subtitle'], { case => \'blah'}),
   'this is title:blah:');
