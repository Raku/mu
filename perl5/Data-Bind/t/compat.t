#!/usr/bin/perl -w
use strict;
use Test::More tests => 6;
use Data::Bind;

my $db_formalize_sig = Data::Bind->sig
    ({ var => '$title' },
     { var => '$subtitle' },
     { var => '$case', named_only => 1 },
     { var => '$justify', named_only => 1 });

#warn Dumper($db_formalize_sig);


is( $db_formalize_sig->is_compatible
      ([\'this is title', \'subtitle'], { case => \'blah'}),
    1 );

is( $db_formalize_sig->is_compatible
      ([\'this is title', \'subtitle'], { justify => \'blah'}),
    1 );

is( $db_formalize_sig->is_compatible([\'this is title'], { justify => \'blah'}),
    0);

$db_formalize_sig = Data::Bind->sig
    ({ var => '$title' },
     { var => '$subtitle', optional => 1 },
     { var => '$case', named_only => 1 },
     { var => '$justify', named_only => 1, required => 1});

is($db_formalize_sig->is_compatible([\'this is title'], { justify => \'blah'}),
   1);

is($db_formalize_sig->is_compatible([], { title => \'title by name', justify => \'blah'}),
   1);

is( $db_formalize_sig->is_compatible([\'this is title'], { case => \'blah'}),
    0);
