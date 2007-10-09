
say '1..4';

my $s = Gather.new( sub { take 42 } );

say 'ok 1 - load Gather.pm';

if ( $s[0] == 42 ) {
  say 'ok 2 - take works';
}
else {
  say 'not ok 2';
};


$s = Gather.new( sub { 
    my $i = 0;
    while 1 { take $i; $i = $i + 1 };
  } );

if ( $s[5] == 5 ) {
  say 'ok 3 - lazy take works';
}
else {
  say 'not ok 3';
};

$s = gather { take 42 };
if ( $s[0] == 42 ) {
  say 'ok 4 - gather syntax works';
}
else {
  say 'not ok 4';
};

