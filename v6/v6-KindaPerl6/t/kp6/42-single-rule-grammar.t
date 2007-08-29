
  say '1..1';

  my $a = 'test';

  token mytok {
    'test'
  };

  $_ := $a;
  if mytok() {
      say 'ok 1';
  } else {
      say 'not ok 1';
  };

