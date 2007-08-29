grammar MyGrammar {
    token mytok {
        'test'
    };
}
module Main {
  say '1..1';

  my $a = 'test';

  $_ := $a;
  if MyGrammar.mytok() {
      say 'ok 1';
  } else {
      say 'not ok 1';
  };
}
