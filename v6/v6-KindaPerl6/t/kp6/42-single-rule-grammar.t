grammar MyGrammar {
    token mytok {
        'test'
    };
}
module Main {
  say '1..2';

  my $a = 'test';

  # This is probably a bad idea, but as it works that way, here it is.
  if MyGrammar.mytok($a) {
      say 'ok 1';
  } else {
      say 'not ok 1';
  };

  $_ := $a;
  if MyGrammar.mytok() {
      say 'ok 2';
  } else {
      say 'not ok 2';
  };
}
