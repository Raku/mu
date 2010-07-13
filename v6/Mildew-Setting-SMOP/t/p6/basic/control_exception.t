say "1..2";
{
  {
     say "ok 1";
     ::ControlExceptionReturn.new().throw();
  }
  say "not ok 2";
}
CONTROL {
  $_.handled = 1;
  say "ok 2";
}
