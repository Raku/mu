$OUT.print("1..2\n");
{
  {
     $OUT.print("ok 1\n");
     ::ControlExceptionReturn.new().throw();
  }.();
  $OUT.print("not ok 2\n");
}.();
CONTROL {
  $_.handled = 1;
  $OUT.print("ok 2\n");
}
