say "1..1";
{
   {
       my $failure = fail;
       $failure.foo;
   }.();
   CATCH {
       say "ok 1 - calling a method on a failure throws the exception";
   }
}.();
