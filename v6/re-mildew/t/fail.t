say "1..1";
{
   my sub foo {
     fail;
     say "not ok 1 - fail should return from the subroutine";
   }
   {
       my $failure = foo;
       $failure.foo;
   }
   CATCH {
       say "ok 1 - calling a method on a failure throws the exception";
   }
}
