say "1..2";
{
   {
       ::Exception.new().throw();
   }
   CATCH {
       say "ok 1 - Caught, but will retrhow.";
       $_.throw();
   }
}
CATCH {
    say "ok 2 - Caught the rethrow";
    $_.handled = 1;
}
