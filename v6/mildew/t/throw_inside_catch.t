$OUT.print("1..2\n");
{
   {
       ::OutOfItemsException.new().throw();
   }.();
   CATCH {
       $OUT.print("ok 1 - Caught, but will retrhow.\n");
       $_.throw();
   }
}.();
CATCH {
    $OUT.print("ok 2 - Caught the rethrow\n");
}