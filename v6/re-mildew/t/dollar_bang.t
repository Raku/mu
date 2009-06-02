say "1..2";
{
    my $dollar_bang = ::DollarBang.new;
    $dollar_bang.failures = ::Array.new;
    my $f1 = fail 'foo';
    $dollar_bang.failures.push($f1.FETCH);
    $dollar_bang.cleanup;
    CATCH {
        say "ok 1 # unhandled failure got thrown";
    }
}.();
{
    my $dollar_bang = ::DollarBang.new;
    $dollar_bang.failures = ::Array.new;
    my $f1 = fail 'foo';
    $f1.handled = 1;
    $dollar_bang.failures.push($f1.FETCH);
    $dollar_bang.cleanup;
    CATCH {
        say "not ok 2 # handled failure got thrown";
    }
    say "ok 2 # handled failure did not got thrown";
}.();
