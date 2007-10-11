module Test {
    sub ok($cond) {
        if ($cond) {
            say "ok";
        } else {
            say "not ok";
        }
    }
    sub plan($number_of_test) {
        say "1.."~$number_of_test;
    }
}
