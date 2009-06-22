my $test_count = 0;
multi plan($tests) {
    say "1..$tests";
}
sub proclaim($cond,$desc) {
    $test_count = $test_count + 1;
    if $cond {
        print "ok $test_count";
    } else {
        print "not ok $test_count";
    }
    if $desc {
        say " - $desc";
    }
}
multi ok($cond,$desc) {
    proclaim($cond,$desc);
}
multi ok($cond) {
    proclaim($cond,"");
}
$LexicalPrelude.{'&ok'} := &ok;
$LexicalPrelude.{'&plan'} := &plan;
