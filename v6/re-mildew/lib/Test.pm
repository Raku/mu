my $test_count = 0;
role HACK {
    method EXPORTALL($scope) {
        $scope.{'&ok'} := &ok;
        $scope.{'&plan'} := &plan;
        $scope.{'&is'} := &is;
        $scope.{'&isnt'} := &isnt;
        $scope.{'&pass'} := &pass;
        $scope.{'&flunk'} := &flunk;
    }
}
role Test {
}
MY::<Test::> := HACK.new;

multi plan($tests) {
    say "1..$tests";
}
sub proclaim($cond,$desc,$todo) {
    $test_count = $test_count + 1;
    if $cond {
        print "ok $test_count";
    } else {
        print "not ok $test_count";
    }
    if $desc {
        print " - $desc";
    } 
    if $todo {
        print " # TODO";
    }
    print "\n";
}
multi ok($cond,$desc?,:$todo) {
    proclaim($cond,$desc,$todo);
}
multi is($got,$expected,$desc?,:$todo) {
    proclaim($got eq $expected,$desc,$todo);
}
multi isnt($got,$expected,$desc?,:$todo) {
    proclaim($got ne $expected,$desc,$todo);
}
multi pass($desc?,:$todo) {
    proclaim(1,$desc,$todo);
}
multi flunk($desc?,:$todo) {
    proclaim(0,$desc,$todo);
}
