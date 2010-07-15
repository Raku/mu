my $test_count = 0;
role Test {
}
role HACK {
    method EXPORTALL($scope) {
        $scope{'&ok'} := &ok;
        $scope{'&plan'} := &plan;
        $scope{'&is'} := &is;
        $scope{'&isnt'} := &isnt;
        $scope{'&pass'} := &pass;
        $scope{'&flunk'} := &flunk;
        $scope{'&cmp_ok'} := &cmp_ok;
        $scope{'&done_testing'} := &done_testing;
        $scope{'&lives_ok'} := &lives_ok;    
        $scope{'&skip'} := &skip;    
    }
}
MY::<Test::> := HACK.new;

multi plan($tests) is export {
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
multi ok($cond,$desc?,:$todo) is export {
    proclaim($cond,$desc,$todo);
}
multi is($got,$expected,$desc?,:$todo) is export {
    proclaim($got eq $expected,$desc,$todo);
}
multi isnt($got,$expected,$desc?,:$todo) is export {
    proclaim($got ne $expected,$desc,$todo);
}
multi pass($desc?,:$todo) is export {
    proclaim(1,$desc,$todo);
}
multi flunk($desc?,:$todo) is export {
    proclaim(0,$desc,$todo);
}
multi cmp_ok($got,$op,$expected,$desc?,:$todo) is export {
    proclaim($op($got,$expected),$desc,$todo);
}
multi done_testing() is export {
    say "1..$test_count";
}
multi lives_ok($code,$desc?,:$todo) is export {
    {
        $code();
        proclaim(1,$desc,$todo);
        CATCH {
            proclaim(0,$desc,$todo);
        }
    }
}
multi skip($count,$reason?) is export {
    my $i = 0;
    loop {
        if $i == $count {
            return;
        } else {
            $test_count = $test_count + 1;
            print "ok $test_count - # SKIP";
            if $reason {
                print " $reason";
            }
            print "\n";
            $i = $i + 1;
        }
    }

}
