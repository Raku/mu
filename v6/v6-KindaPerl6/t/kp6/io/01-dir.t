use v6-alpha;

module Main {

    say '1..8';

    # ----- mkdir / rmdir
    my $dirname = "kp6_really_strange_temp_file_name_" ~ Math.rand;
    # delete just for the following tests start with a defined state
    IO.rmdir($dirname);

    # create a dir
    my $ret = IO.mkdir($dirname);
    if ($ret == 0) {
        print "not ";
    };
    say "ok 1";

    # second create should fail
    my $ret = IO.mkdir($dirname);
    if ($ret != 0) {
        print "not ";
    };
    say "ok 2";

    # delete it
    my $ret = IO.rmdir($dirname);
    if ($ret == 0) {
        print "not ";
    };
    say "ok 3";

    # second remove should fail
    my $ret = IO.rmdir($dirname);
    if ($ret != 0) {
        print "not ";
    };
    say "ok 4";


    # now the same with global calls

    # ----- mkdir / rmdir
    my $dirname = "kp6_really_strange_temp_file_name_" ~ Math.rand;
    # delete just for the following tests start with a defined state
    rmdir($dirname);

    # create a dir
    my $ret = mkdir($dirname);
    if ($ret == 0) {
        print "not ";
    };
    say "ok 5";

    sleep 2;

    # second create should fail
    my $ret = mkdir($dirname);
    if ($ret != 0) {
        print "not ";
    };
    say "ok 6";

    # delete it
    my $ret = rmdir($dirname);
    if ($ret == 0) {
        print "not ";
    };
    say "ok 7";

    # second remove should fail
    my $ret = rmdir($dirname);
    if ($ret != 0) {
        print "not ";
    };
    say "ok 8";

}
