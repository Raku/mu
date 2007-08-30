use v6-alpha;

module Main {

    say '1..4';

    # ----- mkdir / rmdir
    my $dirname = "/tmp/really_strange_temp_file_name_916532752";
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

}
