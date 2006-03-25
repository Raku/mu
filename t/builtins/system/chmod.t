#!/usr/bin/pugs

use v6;
use Test;

=kwid

chmod - the unix chmod command, changing the rights on a file

Proposed behaviour
LIST = chmod MODE, LIST
Given a list of files and directories change the rights on them.
MODE should be an octet representing or a string like similar to what can be used in
     the same UNIX program:
     one or more of the letters ugoa, one of the symbols +-= and one or more of the letters rwxXstugo.
     
return list should be the list of files that were successfully changed
in scalar context it should be the number of files successfully changed

While some of the modes are UNIX specific, it would be nice to find similar
  modes in other operating system and do the right thing there too.


We really need the stat() function in order to test this.

=cut

plan 18;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

if($*OS eq any<MSWin32 mingw msys cygwin>) {
    skip 18, "file tests not fully available on win32";
    exit;
};


{
    my $file = create_temporary_file;
    my @result = chmod 0, $file;
    is +@result, 1, "One file successfully changed";
    is @result[0], $file, "name of the file returned", :todo;
    ok !-r $file, "not readable after 0";
    ok !-w $file, "not writabel after 0";
    ok !-x $file, "not executable after 0";
    remove_file($file);
}


{
    my $file = create_temporary_file;
    my @result = chmod 700, $file;
    is +@result, 1, "One file successfully changed";
    is @result[0], $file, "name of the file returned", :todo;

    ok -r $file, "readable after 700";
    ok -w $file, "writabel after 700";
    ok -x $file, "executable after 700";
    remove_file($file);
}


{
    my $file = create_temporary_file;
    my @result = chmod 777, $file;
    is +@result, 1, "One file successfully changed";
    is @result[0], $file, "name of the file returned", :todo;

    ok -r $file, "readable after 777";
    ok -w $file, "writabel after 777";
    ok -x $file, "executable after 777";
    remove_file($file);
}

sub create_temporary_file {
    my $time = time;
    my $file = "temp_$time";
    my $fh = open $file, :w err die "Could not create $file";
    diag "Using file $file";
    return $file;
}
sub remove_file ($file) {
    unlink $file;
    ok(! -e $file, "Test file was successfully removed");
}


