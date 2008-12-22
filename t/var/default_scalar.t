use v6;
use Test;

# This has some tests for $_, but $_ is under-spec'ed now.


plan 2;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

# L<S16/"Filehandles, files, and directories"/"=item open">
# L<S16/"Input and Output"/"=item say">

# work around missing capabilities
# to get the output of 'say' into a test; 
    my $out = open("tmpfile", :w);
    $out.say(3);
    close $out; 
    my$in = open "tmpfile"; 
    my $s = =$in; close $in; 
    unlink "tmpfile";

    is $s,"3", 'and is the default argument for "say"';

#pugs> for .. { say }; 

    my $out = open("tmpfile", :w);
    for 1 { $out.say() };
    close $out; 
    my$in = open "tmpfile"; 
    my $s = =$in; close $in;
    unlink "tmpfile";

    isnt $s,"3", 'and global $_ should not be the default topic of "for"'; 
# #*** Error: cannot modify constant item at 1

