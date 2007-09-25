#!/usr/bin/env perl
use strict;
use warnings;
use File::Temp qw/ tempfile /;

# Create a t-lisp-exe with just the dirs
system "cp -Rvp t t-lisp-exe";
system "find t-lisp-exe -type f -exec rm -v {} \\;";

# Compile the t files to .lisp
open my $cmd, "find t -type f |" or die $!;

while (my $file = <$cmd>) {
    chomp $file;
    my $rel = $file;
    $rel =~ s[^t/][];

    my (undef, $tmp) = tempfile();
    print "Compiling $file\n";
    system "perl kp6-mp6-perl5.pl --lisp > $tmp < $file";

    my $cmd = qq[sbcl --disable-debugger --load $tmp --eval '(sb-ext:save-lisp-and-die "t-lisp-exe/$rel.exe" :toplevel (lambda () (Main::Main) 0) :executable t)'];
    system $cmd;

    next unless -f "t-lisp-exe/$rel.exe";

    open my $fh, ">t-lisp-exe/$rel" or die $!;
    print $fh 'system "./$0.exe"' . "\n";
    
}




