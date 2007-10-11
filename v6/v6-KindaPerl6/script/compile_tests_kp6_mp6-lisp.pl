#!/usr/bin/env perl
use strict;
use warnings;
use File::Temp qw/ tempfile /;

# Create a t-lisp-exe with just the dirs
system "rm -rf t-lisp-exe";
system "cp -R t t-lisp-exe";
system q[find t-lisp-exe/ -type f -name '*.t' -exec rm -f {} \\;];
system q[find t-lisp-exe/ -type d -name '.svn' -exec rm -rf {} \\;];

# Compile the t files to .lisp
open my $cmd, "find t -type f -name '*.t' |" or die $!;

while (my $file = <$cmd>) {
    chomp $file;
    my $rel = $file;
    $rel =~ s[^t/][];

    my (undef, $tmp) = tempfile();
    print "Compiling $file\n";
    system "perl script/kp6 --lisp --no-autoexecute > $tmp < $file";

    my $cmd = qq[sbcl --noinform --disable-debugger --noprint --eval '(declaim (sb-ext:muffle-conditions sb-ext:compiler-note warning style-warning))' --load $tmp --eval '(sb-ext:save-lisp-and-die "t-lisp-exe/$rel.exe" :toplevel (lambda () (kp6-lisp-user::main) 0) :executable t)' --no-linedit];
    system $cmd;

    my $prog = "t-lisp-exe/$rel.exe";
    next unless -f $prog;

    open my $fh, ">t-lisp-exe/$rel" or die $!;

    my $out = qx/$prog 2>&1/;
    if ($? != 0) {
        my @out = split /^/, $out;
        my @want = @out[1..$#out]; # Chop out first line
        my $print = join " ", map { chomp; $_ } @want;
        $print =~ s/\s+/ /g;

        print $fh 'print qq[1..1\n];' . "\n";
        print $fh 'print qq[not ok # ' . quotemeta($print) . "\\n];\n";
    } else {
        print $fh 'system "./$0.exe"' . "\n";
    }
}




