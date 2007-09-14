#!/usr/bin/perl

#To have a fall-back for pugs, we need to revert to a working devel version.
#So I must build a devel version every day, do a sanity check and store the
#revision 
#pugs -V:pugs_revision
#The simple solution is:
#-Try to build once a day
#test /usr/bin/pugs, if it's sane, build. else skip
my $ok=0;
my $rev=`pugs -V:pugs_revision`;
print "Current revision: $rev\n";
$rev=~s/^.*:\s+//;
chomp $rev;
my $reply=`PUGS_SAFEMODE=1 /usr/bin/pugs -e \"print 42\" 2>/dev/null`;
print "Test reply: $reply\n";
#die;
if ($reply==42) {
$ok=1;
} else {
print "PUGS_SAFEMODE=1 /usr/bin/pugs FAILED, using previous version.\n";
}
#chdir("pugs");
if ($ok==1) {
print "PUGS_SAFEMODE=1 /usr/bin/pugs OK, svn up -r $rev\n";
system("svn up -r $rev");

print "\nBuilding Pugs...\n\n";
system("touch ../pugs-lock");
system("make clean");
system("perl Makefile.PL");
system("make soon");
#if (-e "./pugs") {
#system("rsync -ua blib6 ../../pugs-dev/");
#system("rsync -ua pugs ../../pugs-dev/");
#}
}
unlink "../pugs-lock";
