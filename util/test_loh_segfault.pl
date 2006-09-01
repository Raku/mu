use v6-alpha;

=for comment

This will document whether a literal LoH's segfault is occurring
and report useful config information for debugging why.

=cut

my $email_addr is constant = 'trey@lopsa.org';
my %attrs;

print '
Please run pugs interactively as you normally would at this shell.
Enter the following at the pugs prompt:
  ({a => 1}, {b => 2})
Observe whether pugs segfaults.
Then exit pugs if necessary and the shell.

';
system $+SHELL;

print "\nDid pugs segfault? (y/n) ";
%attrs<success> = =$*IN;

print '
Thank you.  If you have any comments to add (such as weird behavior
besides segfaulting when you tried to interpret the list of hashes),
please do so below.  End your comments with a single . on a line by
itself.

';

loop {
    $_ = =$*IN;
    last if $_ eq ".";
    %attrs<comments> ~= "$_\n";
}

say "
Thank you.  Please email the following to $email_addr.
Your hostname is obscured, but feel free to obscure anything else.";
say "=" x 70;

%attrs<case> = time;
%attrs<ghc_version> = chomp `ghc --version`;
%attrs<pugs_revision> = %?CONFIG<pugs_revision>;
%attrs<uname> = chomp `uname -a`;

# Obscure hostname for privacy
%attrs<uname> ~~ s/<after \s+> (\S+)/HOSTOBSCURED/;

say %attrs.yaml;
