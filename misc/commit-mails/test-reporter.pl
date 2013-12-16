#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

use MIME::Lite;
use Pod::Usage;
use Getopt::Long;

my %OPT = (
    smtp => 'localhost',
);

Getopt::Long::Configure( 'bundling' );
GetOptions(
    'dir|d=s'     => \$OPT{dir},
    'from|f=s'    => \$OPT{from},
    'smtp=s'      => \$OPT{smtp},

    'help|h'      => sub { pod2usage(1) },
    'man'         => sub { pod2usage( -exitstatus => 0, -verbose => 2 ) },
) or pod2usage(2);

if ( ! $OPT{from} ) {
    warn "specify a 'from' address with -f";
    pod2usage(2);
}

chdir $OPT{dir} or die "Can't chdir to (-d) '$OPT{dir}': $!";

if ( 0 != system qq( git pull --quiet ) ) {
    die "Can't update";
}

my $prev = 'fdd053c8b9eeaf133580c6ed461e50efc6c2565a';
if (-e 'previous_mail_hash') {
    $prev = `cat previous_mail_hash`;
    chomp $prev;
}

open my $h, '-|', 'git', qw(log --reverse --pretty=format:%H:%an:%s),
    "$prev..HEAD"
    or die "Error while launching git log: $!";

my $msg_rx = qr{
    test s?
    \s+
    (?: for \s+ )?
    \[?
        (?: bug | rt | perl )
        \s* [#]? (?<bugno> \d\d+ )
    \]?
}xmsi;

while (<$h>) {
    chomp;
    my ($hash, $author, $msg) = split /:/;

    next unless $msg =~ $msg_rx;

    my $bugno = $+{bugno};
    $prev = $hash;
    system("echo $prev > previous_mail_hash");

    my @files = filelist_for_hash($hash);
    next unless @files;

    my $files = @files == 1
        ? "$files[0]"
        : "at least one of these files: " . join(q{, }, @files);

    my $diff = qx/git show $hash/;
#    say $msg;
    my $mail = MIME::Lite->new(
            From    => $OPT{from},
            To      => 'perl6-bugs-followup@perl.org',
            Subject => "[perl #$bugno] tests available",
            Type    => 'TEXT',
            Data    => "This is an automatically generated mail to "
                       . "inform you that tests are now available in "
                       . $files
                       . "\n\n"
                       . $diff,
    );
    $mail->send('smtp', $OPT{smtp});
    sleep 1;
}

sub filelist_for_hash {
    my $hash = shift;
    my @res = qx/git show $hash/;
    chomp @res;
    return
        map { s{^b/}{}; $_ }
        map { (split ' ', $_, 2)[1] }
        grep { /^\+\+\+ b/ } @res;
}
__END__

=head1 NAME

test-reporter.pl -- report tests of RT tickets via email

=head1 SYNOPSIS

test-reporter.pl -f '<emailaddr>' -d <dir>

 Options:
   -h, --help              brief help message
       --man               full documentation
   -d, --dir               where to find a git checkout of roast
   -f, --from              email address for the from line
       --smtp              SMTP server to use to send mail

=head1 DESCRIPTION

This is meant to be run as a cron job.  It assumes there's a checkout of
the L<roast repository|https://github.com/perl6/roast>.
Specify the location of this repo
with the B<-d> option.  It will go there, update the repo, and look through
recent commits for commit messages of a form that indicates a test was
written for a bug report.  If it finds a commit like that, it will generate
an email to rt.perl.org to note the existence of the test in the ticket.

=head1 USAGE

Use C<crontab -e> to edit your crontab on a Unix-ish system.  A line like
this will run the command every five minutes:

    */5 * * * * perl /home/example/pugs/misc/commit-mails/test-reporter.pl -f 'postmaster@example.com' -d /home/example/pugs

=head1 OPTIONS

=over 8

=item B<--help>

=item B<-h>

Print a brief help message and exit.

=item B<--man>

Print the full documentation and exit.

=item B<--dir>

=item B<-d>

This specifies the directory in which to find a checkout of the
L<roast repository|https://github.com/perl6/roast>.

=item B<--from>

=item B<-f>

Specifies the email address to use on outgoing emails.

=item B<--smtp>

Default: localhost

SMTP server to use to send email.

=back

=head1 AUTHOR

Moritz Lenz

=cut
