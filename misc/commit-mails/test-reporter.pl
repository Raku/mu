use strict;
use warnings;
use 5.010;
use MIME::Lite;

chdir '/home/kyle/src/gitpugs' or die "Can't chdir: $!";

if ( 0 != system qq( git svn rebase --quiet 2> /dev/null > /dev/null ) ) {
    die "Can't update";
}

my $prev = 'ec0d6d9b012dd3b082394e57669507313d039baf';
#my $prev = '8b643d6119fd9e4a460e6ec12711fa347207a0ec';
if (-e 'previous_mail_hash') {
    $prev = `cat previous_mail_hash`;
    chomp $prev;
}

open my $h, '-|', 'git', qw(log --reverse --pretty=format:%H:%an:%s),
    "$prev..HEAD"
    or die "Error while launching git log: $!";
while (<$h>) {
    chomp;
    my ($hash, $author, $msg) = split /:/;
    if ($msg =~ m/tests?\s+(?:for\s+)?\[?(?:bug|rt|perl)\s*#?(\d+)\]?/i) {
        my $bugno = $1;
        $prev = $hash;
        system("echo $prev > previous_mail_hash");
        my @files = filelist_for_hash($hash);
        next unless @files;
        my $mail_body = @files == 1
            ? "$files[0]"
            : "at least one of these files: " . join(', ', @files);
#        say $msg;
        my $mail = MIME::Lite->new(
                From => 'kyleha@gmail.com',
                To => 'perl6-bugs-followup@perl.org',
                Subject => "[perl #$bugno] tests available",
                Type => 'TEXT',
                Data => "This is an automatically generated mail to "
                         . "inform you that tests are now available in "
                         . $mail_body,
        );
        $mail->send('smtp', 'localhost');
        sleep 1;
    }
}
 
sub filelist_for_hash {
    my $hash = shift;
    my @res = qx/git show $hash/;
    chomp @res;
    return
#        map { s{^b/}{t/}; $_ }
        map { (split ' ', $_, 2)[1] }
        grep { /^\+\+\+ b/ } @res;
}
