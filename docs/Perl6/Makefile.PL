use strict;
use lib "../..", "../../inc";
use inc::Module::Install (
    -d '../../misc/pX' ? (prefix => '../../inc') : ()
);
use File::Path;

name       ('Perl6-Doc');
version    (0.01);
license    ('perl');

install_script( 'p6doc' );
makemaker_args( PMLIBDIRS => [ grep { -d } glob("[A-Z]*") ]);

refresh_specs() if -d '../../misc/pX';

WritePugs  (5);

sub refresh_specs {
    # okay, we are at author side - refresh Synopses
    local $@;
    eval { require LWP::Simple } or return;

    use vars '$ua';
    LWP::Simple->import('mirror', '$ua');

    my $BaseURI = 'http://svn.perl.org/perl6/doc/trunk/design/syn';
    my %Spec = qw(
        01 Overview 02 Syntax       03 Operator     04 Block
        05 Rule     06 Subroutine   09 Structure    10 Package
        11 Module   12 Object       13 Overload
    );

    my $request = HTTP::Request->new(HEAD => $BaseURI);
    my $response = $ua->request($request);

    $response->is_success or return;

    my $etags = $response->header('ETag') or return;
    ($etags =~ /(\d+)/) or return;

    local $| = 1;
    print "==> Refreshing specs from $BaseURI...\n";

    my $remote_revision = $1;
    my $local_revision = -1;

    if ( open(FH, '.spec-revision') ) {
        chomp($local_revision = <FH>);
        close FH;
    }

    if ($local_revision == $remote_revision) {
        print "==> No changes.\n";
        return;
    }

    print "==> Synchronizing to revision $remote_revision:\n";

    for my $num (sort keys %Spec) {
        my $file = $Spec{$num};
        print "  $num --> Spec/$file\n";
        LWP::Simple::mirror(
            "$BaseURI/$num.pod" => "Spec/$file.pod"
        );
    }

    open FH, '>', '.spec-revision' or return;
    print FH $remote_revision, "\n";
    close FH;
}
