#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# This program is still under active development, and
# you're very welcome to improve it.
# Most of the code has already moved to the Smart-Links subdirectory
# with the aim to make it generic enought to be usefule for any
# perl script, module or application.

# Please read the Pod documentation at the end of file before reading and/or
# modifying the source.
# CAUTION: please make sure your changes don't break anything, because
# breakage of this script will also break http://perlcabal.org/syn/
# immediately. Running the tests of Smart::Links and trying the script using 
# perl smartlinks.pl --dir ../../t/ --out-dir ../../html/
# and then checking the result before committing is
# strongly recommended. Thank you for your contribution :)

use strict;
use warnings;
no warnings 'once';

use Getopt::Long;
use File::Basename;
use FindBin;
use File::Find::Rule;
use File::Slurp;
use Data::Dumper;

#use Pod::Simple::HTML;

use lib "$FindBin::Bin/Smart-Links/lib";
use Smart::Links;

sub help () {
    print <<_EOC_;
Usage:
  $0 t/*/*.t t/*/*/*.t
  $0 --dir t
  $0 --css foo.css --out-dir=public_html t/syntax/*.t
  $0 --check t/*/*.t t/*/*/*.t
  $0 --check t/some/test.t
  $0 --check --missing t/*/*.t t/*/*/*.t

Options:
  --help          Show this help.
  --check         Only check the validity of the smartlinks, no
                  HTML outputs.
  --missing       Print files whitout smartlinks
  --wiki          Print out a wiki link to test files missing links
  --count         Show the count of links per file 
  --out-dir <dir> Specify the output directory for HTML files.
  --css <file>    Specify the CSS file used by the HTML outputs,
                  defaults to http://dev.perl.org/css/perl.css.
  --test-res <ymlfile>
                  Set .yml file generated from Test::TAP::Model's
                  ``structure''. Usually <ymlfile> should be set
                  to ``smoke.yml''.
  --syn-dir       Specify the directory where the Synopses live,
                  defaults to pugs' docs/Perl6/Spec. Please don't
                  set syn-dir to elsewhere unless you have a good
                  reason.
  --index         Also generates an index.html page with links to
                  pages.
  --dir <dir>     Name of the directory where to look for .t files
                  recursively.
  --line-anchor   Insert line anchors to the resulting HTML pages.
_EOC_
    exit(0);
}

sub main {
    my ($syn_dir, $out_dir, $help, $cssfile, $yml_file, $index, $dir,$count,$wiki);
    my $check;
    my $line_anchor;
    my $print_missing;
    GetOptions(
        'check'       => \$check,
        'count'       => \$count,
        'missing'     => \$print_missing,
        'wiki'        => \$wiki,
        'syn-dir=s'   => \$syn_dir,
        'out-dir=s'   => \$out_dir,
        'css=s'       => \$cssfile,
        'help'        => \$help,
        'test-res=s'  => \$yml_file,
        'index'       => \$index,
        'dir=s'       => \$dir,
        'line-anchor' => \$line_anchor,
    ) or help();

    if ($help || !@ARGV && !$dir) {
        help();
    }
    $cssfile ||= 'http://dev.perl.org/css/perl.css';

    my $sl = Smart::Links->new({
        count         => $count,
        check         => $check,
        line_anchor   => $line_anchor,
        print_missing => $print_missing,
        wiki          => $wiki,  # #TODO: do we need this flag?
        out_dir       => $out_dir,
        cssfile       => $cssfile,
        version       => get_pugs_rev(),
    });


    $out_dir = $sl->out_dir;
    mkdir $out_dir if !-d $out_dir;

    my @t_files = map glob, @ARGV;
    push @t_files, File::Find::Rule->file()->name('*.t')->in($dir) if $dir;
    $sl->process_test_files(@t_files);
    $sl->process_yml_file($yml_file);

    $syn_dir ||= "$FindBin::Bin/../docs/Perl6/Spec";
    my @syns = File::Find::Rule->file()->name('*.pod', '*.pm')->relative->in($syn_dir);
    $sl->{docs} = \@syns;
    for my $syn (@syns) {
        $sl->process_syn($syn_dir, $syn, 1);
    }

    $sl->report_broken_links;
    $sl->report_stats;
    $sl->create_stats_page();
    $sl->create_index() if $index;

    exit;
}


sub get_pugs_rev {
    my $stdout = `$^X $FindBin::Bin/version_h.pl`;
    my ($pugs_rev) = ($stdout =~ /Current version is (\d+)/);
    if (!$pugs_rev) {
        # if we don't have access to others' svk info
        # (which is the case on feather where i'm using
        # Audrey's pugs working copy), then parse pugs_version.h
        # directly:
        if (open my $in, "$FindBin::Bin/../src/Pugs/pugs_version.h") {
            warn "reading pugs_version.h...\n";
            local $/;
            my $str = <$in>;
            ($pugs_rev) = ($str =~ /PUGS_SVN_REVISION\s+(\d+)/);
        }
    }
    return $pugs_rev;
}



main() if ! caller;

1;
__END__

=head1 NAME

smartlinks.pl - The successor to catalog_tests.pl.

See L<Smart::Links>

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

=head1 COPYRIGHT

Copyright (c) 2006 - 2008 by the Pugs Team.

