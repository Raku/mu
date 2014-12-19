#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# This program is still under active development, and
# you're very welcome to improve it.
# Most of the code has already moved to the Text-SmartLinks subdirectory
# with the aim to make it generic enought to be usefule for any
# perl script, module or application.

# Please read the Pod documentation of Text::SmartLinks before reading and/or
# modifying the source.
#
# CAUTION: please make sure your changes don't break anything, because
# breakage of this script will also break http://design.perl6.org/
# immediately. Running the tests of Text::SmartLinks and trying the script
# using (from the current directory):
#
#   perl -I ../lib smartlinks.pl \
#       --dir ../../t \
#       --out-dir ../../../docs/feather/ \
#       --pod-dir ../../../docs/Perl6/Spec
#
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

use Text::SmartLinks;

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
  --pod-dir       Specify the directory where the .pod and .pm files 
                  are located. smarlinks.pl will recurse in the 
                  subdirectories to locate the files. Defaults to lib/
  --index         Also generates an index.html page with links to
                  pages.
  --dir <dir>     Name of the directory where to look for .t files
                  recursively.
  --line-anchor   Insert line anchors to the resulting HTML pages.
  --version <VERSION>
_EOC_
    exit(0);
}

sub main {
    my ($pod_dir, $out_dir, $help, $cssfile, $yml_file, $index, $dir,$count,$wiki);
    my $check;
    my $line_anchor;
    my $print_missing;
    my $version;
    GetOptions(
        'check'       => \$check,
        'count'       => \$count,
        'missing'     => \$print_missing,
        'wiki'        => \$wiki,
        'pod-dir=s'   => \$pod_dir,
        'out-dir=s'   => \$out_dir,
        'css=s'       => \$cssfile,
        'help'        => \$help,
        'test-res=s'  => \$yml_file,
        'index'       => \$index,
        'dir=s'       => \$dir,
        'line-anchor' => \$line_anchor,
        'version=s'   => \$version,
    ) or help();

    if ($help || !@ARGV && !$dir) {
        help();
    }
    $cssfile ||= 'http://dev.perl.org/css/perl.css';

    my $sl = Text::SmartLinks->new({
        count         => $count,
        check         => $check,
        line_anchor   => $line_anchor,
        print_missing => $print_missing,
        wiki          => $wiki,  # #TODO: do we need this flag?
        out_dir       => $out_dir,
        cssfile       => $cssfile,
        version       => $version,
    });

    $out_dir = $sl->out_dir;
    mkdir $out_dir if !-d $out_dir;

    my @t_files = map glob, @ARGV;
    push @t_files, File::Find::Rule->file()->name('*.t')->in($dir) if $dir;
    $sl->process_test_files(@t_files);
    $sl->process_yml_file($yml_file);

	die "--pod-dir was not given. It usually should be '.' or lib/\n" if not defined $pod_dir;
	die "Directory '$pod_dir' does not exist\n" if not -e $pod_dir;
    my @pod_files = sort File::Find::Rule->file()->name('*.pod', '*.pm')->relative->in($pod_dir);
    $sl->{docs} = \@pod_files;
    for my $pod_file (@pod_files) {
        my $success = eval {
            $sl->process_pod_file($pod_dir, $pod_file, 1);
            1;
        };
        unless ($success) {
            warn "Error while parsing '$pod_file': $@\n";
        }
    }

    $sl->report_broken_links;
    $sl->report_stats;
    $sl->create_stats_page();
    $sl->create_index() if $index;
	$sl->create_x_page('X');
	$sl->create_x_page('C');

    exit;
}


main();

1;

__END__

=head1 NAME

smartlinks.pl - The successor to catalog_tests.pl.

See L<Text::SmartLinks>

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

=head1 COPYRIGHT

Copyright (c) 2006 - 2010 by the Pugs Team.

