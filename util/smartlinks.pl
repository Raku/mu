#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# This program is still under active development, and
# you're very welcome to improve it.
# Please read the Pod documentation at the end of file before reading and/or
# modifying the source.
# CAUTION: please make sure your changes don't break anything, because
# breakage of this script will also break http://perlcabal.org/syn/
# immediately. Running *-smartlinks.t under util/t/ before committing is
# strongly recommended. Thank you for your contribution :)


# TODO: currently the S32 documents are unlinkable, and don't produce HTML copies

use strict;
use warnings;
no warnings 'once';

#use Smart::Comments;
#use YAML::Syck;
use Getopt::Long;
use File::Basename;
use FindBin;
use File::Find::Rule;
use File::Slurp;

#use Pod::Simple::HTML;

use lib "$FindBin::Bin/Smart-Links/lib";
use Smart::Links;
my $sl;

my %Spec = reverse qw(
    01 Overview    02 Syntax        03 Operator      04 Block
    05 Rule        06 Subroutine    07 Iterator
    09 Structure   10 Package
    11 Module      12 Object        13 Overload      16 IO
    17 Concurrency 19 Commandline   22 CPAN          26 Documentation
    29 Functions
);


=begin private

=head2 process_syn

  process_syn($syn);

Process synopses one by$sl->link_count  one.

=end private

=cut

sub process_syn {
    my ($infile) = @_;

    my $syn_id;
    if ($infile =~ /\bS(\d+)(?:-\w+)+.pod$/) {
        $syn_id = $1;
    } else {
        die "Can't match file '$infile'\n";
    }

    # S26 is in Pod6, we treat it specifically for now.
    if ($syn_id == 26) {
      return if $sl->check;
      eval "use Perl6::Perldoc 0.000005; use Perl6::Perldoc::Parser; use Perl6::Perldoc::To::Xhtml;";
      if ($@) {
          warn "Please install Perl6::Perldoc v0.0.5 from the CPAN to parse S26";
          return;
      }

      my $toc = "=TOC\nP<toc:head1 head2 head3>\n\n";
      my $pod6 = $toc . read_file($infile);

      my $perldochtml = Perl6::Perldoc::Parser->parse(
          \$pod6, {all_pod => 1}
      )->report_errors()->to_xhtml(
          {full_doc => {title => 'S26'}}
      );
      $perldochtml =~ s{</head>}{<link rel="stylesheet" type="text/css" title="pod_stylesheet" href="http://dev.perl.org/css/perl.css">\n$&};
      my $preamble = $sl->gen_preamble();
      $perldochtml =~ s{<body>}{$&$preamble};
      $sl->add_footer(\$perldochtml);

	  my $out_dir = $sl->out_dir;
      my $htmfile = "$out_dir/S$syn_id.html";
      warn "info: generating $htmfile...\n";
      open my $out, "> $htmfile" or
          die "Can't open $htmfile for writing: $!\n";
      print $out $perldochtml;
      close $out;
      return;
    }
    my $podtree = $sl->parse_pod($infile);
    #print Dump $podtree if $syn_id eq '29';

    #use Data::Dumper;
    #$Data::Dumper::Indent = 1;
    #print Dumper $linktree if $syn_id eq '02';

    my $linktree_sections = $sl->{linktree}->{"S$syn_id"};
#    if (!$linktree_sections && $syn_id != 7) {
#        # We won't generate the HTML file if there's no smartlink in it.
#        return;
#    }
    while (my ($section_name, $links) = each %$linktree_sections) {
        #warn "checking $section...";
        my @links = @$links;
        my $paras = $podtree->{$section_name};
        if (!$paras) {
            my $link = $links[0];
            my ($t_file, $from) = @{ $link->[1] };
            $from--;
            $sl->error("$t_file: line $from:", "section ``$section_name'' not found in S$syn_id.");
            $sl->broken_link_count_inc;
            next;
        }
        for my $link (reverse @links) {
            my ($pattern, $location) = @$link;
            my $i = 0;
            if (!$pattern) { # match the whole section
                if (!$sl->check) {
                    unshift @$paras, $sl->gen_code_snippet($location);
                    $i = 1;
                }
                next;
            }
            my $regex = $sl->parse_pattern($pattern);
            my $matched;
            while ($i < @$paras) {
                my $para = $paras->[$i];
                next if !$para or $para =~ /\?hide_quotes=no/;
                if ($sl->process_paragraph($para) =~ /$regex/) {
                    if (!$sl->check) {
                        splice @$paras, $i+1, 0, $sl->gen_code_snippet($location);
                        $i++;
                    }
                    $matched = 1;
                    last;
                }
            } continue { $i++ }
            if (!$matched) {
                my ($file, $lineno) = @$location;
                $sl->error("$file: line $lineno: pattern '$pattern' failed to match any paragraph in L<S${syn_id}/${section_name}>.");
                $sl->broken_link_count_inc;
            }
        }
    }

    # We need this to check invalid smartlinks pointed to non-existent docs:
    delete $sl->{linktree}->{"S$syn_id"};

    if (!$sl->check) {
        #use Data::Dumper;
        #$Data::Dumper::Indent = 1;
        #print Dumper $podtree if $syn_id eq '02';

        my $pod = $sl->emit_pod($podtree);

        #print $pod if $syn_id eq '02';
        #if ($syn_id eq '29') {
        #    use File::Slurp;
        #    write_file("db_S$syn_id.pod", $pod);
        #}

         my $html = $sl->gen_html($pod, $syn_id);

        #write_file("db_S$syn_id.html", $html);

        my $preamble = $sl->gen_preamble();
        $html =~ s{<!-- start doc -->}{$&$preamble};
        my $out_dir = $sl->out_dir;
        my $htmfile = "$out_dir/S$syn_id.html";
        warn "info: generating $htmfile...\n";
        open my $out, "> $htmfile" or
            die "Can't open $htmfile for writing: $!\n";
        print $out $html;
        close $out;
    }

    #warn "$syn_id: $infile\n";
}


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

sub main () {
    my ($syn_dir, $out_dir, $help, $cssfile, $fast, $yml_file, $index, $dir,$count,$wiki);
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
        'fast'        => \$fast,
        'test-res=s'  => \$yml_file,
        'index'       => \$index,
        'dir=s'       => \$dir,
        'line-anchor' => \$line_anchor,
    ) or help();

    if ($help || !@ARGV && !$dir) {
        help();
    }
    $cssfile ||= 'http://dev.perl.org/css/perl.css';

	$sl = Smart::Links->new({
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
    create_index($out_dir) if $index;

    my @t_files = map glob, @ARGV;
    push @t_files, File::Find::Rule->file()->name('*.t')->in($dir) if $dir;
    $sl->process_test_files(@t_files);


    my $pugs_syn_dir = "$FindBin::Bin/../docs/Perl6/Spec";
    $syn_dir ||= $pugs_syn_dir;

	$sl->process_yml_file($yml_file);

    my @syns = map glob, "$syn_dir/*.pod";
    for my $syn (@syns) {
        process_syn($syn);
    }

    # check for pending smartlinks:
    while (my ($syn, $linktree_sections) = each %{ $sl->{linktree} }) {
        for my $links (values %$linktree_sections) {
            for my $link (@$links) {
                my ($file, $lineno) = @{ $link->[1] };
                $sl->error("$file: line $lineno: smartlink pointing to an unknown synopsis ($syn)"),
                $sl->broken_link_count_inc;
            }
        }
    }

	my $test_file_count = scalar @t_files;
	my $test_files_missing_links = scalar $sl->test_files_missing_links;
    warn sprintf("info: %s smartlinks found and %s broken in $test_file_count test files ($test_files_missing_links test files had no links).\n",
		$sl->link_count ,$sl->broken_link_count);
    if (!$sl->check and $sl->broken_link_count > 0) {
        warn "hint: use the --check option for details on broken smartlinks.\n";
    }
    $sl->create_stats_page();
    exit;
}

sub create_index {
    my ($out_dir) = @_;

    my $html = "<html><head><title>Synopsis</title></head><body>\n";
    foreach my $syn (sort { $Spec{$a} <=> $Spec{$b} }  keys %Spec) {
        $html .= qq(<a href="S$Spec{$syn}.html">$Spec{$syn} $syn</a><br />\n);
    }
    $html .= "</body></html>";

    if (open my $fh, '>', "$out_dir/index.html") {
        print {$fh} $html;
    } else {
        warn "Could not create index.html: $!";
    }
    return;
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

