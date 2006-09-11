#!/usr/bin/env perl

# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# This program is still under active development.
# Please read the Pod documentation at the end of file before reading and/or
# modifying the source.
# Contact agentzh on #perl6 if you have a patch or any good ideas for this tool.

use strict;
use warnings;

#use YAML::Syck;
use Getopt::Long;
use File::Basename;
use FindBin;
use Pod::Simple::HTML;

my $check;
my $test_result;
my ($syn_rev, $pugs_rev);
my ($count, $broken_count);
my (@snippets, $snippet_id);

my %Spec = reverse qw(
    01 Overview 02 Syntax        03 Operator     04 Block
    05 Rule     06 Subroutine    09 Structure    10 Package
    11 Module   12 Object        13 Overload     17 Concurrency
    22 CPAN     26 Documentation 29 Functions
);

my $javascript = <<'_EOC_';
<script type="text/javascript">
var agt = navigator.userAgent.toLowerCase();

var is_opera = (agt.indexOf("opera") != -1);
var is_ie = (agt.indexOf("msie") != -1) && document.all && !is_opera;
var is_ie5 = (agt.indexOf("msie 5") != -1) && document.all;

function tog() {
  // tog: toggle the visibility of html elements (arguments[1..]) from none to
  // arguments[0].  Return what should be returned in a javascript onevent().
  display = arguments[0];
  for( var i=1; i<arguments.length; i++ ) {
    var x = document.getElementById(arguments[i]);
    if (!x) continue;
    if (x.style.display == "none" || x.style.display == "") {
      x.style.display = display;
    } else {
      x.style.display = "none";
    }
  }

  var e = is_ie ? window.event : this;
  if (e) {
    if (is_ie) {
      e.cancelBubble = true;
      e.returnValue = false;
      return false;
    } else {
      return false;
    }
  }
}

function tog_quote( idnum ) {
  return tog( 'block', 'header_shown_' + idnum, 'header_hidden_' + idnum,
       'hide_' + idnum );
}

</script>
_EOC_

=begin private

=head2 add_link

  add_link($linktree, $synopsis, $section, $pattern, $infile, $from, $to);

Side Effects:
 - modifies global C<$count> 

=end private

=cut 

sub add_link ($$$$$$$)  {
    my ($linktree, $synopsis, $section, $pattern, $t_file, $from, $to) = @_;
    if ($from == $to) {
        warn "WARNING: empty snippet detected at $t_file (line $from ~ $to).\n";
    }
    $linktree->{$synopsis} ||= {};
    $linktree->{$synopsis}->{$section} ||= [];
    if ($pattern and substr($pattern, -1, 1) eq '/') { $pattern = "/$pattern"; }
    push @{ $linktree->{$synopsis}->{$section} },
        [$pattern => [$t_file, $from, $to]];
    $count++;
}

sub error {
    if ($check) { warn "ERROR: @_\n"; }
}

sub process_t_file ($$) {
    my ($infile, $linktree) = @_;
    open my $in, $infile or
        die "error: Can't open $infile for reading: $!\n";
    my ($setter, $from, $to);
    while (<$in>) {
        chomp;
        my $new_from;
        my ($synopsis, $section, $pattern);
        if (/^ \s* \#? \s* L< (S\d+) \/ ([^\/]+) >\s*$/xo) {
            ($synopsis, $section) = ($1, $2);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            #warn "$synopsis $section" if $synopsis eq 'S06';
            $new_from = $.;
            $to = $. - 1;
        }
        elsif (/^ \s* \#? \s* L(<<?) (S\d+) \/ ([^\/]+) \/ (.*) /xo) {
            #warn "$1, $2, $3\n";
            my $brackets;
            ($brackets, $synopsis, $section, $pattern) = ($1, $2, $3, $4);
            $brackets = length($brackets);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            if (!$section) {
                error "$infile: line $.: section name can't be empty.";
            }
            $pattern =~ s/^\s+|\s+$//g;
            if (substr($pattern, -1, 1) ne '>') {
                $_ = <$in>;
                s/^\s*\#?\s*|\s+$//g;
                if (!s/>{$brackets}$//) {
                    error "$infile: line $.: smart links must terminate",
                        "in the second line.";
                    next;
                }
                $pattern .= " $_";
                $new_from = $. - 1;
                $to = $. - 2;
            } else {
                $new_from = $.;
                $to = $. - 1;
                $pattern =~ s/\s*>{$brackets}$//;
            }
            #warn "*$synopsis* *$section* *$pattern*\n";
        }
        elsif (/^ \s* \#? \s* L<? S\d+\b /xoi) {
            error "$infile: line $.: syntax error in the magic link:\n\t$_";
        }
        else { next; }

        #warn "*$synopsis* *$section*\n";
        if ($from and $from == $to) {
            my $old_setter = $setter;
            my $old_from = $from;
            $setter = sub {
                add_link($linktree, $synopsis, $section, $pattern, $infile, $_[0], $_[1]);
                $old_setter->($old_from, $_[1]);
                #warn "$infile - $old_from ~ $_[1]";
            };
            #warn "$infile - $from ~ $to";
        } else {
            $setter->($from, $to) if $setter and $from;
            $setter = sub {
                add_link($linktree, $synopsis, $section, $pattern, $infile, $_[0], $_[1]);
            };
        }
        $from = $new_from;
    }
    $setter->($from, $.) if $setter and $from;
    close $in;
}

sub parse_pod ($) {
    my $infile = shift;
    open my $in, $infile or
        die "can't open $infile for reading: $!\n";
    my $podtree = {};
    my $section;
    while (<$in>) {
        if (/^ =head(\d+) \s* (.*\S) \s* $/x) {
            #warn "parse_pod: *$1*\n";
            my $num = $1;
            $section = $2;
            $podtree->{_sections} ||= [];
            push @{ $podtree->{_sections} }, [$num, $section];
        } elsif (!$section) {
            $podtree->{_header} .= $_;
        } elsif (/^\s*$/) {
            $podtree->{$section} ||= [];
            #push @{ $podtree->{$section} }, "\n";
            push @{ $podtree->{$section} }, '';
        } elsif (/^\s+(.+)/) {
            $podtree->{$section}->[-1] .= $_;
            push @{ $podtree->{$section} }, '';
        } else {
            $podtree->{$section}->[-1] .= $_;
        }
    }
    close $in;
    $podtree;
}

sub emit_pod ($) {
    my $podtree = shift;
    my $str;
    $str .= $podtree->{_header} if $podtree->{_header};
    for my $elem (@{ $podtree->{_sections} }) {
        my ($num, $sec) = @$elem;
        $str .= "=head$num $sec\n\n";
        for my $para (@{ $podtree->{$sec} }) {
            if ($para eq '') {
                $str .= "\n";
            } elsif ($para =~ /^\s+/) {
                $str .= $para;
            } else {
                $str .= "$para\n";
            }
        }
    }
    $str;
}

# convert patterns used in smartlinks to perl 5 regexes
sub parse_pattern ($) {
    my $pat = shift;
    if ($pat =~ m{^/(.*)/}) {
        return $1;
    }
    my @keys;
    while (1) {
        if ($pat =~ /\G\s*"([^"]+)"/gc ||
            $pat =~ /\G\s*'([^']+)'/gc ||
            $pat =~ /\G\s*(\S+)/gc) {
                push @keys, $1;
        } else { last }
    }
    my $str = join('.+?', map {
        my $key = quotemeta $_;
        $key =~ s/^\w/\\b$&/;
        $key =~ s/\w$/$&\\b/;
        $key;
    } @keys);

    $str;
}

# process paragraphs of the synopses: unwrap lines, strip POD tags, and etc.
sub process_paragraph ($) {
    my $str = shift;

    # unwrap lines:
    $str =~ s/\s*\n\s*/ /g;

    # strip POD tags:
    # FIXME: obviously we need a better way to do this:
    $str =~ s/[LCFIB]<<<\s+(.*?)\s+>>>/$1/g;
    $str =~ s/[LCFIB]<<\s+(.*?)\s+>>/$1/g;
    $str =~ s/[LCFIB]<(.*?)>/$1/g;
    $str;
}

sub gen_html ($$$) {
    my ($pod, $syn_id, $cssfile) = @_;

    $Pod::Simple::HTML::Content_decl =
        q{<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" >};

    $Pod::Simple::HTML::Doctype_decl =
        qq{<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
           "http://www.w3.org/TR/html4/loose.dtd">\n};

    my $pod2html = new Pod::Simple::HTML;
    $pod2html->index(1);
    $pod2html->html_css($cssfile);
    $pod2html->html_javascript($javascript);
    $pod2html->force_title('S'.$syn_id);

    my $html;
    open my $in, '<', \$pod;
    open my $out, '>', \$html;
    $pod2html->parse_from_file($in, $out);

    # substitutes the placeholders introduced by `gen_code_snippet`
    # with real code snippets:
    $html =~ s,<p>\s*_SMART_LINK_(\d+)\s*</p>,$snippets[$1],sg;
    $html;
}

# Note that this function has been optimized for space rather
# than time.
sub gen_code_snippet ($) {
    my $location = shift;
    my ($file, $from, $to) = @$location;
    #warn "gen_code_snippet: @$location\n";
    open my $in, $file or
        die "Can't open $file for reading: $!\n";

    # Strip leading realpath so the names start at t/
    $file =~ s{.*?/t/}{t/};

    my $i = 1;
    my $src;
    my $file_info;
    $file_info = $test_result->{$file} if $test_result;
    my ($ok_count, $failed_count) = (0, 0);
    while (<$in>) {
        next if $i < $from;
        last if $i > $to;
        s/\&/\&amp;/g;
        s/"/\&quot;/g;
        s/</\&lt;/g;
        s/>/\&gt;/g;
        s{^(  *)}{"&nbsp; " x (length($1) / 2)}gem;
        s/  / &nbsp;/g;
        s{L\&lt;(http://.*?)\&gt;}{L\&lt;<a href="$1">$1</a>\&gt;}g;
        s{L\&lt;\&quot;(http://.*?)\&quot;\&gt;}
         {L\&lt;<a href="$1">\&quot;$1\&quot;</a>\&gt;}g;
        if (!$file_info) {
            $src .= $_;
            next;
        }
        chomp;
        my $mark;
        if (!exists $file_info->{$i}) {
            $mark = '';
        } elsif ($file_info->{$i}) {
            $mark = qq{<span style="color: green; font-weight: bold"> √ </span>};
            $ok_count++;
        } else {
            $mark = qq{<span style="color: red; font-weight: bold"> × </span>};
            $failed_count++;
        }
        $src .= qq{<tr><td><code>$mark</code></td><td><code>$_</code></td></tr>\n};
    } continue { $i++ }

    close $in;

    $src =~ s/\n+$//sg;

    $snippet_id++;
    
    #warn $snippet_id;
    #warn "$file $to $from";
    warn "NOT DEFINED!!! @$location $snippet_id" if !defined $src;

    my $snippet;
    if (!$test_result) {
        $snippet = qq{<pre style="margin-left: 6px">$src</pre>};
    } else {
        $snippet = qq{
            <table style="margin-left: 6px; border-width: 0; cellspacing: 0; cellpadding: 0">
                $src
            </table>
        };
    }

    my $stat = $test_result ?
        " &nbsp;&mdash; &nbsp;<code>$ok_count √, $failed_count ×</code>" :
        '';

    my $nlines = $to - $from + 1;
    my $html = <<"_EOC_";
<a href="#" onclick="return tog_quote(${snippet_id});">
<div ID="header_shown_${snippet_id}" style="display: none;">
- Hide the snippet from $file (line $from ~ line $to$stat) -
</div>
<div ID="header_hidden_${snippet_id}" style="display: block;">
- Show the snippet from $file (line $from ~ line $to$stat) -
</div>
</a>
<div ID="hide_${snippet_id}" style="display:none; border:1px solid">
$snippet
</div>
_EOC_
    $snippets[$snippet_id] = $html;
    "\n\n_SMART_LINK_$snippet_id\n\n";
}

=begin private

=head2 process_syn

  process_syn($syn, $out_dir, $cssfile, $linktree);

Process synopses one by one.

Side Effects: 
  modifies global C<$broken_count>

=end private 

=cut 

sub process_syn ($$$$) {
    my ($infile, $out_dir, $cssfile, $linktree) = @_;
    my $syn_id;
    if ($infile =~ /\bS(\d+)\.pod$/) {
        $syn_id = $1;
    } else {
        my $base = basename($infile, '.pod');
        $syn_id = $Spec{$base};
    }
    if (!$syn_id) {
        warn "  warning: $infile skipped.\n";
        return;
    }
    my $podtree = parse_pod($infile);
    #print Dump $podtree if $syn_id eq '29';

    #use Data::Dumper;
    #$Data::Dumper::Indent = 1;
    #print Dumper $linktree if $syn_id eq '02';

    my $linktree_sections = $linktree->{"S$syn_id"};
    if (!$linktree_sections) {
        # We won't generate the HTML file if there's no smartlink in it.
        return;
    }
    $snippet_id = 0;
    while (my ($section_name, $links) = each %$linktree_sections) {
        #warn "checking $section...";
        my @links = @$links;
        my $paras = $podtree->{$section_name};
        if (!$paras) {
            my $link = $links[0];
            my ($t_file, $from) = @{ $link->[1] };
            $from--;
            error "$t_file: line $from:",
                "section ``$section_name'' not found in S$syn_id.";
            $broken_count++;
            next;
        }
        for my $link (reverse @links) {
            my ($pattern, $location) = @$link;
            my $i = 0;
            if (!$pattern) { # match the whole section
                if (!$check) {
                    unshift @$paras, gen_code_snippet($location);
                    $i = 1;
                }
                next;
            }
            my $regex = parse_pattern($pattern);
            my $matched;
            while ($i < @$paras) {
                my $para = $paras->[$i];
                next if !$para or $para =~ /\?hide_quotes=no/;
                if (process_paragraph($para) =~ /$regex/) {
                    if (!$check) {
                        splice @$paras, $i+1, 0, gen_code_snippet($location);
                        $i++;
                    }
                    $matched = 1;
                    last;
                }
            } continue { $i++ }
            if (!$matched) {
                my ($file, $lineno) = @$location;
                error("$file: line $lineno: pattern ``$pattern'' failed to match any",
                    "paragraph in L<S${syn_id}/${section_name}>.");
                $broken_count++;
            }
        }
    }

    # We need this to check invalid smartlinks pointed to unexistent docs:
    delete $linktree->{"S$syn_id"};

    if (!$check) {
        #use Data::Dumper;
        #$Data::Dumper::Indent = 1;
        #print Dumper $podtree if $syn_id eq '02';

        my $pod = emit_pod($podtree);
        
        #print $pod if $syn_id eq '02';
        #if ($syn_id eq '29') {
        #    use File::Slurp;
        #    write_file("db_S$syn_id.pod", $pod);
        #}
        
        my $html = gen_html($pod, $syn_id, $cssfile);
        
        #write_file("db_S$syn_id.html", $html);
        
        my ($sec, $min, $hour, $mday, $mon, $year) = gmtime;
        $year += 1900; $mon += 1;
        my $time = sprintf "%04d-%02d-%02d %02d:%02d:%02d GMT",
            $year, $mon, $mday, $hour, $min, $sec;
        $html =~ s{<!-- start doc -->}{$&
            <I>This page was generated at $time.
            (syn <strong>$syn_rev</strong>, pugs <strong>$pugs_rev</strong>)</I>
        };
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
  $0 --css foo.css --out-dir=public_html t/syntax/*.t
  $0 --check t/*/*.t t/*/*/*.t
  $0 --check t/some/test.t

Options:
  --help          Show this help.
  --check         Only check the validity of the smartlinks, no
                  HTML outputs.
  --out-dir <dir> Specify the output directory for HTML files.
  --css <file>    Specify the CSS file used by the HTML outputs,
                  defaults to http://dev.perl.org/css/perl.css.
  --fast          Do not update the Synopses from the web.
  --test-res <ymlfile>
                  Set .yml file generated from Test::TAP::Model's
                  ``structure''. Usually <ymlfile> should be set
                  to ``tests.yml''.
  --syn-dir       Specify the directory where the Synopses live,
                  defaults to pugs' docs/Perl6/Spec. Please don't
                  set syn-dir to elsewhere unless you have a good
                  reason.
_EOC_
    exit(0);
}

sub main () {
    my ($syn_dir, $out_dir, $help, $cssfile, $fast, $yml_file);
    GetOptions(
        'check'       => \$check,
        'syn-dir=s'   => \$syn_dir,
        'out-dir=s'   => \$out_dir,
        'css=s'       => \$cssfile,
        'help'        => \$help,
        'fast'        => \$fast,
        'test-res=s'  => \$yml_file,
    );

    if ($help || !@ARGV) {
        help();
    }

    $cssfile ||= 'http://dev.perl.org/css/perl.css';

    $count = 0;
    $broken_count = 0;

    $out_dir ||= '.';
    mkdir $out_dir if !-d $out_dir;

    my @t_files = map glob, @ARGV;
    my $linktree = {};
    for my $t_file (@t_files) {
        process_t_file($t_file, $linktree);
    }
    #print Dump($linktree);

    my $pugs_syn_dir = "$FindBin::Bin/../docs/Perl6/Spec";
    $syn_dir ||= $pugs_syn_dir;

    #warn "$fast";
    my $update_script = "$syn_dir/update";
    if (-f $update_script) {
        #warn "HERE";
        system "$^X $update_script" if !$fast;
        my $rev_file = "$syn_dir/.spec-revision";
        #warn $rev_file;
        #warn -f $rev_file, "\n";
        if (open my $in, $rev_file) {
            $syn_rev = <$in>;
            chomp $syn_rev;
            close $in;
        }
    }

    $syn_rev = $syn_rev ? "r$syn_rev" : 'unknown';
    warn "info: synopses are at $syn_rev.\n";

    if (!$yml_file) {
        my $stdout = `$^X $FindBin::Bin/version_h.pl`;
        ($pugs_rev) = ($stdout =~ /Current version is (\d+)/);
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
    }

    if ($yml_file) {
        eval {
            require Test::TAP::Model;
            require YAML::Syck;
        };
        if ($@) {
            die "--smoke-res option requires both Test::TAP::Model and YAML::Syck. ".
                "At least one of them is not installed.\n";
        }
        my $data = YAML::Syck::LoadFile($yml_file);
        #warn $data;
        my $structure;
        if ($data->{meat}) {
            $structure = delete $data->{meat};
        }
        my $tap = Test::TAP::Model->new_with_struct($structure);
        $test_result = {};
        for my $file ($tap->test_files) {
            #warn "  $file...\n";
            (my $fname = $file->name) =~ s{.*?/t/}{t/};
            my %file_info;
            $test_result->{$fname} = \%file_info;
            for my $case ($file->cases) {
                next if $case->skipped or !$case->test_line;
                $file_info{$case->test_line} = $case->actual_ok;
            }
        }
        #YAML::Syck::DumpFile('test_result.yml', $test_result);
        $pugs_rev = $data->{revision};
    }

    $pugs_rev = $pugs_rev ? "r$pugs_rev" : 'unknown';
    warn "info: pugs is at $pugs_rev.\n";

    my @syns = map glob, "$syn_dir/*.pod";
    for my $syn (@syns) {
        process_syn($syn, $out_dir, $cssfile, $linktree);
    }

    # check for hanging smartlinks:
    while (my ($syn, $linktree_sections) = each %$linktree) {
        for my $links (values %$linktree_sections) {
            for my $link (@$links) {
                my ($file, $lineno) = @{ $link->[1] };
                error("$file: line $lineno: smartlink pointing to " .
                    "an unknown synopsis ($syn)"),
                $broken_count++;
            }
        }
    }

    warn "info: $count smartlinks found and $broken_count broken.\n";
    if (!$check) {
        warn "hint: use the --check option for details on broken smartlinks.\n";
    }
    exit;
}

main() if ! caller;

1;
__END__

=head1 NAME

smartlinks.pl - The successor to catalog_tests.pl.

=head1 SYNOPSIS

  smartlinks.pl t/*/*.t t/*/*/*.t
  smartlinks.pl --css foo.css --out-dir=public_html t/syntax/*.t
  smartlinks.pl --check t/*/*.t t/*/*/*.t
  smartlinks.pl --check t/some/test.t

=head1 Design Decisions

=over

=item *

This script should have as few module dependencies as possible.

=item *

One doesn't have to build pugs so as to run F<smartlinks.pl>. Of course,
optional advanced features may require the user to run pugs'
"make" or even "make smoke".

=back

=head1 Smartlink Syntax

Smartlinks are planted in the test file, and are pointed to the appropriate sections
of the Synopsis you are using to write the test.

They look like pod links:

    L<S06/Blocks>            # "S06" is synopsis 6, and "Blocks" is the section
    L<S03/"Hyper operators"> # quotes can be used when spaces are in the title,
                             # but is NOT required.
    L<S03/Hyper operators>   # just fine

The section name should be copied verbatim from the POD (usually after =head),
including any POD tags like C<...> and punctuations.

The smartlinks also have a weird (also important) extension: you can specify some 
keyphrases, to skip forward from the linked section, so the smartlink is put into
a more specific place:

    L<S05/"Return values from matches"/"In numeric context" number 'matches:'>

The above smartlink is appropriate next to a test case checking rule application in
numeric context, and it will place the backlink appropriately.

All the keyphrases listed after the second slash in a smartlink should appear in
a single sentence from the synopsis text, and the order is significant. If 
there're spaces in a keyphrase, quote it using either double-quotes or signle-quotes.

In contrast with the case of section name, you should never use POD tags like
C<...> in a keyphrase. util/smartlinks.pl will do the right thing. You can use,
however, pod directives in the keyphrases, just like this:

    # L<S04/Str/"=item split">

Smartlinks in .t files can be preceded by nothing but spaces or "#", otherwise
they can't be recognized by tools.

There's also a variant for the smartlink syntax:

   # L<<syn/sec/key phrases>>

A smartlink can span at most 2 lines:

   # L<S04/section name/key1
   #   "key2" key3 key4>

Only the keyphrase list part can continue to the next line. So the following example
is invalid:

   # L<S04/section
   #   name/blah blah blah>      # WRONG!!!

Please don't put a smartlink in the middle of a group of tests. Put it right
*before* the group of tests it is related to.

Multiple adjacent smartlinks can share the same snippet of tests right below
them:

    # L<S02/Context/boolean "?">
    # L<S03/Changes to Perl 5 operators/"?" imposes boolean context>
    { ... }

smartlinks.pl can take care of this kind of special cases.

You can put a URL to create a generic link:

  L<"http://groups.google.de/group/perl.perl6.language/msg/07aefb88f5fc8429">

or without quotes:

  L<http://www.nntp.perl.org/group/perl.perl6.language/26071>

Try running 'grep -r "L<" t/' to see some examples, or look at
t/syntax/comments.t.

There're also some legacy smartlinks using the following syntax:

   L<S04/"section name" /regex/>
   L<<S04/"section name" /regex/>>

They're also supported by util/smartlinks.pl for backward-compatibility,
but are definitely deprecated now.

=head1 Basic Algorithm

=over

=item 1.

We scan over all the specified .t files; collect smartlinks and positional
info about the test code snippets as we go. When all these work have been finished,
we obtain a tree structure, which is named C<$linktree> in the source code.

To this tree minimal, we only store the .t file name and line numbers, rather
than the snippets' source code itself.

The structure of $linktree is like this:

    {
      'S12' => {
        'Traits' => [
          [
            undef,
            [
              't/oo/traits/basic.t',
              '13',
              '38'
            ]
          ],
          [
            '/If you say/',
            [
              't/oo/delegation.t',
              '56',
              '69'
            ]
          ],
        ],
      },
      'S02' => {
        'Whitespace and Comments' => [
          [
            '"Embedded comments" "#" plus any bracket',
            [
              't/syntax/comments.t',
              10,
              48
            ]
          ],
        ]
      }
    }

This step is mostly done by sub C<process_t_file>.

=item 2.

We process the synopses .pod files one by one and generate
HTML files integrated with test code snippets using the 
C<$linktree> structure discussed above.

This is mostly done by sub C<process_syn>.

Because it is an enormous step, we can further divide it into several
sub steps:

=over

=item *

Then we parse each .pod into a tree, which is known as C<$podtree> in the
source code. (See sub C<parse_pod>.)

The structure of C<$podtree> looks like this:

    {
      'Names and Variables' => [
        '=over 4' . "\n",
        '=item *' . "\n",
        'The C<$Package\'var> syntax is gone.  Use C<$Package::var> instead.' . "\n",
        '=item *' . "\n",
        'Perl 6 includes a system of B<sigils> to mark the fundamental' . "\n".
            'structural type of a variable:' . "\n",
        ...
      ],
      ...
    }

=item *

We look up every related smartlinks from every C<$podtree>, generate .t code
snippets along the way, and insert placeholders (like "_SMART_LINK_3" into
the corresponding C<$podtree>. (See subs C<parse_pattern>, C<process_paragrph>,
and C<gen_code_snippet>.)

=item *

Now we emit Pod source back from the modified $C<podtree>'s. (See sub C<emit_pod>.)

=item *

After that, we generate HTML source from the Pod source with snippet placeholders
using Pod::Simple::HTML. (See sub C<gen_html>.)

=item *

At last, we replace every snippet placeholders in the HTML source with the real
snippet code.

=back

=back

=head1 SEE ALSO

=over

=item *

t/README in the Pugs source tree.

=item * 

The article on Audrey's blogs:

L<http://pugs.blogs.com/pugs/2006/08/integrating_the.html>

=item *

Consult util/t/smartlinks.t in the Pugs source tree for unit 
tests and usage of the internal API.

=back

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial implementation, getting
help from many others in the Pugs Team.

=head1 COPYRIGHT

Copyright (c) 2006 by the Pugs contributors.
