# util/smartlinks.pl - The successor to util/catalog_tests.pl.

# Please look at t/README for documentation.
# See util/t/smartlinks.t for unit tests and usage of the internal API.
# This program is still under active development.
# Contact agentzh on #perl6 if you have a patch or some good idea for this tool.
# Currently you can use it to verify the smart link used in your script:
#   $ perl util/smartlinks.pl --check t/some/test.t
# and generate HTML files with test snippets embedded:
#   $ perl util/smartlinks.pl --out-dir=. t/*/*.t t/*/*/*.t

use strict;
use warnings;

use YAML::Syck;
use Getopt::Long;
use File::Basename;
use FindBin;
use Pod::Html;
use File::Temp 'tempfile';

my ($check, $count, $broken_count, $snippet_id);

my %Spec = reverse qw(
    01 Overview 02 Syntax       03 Operator     04 Block
    05 Rule     06 Subroutine   09 Structure    10 Package
    11 Module   12 Object       13 Overload     29 Functions
);

sub add_link ($$$$$$$)  {
    my ($links, $synopsis, $section, $pattern, $t_file, $from, $to) = @_;
    $links->{$synopsis} ||= {};
    $links->{$synopsis}->{$section} ||= [];
    if ($pattern and substr($pattern, -1, 1) eq '/') { $pattern = "/$pattern"; }
    push @{ $links->{$synopsis}->{$section} },
        [$pattern => [$t_file, $from, $to]];
    $count++;
}

sub error {
    if ($check) { warn "ERROR: @_\n"; }
}

sub process_t_file ($$) {
    my ($infile, $links) = @_;
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
        elsif (/^ \s* \#? \s* L<<? (S\d+) \/ ([^\/]+) \/ (.*) /xo) {
            #warn "$1, $2, $3\n";
            ($synopsis, $section, $pattern) = ($1, $2, $3);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            if (!$section) {
                error "$infile: line $.: section name can't be empty.";
            }
            $pattern =~ s/^\s+|\s+$//g;
            if (substr($pattern, -1, 1) ne '>') {
                $_ = <$in>;
                s/^\s*\#?\s*|\s+$//g;
                if (!s/>>?$//) {
                    error "$infile: line $.: smart links must termanate",
                        "in the second line.";
                    next;
                }
                $pattern .= " $_";
                $new_from = $. - 1;
                $to = $. - 2;
            } else {
                $new_from = $.;
                $to = $. - 1;
                chop $pattern;
            }
            #warn "*$synopsis* *$section* *$pattern*\n";
        }
        elsif (/^ \s* \#? \s* L<? S\d+\b /xoi) {
            error "$infile: line $.: syntax error in the magic link:\n\t$_";
        }
        else { next; }

        #warn "*$synopsis* *$section*\n";
        $setter->($from, $to) if $setter and $from;
        $setter = sub {
            add_link($links, $synopsis, $section, $pattern, $infile, $_[0], $_[1]);
        };
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
        if ($pat =~ /\G\s*"([^"]+)"/gco ||
            $pat =~ /\G\s*'([^']+)'/gco ||
            $pat =~ /\G\s*(\S+)/gco) {
                push @keys, $1;
        } else { last }
    }
    my $str = join('.*?', map quotemeta, @keys);
    $str;
}

# process paragraphs of the synopses: unwrap lines, strip POD tags, and etc.
sub process_paragraph ($) {
    my $str = shift;

    # unwrap lines:
    $str =~ s/\s*\n\s*/ /go;

    # strip POD tags:
    $str =~ s/L<<(.*?)>>/$1/go;
    $str =~ s/L<(.*?)>/$1/go;
    $str =~ s/C<<(.*?)>>/$1/go;
    $str =~ s/C<(.*?)>/$1/go;
    $str =~ s/I<<(.*?)>>/$1/go;
    $str =~ s/I<(.*?)>/$1/go;
    $str =~ s/B<<(.*?)>>/$1/go;
    $str =~ s/B<(.*?)>/$1/go;
    $str;
}

sub gen_html ($$) {
    my ($pod, $cssfile) = @_;

    my ($fh, $podfile) = tempfile(SUFFIX => '.pod');
    print $fh $pod;
    close $fh;
    my $htmfile = "$podfile.html";

    pod2html($podfile,
        "--backlink=Back to Top",
        "--htmlroot=.",
        "--recurse",
        "--infile=$podfile",
        "--outfile=$htmfile",
        "--css=$cssfile",
        "--header",
        "--quiet",
    );
    unlink 'pod2htmd.tmp';
    unlink 'pod2htmi.tmp';

    open my $in, $htmfile or
        die "Can't open $htmfile for reading: $!";
    local $/;
    my $html = <$in>;
    close $in;

    my $header = <<'_EOC_';
<script language="javascript1.2"><!--
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

//--></script>
_EOC_

    if ($html !~ s,text/html; charset=[^">]+,text/html; charset=UTF-8,) {
        $html =~ s,<head>,
            <meta HTTP-EQUIV="content-type" CONTENT="text/html; charset=UTF-8">
            <meta name="save" content="history">
        ,;
    }
    $html =~ s,</head>,$header</head>,;
    # stripped the line prefixes introduced by `gen_code_snippet`:
    $html =~ s/^QQQ://msg;
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
    my $i = 1;
    my $src;
    while (<$in>) {
        next if $i < $from;
        last if $i > $to;
        # we need the 'QQQ:' prefix in order to work around
        # '=begin/=end' in the test snippts themselves:
        s/\&/&amp;/g;
        s/</\&lt;/g;
        s/>/\&gt;/g;
        $src .= "QQQ:$_";
    } continue { $i++ }
    close $in;
    $snippet_id++;
    #warn $snippet_id;
    #warn "$file $to $from";
    warn "BBBBBBB @$location $snippet_id" if !defined $src;
    my $nlines = $to - $from + 1;
    return <<"_EOC_";

=begin html

<a name="msg_${snippet_id}"></a>
<a href="?hide_quotes=no#msg_${snippet_id}" onclick="return tog_quote(${snippet_id});">
<div ID=header_shown_${snippet_id} style="display: none;">
- Hide the snippet from $file (line $from ~ line $to) -
</div>
<div ID=header_hidden_${snippet_id} style="display: block;">
- Show the snippet from $file (line $from ~ line $to, $nlines lines) -</div>
</a>
<div ID=hide_${snippet_id} style="display:none; border:1px solid">
<pre>
$src
</pre>
</div>

=end html

_EOC_
}

=begin comment

process_syn: process synopses one by one.

=end comment

=cut

sub process_syn ($$$$) {
    my ($infile, $out_dir, $cssfile, $links) = @_;
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
    #print Dump $podtree if $syn_id eq '02';

    my $sections = $links->{"S$syn_id"};
    if (!$sections) {
        return;
    }
    $snippet_id = 1;
    while (my ($section, $links) = each %$sections) {
        #warn "checking $section...";
        my @links = @$links;
        my $paras = $podtree->{$section};
        if (!$paras) {
            my $link = $links[0];
            my ($t_file, $from) = @{ $link->[1] };
            $from--;
            error "$t_file: line $from:",
                "section ``$section'' not found in S$syn_id.";
            $broken_count++;
            next;
        }
        for my $link (@links) {
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
                next if !$para;
                if (process_paragraph($para) =~ /$regex/) {
                    if (!$check) {
                        splice @$paras, $i+1, 0, gen_code_snippet($location);
                        $i++;
                    }
                    $matched = 1;
                }
            } continue { $i++ }
            if (!$matched) {
                my ($file, $lineno) = @$location;
                $lineno--;
                error("$file: line $lineno: pattern ``$pattern'' failed to match any",
                    "paragraph in L<S${syn_id}/${section}>.");
                $broken_count++;
            }
        }
    }

    if (!$check) {
        my $pod = emit_pod($podtree);
        #print $pod if $syn_id eq '02';
        #if ($syn_id eq '02') {
        #    use File::Slurp;
        #    write_file("S$syn_id.pod", $pod);
        #}
        my $html = gen_html($pod, $cssfile);
        $html =~ s,<title>\s*TITLE\s*</title>,<title>S$syn_id</title>,s;
        my $htmfile = "$out_dir/S$syn_id.html";
        warn "info: generating $htmfile...\n";
        open my $out, "> $htmfile" or
            die "Can't open $htmfile for writing: $!\n";
        print $out $html;
        close $out;
    }

    #warn "$syn_id: $infile\n";
}

sub Usage {
    print <<_EOC_;
Usage:
  $0 --check t/*/*.t t/*/*/*.t
  $0 --check t/some/test.t
  $0 --css foo.css --out-dir=. t/syntax/*.t
_EOC_
    exit(0);
}

sub main {
    my ($syn_dir, $out_dir, $help, $cssfile);
    GetOptions(
        'check'     => \$check,
        'syn-dir=s' => \$syn_dir,
        'out-dir=s' => \$out_dir,
        'help'      => \$help,
        'css=s'     => \$cssfile,
    );

    if ($help || !@ARGV) {
        Usage();
    }

    $cssfile ||= 'http://dev.perl.org/css/perl.css';

    $count = 0;
    $broken_count = 0;
    
    $out_dir ||= '.';
    mkdir $out_dir if !-d $out_dir;

    my @t_files = map glob, @ARGV;
    mkdir 'tmp' if !-d 'tmp';
    my $links = {};
    for my $t_file (@t_files) {
        process_t_file($t_file, $links);
    }
    #print Dump($links);

    my $pugs_syn_dir = "$FindBin::Bin/../docs/Perl6/Spec";
    $syn_dir ||= $pugs_syn_dir;

    if ($syn_dir eq $pugs_syn_dir) {
        system "$^X $syn_dir/update";
    }
    my @syns = map glob, "$syn_dir/*.pod";
    for my $syn (@syns) {
        process_syn($syn, $out_dir, $cssfile, $links);
    }
    warn "info: $count smartlinks found and $broken_count broken.\n";
    if (!$check) {
        warn "hint: use the --check option for details on broken smartlinks.\n";
    }
    exit;
}

main() if ! caller;

1;
