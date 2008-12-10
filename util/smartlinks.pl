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

use strict;
#use warnings;

#use Smart::Comments;
#use YAML::Syck;
use Getopt::Long;
use File::Basename;
use FindBin;
use File::Find qw(find);
#use File::Slurp;
#use Pod::Simple::HTML;

my $check;
my $print_missing;
my $test_result;
my $line_anchor;
my ($pugs_rev, $smoke_rev);
my ($link_count, $broken_link_count);
my (@snippets, $snippet_id);

my %Spec = reverse qw(
    01 Overview    02 Syntax        03 Operator      04 Block
    05 Rule        06 Subroutine    07 Iterator
    09 Structure   10 Package
    11 Module      12 Object        13 Overload      16 IO
    17 Concurrency 22 CPAN          26 Documentation 29 Functions
);

my $javascript = '';

# EVENT HANDLING
$javascript .= <<'_EOC_';
// http://therealcrisp.xs4all.nl/upload/addEvent_dean.html
// written by Dean Edwards, 2005
// with input from Tino Zijdel - crisp@xs4all.nl
// http://dean.edwards.name/weblog/2005/10/add-event/
// modified by Aankhen

var addEvent;

if (window.addEventListener) {
    addEvent = function (element, type, handler) { element.addEventListener(type, handler, (typeof arguments[3] != 'undefined') ? arguments[3] : false); };
} else {
  addEvent = function (element, type, handler) {
    // assign each event handler a unique ID
    if (!handler.$$guid) handler.$$guid = addEvent.guid++;
    // create a hash table of event types for the element
    if (!element.events) element.events = {};
    // create a hash table of event handlers for each element/event pair
    var handlers = element.events[type];
    if (!handlers) {
      handlers = element.events[type] = {};
      // store the existing event handler (if there is one)
      if (element['on' + type]) {
        handlers[0] = element['on' + type];
      }
      // assign a global event handler to do all the work
      element['on' + type] = handleEvent;
    }

    // store the event handler in the hash table
    handlers[handler.$$guid] = handler;
  }
}

// a counter used to create unique IDs
addEvent.guid = 1;

function removeEvent(element, type, handler) {
  if (element.removeEventListener)
    element.removeEventListener(type, handler, false);
  // delete the event handler from the hash table
  else if (element.events && element.events[type] && handler.$$guid)
    delete element.events[type][handler.$$guid];
}

function handleEvent(event) {
  // grab the event object (IE uses a global event object)
  event = event || fixEvent(window.event);

  var returnValue = true;

  // get a reference to the hash table of event handlers
  var handlers = this.events[event.type];

  // execute each event handler
  for (var i in handlers) {
    // don't copy object properties
    if (!Object.prototype[i]) {
      this.$$handler = handlers[i];
      if (this.$$handler(event) === false) {
        returnValue = false;
        // in accordance with DOM2-Events, all remaining event handlers on the object will be triggered, hence the absence of a `break`
      }
    }
  }

  // clean up
  if (this.$$handler) this.$$handler = null;

  return returnValue;
}

function fixEvent(event) {
  // add W3C standard event methods
  event.preventDefault = fixEvent.preventDefault;
  event.stopPropagation = fixEvent.stopPropagation;
  return event;
}

fixEvent.preventDefault = function() {
  this.returnValue = false;
};

fixEvent.stopPropagation = function() {
  this.cancelBubble = true;
};
_EOC_

# VISIBILITY TOGGLE
$javascript .= <<'_EOC_';
function toggle_snippet (e) {
  var matches = this.id.match(/smartlink_toggle(\d+)/);
  var num = matches[1];

  var id = 'smartlink_' + num;
  var div = document.getElementById(id);
  div.style.display = (div.style.display == 'none') ? '' : 'none';

  var text = this.firstChild;
  text.nodeValue = text.nodeValue.replace(/^- (Show|Hide)/, function (full, p1) { return "- " + ((p1 == 'Show') ? 'Hide' : 'Show') }); // this may be unnecessarily complicated, or it may not.  you get to decide. :-)

  e.stopPropagation();
  e.preventDefault();

  return false;
}

function toggle_hilite(id,url) {
    var el = document.getElementById(id);
    if(el) {
        if(el.style.display == "none") {
            el.src = url;
            el.style.display = "block";
        } else {
            el.style.display = "none";
        }
    }
    return false;
}

_EOC_

# LINK GENERATION
# this would be simpler if we used a library like YUI to simplify retrieval and creation of elements, but oh well
$javascript .= <<'_EOC_';

function collectionToArray(col) {
  a = new Array();
  for (i = 0; i < col.length; i++)
    a[a.length] = col[i];
  return a;
}

addEvent(window, 'load', function () {
  var divs = collectionToArray(document.getElementsByTagName('div'));

  for (var i = 0, j = divs.length; i < j; i++) {
    var curr = divs[i];
    if (curr.id && curr.id.match(/smartlink_(\d+)/)) {
      var num = RegExp.$1;

      var toBeRemoved = [ "smartlink_skip_", "smartlink_skipto_" ]; // let it be reusable since this list could conceivably grow :-)
      for (var k = 0, l = toBeRemoved.length; k < l; k++) {
        var id = toBeRemoved[k] + num;
        var elm = document.getElementById(id);
        elm.parentNode.removeChild(elm);
      }

      var p = curr.previousSibling;

      while (p.nodeType != 1) { p = p.previousSibling; } // ignore any whitespace-only nodes

      var text = p.firstChild;
      text.nodeValue = text.nodeValue.replace(/^From/, '- Show');

      var link = document.createElement('a');

      var child;
      while (child = p.firstChild) {
        link.appendChild(child);
      }

      var end = link.lastChild;
      if ((end.nodeType == 3) && (end.nodeValue.search(/:$/) > -1)) {
        end.nodeValue = end.nodeValue.replace(/:$/, ' -');
      }

      link.href = '#';
      link.id = 'smartlink_toggle' + num;
      addEvent(link, 'click', toggle_snippet);

      p.appendChild(link);
      curr.parentNode.insertBefore(p, curr);
      curr.style.display = 'none';
    }

    // explicitly jump to the page anchor (if any) since the code above messes it up
    if (location.hash && location.hash.match(/#.+/)) location.hash = RegExp.lastMatch;
  }
});
_EOC_

=begin private

=head2 add_link

  add_link($linktree, $synopsis, $section, $pattern, $infile, $from, $to);

Side Effects:
 - modifies global C<$link_count>

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
    $link_count++;
}

sub error {
    if ($check) { warn "ERROR: @_\n"; }
}

sub process_t_file ($$) {
    my ($infile, $linktree) = @_;
    open my $in, $infile or
        die "error: Can't open $infile for reading: $!\n";
    my ($setter, $from, $to);
    my $found_link = 0;
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
            $found_link++;
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
            $found_link++;
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
    print "No smartlink found in <$infile>\n" if (defined $print_missing && $found_link == 0);
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
            my @new = ('');;
            if ($line_anchor and $podtree->{$section}->[-1] !~ /^=over\b|^=item\b/) {
                unshift @new, "_LINE_ANCHOR_$.\n";
            }
            push @{ $podtree->{$section} }, @new;
        } elsif (/^\s+(.+)/) {
            $podtree->{$section} ||= [''];
            $podtree->{$section}->[-1] .= $_;
            push @{ $podtree->{$section} }, '';
        } else {
            $podtree->{$section} ||= [''];
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
    $str = "=pod\n\n_LINE_ANCHOR_1\n\n$str" if $line_anchor;
    $str;
}

# convert patterns used in 00-smartlinks.to perl 5 regexes
sub parse_pattern ($) {
    my $pat = shift;
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

    eval { require Pod::Simple::HTML };
    $Pod::Simple::HTML::Perldoc_URL_Prefix  = 'http://perlcabal.org/syn/';
    $Pod::Simple::HTML::Perldoc_URL_Postfix = '.html';
    die "error: Pod::Simple::HTML is not installed on your machine.\n"
        if $@;

    $Pod::Simple::HTML::Content_decl =
        q{<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" >};

    $Pod::Simple::HTML::Doctype_decl =
        qq{<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
           "http://www.w3.org/TR/html4/loose.dtd">\n};

    my $pod2html = new Pod::Simple::HTML;
    $pod2html->index(1);
    $pod2html->html_css($cssfile);
    $pod2html->html_javascript(qq{<script type="text/javascript">$javascript</script>});
    $pod2html->force_title('S'.$syn_id);

    my $html;
    open my $in, '<', \$pod;
    open my $out, '>', \$html;
    $pod2html->parse_from_file($in, $out);

    # substitutes the placeholders introduced by `gen_code_snippet`
    # with real code snippets:
    $html =~ s,(?:<p>\s*)?\b_SMART_LINK_(\d+)\b(?:\s*</p>)?,$snippets[$1],sg;
    fix_line_anchors(\$html) if $line_anchor;
    add_footer(\$html);
    add_user_css(\$html);
    $html
}

sub fix_line_anchors {
    my ($html) = @_;
    my @lineno; # line numbers for each paragraph
    while ($$html =~ /\b_LINE_ANCHOR_(\d+)\b/gsm) {
        push @lineno, $1;
    }
    $$html =~ s{(?:<p>\s*)?\b_LINE_ANCHOR_(\d+)\b(?:\s*</p>)?}{ gen_line_anchors(\@lineno) }sge;
}

sub gen_line_anchors {
    my $list = shift;
    my $curr = shift @$list;
    my $html = '';
    for ($curr .. $list->[0] - 1) {
        $html .= qq{<a id="line_$_"></a>\n};
    }
    $html;
}

sub add_footer {
    my ($html) = @_;
    $$html =~ s{</body>}{
        [ <a href="#__top">Top</a> ] &nbsp;
        [ <a href="http://perlcabal.org/syn/">Index of Synopses</a> ]
        </body>};
}

# isn't there a prettier way to do this?
sub add_user_css {
    my($html) = @_;
    my $user_css = << '.';
<style type="text/css">
.ok {
    color: green;
    font-weight: bold;
}
.nok {
    color: red;
    font-weight: bold;
}
.snip { margin-left: 6px; }
.snipres {
    margin-left: 6px;
    border-width: 0;
}
.smartlink_snippet {
    border: 1px solid;
    padding: 0.2em;
}
</style>
.
    $$html =~ s{(</head>)}{$user_css\n$1};
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
        my $mark = '';
        if ($file_info) {
            chomp;
            if (!exists $file_info->{$i}) {
                $mark = '';
            } elsif ($file_info->{$i}) {
                $mark = qq{<span class="ok"> √ </span>};
                $ok_count++;
            } else {
                $mark = qq{<span class="nok"> × </span>};
                $failed_count++;
            }
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
        #warn "No test results for $file $from to $to";
        $snippet = qq{<pre class="snip">$src</pre>};
    } else {
        $snippet = qq{
            <table class="snipres">
                $src
            </table>
        };
    }

    my $stat;
    if ($test_result) {
        if ($ok_count == 0 && $failed_count == 0) {
            $stat = " (no results)";
        } else {
            $stat = " (<code>$ok_count √, $failed_count ×</code>)";
        }
    } else {
        $stat = '';
    }

    my $nlines = $to - $from + 1;
    my $html_file = $file;
    $html_file =~ s{t/}{};
    my $simple_html = $html_file . ".simple.html";
    my $full_html = $html_file . ".html";
    my $simple_snippet_id = "simple_$snippet_id";

    my $html = <<"_EOC_";
<p>From $file lines $from&ndash;$to$stat:<span id="smartlink_skip_${snippet_id}"> <a href="#smartlink_skipto_${snippet_id}">(skip)</a></span></p>
<div id="smartlink_${snippet_id}" class="smartlink_snippet">
$snippet
</div>
<span id="smartlink_skipto_${snippet_id}">&nbsp;</span>
<span style="color:DarkBlue">Highlighted:
<a href="#" 
onclick="return toggle_hilite('$simple_snippet_id','/~azawawi/html/$simple_html')">small</a>|<a href="/~azawawi/html/$full_html" target="_blank">full</a>
</span>
<iframe id="$simple_snippet_id" style="display:none;" width="100%"></iframe>
_EOC_
    $snippets[$snippet_id] = $html;
    "\n\n_SMART_LINK_$snippet_id\n\n";
}

=begin private

=head2 process_syn

  process_syn($syn, $out_dir, $cssfile, $linktree);

Process synopses one by one.

Side Effects:
  modifies global C<$broken_link_count>

=end private

=cut

sub process_syn ($$$$) {
    my ($infile, $out_dir, $cssfile, $linktree) = @_;
    my $syn_id;
    if ($infile =~ /\bS(\d+)(?:-\w+)+.pod$/) {
        $syn_id = $1;
    } else {
        die "Can't match file '$infile'\n";
    }

    # S26 is in Pod6, we treat it specifically for now.
    if ($syn_id == 26) {
      return if $check;
      eval "use Perl6::Perldoc 0.000005; use Perl6::Perldoc::Parser; use Perl6::Perldoc::To::Xhtml;";
      if ($@) {
          warn "Please install Perl6::Perldoc v0.0.5 from the CPAN to parse S26";
          return;
      }
      eval "use File::Slurp";
      if ($@) {
          warn "Please install File::Slurp from CPAN";
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
      my $preamble = gen_preamble();
      $perldochtml =~ s{<body>}{$&$preamble};
      add_footer(\$perldochtml);

      my $htmfile = "$out_dir/S$syn_id.html";
      warn "info: generating $htmfile...\n";
      open my $out, "> $htmfile" or
          die "Can't open $htmfile for writing: $!\n";
      print $out $perldochtml;
      close $out;
      return;
    }
    my $podtree = parse_pod($infile);
    #print Dump $podtree if $syn_id eq '29';

    #use Data::Dumper;
    #$Data::Dumper::Indent = 1;
    #print Dumper $linktree if $syn_id eq '02';

    my $linktree_sections = $linktree->{"S$syn_id"};
    if (!$linktree_sections && $syn_id != 7) {
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
            $broken_link_count++;
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
                $broken_link_count++;
            }
        }
    }

    # We need this to check invalid smartlinks pointed to non-existent docs:
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

        my $preamble = gen_preamble();
        $html =~ s{<!-- start doc -->}{$&$preamble};
        my $htmfile = "$out_dir/S$syn_id.html";
        warn "info: generating $htmfile...\n";
        open my $out, "> $htmfile" or
            die "Can't open $htmfile for writing: $!\n";
        print $out $html;
        close $out;
    }

    #warn "$syn_id: $infile\n";
}

sub gen_preamble {
     my ($sec, $min, $hour, $mday, $mon, $year) = gmtime;
     $year += 1900; $mon += 1;
     my $time = sprintf "%04d-%02d-%02d %02d:%02d:%02d GMT",
         $year, $mon, $mday, $hour, $min, $sec;
     my $smoke_info = $smoke_rev ?
         ", <a href=\"http://perlcabal.org/smoke.html\">pugs-smoke</a> <strong>$smoke_rev</strong>"
         :
         '';
     ## $smoke_info
    return qq{
            <I>This page was generated at $time.<br/>
            (<a href="http://svn.pugscode.org/pugs/docs/Perl6/spec/">syn</a> <strong>$pugs_rev</strong>$smoke_info)</I>
            &nbsp; [ <a href="http://perlcabal.org/syn/">Index of Synopses</a> ] <br/>
            <a id='__top'></a>
     };
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
    my ($syn_dir, $out_dir, $help, $cssfile, $fast, $yml_file, $index, $dir);
    GetOptions(
        'check'       => \$check,
        'missing'     => \$print_missing,
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

    $link_count = 0;
    $broken_link_count = 0;

    $out_dir ||= '.';
    mkdir $out_dir if !-d $out_dir;
    create_index($out_dir) if $index;

    my @t_files = map glob, @ARGV;
    push @t_files, list_t_files($dir) if $dir;
    #use Data::Dumper;
    #print Dumper \@t_files;

    my $linktree = {};
    for my $t_file (@t_files) {
        process_t_file($t_file, $linktree);
    }
    #print Dump($linktree);

    my $pugs_syn_dir = "$FindBin::Bin/../docs/Perl6/Spec";
    $syn_dir ||= $pugs_syn_dir;

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
    ### $pugs_rev

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
        $smoke_rev = $data->{revision};
        $pugs_rev ||= $smoke_rev;
        $smoke_rev = $smoke_rev ? "r$smoke_rev" : 'unknown';
        warn "info: pugs smoke is at $smoke_rev.\n";
    }

    $pugs_rev = $pugs_rev ? "r$pugs_rev" : 'unknown';
    warn "info: pugs test suite is at $pugs_rev.\n";

    my @syns = map glob, "$syn_dir/*.pod";
    for my $syn (@syns) {
        process_syn($syn, $out_dir, $cssfile, $linktree);
    }

    # check for pending smartlinks:
    while (my ($syn, $linktree_sections) = each %$linktree) {
        for my $links (values %$linktree_sections) {
            for my $link (@$links) {
                my ($file, $lineno) = @{ $link->[1] };
                error("$file: line $lineno: smartlink pointing to " .
                    "an unknown synopsis ($syn)"),
                $broken_link_count++;
            }
        }
    }

    warn "info: $link_count smartlinks found and $broken_link_count broken.\n";
    if (!$check and $broken_link_count > 0) {
        warn "hint: use the --check option for details on broken smartlinks.\n";
    }
    exit;
}

sub create_index($) {
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

{
    my @my_t_files;
    sub list_t_files($) {
        my ($dir) = @_;
        #warn "DIR is ", $dir, "\n";
        find(\&_list_t_files, $dir);
        return @my_t_files;
    }
    sub _list_t_files {
        if ('.t' eq substr($_, -2) and -f $_) {
            push @my_t_files, $File::Find::name;
        }
    }
}

main() if ! caller;

1;
__END__

=head1 NAME

smartlinks.pl - The successor to catalog_tests.pl.

=head1 SYNOPSIS

  smartlinks.pl t/*/*.t t/*/*/*.t
  smartlinks.pl --dir t
  smartlinks.pl --css foo.css --out-dir=public_html t/syntax/*.t
  smartlinks.pl --check t/*/*.t t/*/*/*.t
  smartlinks.pl --check t/some/test.t
  smartlinks.pl --missing t/*/*.t t/*/*/*.t

=head1 Design Decisions

=over

=item *

This script should have as few non-core module dependencies as possible.

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

The section name should be copied verbatim from the POD
(usually after C<=head>), including any POD tags like C<...>
and punctuations. The sections, however, are not supposed to be nested.
That is, a C<=head1> won't really contain a C<=head2>; they're disjoint
according to the current implementation.

The smartlinks also have a weird (also important) extension:
you can specify some keyphrases, to skip forward from the linked
section, so the smartlink is put into
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

Smartlinks in .t files can be preceded by nothing but spaces or "#", furthermore,
there should be no trailing text on the same line, otherwise
they can't be recognized by tools. Here're some *invalid* samples:

    # the following smartlink is INVALID!!!
    # Link is L<S04/Str>

    # the following smartlink is INVALID TOO!!!
    # L<S04/Str> # This is a comment

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

By doing this, one can effectively link one group of tests to
multplie places in the Synopses, leading to m-to-n correspondance.

smartlinks.pl can take care of this kind of special cases.

You can put a URL to create a generic link:

  L<"http://groups.google.de/group/perl.perl6.language/msg/07aefb88f5fc8429">

or without quotes:

  L<http://www.nntp.perl.org/group/perl.perl6.language/26071>

Try running 'grep -r "L<" t/' to see some examples, or look at
F<t/syntax/comments.t>.

There were also some legacy smartlinks using the following syntax:

   L<S04/"section name" /regex/>
   L<<S04/"section name" /regex/>>

They're no longer supported by util/smartlinks.pl. Use the current syntax.

=head1 Basic Algorithm

=over

=item 1.

We scan over all the specified .t files; collect smartlinks and positional
info about the test code snippets as we go. When all these work have been finished,
we obtain a tree structure, which is named C<$linktree> in the source code.

To make this tree minimal, we only store the .t file name and line numbers, rather
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

We process the synopsis .pod files one by one and generate
HTML files integrated with test code snippets using the
C<$linktree> structure discussed above.

This is mostly done by sub C<process_syn>.

Because it is an enormous step, we can further divide it into several
sub steps:

=over

=item *

We parse each .pod into a tree, which is known as C<$podtree> in the
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

We look up every related smartlink from every C<$podtree>, generate .t code
snippets along the way, and insert placeholders (like "_SMART_LINK_3" into
the corresponding C<$podtree>. (See subs C<parse_pattern>, C<process_paragrph>,
and C<gen_code_snippet>.)

=item *

Now we emit Pod source back from the modified $C<podtree>'s. (See sub C<emit_pod>.)

=item *

After that, we generate HTML source from the Pod source with snippet placeholders
using L<Pod::Simple::HTML>. (See sub C<gen_html>.)

=item *

At last, we replace every snippet placeholders in the HTML source with the real
snippet code (also in HTML format).

=back

=back

=head1 SEE ALSO

=over

=item *

F<t/README> in the Pugs source tree.

=item *

The articles on the Pugs blogs:

L<http://pugs.blogs.com/pugs/2006/08/integrating_the.html>

L<http://pugs.blogs.com/pugs/2006/09/check_smoke_res.html>

L<http://pugs.blogs.com/pugs/2006/09/the_benefits_of.html>

=item *

Consult F<util/t/00-smartlinks.t> in the Pugs source tree for unit
tests and usage of the internal API.

=item *

See F<util/t/01-smartlinks.t> for a corresponding regression test
suite harness.

=item *

The synopses in L<http://perlcabal.org/syn> are generated by this script.

=back

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

=head1 COPYRIGHT

Copyright (c) 2006, 2007 by the Pugs Team.

