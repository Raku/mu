package Text::SmartLinks;
use strict;
use warnings;
use 5.006;

our $VERSION = '0.01';

use File::ShareDir;
use FindBin;
use File::Spec;
use File::Path qw(mkpath);
use File::Basename qw(dirname basename);
use File::Slurp;
use CGI;
use Pod::Simple::HTML;
use Data::Dumper;

use base 'Class::Accessor';
__PACKAGE__->mk_accessors(qw(check count cssfile line_anchor 
    out_dir print_missing smoke_rev test_files version wiki));

# TODO: treat non-breaking spaces as breaking spces in the smart links
# in docs/Perl6/Spec/S03-operators.pod the section called
# "Changes to Perl 5 operators" has a non-breaking space between Perl and 5
# while the smartlink pointing to it does not have. This should be acceptable.
# probably by replacing every space by [\s$nbsp]+ in the regex.
# use charnames ":full";
# my $nbsp = "\N{NO-BREAK SPACE}";

=head1 NAME

Text::SmartLinks - connecting test files with pod documentation

=head1 SYNOPSIS

  smartlinks.pl t/*/*.t t/*/*/*.t
  smartlinks.pl --dir t
  smartlinks.pl --css foo.css --out-dir=public_html t/syntax/*.t
  smartlinks.pl --check t/*/*.t t/*/*/*.t
  smartlinks.pl --check t/some/test.t
  smartlinks.pl --missing t/*/*.t t/*/*/*.t
  
If in the root directory of a CPAN package type the following:

  smartlinks.pl --pod-dir lib/ --dir t/ --out-dir html/ --index

In the root of Text::SmartLinks type in the following:

  perl -Ilib script/smartlinks.pl --pod-dir lib/ --dir t/ --out-dir html/ --index

=head1 DESCRIPTION

The plan is to change the Text::SmartLinks module and write a new 
smartlinks.pl script so it will be usable in any Perl 5 or Perl 6 
project to generate the HTML pages combining the POD content from
the .pod and .pm files and test scripts.

In addition the script should be able to generate further reports
in HTML format that help the developers.

The usage should default to parsing the files in lib/ for documentation
and the .t files in the t/ subdirectory.

=head1 Requirements

Process both Perl 5 and Perl 6 test files in an arbitraty directory
to collect smartlinks.
Default should be either the local t/ directory or the t/spec directory 
of Pugs (for historical reasons).

Process .pod and .pm files (but maybe other files as well) with either Perl 5 
or Perl 6 pod in them and with possibly also code in them.

Smartlinks should be able to say the name of the document where they link to.

    L<Smolder/Text of head1>
    L<Smolder::Util/Text o head2>

Map to either Smolder.pm or Smolder.pod and Smolder/Util.pm or Smolder/Util.pod

Need special cases for the Perl 6 documentation so the smartlinks can
have the following links pointing to S06-routines.pod and 
S32-setting-library/Abstraction.pod

    L<S06/Blocks>
    L<S32::Abstraction>


=head1 Old Design Decisions

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

   # L<syn/sec/key phrases>>

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

To see some examples, or look at the *.t files in the t/ directory of this project.

There were also some legacy smartlinks using the following syntax:

   L<S04/"section name" /regex/>
   L<<S04/"section name" /regex/>>
   L<<S04/"section name">>

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

This is mostly done by sub C<process_pod_file>.

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
the corresponding C<$podtree>. (See subs C<parse_pattern>, C<process_paragraph>,
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

The synopses in L<http://perlcabal.org/syn> are generated by this script.

=back

=head1 METHODS

=cut

=head2 new

Constructor, can get a HASH reference as it is a base class
of L<Class::Accessor>

=cut

sub new {
    my $class = shift;

    my $self = $class->SUPER::new(@_);

    $self->{link_count}        = 0;
    $self->{broken_link_count} = 0;
    $self->{snippet_id}        = 0;
    $self->{test_files_missing_links} = [];
    $self->{out_dir}          ||= '.';
    $self->{errors}   = [];
    $self->{X}        = {};
    $self->{C}        = {};
    
    $self->{invalid_link}      = 0;

    return $self;
}

=head2 process_test_files

Gets a list of .t test files, calls L<process_t_file> on each on of them.

=cut

sub process_test_files {
    my ($self, @t_files) = @_;

    $self->{test_files} = \@t_files;

    for my $t_file (@t_files) {
        my $links = $self->process_t_file($t_file);
        if ($links) {
            print "Found $links links in <$t_file>\n" if defined $self->count;
        } else {
            print "No smartlink found in <$t_file>\n" if defined $self->print_missing;
            print "\"$t_file\"<http://github.com/perl6/roast/blob/master/$t_file>\n" if defined $self->wiki;
            push @{ $self->{test_files_missing_links} }, $t_file;
        }
    }
}

=head2 process_t_file

Gets a path to a .t file, reads line by line and collects
the smartlinks in it to a hash structure using the 
C<add_link> function.

=cut

sub process_t_file {
    my ($self, $infile) = @_;

    open my $in, $infile or
        die "error: Can't open $infile for reading: $!\n";
    my ($setter, $from, $to);
    my $found_link = 0;
    while (<$in>) {
        chomp;
        my $new_from;
        my ($synopsis, $section, $pattern);
        if (m{L<"?http://}) {
            # TODO shall we also collect the http links for later reuse?
            next;
        }
        elsif (m{^ \s* \# \s* (L<<+)}xoi) {
            $self->error("Legacy smartlink. Use L< instead of $1 in line $. '$_'  in file '$infile'");
            $self->{invalid_link}++;
            next;
        }
        elsif (m{^ \s* \# \s* L< ([^/]+) / ([^/]+) >\s*$}xo) {
            ($synopsis, $section) = ($1, $2);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            #warn "$synopsis $section" if $synopsis eq 'S06';
            $new_from = $.;
            $to = $. - 1;
            $found_link++;
        }
        # extended and multiline smartlinks
        elsif (m{^ \s* \# \s* L(<) ([^/]+) / ([^/]+) / (.*) }xo) {
            #warn "$1, $2, $3\n";
            my $brackets;
            ($brackets, $synopsis, $section, $pattern) = ($1, $2, $3, $4);
            $brackets = length($brackets);
            $section =~ s/^\s+|\s+$//g;
            $section =~ s/^"(.*)"$/$1/;
            if (!$section) {
                $self->error("$infile: line $.: section name can't be empty.");
            }
            $pattern =~ s/^\s+|\s+$//g;
            if (substr($pattern, -1, 1) ne '>') {
                $_ = <$in>;
                s/^\s*\#?\s*|\s+$//g;
                if (!s/>{$brackets}$//) {
                    $self->error("$infile: line $.: smart links must terminate in the second line.");
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
        # there are some # L<"http://... links that we should skip for now
        # and not even report them as errors.
        # any other L< thing should be reported.
        elsif (m{^ \s* \# \s* L<}xoi) {
            $self->error("Could not parse smartlink in line $. '$_'  in file '$infile'");
            $self->{invalid_link}++;
            next;
        }
        else {
            next;
        }

        #warn "*$synopsis* *$section*\n";
        if ($from and $from == $to) {
            my $old_setter = $setter;
            my $old_from = $from;
            $setter = sub {
                $self->add_link($synopsis, $section, $pattern, $infile, $_[0], $_[1]);
                $old_setter->($old_from, $_[1]);
                #warn "$infile - $old_from ~ $_[1]";
            };
            #warn "$infile - $from ~ $to";
        } else {
            $setter->($from, $to) if $setter and $from;
            $setter = sub {
                $self->add_link($synopsis, $section, $pattern, $infile, $_[0], $_[1]);
            };
        }
        $from = $new_from;
    }
    $setter->($from, $.) if $setter and $from;
    close $in;
#   print "No smartlink found in <$infile>\n" if (defined $print_missing && $found_link == 0);
    return $found_link;
}

=begin private

=head2 add_link

  add_link($synopsis, $section, $pattern, $infile, $from, $to);

=end private

=cut

# TODO add tests
sub add_link  {
    my ($self, $synopsis, $section, $pattern, $t_file, $from, $to) = @_;
    
    if ($from == $to) {
        warn "WARNING: empty snippet detected at $t_file (line $from ~ $to).\n";
    }
    $self->{linktree}->{$synopsis} ||= {};
    $self->{linktree}->{$synopsis}->{$section} ||= [];
    if ($pattern and substr($pattern, -1, 1) eq '/') { $pattern = "/$pattern"; }
    push @{ $self->{linktree}->{$synopsis}->{$section} },
        [$pattern => [$t_file, $from, $to]];
        
    return $self->link_count_inc;
}

=head2 parse_pattern

Convert patterns used in 00-smartlinks.to perl 5 regexes

=cut

sub parse_pattern {
    my ($self, $pat) = @_;

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

=head2 process_paragraph

Process paragraphs of the pod file: unwrap lines, strip POD tags, and etc.

=cut

sub process_paragraph {
    my ($self, $str) = @_;

    # unwrap lines:
    $str =~ s/\s*\n\s*/ /g;

    # strip POD tags:
    # FIXME: obviously we need a better way to do this:
    $str =~ s/[LCFIB]<<<\s+(.*?)\s+>>>/$1/g;
    $str =~ s/[LCFIB]<<\s+(.*?)\s+>>/$1/g;
    $str =~ s/[LCFIB]<(.*?)>/$1/g;
    $str;
}

=head2 gen_code_snippet

Gets a triplet of [file, from, to] and generates an HTML 
snippet from that section of the given file.


Note that this function has been optimized for space rather
than time.

=cut

sub gen_code_snippet {
    my ($self, $location) = @_;
    my ($file, $from, $to) = @$location;
    #warn "gen_code_snippet: @$location\n";
    open my $in, $file or
        die "Can't open $file for reading: $!\n";

    # Strip leading realpath so the names start at t/
    $file =~ s{.*?/t/}{t/};

    my $i = 1;
    my $src;
    my $file_info;
    $file_info = $self->{test_result}->{$file} if $self->{test_result};
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

    $src =~ s/\n{3,}/\n\n/sg;
    $src =~ s/\n+$//sg;

    my $snippet_id = $self->snippet_id_inc;

    #warn $snippet_id;
    #warn "$file $to $from";
    warn "NOT DEFINED!!! @$location $snippet_id" if !defined $src;

    my $snippet;
    if (!$self->{test_result}) {
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
    if ($self->{test_result}) {
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
<div class="smartlink">
  <p class="smartlink-file">From <code>$file</code> lines <code>$from&ndash;$to</code>$stat:<span id="smartlink_skip_${snippet_id}"> <a href="#smartlink_skipto_${snippet_id}">(skip)</a></span>
  - <a href="#" 
onclick="return toggle_hilite('$simple_snippet_id','/~azawawi/html/$simple_html')"><img src="hilite-small.png" border="0" width="16" height="16"></a> <a href="/~azawawi/html/$full_html" target="_blank"><img src="hilite-full.png" border="0" width="22" height="22"></a>
</p>

<div id="smartlink_${snippet_id}" class="smartlink_snippet" style="display:none">
$snippet
</div>
<span id="smartlink_skipto_${snippet_id}">&nbsp;</span>
<iframe id="$simple_snippet_id" style="display:none;" width="100%"></iframe>
</div>
_EOC_
    $self->set_snippet($snippet_id, $html);

    return "\n\n_SMART_LINK_$snippet_id\n\n";
}

=head2 get_javascript

Returns the content of the smartlink.js file.
Probably we should just copy the .js file to the html directory
and not embed it.

=cut

sub get_javascript {

    # for the test scripts in t/ and the smartlinks.pl in script/  directory
    my $file = File::Spec->catfile($FindBin::Bin, '..', 'share', 'smartlinks.js');
    
    if (not -e $file) {
        # for smarlinks.pl in utils/ directory of Pugs if Text::SmartLinks is not installed
        $file = File::Spec->catfile($FindBin::Bin, 'Text-SmartLinks', 'share', 'smartlinks.js');
    }

    # installed version of the file
    if (not -e $file) {
        $file = File::Spec->catfile(File::ShareDir::dist_dir('Text-SmartLinks'), 'smartlinks.js');
    }
    if (not $file) {
        warn "Could not find 'smartlinks.js'\n";
        return '';
    }
    #warn $file;
    if (open my $fh, '<', $file) {
        local $/ = undef;
        return <$fh>;
    }
    warn "could not open '$file'";
    return '';
}

sub test_files_missing_links {
    return @{ $_[0]->{test_files_missing_links} };
}


sub emit_pod {
    my ($self, $podtree) = @_;

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
    $str = "=pod\n\n_LINE_ANCHOR_1\n\n$str" if $self->line_anchor;

    return $str;
}

# split a C<>, C<< >>, C<<< >>>, or C«» code into ($tag, $value)
sub split_code {
    local ($_) = @_;
    my $tag = substr $_, 0, 1;
    s/^\w<<<\s+(.+?)\s+>>>$/$1/
    or s/^\w<<\s+(.+?)\s+>>$/$1/
    or s/^\w<(.+?)>$/$1/
    or s/^\w«(.+?)»$/$1/;
    return ($tag, $_);
}

sub parse_pod {
    my ($self, $pod, $url) = @_;
    my $podtree = {};
    my $section;
    my $line_no = 0;
    foreach (@$pod) {
        $line_no++;
        # collect the X<> and C<> tags
        while (/([CX](?:
            (?:<<<\s+.+?\s+>>>)
            |
            (?:<<\s+.+?\s+>>)
            |
            (?:<.+?>)
            |
            (?:«.+?»)
        ))/gx) {
            my ($tag, $value) = split_code($1);
            
            if ($section) {
                (my $s = $section) =~ s/ +/_/g;
                $self->{$tag}{$value}{"$url#$s"}++;
            } else {
                $self->{$tag}{$value}{$url}++;
            }
        }
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
            if ($self->line_anchor and @{ $podtree->{$section} } and $podtree->{$section}->[-1] !~ /^=over\b|^=item\b/) {
                unshift @new, "_LINE_ANCHOR_$line_no\n";
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
    $podtree;
}


sub process_yml_file {
    my ($self, $yml_file) = @_;
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
        for my $file ($tap->test_files) {
            #warn "  $file...\n";
            (my $fname = $file->name) =~ s{.*?/t/}{t/};
            my %file_info;
            $self->{test_result}->{$fname} = \%file_info;
            for my $case ($file->cases) {
                next if $case->skipped or !$case->test_line;
                $file_info{$case->test_line} = $case->actual_ok;
            }
        }
        #YAML::Syck::DumpFile('test_result.yml', $self->{test_result});
        my $smoke_rev = $data->{revision};
        $self->smoke_rev($smoke_rev);
        $smoke_rev = $smoke_rev ? "r$smoke_rev" : 'unknown';
        warn "info: pugs smoke is at $smoke_rev.\n";
    }
}


sub gen_html {
    my ($self, $pod, $title) = @_;

    $Pod::Simple::HTML::Perldoc_URL_Prefix  = 'http://perlcabal.org/syn/';
    $Pod::Simple::HTML::Perldoc_URL_Postfix = '.html';

    $Pod::Simple::HTML::Content_decl =
        q{<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" >};

    $Pod::Simple::HTML::Doctype_decl =
        qq{<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
           "http://www.w3.org/TR/html4/loose.dtd">\n};

    my $pod2html = new Pod::Simple::HTML;
    $pod2html->index(1);
    $pod2html->html_css($self->cssfile);
    my $javascript = $self->get_javascript();
    $pod2html->html_javascript(qq{<script type="text/javascript">$javascript</script>});
    $pod2html->force_title($title);

    my $html;
    open my $in, '<', \$pod;
    open my $out, '>', \$html;
    $pod2html->parse_from_file($in, $out);

    # substitutes the placeholders introduced by `gen_code_snippet`
    # with real code snippets:
    $html =~ s,(?:<p>\s*)?\b_SMART_LINK_(\d+)\b(?:\s*</p>)?,$self->get_snippet($1),sge;
    $self->fix_line_anchors(\$html) if $self->line_anchor;
    $self->add_footer(\$html);
    $self->add_user_css(\$html);
    
    return $html
}



sub _gen_line_anchors {
    my $list = shift;
    my $curr = shift @$list;
    my $html = '';
    return $html if not defined $list->[0];
    for ($curr .. $list->[0] - 1) {
        $html .= qq{<a id="line_$_"></a>\n};
    }
    $html;
}

sub fix_line_anchors {
    my ($self, $html) = @_;
    my @lineno; # line numbers for each paragraph
    while ($$html =~ /\b_LINE_ANCHOR_(\d+)\b/gsm) {
        push @lineno, $1;
    }
    $$html =~ s{(?:<p>\s*)?\b_LINE_ANCHOR_(\d+)\b(?:\s*</p>)?}{ _gen_line_anchors(\@lineno) }sge;
}


sub add_footer {
    my ($self, $html) = @_;
    $$html =~ s{</body>}{
        [ <a href="#__top">Top</a> ] &nbsp;
        [ <a href="http://perlcabal.org/syn/">Index of Synopses</a> ]
        </body>};
}

# isn't there a prettier way to do this?
sub add_user_css {
    my ($self, $html) = @_;
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

sub gen_preamble {
    my ($self) = @_;

    my ($sec, $min, $hour, $mday, $mon, $year) = gmtime;
    $year += 1900; $mon += 1;
    my $time = sprintf "%04d-%02d-%02d %02d:%02d:%02d GMT",
        $year, $mon, $mday, $hour, $min, $sec;
    my $smoke_rev = $self->smoke_rev;
    my $smoke_info = $smoke_rev ?
        qq(, <a href="http://perlcabal.org/smoke.html">pugs-smoke</a> <strong>$smoke_rev</strong>)
        :
         '';
    my $pugs_rev = $self->version;
    $pugs_rev = $pugs_rev ? "r$pugs_rev" : 'unknown';
    $pugs_rev ||= $smoke_rev;

    return qq{
            <I>This page was generated by Text::SmartLinks v$VERSION at $time.<br/>
            (<a href"http://github.com/perl6/specs/">syn</a> <strong>$pugs_rev</strong>$smoke_info)</I>
            &nbsp; [ <a href="http://perlcabal.org/syn/">Index of Synopses</a> ] <br/>
            <a id='__top'></a>
     };
}

sub create_stats_page {
    my ($self) = @_;
    my $html = "<html><head><title>Stats</title></head><body>";
    $html .= $self->gen_preamble;

    my $test_file_count = scalar @{ $self->test_files };
    my $test_files_missing_links = scalar $self->test_files_missing_links;
    $html .= sprintf("info: %s smartlinks found and %s broken in $test_file_count test files ($test_files_missing_links test files had no links).\n",
        $self->link_count ,$self->broken_link_count);
    #if (!$self->check and $self->broken_link_count > 0) {
    #    warn "hint: use the --check option for details on broken smartlinks.\n";
    #}
    
    if ($self->test_files_missing_links) {
        $html .= "<h2>Test files without links</h2>\n<ul>";
        foreach my $file ($self->test_files_missing_links) {
            $html .= "<li>$file</li>\n";
        }
        $html .= "</ul>\n";
    }
    if (@{ $self->{errors} }) {
        $html .= "<h2>Errors</h2>\n<ul>";
        foreach my $e (@{ $self->{errors} }) {
            $html .= "<li>" . CGI::escapeHTML(join "", @$e) . "</li>\n";
        }
        $html .= "</ul>\n";
    }
    
    
    $html .= "</body></html>";
    write_file(File::Spec->catfile($self->out_dir, 'stats.html'), $html);

    return;
}

sub outfile_name {
    my ($self, $infile) = @_;

    (my $syn_id = $infile) =~ s/\.(pod|pm)$//;
    $syn_id =~ s{[/\\]}{::}g;

    # special case for Perl 6 Synopsis
    if ($ENV{PUGS_SMARTLINKS}) {
        $syn_id  =~ s{(S\d+)-[^:]+}{$1};
    }
    (my $outfile = $syn_id) =~ s{::}{/}g;
    $outfile .= ".html";
    return ($outfile, $syn_id);
}

=begin private

=head2 process_pod_file

  process_pod_file($syn);

Process synopses one by one.

=end private

=cut

sub process_pod_file {
    my ($self, $root, $infile) = @_;

    my ($outfile, $syn_id) = $self->outfile_name($infile);
    my $out_dir = $self->out_dir;

    my @pod = read_file(File::Spec->catfile($root, $infile));
    if (grep /^=begin pod/, @pod) {
        $self->process_perl6_file(
                \@pod,
                File::Spec->catfile($out_dir, $outfile));
    } else {
        $self->process_perl5_file(
                \@pod,
                $outfile,
                File::Spec->catfile($out_dir, $outfile),
                $syn_id);
    }
}

sub process_perl5_file {
    my ($self, $pod, $url, $outfile, $syn_id) = @_;

    my $podtree = $self->parse_pod($pod, $url);
    my $linktree_sections = $self->{linktree}->{$syn_id};

    foreach my $section_name (sort keys %$linktree_sections) {
        my $links = $linktree_sections->{$section_name};
        my @links = @$links;
        my $paras = $podtree->{$section_name};
        if (!$paras) {
            foreach my $link (@$links) {
                my ($t_file, $from) = @{ $link->[1] };
                $self->error("$t_file: line $from: section '$section_name' not found in $syn_id.");
                $self->broken_link_count_inc;
            }
            next;
        }
        for my $link (reverse @links) {
            my ($pattern, $location) = @$link;
            my $i = 0;
            if (!$pattern) { # match the whole section
                if (!$self->check) {
                    unshift @$paras, $self->gen_code_snippet($location);
                    $i = 1;
                }
                next;
            }
            my $regex = $self->parse_pattern($pattern);
            my $matched;
            while ($i < @$paras) {
                my $para = $paras->[$i];
                next if !$para or $para =~ /\?hide_quotes=no/;
                if ($self->process_paragraph($para) =~ /$regex/) {
                    if (!$self->check) {
                        splice @$paras, $i+1, 0, $self->gen_code_snippet($location);
                        $i++;
                    }
                    $matched = 1;
                    last;
                }
            } continue { $i++ }
            if (!$matched) {
                my ($file, $lineno) = @$location;
                $self->error("$file: line $lineno: pattern '$pattern' failed to match any paragraph in L<${syn_id}/${section_name}>.");
                $self->broken_link_count_inc;
            }
        }
    }

    # We need this to check invalid smartlinks pointed to non-existent docs:
    delete $self->{linktree}->{$syn_id};

    if (!$self->check) {
        my $pod      = $self->emit_pod($podtree);
        my $html     = $self->gen_html($pod, $syn_id);
        my $preamble = $self->gen_preamble();
        $html =~ s{<!-- start doc -->}{$&$preamble};
        warn "info: generating $outfile...\n";
        mkpath dirname($outfile);
        write_file($outfile, $html);
    }
}

sub process_perl6_file {
    my ($self, $pod, $outfile) = @_;

    return if $self->check;
    eval "use Perl6::Perldoc 0.000005; use Perl6::Perldoc::Parser; use Perl6::Perldoc::To::Xhtml;";
    if ($@) {
        warn "Please install Perl6::Perldoc v0.0.5 from the CPAN to generate $outfile";
        return;
    }

    my $toc = "=TOC\nP<toc:head1 head2 head3>\n\n";
    my $pod6 = $toc . join "", @$pod;

    my $perldochtml = Perl6::Perldoc::Parser->parse(
        \$pod6, {all_pod => 1}
    )->report_errors()->to_xhtml(
        {full_doc => {title => basename($outfile)}}
    );
    my $css = $self->cssfile;
    $perldochtml =~ s{</head>}{<link rel="stylesheet" type="text/css" title="pod_stylesheet" href="$css">\n$&};
    my $preamble = $self->gen_preamble();
    $perldochtml =~ s{<body>}{$&$preamble};
    $self->add_footer(\$perldochtml);

    warn "info: generating $outfile...\n";
    write_file($outfile, $perldochtml);
    return;
}

sub report_stats {
    my ($self) = @_;

    my $test_file_count = scalar @{ $self->{test_files} };
    my $test_files_missing_links = scalar $self->test_files_missing_links;
    warn sprintf("info: %s smartlinks found and %s broken in $test_file_count test files ($test_files_missing_links test files had no links).\n",
        $self->link_count ,$self->broken_link_count);
    if (!$self->check and $self->broken_link_count > 0) {
        warn "hint: use the --check option for details on broken smartlinks.\n";
    }
}

sub report_broken_links {
    my ($self) = @_;

    foreach my $syn (sort keys %{ $self->{linktree} }) {
        my $linktree_sections = $self->{linktree}{$syn};
        for my $section (sort keys %$linktree_sections) {
            my $links = $linktree_sections->{$section};
            for my $link (@$links) {
                my ($file, $lineno) = @{ $link->[1] };
                $self->error("$file: line $lineno: smartlink pointing to an unknown synopsis ($syn) section $section"),
                $self->broken_link_count_inc;
            }
        }
    }
}
sub create_x_page {
    my ($self, $tag) = @_;
    my $out_dir = $self->out_dir;
    my $html = qq(<html><head><title>Indexing $tag tags</title></head><body>\n);
    $html .= $self->gen_preamble;
    $html .= "<ul>\n";
    foreach my $key (sort keys %{ $self->{$tag} }) {
        $html .= "<li>$key<ul>";
        #$html .= join ", ", map {qq(<a href="$_">$_</a>)} @{ $self->{$tag}{$key} };
        $html .= join "", map {qq(<li><a href="$_">$_</a></li>)} sort keys %{ $self->{$tag}{$key} };
        $html .= "</ul></li>\n";
    }
    $html .= "</ul>\n";
    $html .= qq(</body></html>);
    write_file(File::Spec->catfile($out_dir, "index_$tag.html"), $html);

	# temporary need
	unlink File::Spec->catfile($out_dir, "x.html");
}

sub create_index {
    my ($self) = @_;
    my $out_dir = $self->out_dir;

    my $html = qq(<html><head><title>Documentation</title></head><body>\n);
    foreach my $file (sort @{ $self->{docs} }) {
        my ($outfile, $syn_id) = $self->outfile_name($file);
        $html .= qq(<a href="$outfile">$file</a><br />\n);
    }
    $html .= qq(<hr><a href="stats.html">stats and errors</a>);
    $html .= qq(</body></html>);

    write_file(File::Spec->catfile($out_dir, 'index.html'), $html);
    return;
}


sub snippet_id_inc    { $_[0]->{snippet_id}++ };
sub snippet_id        { $_[0]->{snippet_id} };

=head2 link_count

link_count_inc increments the link counter.

link_count returns the current number of links.

=cut

sub link_count_inc { $_[0]->{link_count}++ };
sub link_count     { $_[0]->{link_count} };

sub broken_link_count_inc { $_[0]->{broken_link_count}++ };
sub broken_link_count     { $_[0]->{broken_link_count} };

sub error {
    my $self = shift;

    push @{ $self->{errors} }, [@_];
    if ($self->check) { warn "ERROR: @_\n"; }
}

sub set_snippet {
    my ($self, $id, $str) = @_;
    $self->{snippets}[$id] = $str;
}
sub get_snippet {
    my ($self, $id) = @_;
    return $self->{snippets}[$id];
}

=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

Current maintainer: The Pugs team

=head1 COPYRIGHT

Copyright (c) 2006 - 2010 by the Pugs Team.

=head1 LICENSE

Text::SmartLinks is free software; you can redistribute it and/or modify it under the
terms of the Artistic License 2.0.  (Note that, unlike the Artistic License
1.0, version 2.0 is GPL compatible by itself, hence there is no benefit to
having an Artistic 2.0 / GPL disjunction.)

=cut


1;
