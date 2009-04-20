package Smart::Links;
use strict;
use warnings;
use 5.006;

our $VERSION = '0.01';

use File::ShareDir;
use FindBin;
use base 'Class::Accessor';
__PACKAGE__->mk_accessors(qw(check count cssfile line_anchor 
	out_dir print_missing smoke_rev wiki));

=head1 NAME

Smart::Links - connecting test files with pod documentation

=head1 SYNOPSIS

  smartlinks.pl t/*/*.t t/*/*/*.t
  smartlinks.pl --dir t
  smartlinks.pl --css foo.css --out-dir=public_html t/syntax/*.t
  smartlinks.pl --check t/*/*.t t/*/*/*.t
  smartlinks.pl --check t/some/test.t
  smartlinks.pl --missing t/*/*.t t/*/*/*.t
  
=head1 DESCRIPTION

The plan is to change the Smart::Links module and write a new 
smartlinks.pl script so it will be usable in any Perl 5 or Perl 6 
project to generate the HTML pages combining the POD content from
the .pod and .pm files and test scripts.

In addition the script should be able to generate further reports
in HTML format that help the developers.

The usage should default to parsing the files in lib/ for documentation
and the .t files in the t/ subdirectory.


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

=head1 DESCRIPTION

=cut

sub new {
	my $class = shift;

	my $self = $class->SUPER::new(@_);

	$self->{link_count}        = 0;
	$self->{broken_link_count} = 0;
    $self->{snippet_id}        = 0;
	$self->{test_files_missing_links} = [];
    $self->{out_dir}          ||= '.';

	return $self;
}


# convert patterns used in 00-smartlinks.to perl 5 regexes
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

# process paragraphs of the synopses: unwrap lines, strip POD tags, and etc.
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

sub get_javascript {

	# for the test scripts in t/ and the smartlinks.pl in script/  directory
	my $file = File::Spec->catfile($FindBin::Bin, '..', 'share', 'smartlinks.js');
	
	if (not -e $file) {
		# for smarlinks.pl in utils/ directory of Pugs if Smart::Links is not installed
		$file = File::Spec->catfile($FindBin::Bin, 'Smart-Links', 'share', 'smartlinks.js');
	}

	# installed version of the file
	if (not -e $file) {
		$file = File::Spec->catfile(File::ShareDir::dist_dir('Smart::Links'), 'smartlinks.js');
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

sub process_test_files {
	my ($self, @t_files) = @_;
    for my $t_file (@t_files) {
        my $links = $self->process_t_file($t_file);
        if ($links) {
			print "Found $links links in <$t_file>\n" if defined $self->count;
        } else {
            print "No smartlink found in <$t_file>\n" if defined $self->print_missing;
            print "\"$t_file\"<http://svn.pugscode.org/pugs/t/spec/$t_file>\n" if defined $self->wiki;
			push @{ $self->{test_files_missing_links} }, $t_file;
        }
    }
}
sub test_files_missing_links {
	return @{ $_[0]->{test_files_missing_links} };
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

sub parse_pod {
    my ($self, $infile) = @_;
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
            if ($self->line_anchor and $podtree->{$section}->[-1] !~ /^=over\b|^=item\b/) {
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
                $self->error("$infile: line $.: section name can't be empty.");
            }
            $pattern =~ s/^\s+|\s+$//g;
            if (substr($pattern, -1, 1) ne '>') {
                $_ = <$in>;
                s/^\s*\#?\s*|\s+$//g;
                if (!s/>{$brackets}$//) {
                    $self->error("$infile: line $.: smart links must terminate",
                        "in the second line.");
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
            $self->error("$infile: line $.: syntax error in the magic link:\n\t$_");
        }
        else { next; }

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
    my ($self, $pod, $syn_id) = @_;

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
    $pod2html->html_css($self->cssfile);
    my $javascript = $self->get_javascript();
    $pod2html->html_javascript(qq{<script type="text/javascript">$javascript</script>});
    $pod2html->force_title('S'.$syn_id);

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

sub create_stats_page {
}


sub snippet_id_inc    { $_[0]->{snippet_id}++ };
sub snippet_id        { $_[0]->{snippet_id} };

sub link_count_inc { $_[0]->{link_count}++ };
sub link_count     { $_[0]->{link_count} };

sub broken_link_count_inc { $_[0]->{broken_link_count}++ };
sub broken_link_count     { $_[0]->{broken_link_count} };

sub error {
	my $self = shift;
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

Copyright (c) 2006 - 2009 by the Pugs Team.

=head1 LICENSE

Smart::Links is free software; you can redistribute it and/or modify it under the
terms of the Artistic License 2.0.  (Note that, unlike the Artistic License
1.0, version 2.0 is GPL compatible by itself, hence there is no benefit to
having an Artistic 2.0 / GPL disjunction.)

=cut


1;
