#!/usr/bin/perl -w

=kwid

To generate a test catalog HTML dir, run

    $ rm -rf test_catalog
    $ perl util/catalog_tests.pl t/ test_catalog    # regenerates the pods
    $ rm -rf test_catalog_html
    $ mkdir test_catalog_html
    $ perl -MPod::Simple::HTMLBatch -e 'Pod::Simple::HTMLBatch::go' test_catalog/ test_catalog_html/

and now you have test_catalog_html/

Note that only backlinked synopses are generated.

= TODO

== important

* rewrite to emit HTML
	- by converting pods with Pod::Simple::HTMLBatch, or otherwise
	- and by emiting HTML directly for tests
		- perhaps with an output more similar to Devel::Cover
* put backlinks:
	- for links with no regex right after the heading, in a comma
	  separated list so they don't take too much space
	- for links with a regex, as a superscripted number with a
	  <a name="file">, at the *end* of the regex match
* 

== unimportant

* cause L<news:msg@id.com> to link to google groups or wherever
* correlate test results to their files

== long term

* determine if the regexes could be coerced into a more concrete link (low priority)

= BUGS

* cannot link to something like C<=item "Pointy subs">
* synopsis pod is not parsed, but regexed
* stuff is planted in a naughty way (substr)
* too slurpy
* there is no well defined object for test meta data. It should look like:
	- Pugs::TestFile
		- lines
		- test cases
			- type
			- todo
			- description
		- plan
		- links

=cut

use strict;
use Fatal qw/open close opendir closedir/;
use File::Spec::Functions qw/catfile curdir updir splitdir/;
use Regexp::Common qw/balanced delimited/;
use Pod::ParseUtils;
use File::Path qw/mkpath/;
use File::Basename;
use File::Slurp;

$\ = "\n\n";

my ($in, $out) = @ARGV;
$in ||= "t";
$out ||= "test_catalog";

catalog_tests($in, $out);


sub sort_files {
    return ((-d $a && -f $b) ? 1 :       # file beats directory
            (-d $a && -f $b) ? -1 :      # file beats directory
                (lc($a) cmp lc($b)))     # otherwise just sort 'em
}

sub list_dir {
    my ($path) = @_;
    opendir DIR, $path;
    my @contents = map { catfile($path, $_) } grep {
                    $_ ne curdir() && $_ ne updir()
                    } readdir DIR;
    closedir DIR;
    return sort "sort_files", @contents;
}

sub catalog_tests {
    my ($dir, $output) = @_;
    for (list_dir($dir)) {
        if (-d $_ && !/Synopsis/ && !/Dialects/ && !/\.svn/) {
            catalog_tests($_, $output);
        }
        elsif (-f $_ && /\.t$/) {
            catalog_file($_, $output);
        }
    }
}

sub catalog_file {
    my ($file, $output) = @_;
    
    my $dom = parse_file($file); # create a big hash of the info we care about, from each test

    $file =~ s/\.t$//; # this is because stupid Pod::Simple::Search won't eat foo.t.pod
    
    prettify($file, $output, $dom); # write out a pod for the test, with all the data in it
    cross_index($file, $output, $dom); # and also plant backlinks in the synopses it links to
}

sub prettify {
    my ($file, $output, $dom) = @_;

    my $target = catfile($output, "$file.pod");
    mkpath dirname($target);
    open FILE, ">", $target;
    
    my $doc = join("\n", map { $_->{text} } grep { $_->{type} eq 'doc' } @{ $dom->{items} });
    
    print FILE $_ for("=pod", "=head1 NAME", $file, "=head1 DESCRIPTION");
    print FILE <<DESC;
This is an automatically generated file of describing the aforementioned test.
It might contain useful info such as test cases, and some info about them, as
well as links to the synopses.
DESC
    print FILE $_ for ("=head1 DOCUMENTATION", $doc, "=head1 CONTENTS", "=over 4");

    foreach my $item (grep { $_->{type} eq 'test' or $_->{'type'} eq 'link' } @{ $dom->{items} }){
        print FILE "=item line $item->{line} - $item->{type}";
        if ($item->{type} eq "test"){
            print FILE "=over 4";
            
            if ($item->{kind}){
                print FILE "=item *";
                print FILE "kind: $item->{kind}()" . ($item->{todo} ? " # TODO" : "");
            }
            
            if ($item->{desc}){
                print FILE "=item *";
                print FILE "description: $item->{desc}";
            }
            
            if ($item->{comment}){
                print FILE "=item *";
                print FILE "comment: $item->{comment}";
            }
            
            print FILE "=back";
        } elsif ($item->{type} eq 'link'){
            if ($item->{obj}){
                print FILE "L<" . $item->{obj}->link . ">";
            } else {
                print FILE "COULD NOT BE PARSED: $item->{orig}";
            }
        }
    }

    print FILE "=$_" for qw/back cut/;
    close FILE;
}

my %synopses; # contains the full synopses pods
sub cross_index {
    my ($file, $output, $dom) = @_;
    
    foreach my $link (grep { defined $_->{obj} } grep { $_->{type} eq 'link' } @{ $dom->{items} }){
        my $syn = $link->{obj}->page;
        my $sfile = catfile(split("::", $syn)) . ".pod";
        $synopses{my $key = catfile($output, $sfile)} ||= read_file(catfile($in, $sfile));
        my $node = $link->{obj}->node;
        my $regex = $link->{re};
        my $test_name = join("::", splitdir($file));
        
        $regex ||= qr//;
        # this is a replacement for a proper lookup of a page/node -> regex thing
        # just constructs a big regex, and matches it against the entire pod
        $regex = qr/=(head\d+|item)\s+(?:\Q$node\E).*?($regex)/s;
        
        # if we succeed
        if ($synopses{$key} =~ /$regex/){
            # find a paragraph end somewhere, and just
            # stick the link inside it, with substr
            my $before = $+[-1] + 2;
            my $at = rindex($synopses{$key}, "\n\n", $before);
            substr($synopses{$key}, $at, 0, qq(\n\nL<$file, around line $link->{line}|$test_name/"line $link->{line} - link">)); # i should be punished, but otherwise match\ing regexes is not that easy
        } else {
            warn qq{couldn't resolve link $link->{orig} in $file ($regex)\n};
        }
    }
}
END {
    # when we're done, write out the new synopses
    foreach my $file (keys %synopses){
        mkpath(dirname($file));
        write_file($file, $synopses{$file});
    }
}

sub parse_file {
    my ($file) = @_;
    my $in_doc;
    
    my @items;
    my $todo_tests = 0;
    my $tests_planned = 0;
    
    open FH, "<", $file;
    LINE: while (<FH>) {
        chomp();
        
        next if /^$/;
        
        # this part collects documentation
        if (/=pod/ || /=kwid/) {
            $in_doc = 1;
            next;
        }
        if (/=cut/) {
            $in_doc = 0;
            next;
        }
        if ($in_doc) {
            push @items, { type => "doc",  text => $_ };
            next;
        }


        # this part fishes out real data,       
        if (/^\s*plan.*?(\d+)/) {
            $tests_planned = $1;
        }
        
        # find links
        if (/(L$RE{balanced}{-begin => "<|<<|<<<"}{-end => ">|>>|>>>"})/){
            my ($link, $re) = dissect_link(my $orig = $1);
            push @items, {
                type => "link",
                obj => $link,
                re => $re,
                orig => $orig,
                line => $.,
            };
        }
        
        # find test cases
        if (/( # the whole subname
            (todo_)?  # can be todo
            (
                (?:eval)? # or eval
                (?:ok|is|fail|isa_ok) # must be one of these
            )
        )/x){
            my ($test_sub, $todo, $test_type) = ($1, $2, $3);
            my ($desc, $comment);

            if (/
                .* # lots of stuff
                \,\s* # a comma, followed by
                ['"](.*)['"] # some quoted text
            /x){ $desc = $1 }
            
            if (/
                ;\s* # ending the statement, and from there on whitespace
                \#\s*(.*)$ # followed by a comment
            /x){ $comment = $1 }
        
            $todo &&= 1==1; # booleanize
            
            $todo_tests++ if $todo;
            
            push @items, {
                type => 'test',
                line => $.,
                kind => $test_type,
                desc => $desc,
                comment => $comment,
                todo => $todo,
            };
        }
    }
    close FH;
    
    return {
        file => $file,
        items => \@items,
        plan => $tests_planned,
        todo => $todo_tests,
    }
}

# this function will take our funny link format
# and make into a valid Pod::Hyperlink object and a regex
sub dissect_link {
    my $funny = shift;

    my $qword = qr/\w+|$RE{delimited}{-delim=>'"'}/;
    my $funny_link_format = qr{
        ^(L<+)
        ($qword(?:/$qword)?)
        (?:
            \s+? # match some whitespace between the podlink and the regex
            $RE{delimited}{-delim=>'/'}{-keep} # and then a delimited regex # find some more delims
            ([xim]*) # with optional options ;-) # /s is always used
        )?
        (>+)$ # yuck yuck yuck yuck
    }x;

    $funny =~ $funny_link_format or return undef;

    my $regex = $5;
    my $ropt = $7;
    my $proper = Pod::Hyperlink->new($2) || die "$@";
    /^S\d{2}$/ and $proper->page("Synopsis::$_") for $proper->page;
    my $qr;

    return $proper if not wantarray;
    
    if (defined $regex){
        $regex =~ s/ +/\\s+/g;
        $qr = eval "qr\0$regex\0s$ropt";
        warn "error compiling regex qr/$regex/: $@" if $@;
    }

    return ($proper, $qr);
}

