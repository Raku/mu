#!/usr/bin/env perl

use strict;
use warnings;

use JSON::Syck;
use File::Slurp;
use Getopt::Long;
use File::Spec ();
use File::Path 'mkpath';
use File::Copy 'copy';

#use Smart::Comments;
use List::MoreUtils qw( uniq );

my $outdir = '.';
GetOptions(
    'outdir=s' => \$outdir
) or die help();
if (!-d $outdir) { mkpath $outdir; }

sub help {
    die "Usage: $0 [--outdir=.] grammar_file input_file  < trace.out\n";
}

my $gmr_file = shift or die "No grammar file given";
my $str_file = shift or die "No str file given";

my (@regex_pos, @str_pos);
my @ops;
while (<STDIN>) {
    if (/^>>BEGIN (\w+)<< (\d+)\.\.(\d+) at (\d+)/) {
        my ($symbol, $from, $to, $pos) = ($1, $2, $3, $4);
        ## $symbol
        ## $from
        ## $to
        ## $pos
        push @regex_pos, $from, $to;
        push @str_pos, $pos;
        push @ops, ['begin', $symbol, $from, $to, $pos];
    } elsif (/^>>END (\w+)<< (\w+) at (\d+)/) {
        my ($symbol, $res, $pos) = ($1, $2, $3);
        ## $symbol
        ## $res
        ## $pos
        push @str_pos, $pos;
        push @ops, ['end', $symbol, $res, $pos];
    }
}

@regex_pos = uniq sort { $a <=> $b } @regex_pos;
@str_pos = uniq sort { $a <=> $b } @str_pos;

### @regex_pos
### @str_pos

my $gmr = read_file($gmr_file);
my $id = "R" . $regex_pos[0] . "-" . $regex_pos[1];
#warn $id;

my $js_ops = JSON::Syck::Dump(\@ops);
my $js_regex_pos = JSON::Syck::Dump(\@regex_pos);
my $js_str_pos = JSON::Syck::Dump(\@str_pos);

#warn scalar(@ops);
#warn $js_ops;
my $html = to_html($gmr, \@regex_pos);
my $outfile = File::Spec->catfile($outdir, 'gmr.html');
print "Write $outfile\n";
write_file($outfile, $html);

my $str = read_file($str_file);
$html = to_html($str, \@str_pos);
$outfile = File::Spec->catfile($outdir, 'str.html');
print "Write $outfile\n";
write_file($outfile, $html);
my $tracer_js = read_file('tracer/tracer.js');

$html = <<"_EOC_";
<html>
    <head>
    <title>Test</title>
    <script src="tracer/jquery.js"></script>
    <script type="application/javascript">
        var Ops = $js_ops;
        var Id = '$id';
        var regexPos = $js_regex_pos;
        var strPos = $js_str_pos;
        $tracer_js
    </script>
    </head>
<body>
\&nbsp;
<button id="reset">Reset</button>
<button id="next">Next</button>
\&nbsp;<span id="label"></span><br />
<iframe id="gmr" src="gmr.html" width="49.7%" height="95%" style="margin-top: 0.5em"></iframe>
<iframe id="str" src="str.html" width="49.7%" height="95%" style="margin-top: 0.5em"></iframe>
</body>
</html>
_EOC_

$outfile = File::Spec->catfile($outdir, 'index.html');
print "Write $outfile\n";
write_file($outfile, $html);

#my $tracer_js = File::Spec->catfile($outdir, 'tracer/tracer.js');
my $jquery_js = File::Spec->catfile($outdir, 'tracer/jquery.js');

if ($outdir ne '.') {
    if (!-d "$outdir/tracer") {
        mkpath("$outdir/tracer");
    }
    print "Write $jquery_js\n";
    copy('tracer/jquery.js', $jquery_js);
    #print "Write $tracer_js\n";
    #copy('tracer/tracer.js', $tracer_js);
}

sub escape ($) {
    my $s = shift;
    $s =~ s/\&/\&amp;/g;
    $s =~ s/</\&lt;/g;
    $s =~ s/>/\&gt;/g;
    $s =~ s/ /\&nbsp;/g;
    $s =~ s{\n}{<br />}g;
    $s;
}

sub to_html {
    my ($str, $list) = @_;
    my @pos = @$list;
    return unless @pos;
    my $to = $pos[0];
    my $from = 0;
    my $out = escape substr($str, $from, $to - $from);
    for my $i (1..$#pos) {
        $from = $to;
        $to = $pos[$i];
        #warn "$from <=> $to";
        my $id = "R" . $from . "-" . $to;
        #warn $id;
        my $content = escape substr($str, $from, $to - $from);
        $out .= qq{<span id="$id">$content</span>};
    }
    $out .= escape substr($str, $to);
    #warn $out;
    return "<code>$out</code>";
}

__END__

=head1 NAME

gen-tracer-view.pl - Generate the HTML view for the PCR tracer

=head1 SYNOPSIS

  $ perl util/compile_p6grammar.pl -D examples/digits.grammar > Digits.pm
  $ echo '7c3d54' > digits.input
  $ perl -Ilib -MDigits -e 'print Digits->count(<>)->(), "\n"' digits.input > trace.out
  $ perl util/gen-tracer-view.pl --outdir tmp/digits examples/digits.grammar digits.input < trace.out
  $ firefox tmp/digits/index.html

=head1 SEE ALSO

=over

=item *

L<compile_p6grammar.pl>

=item *

"A graphical tracer for Perl 6 regexes based on PCR"

L<http://pugs.blogs.com/pugs/2007/10/a-graphical-tra.html>.

=back

=head1 AUTHOR

Agent Zhang E<lt>agentzh@yahoo.cnE<gt>

=head1 COPYRIGHT

Copyright (c) 2007 by Yahoo! China Inc.

=head1 LICENSE

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

