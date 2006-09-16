#!/usr/bin/env perl

# podhtm.pl - command line utility for Pod::Simple::HTML

use strict;
use warnings;

use Getopt::Long;
use Pod::Simple::HTML;

sub Usage {
    print <<_EOC_;
Usage:
  $0 [options] foo.pod
  $0 --index --out foo.html --css bar.css baz.pod
  $0 --title "S04" --charset 'UTF-8' baz.pod
  $0 --url-prefix=http://feather.perl6.nl/syn/ --url-postfix=.html my.pod

Options:
  --help            Show this help.
  --out <file>      Specify the output file name.
  --css <file>      Specify the CSS file used by the HTML outputs,
                    defaults to http://dev.perl.org/css/perl.css.
  --charset <name>  Specify the charset appear in the HTML head.
  --index           Generate the table of contents at the beginning.
  --title <str>     Specify HTML title for the output page.
  --url-prefix <s>  Specify the prefix used by Pod urls.
  --url-postfix <s> Specify the postfix used by Pod urls.
_EOC_
    exit(0);
}

my ($help, $outfile, $cssfile, $charset, $index);
my ($title, $url_prefix, $url_postfix);
GetOptions(
    'help'          => \$help,
    'out=s'         => \$outfile,
    'css=s'         => \$cssfile,
    'charset=s'     => \$charset,
    'index'         => \$index,
    'title=s'       => \$title,
    'url-prefix=s'  => \$url_prefix,
    'url-postfix=s' => \$url_postfix,
);

if ($help || !@ARGV) {
    Usage();
}

my $infile = shift;
$cssfile ||= 'http://dev.perl.org/css/perl.css';

if (!defined $outfile) {
    $outfile = '-';
}

if (defined $charset) {
    $Pod::Simple::HTML::Content_decl =
        qq{<meta http-equiv="Content-Type" content="text/html; charset=$charset" >};
}

$Pod::Simple::HTML::Doctype_decl =
    qq{<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
       "http://www.w3.org/TR/html4/loose.dtd">\n};

my $pod2html = new Pod::Simple::HTML;
$pod2html->index($index);
$pod2html->html_css($cssfile);
$pod2html->perldoc_url_prefix($url_prefix) if defined $url_prefix;
$pod2html->perldoc_url_postfix($url_postfix) if defined $url_postfix;

#$pod2html->html_javascript($javascript) if $javascript;

$pod2html->force_title($title) if defined $title;
$pod2html->parse_from_file($infile, $outfile);
print "$outfile generated.\n" if -e $outfile;
