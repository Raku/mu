#!/usr/bin/perl

use strict;
use warnings;
use IPC::Open3;
use Symbol;

my ($base, $in, $out) = @ARGV;

die 'You need to checkout v6/ and not v6/smop, because smop depends on KP6' unless -d $base.'/../v6-KindaPerl6';

open my $input, '<', $in or die $!;
open my $output, '>', $out or die $!;

my $sm0p_code = '';
my $out_count = 1;
print {$output} qq{#line 1 "$in"\n};
PRINCIPAL:
while (<$input>) {
    if (/q:sm0p/) {
        $sm0p_code = $_;
        while (<$input>) {
            $sm0p_code .= $_;
            if ( $_ =~ /^\s*\}/ ) {
                my $next_inline = $. + 1;
                my $next_outline = $out_count + 2;
                my $lines = qq{#line $next_outline "$out"\n}
                               . preprocess($sm0p_code)
                               . qq{#line $next_inline "$in"\n};
                print {$output} $lines;
                $out_count += $lines =~ tr/\n//;
                next PRINCIPAL;
            };
        }
    }
    $out_count++;
    print {$output} $_;
}


sub preprocess {
    my $code = shift;
    my ($writer, $reader, $error) = map { gensym } 1..3;
    my $pid = open3($writer, $reader, $error,
        'perl',"-I$base/../../src/perl6",
        '-I'.$base.'/sm0p',
        $base.'/sm0p/sm0p_with_actions') || die "$@";
    print {$writer} $code;
    close $writer;
    print join '', <$error>;
    my $ret = join '', <$reader>;
    die 'Bad sm0p code at '.$in unless $ret && $ret ne "\n";
    close $reader;
    close $error;
    waitpid($pid,0);
    die 'KP6sm0p.pl returned failure '.$? if $?;
    return $ret;
}
