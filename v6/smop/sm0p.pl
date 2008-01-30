#!/usr/bin/perl

use strict;
use warnings;
use IPC::Open3;

my ($in, $out) = @ARGV;

open my $input, '<', $in or die $!;
open my $output, '>', $out or die $!;

my $sm0p_code = '';
PRINCIPAL:
while (<$input>) {
    if (/q:sm0p/) {
        $sm0p_code = $_;
        while (<$input>) {
            $sm0p_code .= $_;
            if ( $_ =~ /\}/ ) {
                print {$output} preprocess($sm0p_code);
                next PRINCIPAL;
            };
        }
    }
    print {$output} $_;
}


sub preprocess {
    my $code = shift;
    my ($writer, $reader, $error) = map { gensym } 1..3;
    my $pid = open3($writer, $reader, $error, 'perl',
                    '-I../v6-KindaPerl6/compiled/perl5-kp6-mp6/lib',
                    'sm0p/sm0p/KP6sm0p.pl');
    print {$writer} $code;
    close $writer;
    my $ret = join '', <$reader>;
    die 'Bad sm0p code at '.$in unless $ret;
    close $reader;
    close $error;
    waitpid($pid,0);
    return $ret;
}
