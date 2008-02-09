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
                    '-I'.$base.'/../v6-KindaPerl6/compiled/perl5-kp6-mp6/lib',
                    $base.'/sm0p/KP6sm0p.pl');
    print {$writer} $code;
    close $writer;
    my $ret = join '', <$reader>;
    die 'Bad sm0p code at '.$in unless $ret && $ret ne "\n";
    close $reader;
    close $error;
    waitpid($pid,0);
    die 'KP6sm0p.pl returned failure '.$? if $?;
    return $ret;
}
