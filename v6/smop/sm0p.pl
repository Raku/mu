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
    if (/^(\s*) \w+ \s+ = \s* q:sm0p/x) {
        $sm0p_code = $_;
        my $indent = $1;
        while (<$input>) {
            $sm0p_code .= $_;
            if ( $_ =~ /^$indent\}/ ) {
                my $next_inline = $. + 1;
                my $next_outline = $out_count + 2;
                my $lines = qq{#line $next_outline "$out"\n}
                               . preprocess_sm0p($sm0p_code)
                               . qq{#line $next_inline "$in"\n};
                print {$output} $lines;
                $out_count += $lines =~ tr/\n//;
                next PRINCIPAL;
            };
        }
    } elsif (/^(\s*)use\s+v6;\s*$/) {
        my $p6_code = '';
        my $indent = $1;
        while (<$input>) {
            $out_count++;
            unless (/^$indent/) {
                print {$output} preprocess_p6($p6_code);
                last;
            }
            $p6_code .= $_;
        }
        #next PRINCIPAL;
    }
    $out_count++;
    print {$output} $_;
}


sub preprocess {
    my $code = shift;
    my ($writer, $reader, $error) = map { gensym } 1..3;
    my $pid = open3($writer, $reader, $error,@_) || die "$@";
    print {$writer} $code;
    close $writer;
    print join '', <$error>;
    my $ret = join '', <$reader>;
    die 'Bad sm0p|p6 code at '.$in unless $ret && $ret ne "\n";
    close $reader;
    close $error;
    waitpid($pid,0);
    die 'KP6sm0p.pl returned failure '.$? if $?;
    return $ret;
}
sub preprocess_p6 {
    my $code = shift;
    my ($writer, $reader, $error) = map { gensym } 1..3;
    my $sm0p = preprocess('','perl',"$base/../../misc/elfish/elfX/elfX",'-C','sm0p','-s','-e',$code);
    return preprocess_sm0p("frame = q:sm0p {".
        $sm0p.
        "\$SMOP__SLIME__CurrentFrame.\$SMOP__ID__forget();\n".
        "\$interpreter.goto(|\$continuation);\n".
        "};\n");
}
sub preprocess_sm0p {
    my $code = shift;
    #warn "got sm0p code <$code>\n";
    return preprocess($code,'perl',"-I$base/../../src/perl6",
        '-I'.$base.'/sm0p',
        $base.'/sm0p/sm0p_with_actions');
}
