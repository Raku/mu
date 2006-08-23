# tests for util/smartlinks.pl
# [TODO] add more tests for the remaining subs.

use strict;
use warnings;
use Test::More tests => 16;
use FindBin;

require "$FindBin::Bin/../smartlinks.pl";

# Test sub parse_pattern
{
    my $got = parse_pattern('/abc .* C<cd>/');
    is $got, 'abc .* C<cd>', 'legacy regex pattern works';

    $got = parse_pattern(q{'abc' '"123"' 567});
    is $got, 'abc.+?\"123\".+?567', 'keyword pattern works';

    unlike 'abcd is 5"123"567', qr/$got/, 'the regex works';
    like 'abcd is 5"123" 567', qr/$got/, 'the regex works';

    $got = parse_pattern(q{he likes me. right?});
    is $got, 'he.+?likes.+?me\..+?right\?', 'chars get quoted';
    like 'i do believe he really likes john and me. am i right? hehe.',
        qr/$got/, 'regex matched';

    $got = parse_pattern(q{'he likes me. right?'});
    is $got, 'he\ likes\ me\.\ right\?',
        'chars get quoted and spaces are reserved';
    ok 'i do believe he really likes john and me. am i right? hehe.' !~
        qr/$got/, 'regex not matched';

    $got = parse_pattern(q{"he likes me. right?"});
    is $got, 'he\ likes\ me\.\ right\?',
        'chars get quoted and spaces are reserved';
    ok 'i do believe he really likes john and me. am i right? hehe.' !~
        qr/$got/, 'regex not matched';
}

# Test sub process_paragraph
{
    my $para = <<_EOC_;
Characters with no corresponding closing character do not qualify
as opening brackets.  This includes the second section of the Unicode
BidiMirroring data table, as well as C<U+201A> and C<U+201E>.
_EOC_
    my $str = process_paragraph($para);
    is $str,
       'Characters with no corresponding closing character do not qualify '.
       'as opening brackets.  This includes the second section of the Unicode '.
       'BidiMirroring data table, as well as U+201A and U+201E. ',
    'paragraph gets processed.';

    is process_paragraph('I I<love> B<perl>!'), 'I love perl!',
        'I<...> and B<...> stripped';
    is process_paragraph('so C<< a < b >> holds'), 'so a < b holds',
        'C<<...>> stripped';
}

# Test subs 
{
    my $para = <<_EOC_;
As a special case to facilitate commenting out sections of code with
C<s/^/#/>, C<#> on the beginning of line is always considered a line-end
comment rather than an embedded comment, even if followed by a bracketing
character.
_EOC_

    my $str = process_paragraph($para);
    is $str,
        'As a special case to facilitate commenting out sections of code with '.
        's/^/#/, # on the beginning of line is always considered a line-end '.
        'comment rather than an embedded comment, even if followed by a bracketing '.
        'character. ',
    'paragraph processed as expected';

    my $regex = parse_pattern('"#" on "beginning of line" always "line-end comment"');
    is $regex, '\#.+?on.+?beginning\ of\ line.+?always.+?line\-end\ comment',
        'regex generated as expected';
    like $str, qr/$regex/, 'text matched';
}
