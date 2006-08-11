use v6-alpha;

# Test various forms of comments

use Test;

plan 21;

{
    # L<S02/"Whitespace and Comments"/"Embedded comments"
    #  "#" plus any bracket>

    ok #[
        Multiline
        comments
        is fine
    ] 1, 'multiline embedded comment with #[]';

    ok #(
        Parens works also
    ) 1, 'multiline embedded comment with #()';

    my $var = #{ foo bar } 32;
    is $var, 32, 'embedded comment with #{}';

    $var = 3 + #「 this is a comment 」 56;
    is $var, 59, 'embedded comment with LEFT/RIGHT CORNER BRACKET';

    $var = 2 #『 blah blah blah 』 * 3;
    is $var, 6, 'embedded comment with LEFT/RIGHT WHITE CORNER BRACKET';

    my @list = 'a'..'c';

    # FIXME: $var = @list[ #（注释）2 ];
    is $var, 'c', 'embedded comment with FULLWIDTH LEFT/RIGHT PARENTHESIS';

    $var = @list[ 0 #《注释》];
    is $var, 'a', 'embedded comment with LEFT/RIGHT DOUBLE ANGLE BRACKET';

    $var = @list[#〈注释〉1];
    is $var, 'b', 'embedded comment with LEFT/RIGHT ANGLE BRACKET';

    # Note that 'LEFT/RIGHT SINGLE QUOTATION MARK' (i.e. ‘’) and
    # LEFT/RIGHT DOUBLE QUOTATION MARK (i.e. “”) are not valid delimiter
    # characters.
}

{
    # L<S02/"Whitespace and Comments"/"no space" between "#" and bracket>

    ok !eval("3 * # (invalid comment) 2"), "no space allowed between '#' and '('";
    ok !eval("3 * #\t[invalid comment] 2"), "no tab allowed between '#' and '['";
    ok !eval("3 * #  \{invalid comment\} 2"), "no spaces allowed between '#' and '\{'";
    ok !eval("3 * #\n<invalid comment> 2"), "no spaces allowed between '#' and '<'";
}

{
    # L<S02/"Whitespace and Comments"/"matched by" "equal number"
    #   adjacent closing brackets>

    ok #<<<
        Or this <also> works...
    >>> 1, '#<<<...>>>';

    my $var = \#((( comment ))) 12;
    is $var, 12, '#(((...)))';

    $var #<< < >> = 25;
    is $var, 25, '#<< < >>';

    $var = #<< > >> 36;
    is $var, 36, '#<< > >>';
}

{
    # L<S02/"Whitespace and Comments"/"Bracket counting" "sets of brackets"
    #   "same length">

    is 3, #(
        (Nested parens) works also
    ) 3, 'nested parens #(...(...)...)';

    is -1 #<<<
        Even <this> <<< also >>> works...
    >>>, -1, 'nested brackets in embedded comment';

    is 'cat', #{{
        This comment contains unmatched } and { { { {   (ignored)
        Plus a nested {{ ... }} pair                    (counted)
    }} 'cat', 'embedded comments with nested/unmatched bracket chars';
}

{
    # L<S02/"Whitespace and Comments"/
    #   "#" "left margin" always "line-end comment">

    is 31,
#<this is special cased
    31, '#< on the left margin is a line-end comment';

    ok !eval(" #<this is invalid"),
        'embedded comment not on the left margin';
}
