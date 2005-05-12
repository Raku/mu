#!/usr/bin/perl6

use v6;

=head1 Reversing a String by Word or Character

You want to reverse words or characters in a string

=cut

# reverse the characters in a scalar
$revbytes = reverse($string);
$revbytes = $string.reverse;

# reverse the words in a scalar by splitting, reversing, then joining

$revwords = join ' ', reverse split ' ', $string;
$revwords = $string.split(' ').reverse.join(' ');
# XXX Since ' ' is default for both split and join...
#         $string.split.reverse.join
# And since string context also joins on ' ':
#         ~$string.split.reverse
# (Note that ~ is needed if $revwords is Scalar, but not if it is Str, because
# a list in scalar context is an arrayref).
# (See "lists in string context" p6l thread)

# in scalar context, reverse the characters, in list context, reverse the list# # elements
$gnirts   = reverse($string);       # reverse letters in $string
@sdrow    = reverse(@words);        # reverse elements in @words
$confused = reverse(@words);        # reverse letters in join("", @words)
# XXX No, I that'll be join(' ', @words). If you want it to join on '', 
# @words.reverse.cat, or cat(reverse(@words)). cat is join defaulting to ''.
# See same p6l thread.


# reverse word order
$string = 'Yoda said, "can you see this?"';
@allwords    = split " ", $string;
$revwords    = join " ", reverse @allwords;
say $revwords;

# concise versions of the same
$revwords = join " ", reverse split " ", $string;
# or
$revwords = $string.split(" ").reverse.join(" ");

# to do the same, preserving whitespace
$revwords = join $0, reverse split /(\s+)/, $string;

# test if a word is a palindrome
$word = "reviver";
$is_palindrome = $word eq reverse($word);

# perl one-liner to test a dict file for palindromes
# % perl -nle 'say $_ if $_ eq $_.reverse && $_.chars > 5' /usr/dict/words

