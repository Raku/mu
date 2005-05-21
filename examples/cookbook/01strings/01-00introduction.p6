#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Introduction to Strings

=head1 Definition

In Perl 6, a string is a sequence of zero or more characters and other
simple objects, forming a single unit of data of the type Str. A string
is data usually in its reportable or printable form. You can write a
Perl string using single-quotes '', double-quotes "", and various quote-like
operators. You can create and manipulate strings programmatically, in
many ways. In fact, this is what Perl does best.


=head1 Description

Perl 6 interprets strings variously, as they appear in different
contexts.  A string may consist of zero or more characters, including
letters, spaces, numbers, and other characters: 

    Note:
    text preceded by # is ignored by the Perl 6 interpretor 
    
    print ""        ; # output is an empty string
    print "Hello\n" ; # output is Hello followed by a new line  
    say   "Hello"   ; # same
    say   'Hello'   ; # same 

=cut

print ""        ; # output is an empty string
print "Hello\n" ; # output string is Hello followed by a new line  
say   "Hello"   ; # same
say   'Hello'   ; # same 

=pod

Strings can be appended to one another, using the concatenation
operator, ~

    say "Hello" ~ " World" ~ "!"; 

    # Here, three strings are concatenated into a 
    # single string.  Output is Hello World! followed by a new line  

=cut

say "Hello" ~ " World" ~ "!"; 

=pod

A number might be interpreted as a string, depending on the context

    say  (1+1) ~  " is a number interpreted as a string" ; # 2 is a number interpreted as a string. 

Note the parentheses around (1+1), which cause 1 + 1 to be evaluated
as a numeric expression, before the resulting "2" is evaluated as a
string. 

=cut

say    1  ; # 1 is a number 
say  (1+1) ~  " is a number interpreted as a string" ; 

=pod

Conversely, sometimes a string might be interpreted as a number:

    print  +""     ; # a num-ified empty string evaluates as 0 
    print  "1" + 1 ; # 2 

The string, "1" is treated as a number in this context, added to the
number 1 by the + operator, which returns the number, 2, as a 
string for output.  

=cut

say   +""     ; # a num-ified empty string evaluates as 0 
say  "1" + 1  ; # 2 

=pod

Context sensitivity is the essence of Perl.  Keeping this in mind, what
would you expect to be the output string, for the following?  

    print "1" ~ "1" + 1 ; # 12 

But, "1+1", surrounded by quotation marks, either '' or "", stringifies
the expression, so that it is evaluated as a string. 

    print "1 + 1" ; # literally: 1 + 1

To force the interpretation of a string for any programmatic
value it might contain, use the built-in eval() function:

    say eval "1 + 1";    # 2

On the command-line, you may pass a string to the perl 6 interpretor,
to have it evaluated as a program expression, by using the -e switch: 

    ./pugs -e "say 1+1"; # 2

=head1 Using Strings 

A string object can be created explictly, by declaring a variable
using the Str keyword, and assigning a string value to it. 

    my Str $string = 'This Str is holding a String';

Or a string object can be declared implicitly by assigning a string value to 
a variable, in this case a scalar. 

    my $scalar    = 'String';
    say '$scalar is ' ~ $scalar.ref;   # $scalar is Str

=cut

my Str $string =  'This is $string: a scalar holding a String';
say $string ;
say '$string is ' ~ $string.ref ;                 # Str 
my  $scalar    =  'This is $scalar holding a String';
say $scalar ; 
say '$scalar is ' ~ $scalar.ref ;                 # Str 

=pod 

Assignments of non-strings  set the variable to the
appropriate type:


    my $scalar = 1234;                      
    say '$scalar is ' ~ $string.ref"   # $scalar is Int

An object can be stringified, by using the ~ operator immediately
prior to the variable's sigil

    say '~$scalar is ' ~ (~$scalar).ref; # ~$scalar is Str 

=cut

$scalar = 1234;
say $scalar   ;
say '$scalar is '  ~ $scalar.ref; 
say '~$scalar is ' ~ (~$scalar).ref; 

=pod

TODO: a short paragraph on the difference between the my $scalar 
and my Str $string examples above.

=head2 Almost Verbatim Strings 

Strings that are written with single quotes are almost
verbatim.  However, two backslashes together in single 
quotes are interpolated as one backslash. 

This is so that you can write literal single-quotes within a single-
quoted string, and also be able to write a backslash at the end of a 
single-quote-enclosed string:

    say 'n\'     ; # Error: perl sees no closing '
    say '\\'     ; # \ 
    say 'n\''    ; # n'
    say 'n\n'    ; # n\n
    say 'n\\n'   ; # n\n
    say 'n\\\n'  ; # n\\n

=head2 Interpolation

If you want to interpolate variables within a literal
string, use double quotes around the value:

    my $var1 = 'dog' ;
    say "The quick brown fox jumps over the lazy $var1";

Double quoted strings can also interpolate the elements of an array or a hash, 
backslashed control characters, and other good stuff:

    # interpolate array elements:
    my @animal = ("fox", "dog");
    say "The quick brown @animal[0] jumps over the lazy @animal[1]";
    
    # interpolate hash elements:
    my %animal = (quick => 'fox', lazy => 'dog');
    say "The quick brown %animal{'quick'} jumps over the lazy %animal{'lazy'}.";

    # interpolate special backslash values:
    print "The quick brown fox\n\tjumps over the lazy dog\n";

XXX A few other backslashy escapes work in single quotes too

=cut

my $var1 = 'dog' ;
say "The quick brown fox jumps over the lazy $var1";

my @animal = ("fox", "dog");
say "The quick brown @animal[0] jumps over the lazy @animal[1]";

my %animal = (quick => 'fox', lazy => 'dog');
say "The quick brown %animal{'quick'} jumps over the lazy %animal{'lazy'}.";
print "The quick brown fox jumps\n\tover the lazy dog\n";

=pod

=head2 Using Perl's Quote-like Operators

It's often useful to use something other than single or double quotes when
declaring strings. To do so use the q// and qq// quote operators:

    # Single quoted strings
    say 'I have to escape my \'single quotes\' in this string';
    say q/This string allows 'single quotes' seamlessly/;

    # Double quoted strings
    say "I have to escape my \"double quotes\" in this string";
    say q/This string allows "double quotes" seamlessly/;

The slashes in q// and qq// can be replaced with most other delimiters.

    # Single quoted strings
    say q'Many delimiters are available for quoting';
    say q"Many delimiters are available for quoting";
    say q`Many delimiters are available for quoting`;
    say q[Many delimiters are available for quoting];
    say q(Many delimiters are available for quoting);
    say q<Many delimiters are available for quoting>;
    say q{Many delimiters are available for quoting};
    say q«Many delimiters are available for quoting»;

=head2 Advanced Interpolation Control

Perl 6 allows very fine control over string quoting using the q// quote
operator with specialized adverbs. For instance, q:s// signifies that we only
want scalars interpolated. These adverbs can also be expressed in a short form,
for instance q:s// can be expressed as qs//.

    # Raw quoting: no escaping at all (unless otherwise adverbed)
    say q:0/Here is a code sample &function($param) (no interpolation)/;
    say q0/Here is a code sample &function($param) (no interpolation)/;

    # Single quoting:
    say 'Lots of options for single quotes';
    say q/Lots of options for single quotes/;
    say q:1/Lots of options for single quotes/;
    say q1/Lots of options for single quotes/;
    
    # Double quoting: interpolates scalars, arrays, hashes, functions,
    # closures, and backslash codes
    say "Plenty of ways to double quote too";
    say qq/Plenty of ways to double quote too/;
    say q:2/Plenty of ways to double quote too/;
    say q2/Plenty of ways to double quote too/;

    # Interplate scalars only:
    my ($var1, $var2) = <dog fox>;
    say q:s/The quick brown $var1 jumps over the lazy $var2/;
    say qs/The quick brown $var1 jumps over the lazy $var2/;

    # Interpolate @ vars only:
    say q:a/The quick brown @animal[0] jumps over the lazy @animal[1]/;
    say qa/The quick brown @animal[0] jumps over the lazy @animal[1]/;
    say qa/We have @animal.elems() elements in the \@animals array/;

XXX But @animal[0] is not an array, it is an array element.
    -- see comment below
    
    # interpolate % vars only:
    say q:h/The quick brown %animal{'quick'} jumps over the.../;
    say qh/The quick brown %animal{'quick'} jumps over the.../;

XXX But %animal{quick} is not a hash, it is a hash element.
    -- i think that's irrelevant to the examples, however I will also 
        demonstrate the many other things you can do with hashes and 
        arrays during interpolation --gcomnz
XXX It's relevant because what you say now is just wrong. Both arrays|hashes
*and* array|hash elements are interpolated. While %foo is a hash, %foo{bar} is
just a scalar! 
    -- yeah, you're right. i went looking for a short way to say
        what i mean, and it seems like the synopses is the closest
        i can get right now, so i replaced the comments with
        "% vars" and "@ vars" for now. I'm hesitant to put in an
        interpolation of actual straight %hash and @array right now
        because it seems like it's too complex an example for
        chapter 01-00 --gcomnz
XXX By the way, you probably want %animal{'quick'} there, because
%animal{quick} is %animal{quick()}.
    -- phew, thanks, I just caught up on that whole hash
        subscripting thread  --gcomnz

    # interpolate functions only: both & and () are required
    say q:f/The quick brown &get_animal('quick') jumps.../;
    say qf/The quick brown &get_animal('quick') jumps.../;

    # interpolate closures only:
    say q:c/The quick brown { return 'fox'; } jumps.../;
    say qc/The quick brown { return 'fox'; } jumps.../;

    # interpolate backslash codes only:
    say q:b/The quick brown fox\n\tJumps over the lazy dog/;
    say qb/The quick brown fox\n\tJumps over the lazy dog/;

Adverbs can be strung together to make a specialized quoting environment for
your string.

    # interpolate only scalars and arrays:
    say q:s:a/The quick brown $fox jumps over the lazy @animal[1]/;

=head2 Defining Multiline Strings (Here Documents)

Multiline strings (here documents) can be defined using the q// and qq//
operators with the :to adverb added.

    # A double quoted multiline string:
    my $a = qq:to/EOF/
        This is a multiline here document terminated by EOF on a 
        line by itself with any amount of whitespace before or 
        after the termination string. Leading whitespace equivalent 
        to the indentation of the delimiter will be removed from 
        all preceding lines.
        EOF

When defined in this way the whitespace at the start of each line will be 
removed up to the same amount of indentation used by the closing delimiter, 
a tab character being equal to 8 normal spaces.

A here document can be as exacting with adverbs as any other quoted string. For
instance you can specify that you only want scalars interpolated by adding
the :s adverb.

    # This multiline string will only interpolate scalars
    my $multiline = q:s:to/EOF/
        This $scalar will be interpolated, but this @array won't be.
        EOF

These adverbs apply to the body of the heredoc, not to the terminator, because
the terminator has to be known at compile time. This means that
q:s:to/EO$thing/ doesn't do what you mean.

=cut
