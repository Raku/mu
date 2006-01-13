
# An attempt to convert S29draft.pod to p6 code.
# Based on r8593.
# Multis are defined in terms of others, or of Pugs::Internals:: subs,
# or implemented in p6 (S29 already defines some p6 implementations).
# To contribute, just scroll down until pod lines start appearing, and
# continue conversion to p6 code.

class Char is Str;
class LanguageChar is Char;
class Grapheme     is Char;
class CodePoint    is Char;
class Byte         is Char, Number;

macro LChar () { "LanguageChar" }
macro Grf   () { "Grapheme" }
macro CdPt  () { "CodePoint" }


 type MatchTest ::= Item | Junction;

package Math::Basic {}

 multi  Num::abs (  Num  $x              --> Num )
 {
     Math::Basic::abs($x);
 }
 multi Math::Basic::abs ( Num $x = CALLER::<$_> --> Num )
 {
     $x >= 0 ?? $x :: -$x;
 }

 multi  Num::floor (  Num  $x            --> Int )
 { Pugs::Internals::floor($x) }


 multi  Num::ceiling (  Num  $x          --> Int )
 {
     Pugs::Internals::ceil($x);
 }
 &Num::ceil ::= &Num::ceiling;


 multi  Num::round (  Num  $x            --> Int )
 {
     Pugs::Internals::round($x);
 }

 multi  Num::truncate (  Num  $x         --> Int )
 {
     Pugs::Internals::trunc($x);
 }
 our &Num::int ::= &Num::truncate;

 # XXX - referred to, but not specced, in S29.
 multi int(Num $x = CALLER::<$_> --> Int) { Num::int($x) }


 multi  Num::exp (  Num  $exponent             : Num :$base --> Num )
 {
     Math::Basic::exp($exponent, $base);
 }
 multi Math::Basic::exp ( Num $exponent = CALLER::<$_>, Num :$base --> Num )
 {
     # use (($base // Math::Unspecced::e) ** $exponent) instead?
     Pugs::Internals::exp($exponent,$base // Math::Unspecced::e);
 }


 multi  Num::log (  Num  $x             : Num :$base --> Num )
 {
     Math::Basic::log($x,$base);
 }
 multi Math::Basic::log ( Num $x = CALLER::<$_>, Num :$base --> Num )
 {
     fail "log of 0 is not defined" if $x == 0;
     Pugs::Internals::log_base($x,$base);
 }

sub Pugs::Internals::log_base(Num $x, Num $base) {
    return Pugs::Internals::log($x)   if !defined $base;
    return Pugs::Internals::log10($x) if $base == 10;
    return (Pugs::Internals::log($x) / Pugs::Internals::log($base));
}


 &log10<> := &log<>.assuming:base(10);


 multi Math::Basic::rand ( Num $x = 1 --> Num )
 {
     Pugs::Internals::rand($x);
 }


 multi  Num::sign (  Num  $x             --> Int )
 {
     Math::Basic::sign($x);
 }
 multi Math::Basic::sign ( Num $x = CALLER::<$_> --> Int ) {
   if !defined($x) { return undef };
   if $x < 0       { return -1    };
   if $x > 0       { return  1    };
   if $x == 0      { return  0    };
   undef;
 }


 multi Math::Basic::srand ( Num $seed)
 {
     Pugs::Internals::srand($seed);
 }


 multi  Num::sqrt (  Num  $x             --> Num )
 {
     Math::Basic::sqrt($x);
 }
 multi Math::Basic::sqrt ( Num $x = CALLER::<$_> --> Num )
 {
     $x ** 0.5;
 }


package Math::Trig {}

macro trig_func($func) {
    '
 multi Num::'~$func~' (  Num  $x             : :$base --> Num )
 {
     Math::Trig::'~$func~'($x,$base);
 }
 multi Math::Trig::'~$func~' ( Num $x = CALLER::<$_>, :$base --> Num )
 {
     Pugs::Internals::arc_units(Pugs::Internals::'~$func~'($x), $base);
 }
'
}
sub Pugs::Internals::arc_units (Num $radians, $base) {
    given $base {
	undef   { $radians }
	/:i ^r/ { $radians }
	/:i ^d/ { $radians / (2*pi) * 360 }
	/:i ^g/ { $radians / (2*pi) * 400 }
	(Num)   { $radians / (2*pi) * $base } # XXX - spec ambiguous
	default { fail "invalid trig function base: $base" }
    }
}

trig_func 'sin';
trig_func 'cos';
trig_func 'tan';
trig_func 'asin';
trig_func 'acos';
trig_func 'atan';
trig_func 'sec';
trig_func 'cosec';
trig_func 'cotan';
trig_func 'asec';
trig_func 'acosec';
trig_func 'acotan';
trig_func 'sinh';
trig_func 'cosh';
trig_func 'tanh';
trig_func 'asinh';
trig_func 'acosh';
trig_func 'atanh';
trig_func 'sech';
trig_func 'cosech';
trig_func 'cotanh';
trig_func 'asech';
trig_func 'acosech';
trig_func 'acotanh';


 multi Math::Trig::atan (Num $y, Num $x : Num :$base --> Num )
 {
     Pugs::Internals::atan2($y,$x);
 }


 multi Math::Trig::pi ( --> Num )
 {
     3.1415926535897932384626433832795028841971693993751058209749445923078164062;
     # http://www.research.att.com/~njas/sequences/A000796
 }

multi Math::Unspecced::e ( --> Num ) {
    2.71828182845904523536028747135266249775724709369995957496696762772407663;
    # http://www.research.att.com/~njas/sequences/A001113
}

 multi method Array::delete (@array : *@indices --> List )
 { ... } # XXX


 multi method Array::exists (@array : Int *@indices --> Bool )
 { ... } # XXX
 

 &Array::pop<Array> := &Array::splice<Array>.assuming(offset(-1) :length(1));

 multi Array::pop ( --> Scalar ) {
   pop CALLER::<@_>;
 }


 multi Array::push (@array is rw : *@values --> Int ) {
   Array::splice(@array, @array.elems, 0, @values);
   @array.elems;
 }


 &Array::shift<Array> := &Array::splice<Array>.assuming(offset(0) :length(1));

 multi Array::shift ( --> Scalar ) {
   Array::shift CALLER::<@_>;
 }


 multi Array::splice (       @array is rw 
                                 : Int $offset = 0,
                                   Int $length,
                                       *@values
                                --> List ) is rw
    my $off = +$offset;
    my $len = $length;
    my $size = +@array;

    $off += $size if $off < 0;
    if $off > $size {
        warn "splice() offset past end of array\n";
        $off = $size;
    }
    # $off is now ready

    $len = +$len if defined($len);
    $len = $size - $off if !defined($len);
    $len = $size + $len - $off if $len < 0;
    $len = 0 if $len < 0;
    # $len is now ready

    my $listlen = +@values;
    my $size_change = $listlen - $len;
    my @result;

    if 1 {
        my $i = $off;
        my $stop = $off + $len;
        while $i < $stop {
            push(@result,@array[$i]);
            $i++;
        }
    }

    if $size_change > 0 {
        my $i = $size + $size_change -1;
        my $final = $off + $size_change;
        while $i >= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$i], see t/operators/binding/arrays.t.
            @array.delete($i);
            @array[$i] = @array[$i-$size_change];
            $i--;
        }
    } elsif $size_change < 0 {
        my $i = $off;
        my $final = $size + $size_change -1;
        while $i <= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$i], see t/operators/binding/arrays.t.
            @array.delete($i);
            @array[$i] = @array[$i-$size_change];
            $i++;
        }
        # +@array = $size + $size_change;
        #   doesnt exist yet, so...
        my $n = 0;
        while $n-- > $size_change {
            pop(@array);
        }
    }

    if $listlen > 0 {
        my $i = 0;
        while $i < $listlen {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @array[$off+$i], see t/operators/binding/arrays.t.
            @array.delete($off+$i);
            @array[$off+$i] = @values[$i];
            $i++;
        }
    }

    #  want.List ?? *@result !! +@result ?? @result[-1] !! undef;
    @result;
}


 multi Array::unshift (@array is rw : *@values --> Int ) {
   Array::splice(@array, 0, 0, @values);
   @array.elems;
 }


# XXX - todo: define these all in terms of kv, and leave kv a stub {...}.
 multi Array::keys   (@array : MatchTest *@indextests --> Int|List )
 multi Array::kv     (@array : MatchTest *@indextests --> Int|List )
 multi Array::pairs  (@array : MatchTest *@indextests --> Int|(List of Pair) )
 multi Array::values (@array : MatchTest *@indextests --> Int|List )

Iterates the elements of C<@array>, in order. 

If C<@indextests> are provided, only elements whose indices match
C<$index ~~ any(@indextests)> are iterated.

What is returned at each element of the iteration varies with function.
C<values> returns the value of the associated element; C<kv> returns 
a 2 element list in (index, value) order, C<pairs> a C<Pair(index, value)>.

C<@array> is considered single dimensional. If it is in fact multi-
dimensional, the values returned will be array references to the sub
array.

In Scalar context, they all return the count of elements that would have
been iterated.

=back



=head2 List

=over

=item grep

 multi Array::grep (@values :      Code *&test   --> Lazy )
 multi Array::grep (@values,   MatchTest $test   --> Lazy )
 multi  List::grep (MatchTest $test :   *@values --> Lazy ) {
   gather {
     for @values -> $x {
       take $x if $x ~~ $test;
     }
   }
 }


=item join

 multi Array::join (@values,   Str $delimiter --> Str )
 multi  List::join (Str $delimiter : *@values --> Str ) {
   my $str = ~@values[0];
   for 1..@values.end {
     $str ~= $delimiter ~ @values[$_];
   }
   $str;
 }
 &join<> := &join<Str>.assuming:delimiter(' ');


=item map 

 multi Array::map (@values,   Code $expression --> Lazy ) 
 multi  List::map (Code $expression : *@values --> Lazy ) {
   gather {
     while @values {
       take $expression
          .( splice(@values, 0, $expression.arity) );
     }
   }
 }


=item reduce

 multi Array::reduce (@values : Code *&expression --> Scalar )
 multi  List::reduce (Code $expression : *@values --> Scalar ) {
   my $res;
   for @values -> $cur {
        FIRST {$res = $cur; next;}
     $res = &$expression($res, $cur);
   }
   $res;
 }


=item reverse

 multi Hash::reverse (%hash --> Hash ) is default {
   my %result;
   for %hash.kv -> $k, $v {
     %result{$v} = $k;
   }
   %result;
 }

 multi Array::reverse (   @values --> Lazy|Str) {
 multi  List::reverse ( *@values --> Lazy|Str) {
   given want {
     when List {
       gather {
         1 while take pop @values;
       }
     }
     when Scalar {
       reverse @values ==> join;
     }
   }
 }


=item sort

 type KeyExtractor  ::= Code(Any --> Any );
 type Comparator    ::= Code(Any, Any --> Int );
 type SortCriterion ::= KeyExtractor
                      | Comparator
                      | Pair(KeyExtractor, Comparator);

 multi Array::sort(                 @values is rw,
                                              *&by
                              :           Bit :$inplace
                             --> Array )

 multi Array::sort(                 @values is rw,
                                SortCriterion  @by
                              :           Bit :$inplace
                             --> Array )

 multi Array::sort(                 @values is rw
                              : SortCriterion :$by = &infix:<cmp>,
                                          Bit :$inplace
                             --> Array )

 multi  List::sort(  SortCriterion  @by
                              :               *@values
                             --> List )

 multi  List::sort( SortCriterion  $by = &infix:<cmp>,
                                              *@values
                             --> List )

Returns C<@values> sorted, using criteria C<$by> or C<@by> for
comparisions. C<@by> differs from C<$by> in that each criteria is
applied, in order, until a non-zero (tie) result is achieved.

Criterion can take a few different forms:

=over 8

=item Comparator

A closure with arity of 2, which returns negative/zero/positive,
signaling the first arguement should be before/tied with/after the
second in the final ordering of the List. aka "The Perl 5 way"

=item KeyExtractor

A closure with arity of 1, which returns the "key" by which to sort. If
the closure returns a Num, C<E<lt>=E<gt>> is used for comparison,
otherwise C<cmp>.

=item Pair(KeyExtractor, Comparator)

A combination of the two methods above, for when one wishs to take
advantage of the internal caching of keys that is expected to happen,
but wishes to compare them with something other than C<E<lt>=E<gt>> or
C<cmp>.

=back

Any Criterion may recieve either or both of the traits C<is descending>
and C<is insensitive> to reverse the order of sort, or the adjust the
case sensitivity of C<cmp> as a Comparator.

If all criteria are exhausted when comparing two elements, sort should
return them in the same relative order they had in C<@values>.

If C<$inplace> is specified, the array is sorted in place.

See L<http://www.nntp.perl.org/group/perl.perl6.language/16578> for more
details and examples.



=item zip

 multi Lists::zip ( Array *@lists, Bit :$shortest --> Lazy ) {
   gather {
     while $shortest ?? all(@lists) !! any(@lists) {
       for @lists -> @list {
         take shift @list;
       }
     }
   }
 }

[Note: This should be the definition of each() now.  The zip function needs
to build tuples of the "across" values.  Also, it maybe probably be
in terms of longest non-infinite.  -law]

=back



=head2 Hash

=over 4

=item delete

 multi method Hash::delete ( *@keys --> List )
 multi method Hash::delete (   $key --> Scalar ) is default

Deletes the elements specified by C<$key> or C<$keys> from the invocant.
returns the value(s) that were associated to those keys.

=over

=item Unary Form

Implementations should create a suitable macro, or otherwise support the
unary form C<delete %hash{$key}> in all it's forms. Below are some
example translations. This list is I<not> exhaustive.

 delete %hash{$key}                %hash.delete{$key}
 delete %hash<key>                 %hash.delete{'key'}
 delete %hash<key1>{@keys}         %hash<key1>.delete{@keys}


=back


=item exists
 
 multi method Hash::exists ($key --> Bool )

True if invocant has an element whose key matches C<$key>, false
otherwise.

An unary form is expected. See Hash::delete


=item keys

=item kv

=item pairs

=item values

 multi Hash::keys   (%hash : MatchTest *@keytests --> Int|List )
 multi Hash::kv     (%hash : MatchTest *@keytests --> Int|List )
 multi Hash::pairs  (%hash : MatchTest *@keytests --> Int|(List of Pair) )
 multi Hash::values (%hash : MatchTest *@keytests --> Int|List )
 
Iterates the elements of C<%hash> in no apparent order, but the order
will be the same between successive calls to these functions, as long as
C<%hash> doesn't change. 

If C<@keytests> are provided, only elements whose keys evaluate
C<$key ~~ any(@keytests)> as true are iterated.

What is returned at each element of the iteration varies with function.
C<keys> only returns the key; C<values> the value; C<kv> returns both as
a 2 element list in (key, value) order, C<pairs> a C<Pair(key, value)>.

Note that C<kv %hash> returns the same as C<zip(keys %hash; values %hash)>

In Scalar context, they all return the count of elements that would have
been iterated.

The lvalue form of C<keys> is not longer supported. Use the C<.buckets>
property instead.

=back


=head2 Str

General notes about strings:

A Str can exist at several Unicode levels at once. Which level you
interact with typically depends on what your current lexical context has
declared the "working unicode level to be". Default is LChars.

[Q: Can't be LChars because we don't go into "language" mode unless there's
a specific language declaration saying either exactly what language
we're going into, or what environmental parameter to pay attention to
to select our language.  so I suspect the default should be Grf. -law]

Attempting to use a string at a level higher it can support is handled
without warning. The highest supported level is simply mapped char for
char to the desired level. However, attempting to stuff something into
the string at a higher level that doesn't map to the lower level is an
error (for example, attempting to store Kanji in a Byte uplifted to an LChar).

Attempting to use a string at a level lower than what it supports is not
allowed.

If a function takes a C<Str> and returns a C<Str>, the returned C<Str>
will support the same levels as the input, unless specified otherwise.

=over

=item chop

 multi Str::chop (  Str  $string is rw                 --> Char )
 multi Str::chop ( Str *@strings = (CALLER::<$_>) is rw --> Char )

Trims the last character from C<$string>, and returns it. Called with a
list, it chops each item in turn, and returns the last character
chopped.


=item chomp

 multi Str::chomp (  Str  $string is rw                 --> Int )
 multi Str::chomp ( Str *@strings = (CALLER::<$_>) is rw --> Int )

Related to C<chop>, only removes trailing chars that match C</\n/>. In
either case, it returns the number of chars removed.

Note: Most users should consider setting their I/O handles to autochomp
instead of this step.


=item lc

 multi Str::lc         (  Str $string              --> Str )
 multi Str::lc         ( Str $string = CALLER::<$_> --> Str )

Returns the input string after converting each character to it's lowercase
form, if uppercase.


=item lcfirst

 multi Str::lcfirst    (  Str $string              --> Str )
 multi Str::lcfirst    ( Str $string = CALLER::<$_> --> Str )

Like C<lc>, but only affects the first character.


=item uc

 multi Str::uc         (  Str $string              --> Str )
 multi Str::uc         ( Str $string = CALLER::<$_> --> Str )

Returns the input string after converting each character to it's uppercase
form, if lowercase. This is not a Unicode "titlecase" operation, but a
full "uppercase".


=item ucfirst

 multi Str::ucfirst    (  Str $string              --> Str )
 multi Str::ucfirst    ( Str $string = CALLER::<$_> --> Str )

Performs a Unicode "titlecase" operation on the first character of the string.


=item capitalize

 multi Str::capitalize (  Str $string              --> Str )
 multi Str::capitalize ( Str $string = CALLER::<$_> --> Str )

Has the effect of first doing an C<lc> on the entire string, then performing a
C<s:g/(\w+)/{ucfirst $1}/> on it.


=item length

This word is banned in Perl 6.  You must specify units.

=item index 

Needs to be in terms of StrPos, not Int.

=item pack

=item pos

=item quotemeta

=item rindex 

Needs to be in terms of StrPos, not Int.

=item split

 multi Str::split (  Str $delimiter ,  Str $input = CALLER::<$_>, Int $limit = inf --> List )
 multi Str::split ( Rule $delimiter ,  Str $input = CALLER::<$_>, Int $limit = inf --> List )
 multi Str::split (      Str $input :  Str $delimiter          , Int $limit = inf --> List )
 multi Str::split (      Str $input : Rule $delimiter          , Int $limit = inf --> List )
 &split<> := &split<Str>.assuming:delimiter(' ');

String delimiters must not be treated as rules but as constants.  The
default is no longer ' ' since that would be interpreted as a constant.

# sprintf # XXX - see Prelude.pm  And p5's sv.c

 multi substr(Str $s, StrPos $start  : StrPos $end,      $replace)
{ Pugs::Internals::substr($s,undef,$start,undef,$end,$replace) }
 multi substr(Str $s, StrPos $start,   StrLen $length  : $replace)
{ Pugs::Internals::substr($s,undef,$start,$length,undef,$replace) }
 multi substr(Str $s, StrLen $offset : StrLen $length,   $replace)
{ Pugs::Internals::substr($s,$offset,undef,$length,undef,$replace) }


# unpack

# vec # XXX - obsolete?


package Control::Basic {}

 multi Control::Basic::eval ( Str $code = CALLER::<$_>, Grammar :$lang = CALLER::<$?PARSER>)
{
    Pugs::Internals::eval($code,$lang);
}


 multi Control::Basic::evalfile (Str $filename : Grammar :$lang = Perl6)
{
    eval(slurp($filename),$lang);
}


 multi Control::Basic::exit ( Int $status = 0)
{
    Pugs::Internals::exit($status);
}


 multi Control::Basic::nothing ()


 multi Control::Basic::sleep ( Num $for = Inf --> Num )
{
    Pugs::Internals::sleep($for);
}


# die 

# fail


# bless 

# chr

# ord


 multi Conversions::List::list ( *@list --> List )
{
    @list; #XXX - sufficient to "Forces List Context"?
}


 multi Conversions::Item::item ($item --> Item )
{
    $item; #XXX - sufficient to "Forces generic Item context"?
}

 
 multi prefix:<:16> ( Str $hexstr = CALLER::<$_> --> Num )
{ Pugs::Internals::string_as_number(16,$hexstr) }
 multi prefix:<:8> ( Str $octstr = CALLER::<$_> --> Num )
{ Pugs::Internals::string_as_number(8,$octstr) }
 multi prefix:<:2> ( Str $binstr = CALLER::<$_> --> Num )
{ Pugs::Internals::string_as_number(2,$binstr) }
 multi prefix:<:10> ( Str $binstr = CALLER::<$_> --> Num )
{ Pugs::Internals::string_as_number(10,$decstr) }
# etc. XXX - etc?  arbitrary n?  how arbitrary?

sub Pugs::Internals::string_as_number (Int $radix, Str $string --> Num)
{ ... } # XXX - oh that is non-trivial.


package Time::Local {}
# gmtime 

# localtime 

# time


# study

# defined

# undef

# item 

# want

# caller


# tie tied untie


# -X accept alarm bind binmode chown close closedir connect eof fcntl
# fileno flock getc getpeername
# /[get|set][host|net|proto|serv|sock].*/ glob ioctl link listen 
# lstat mkdir open opendir pipe print printf read readdir readline
# readlink readpipe recv rename rewinddir rmdir seek seekdir select(both)
# send setsockopt shutdown slurp socket socketpair stat symlink
# syscall sysopen sysread sysseek syswrite tell telldir truncate umask
# unlink utime warn


# chroot crypt exec getlogin /[get|set][pw|gr].*/ kill setpgrp setpriority
# system times


# fork lock wait waitpid

