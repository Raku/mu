package Regexp::Parser;

$VERSION = '0.21';

use 5.006;
use Carp qw( carp croak );
use base 'Exporter';
use strict;
use warnings;
use charnames ();

our %loaded;
our @EXPORT = qw( Rx RxPOS RxCUR RxLEN Rf SIZE_ONLY LATEST );

sub Rx :lvalue		{ $_[0]{regex} }
sub RxPOS :lvalue	{ pos ${&Rx} }
sub RxCUR		{ substr ${&Rx}, &RxPOS }
sub RxLEN		{ $_[0]{len} }

sub Rf :lvalue		{ $_[0]{flags}[-1] }

sub SIZE_ONLY		{ ! $_[0]{tree} }
sub LATEST :lvalue	{ $_[0]{tree}[-1] }

use Regexp::Parser::Diagnostics;
use Regexp::Parser::Objects;
use Regexp::Parser::Handlers;


# this handles 'use base "Regexp::Parser"'
# which wouldn't call 'import'
{
  my ($level, $prev, $pkg);
  while (my ($curr) = caller $level++) {
    $pkg = $curr, last if $prev and $prev eq "base" and $curr ne "base";
    $prev = $curr;
  }
  Regexp::Parser->export_to_level($level, $pkg, @EXPORT) if $pkg;
}


sub new {
  my ($class, $rx) = @_;
  my $self = bless {}, $class;
  $self->init;
  $self->regex($rx) if defined $rx;
  return $self;
}


sub regex {
  my ($self, $rx) = @_;
  %$self = (
    regex => \"$rx",
    len => length $rx,
    tree => undef,
    stack => [],
    maxpar => 0,
    nparen => 0,
    captures => [],
    flags => [0],
    next => ['atom'],
  );

  # do the initial scan (populates maxpar)
  # because tree is undef, nothing gets built
  &RxPOS = 0;
  eval { $self->parse };
  $self->{errmsg} = $@, return if $@;

  # reset things, define tree as []
  &RxPOS = 0;
  $self->{tree} = [];
  $self->{flags} = [0];
  $self->{next} = ['atom'];

  return 1;
}


sub parse {
  my ($self, $rx) = @_;
  $self->regex($rx) or return if defined $rx;
  croak "no regex defined" unless &RxLEN;
  1 while $self->next;
  return 1;
}


sub root {
  my ($self) = @_;
  $self->parse if $self->{stack};
  return $self->{tree};
}


sub nparen {
  my ($self) = @_;
  $self->parse if $self->{stack};
  return $self->{nparen};
}


sub captures {
  my ($self, $n) = @_;
  $self->parse if $self->{stack};
  return $self->{captures}[--$n] if $n;
  return $self->{captures};
}


sub nchar {
  my $self = shift;
  return map chr(/^\^(\S)/ ? (64 ^ ord $1) : charnames::vianame($_)), @_;
}


sub error_is {
  my ($self, $enum) = @_;
  return $self->{errnum} && $self->{errnum} == $enum;
}


sub errmsg {
  my ($self) = @_;
  return $self->{errmsg};
}


sub errnum {
  my ($self) = @_;
  return $self->{errnum};
}


sub error {
  my ($self, $enum, $err, @args) = @_;
  $err .= "; marked by <-- HERE in m/%s <-- HERE %s/";
  push @args, substr(${&Rx}, 0, &RxPOS), &RxCUR;
  $self->{errnum} = $enum;
  $self->{errmsg} = sprintf $err, @args;
  croak $self->{errmsg};
}


sub warn {
  my ($self, $enum, $err, @args) = @_;
  $err .= "; marked by <-- HERE in m/%s <-- HERE %s/";
  push @args, substr(${&Rx}, 0, &RxPOS), &RxCUR;
  carp sprintf $err, @args if &SIZE_ONLY;
}


sub awarn {
  my ($self, $enum, $err, @args) = @_;
  local $self->{tree};
  $self->warn($enum, $err, @args);
}


sub next {
  my ($self) = @_;
  croak "no regex defined" unless &RxLEN;

  while (my $try = pop @{ $self->{next} }) {
    if (defined(my $r = $self->$try)) {
      $r->insert($self->{tree});
      return $r;
    }
  }

  $self->error(RPe_RPAREN) if ${&Rx} =~ m{ \G \) }xgc;
  $self->error(0, "PANIC! %d %s", &RxPOS, &RxCUR) if &RxPOS != &RxLEN;

  if (! &SIZE_ONLY) {
    $self->{tree} = pop @{ $self->{stack} } while @{ $self->{stack} };
    delete $self->{stack};
  }

  return;
}


sub walker {
  my ($self, $depth) = @_;
  croak "no regex defined" unless &RxLEN;
  $self->parse if $self->{stack};

  $depth = -1 unless defined $depth;
  my $d = $depth;
  my @stack = @{ $self->{tree} };
  my $next;

  return sub {
    return $depth if @_ and $_[0] eq -depth;
    carp "unexpected argument ($_[0]) to iterator" if @_;

    {
      $next = shift @stack;
      $d += $next->(), redo if ref($next) eq "CODE";
    }

    @stack = @{ $self->{tree} }, return unless $next;
    $next->walk(\@stack, $d);
    return wantarray ? ($next, $depth-$d) : $next;
  }
}


sub visual {
  my ($self) = @_;
  $self->parse if $self->{stack};
  my $vis = join "", map($_->visual, @{ $self->{tree} });
  return $vis;
}


sub qr {
  my ($self) = @_;
  $self->parse if $self->{stack};
  my $rx = $self->{tree};
  no warnings 'regexp';
  use re 'eval';

  if (@$rx == 1 and $rx->[0]->family eq 'group') {
    my $vis = join "", map $_->qr, @{ $rx->[0]->{data} };
    return eval('qr/$vis/' . $rx->[0]->on);
  }

  $rx = join "", map($_->qr, @$rx);
  return qr/$rx/;
}


sub nextchar {
  my ($self) = @_;

  {
    if (${&Rx} =~ m{ \G \(\?\# [^)]* }xgc) {
      ${&Rx} =~ m{ \G \) }xgc and redo;
      $self->error(RPe_NOTERM);
    }
    &Rf & $self->FLAG_x and ${&Rx} =~ m{ \G (?: \s+ | \# .* )+ }xgc and redo;
  }
}


sub object {
  return if &SIZE_ONLY;
  my $self = shift;
  $self->force_object(@_);
}


sub force_object {
  Carp::croak("class name passed where object required") unless ref $_[0];
  my $type = splice @_, 1, 1;
  my $ref = ref $_[0];
  my $class = "${ref}::$type";

  if ($ref ne __PACKAGE__ and !$loaded{$class}++) {
    no strict 'refs';
    my $orig_base = $Regexp::Parser::{$type . '::'};
    my $user_base = ${"${ref}::"}{'__object__::'};

    push @{ "${class}::ISA" }, $ref . "::__object__" if $user_base;
    push @{ "${class}::ISA" }, __PACKAGE__ . "::$type" if $orig_base;
  }

  return $class->new(@_);
}


sub add_flag {
  my ($self, $seq, $func) = @_;
  no strict 'refs';
  no warnings 'redefine';
  *{ ref($self) . "::FLAG_$seq" } = $func;
}


sub del_flag {
  my ($self, @flags) = @_;
  no strict 'refs';
  my $stash = \%{ ref($self) . "::" };
  undef $stash->{"FLAG_$_"} for @flags;
}


sub add_handler {
  my ($self, $seq, $func) = @_;
  no strict 'refs';
  no warnings 'redefine';
  *{ ref($self) . "::$seq" } = $func;
}


sub del_handler {
  my ($self, @handles) = @_;
  no strict 'refs';
  my $stash = \%{ ref($self) . "::" };
  undef $stash->{$_} for @handles;
}


1;

__END__

=cut

=head1 NAME

Regexp::Parser - base class for parsing regexes

=head1 SYNOPSIS

See examples in L<"USAGE">.

=head1 WARNING

This is version B<0.022b>.  The documentation is (still) incomplete.  It
may be a little jumbled or hard to understand.  If you find a problem,
please let L<me|/"AUTHOR"> know.

Documentation has been added and moved around. See
L<Regexp::Parser::Objects> for documentation about nodes and the objects
that represent them.  See L<Regexp::Parser::Handlers> for information
about sub-classing this module.

=head1 DESCRIPTION

This module parses regular expressions (regexes).  Its default "grammar"
is Perl 5.8.4's regex set.  Grammar is quoted because the module does
not so much define a grammar as let each matched node state what it
expects to match next, but there is not currently a way of extracting a
complete grammar.  This may change in future versions.

This module is designed as a replacement (though not drop-in) for my old
F<YAPE::Regex> modules.

=head1 USAGE

=head2 Creating an Instance

To use this module as is, load it, and create an instance:

  use Regexp::Parser;
  my $parser = Regexp::Parser->new;

=head2 Setting a Regex

To have the parser work on a specific regex, you can do use any of the
following methods:

=over 4

=item $parser = Regexp::Parser->new($regex)

You can send the regex to be parsed as the argument to the constructor.

=item $parser->regex($regex)

Clears the parser's memory and sets $regex as the regex to be parsed.

=back

These two approaches do an initial pass over the regex to make sure it
is well-formed -- any warnings or errors will be determined during this
initial pass.

=head3 Fatal Errors

B<If there is a compilation-stopping error>, $parser->errmsg will return
that error message, and $parser->errnum will return the numerical value
of the message.  If you use new() the F<Regexp::Parser> object will
still be returned, but if you use regex() then it will return false.

  if (! $parser->regex($rx)) {
    my $errmsg = $parser->errmsg;
    my $errnum = $parser->errnum;
    # ...
  }

If you want to see if an error is a particular error, see
L</"ERROR HANDLING">.

=head2 Inspecting the Parsing

To intercept each node as it is parsed, use the next() method:

  while (my $node = $parser->next) {
    # $node is a Regexp::Parser::* object
  }

When the regex is finished being parsed, next() returns false, and will
return false if called again.

=head2 Building the Tree

If you don't care to intercept the building of the tree, you can use the
parse() method to explicitly build it:

  $parser->parse;

This is not necessary, though, because the following methods will
invoke parse() if the tree has not been made yet.

=head2 Setting and Parsing Together

You can also use parse() instead of regex() to set the regex and create
the tree in one step:

  my $ok = $parser->parse($new_regex);

Again, $ok will be false if a fatal error was raised in the inital scan
of the regex.

=head2 Getting the Tree

You can access the root of the tree with the root() method:

  my $root = $parser->root;

It will be an array reference of objects.

=head2 Getting the OPEN Count

You can access the number capture groups with the nparen() method:

  my $captgroups = $parser->nparen;

=head2 Getting All Captures

You can access all the capture groups with the captures() method:

  my $all_captures = $parser->captures();

If you want to access a specific capture group, pass its numerical
value:

  my $capture_2 = $parser->captures(2);

=head2 Walking the Tree

To walk over the created tree, create an iterator with walker()Z<>:

  my $iter = $parser->walker;

This will produce an iterator that will traverse the entire parse tree,
to any depth.  To restrict the depth to which it reaches, pass walker()
an argument:

  my $iter = $parser->walker(0);  # top-level
  my $iter = $parser->walker(1);  # top- and second-level
  my $iter = $parser->walker(2);  # top- through third-level

The iterator returned is a function reference.  When called in scalar
context, it returns the next node:

  while (my $node = $iter->()) {
    # $node is a Regexp::Parser::* object
  }

In list context, it returns the next node and its depth:

  while (my ($node, $depth) = $iter->()) {
    # $node is a Regexp::Parser::* object
    # $depth = 0, 1, 2...
  }

If passed the argument C<-depth>, it returns the depth to which it will
look:

  while (my ($node, $depth) = $iter->()) {
    if ($depth == $iter->(-depth)) {
      # this is as deep as it will look
    }
  }

If passed any other argument, it will warn that it is ignoring it.

The iterator will return undef when it has reached the end of the tree;
it will then reset itself, and will start from the beginning the next
time it is called.

=head2 Viewing the Regex

You can get the regex back from the parser with the visual() method:

  my $rx = $parser->visual;

This will not return a F<Regexp> object, but the regex; it might be
slightly different from the regex you passed it, but it will not operate
differently.

The string representation is built by calling the visual() method of
each node in the tree.

=head2 Using the Regex

You can use the qr() method to get back a F<Regexp> object:

  my $real_rx = $parser->qr;

The regex is formed by calling the qr() method of each node in the tree,
which may be different from the visual() method; specifically, in the case
of a sub-class that adds a handler, the qr() method is used to produce
the Perl regex implementation of the new node.

=head2 Named Character Support

Perl's regex engine doesn't see \N{NAME} escapes -- they get interpolated
by Perl first.  In fact, if one slipped through:

  my $rx = '\N{LATIN CAPITAL LETTER R}';
  my $qr = qr/$rx/;

Perl's regex interprets the '\N' as a needlessly backslashed 'N'.  My
module parses them and handles them properly.  The nchar() method takes
a named character's name, and returns the actual character:

  my $R = $parser->nchar("LATIN CAPITAL LETTER R");

This means you must have the F<charnames> pragma installed, but since
this module requires Perl 5.6 or better, I don't expect that to be a
problem.

=head2 Using the Tree

If you want to work with the parse tree independently, use the root()
method to get it.  From there, you're on your own.  You'll probably
want to make a recursive function that takes an object (or a reference
to an array of them) and does something to them (and their children).

=head1 ERROR HANDLING

=head2 Determining Error

Use the errmsg() and errnum() methods to get the error information.

To see if an error is a particular one, use the error_is() method:

  if ($parser->error_is($parser->RPe_BCURLY)) {
    # there was a {n,m} quantifier with n > m
  }

=head2 Standard Warnings and Errors

Here are the standard warning and error messages.  Their values are all
negative; positive values are left available for extensions.  Please
refer to L<perldiag> for the explanations of the messages.

These are all constants in the F<Regexp::Parser> package, which means
you can access them as though they were methods.  They return two
values, their numeric value, and a format string for use with sprintf().

  # for when you have a zero-width chunk
  # with a boundless quantifier on it
  my ($num, $fmt) = $parser->RPe_NULNUL;

=over 4

=item RPe_ZQUANT (-1)

Quantifier unexpected on zero-length expression

=item RPe_NOTIMP (-2)

Sequence (?%.*s...) not implemented

=item RPe_NOTERM (-3)

Sequence (?#... not terminated

=item RPe_LOGDEP (-4)

(?p{}) is deprecated -- use (??{})

=item RPe_NOTBAL (-5)

Sequence (?{...}) not terminated or not {}-balanced

=item RPe_SWNREC (-6)

Switch condition not recognized

=item RPe_SWBRAN (-7)

Switch (?(condition)... contains too many branches

=item RPe_SWUNKN (-8)

Unknown switch condition (?(%.2s

=item RPe_SEQINC (-9)

Sequence (? incomplete

=item RPe_UQUANT (-10)

Useless (%s%s) -- %suse /%s modifier

=item RPe_NOTREC (-11)

Sequence (?%.*s...) not recognized

=item RPe_LPAREN (-12)

Unmatched (

=item RPe_RPAREN (-13)

Unmatched )

=item RPe_BCURLY (-14)

Can't do {n,m} with n > m

=item RPe_NULNUL (-15)

%s matches null string many times

=item RPe_NESTED (-16)

Nested quantifiers

=item RPe_LBRACK (-17)

Unmatched [

=item RPe_EQUANT (-18)

Quantifier follows nothing

=item RPe_BRACES (-19)

Missing braces on \%s{}

=item RPe_RBRACE (-20)

Missing right brace on \%s{}

=item RPe_BGROUP (-21)

Reference to nonexistent group

=item RPe_ESLASH (-22)

Trailing \

=item RPe_BADESC (-23)

Unrecognized escape %s%s passed through

=item RPe_BADPOS (-24)

POSIX class [:%s:] unknown

=item RPe_OUTPOS (-25)

POSIX syntax [%s %s] belongs inside character classes

=item RPe_EMPTYB (-26)

Empty \%s{}

=item RPe_FRANGE (-27)

False [] range "%s-%s"

=item RPe_IRANGE (-28)

Invalid [] range "%s-%s"

=back

=head1 EXTENSIONS

Here are some ideas for extensions (sub-classes) for this module.  Some
of them may be absorbed into the core functionality of F<Regexp::Parser>
in the future.  Module names are merely the author's suggestions.

=over 4

=item Regexp::WordBounds

Adds handlers for C<< < >> and C<< > >> anchors, which match at the
beginning and end of a "word", respectively.  C<< /</ >> is equivalent to
C</(?!\w)(?=\w)/>, and C<< />/ >> is equivalent to C</(?<=\w)(?!\w)/>. (So
that's the object's qr() method for you right there!)

=item Regexp::MinLength

Implements a min_length() method for all objects that determines the
minimum length of a string that would be matched by the regex; provides
a front-end method for the parser.

=item Regexp::QuantAttr

Removes quantifiers as objects, and makes 'min' and 'max' attributes of
other objects themselves.

=item Regexp::Explain (pending, Jeff Pinyan)

Produces a human-readable explanation of the execution of a regex.  Will
be able to produce HTML output that color-codes the elements of the regex
according to a style-sheet (syntax highlighting).

=item Regexp::Reverse (difficulty rating: ****)

Reverses a regex so it matches backwards.  Ex.: C</\s+$/> becomes
C</^\n?\s+/>, which perhaps gets optimized to C</^\s+/>.  The difficulty
rating is so high because of cases like C</(\d+)(\w+)/> which, when
reversed, I<can> match differently.

  "100years" =~ /(\d+)(\w+)/;  # $1 = 100, $2 = years
  "sraey001" =~ /(\w+)(\d+)/;  # $1 = sraey00, $2 = 1

This means character classes should store a hash of what characters
they represent, as well as the macros C<\w>, C<\d>, etc.  Then this
example would be reversed into something like C</(\w+(?<!\d))(\d+)/>.
The other difficulty is complex regexes with if-then assertions.  I
don't want to think about that.  This module is more of a theoretical
exercise, a jump-start to built-in reversing capability in Perl.

=item Regexp::CharClassOps

Implements character class operations like union, intersection, and
subtraction.

=item Regexp::Optimize

Eliminates redundancy from a regex.  It should have various options,
such as whether to do optimize...

  # strings
  /foo|father|fort/  => /f(?:o(?:o|rt)|ather)/

  # char classes
  /[\w\d][a-zaeiou]/ => /[\w][a-z]/

  # redundancy
  /^\n?\s+/          => /^\s+/
  /[\w]/             => /\w/

There are other possibilities as well.

=back

=head1 HISTORY

=head2 0.022b -- July 6, 2004

=over 4

=item Hierarchy Changes

There are now abstract classes I<anchor> and I<assertion>. You can't call
their new() method directly, you can only call it through an object that
inherits from that class.

There are no longer I<star>, I<plus>, and I<curly> classes; they have been
combined into one class, I<quantifier>.  You pass it the min and max,
and the object's C<type> is determined dynamically.

=item Character Class Hashes

Character classes (I<anyof> objects) now have another attribute, C<charmap>,
which is a hash reference holding character values (eg. 65 for 'A') and
the number of times that character appeared in the character class.  The
character class C<[A-CB-E]> would have a character map of C<< { 65 => 1, 66
=> 2, 67 => 2, 68 => 1, 69 => 1} >>.  This will reflect ranges and embedded
classes (such as C<[:cntrl:]> or C<\p{Print}>.

=item Character Class Rendering

The visual() method of I<anyof> objects will quell the repetition of any
character in the class I<outside> of embedded classes, so the class
C<[\w\d:4-65:]> will render as C<[\w\d:4-6]>.  If you want to prevent
characters and ranges from being display if they are included in an embedded
class, set the I<anyof> object's C<strict> attribute to 1; the character
class would render as C<[\w\d:]>.  If you want to go even further and remove
any embedded class that is I<entirely> redundant (that is, I<every>
character in that embedded class is already found in the class), set the
C<strict> attribute to 2; the class above would render as C<[\w:]>.

=back

=head2 0.021 -- July 3, 2004

=over 4

=item I<anyof_class> Changed

If an I<anyof_class> element is a Unicode property or a Perl class (like
C<\w> or C<\S>), the object's C<data> field points to the underlying
object type (I<prop>, I<alnum>, etc.).  If the element is a POSIX class,
the C<data> field is the string "POSIX".  POSIX classes don't exist in a
regex outside of a character class, so I'm a little wary of making them
objects in their own right, even if it would create a better sense of
uniformity.

=item Documentation

Fixed some poor wording, and documented the problem with using F<SUPER::>
inside F<MyClass::__object__>.

=item Bug Fixes

Character classes weren't closing properly in the tree.  Fixed.

Standard escapes (C<\a>, C<\e>, etc.) were being returned as I<exact>
nodes instead of I<anyof_char> nodes when inside character classes.  Fixed.
(Mike Lambert)

Non-grouping parentheses weren't being parsed properly.  Fixed.  (Mike
Lambert)

Flags weren't being turned off.  Fixed.

=back

=head2 0.02 -- July 1, 2004

=over 4

=item Better Abstracting

The object() method calls force_object().  force_object() creates an
object no matter what pass the parser is making; object() will return
immediately if it's just the first pass.  This means that force_object()
should be used to create stand-alone objects.

Each object now has an insert() method that defines how it gets placed
into the regex tree.  Most objects inherit theirs from the base object
class.

The walker() method is also now abstracted -- each node it comes across
will have its walk() method called.  And the ending node for stack-type
nodes has been abstracted to the ender() method of the node.

The init() method has been moved to another file to help keep I<this>
file as abstract as possible.  F<Regexp::Parser> installs its handlers
in F<Regexp/Parser/Handlers.pm>.  That file might end up being where
documentation on writing handlers goes.

The documentation on sub-classing includes an ordered list of what
packages a method is looked up in for a given object of type 'OBJ':
F<YourMod::OBJ>, F<YourMod::__object__>, F<Regexp::Parser::OBJ>,
F<Regexp::Parser::__object__>.

=item Cleaner Grammar Flow

Now the only places 'atom' gets pushed to the queue are after an opening
parenthesis or after 'atom' matches.  This makes things flow more
cleanly.

=item Flag Handlers

Flag handlers now receive an additional argument that says whether
they're being turned on or off.  Also, if the flag handler returns 0,
that flag is removed from the resulting object's visual flag set.  That
means C<(?gi-o)> becomes C<(?i)>.

=item Diagnostics and Bug Fixes

More tests added (specifically, making sure C<(?(N)T|F)> works right).
In doing so, found that the "too many branches" error wasn't being raised
until the second pass.  Figured out how to improve the grammar to get
it to work properly.  Also added tests for the new captures() method.

I changed the field 'class' to 'family' in objects.  I was getting
confused by it, so I figured it was a sign that I'd chosen an awful name
for the field.  There will still be a class() method in F<__object__>,
but it will throw a "use of class() is deprecated" warning.

Quantifiers of the form C<{n}> were being misrepresented as C<{n,}>.
It's been corrected.  (Mike Lambert)

C<\b> was being turned into "b" inside a character class, instead of
a backspace.  (Mike Lambert)

Fixed errant "Quantifier unexpected" warning raised by a zero-width
assertion followed by C<?>, which doesn't warrant the warning.

Added "Unrecognized escape" warnings to I<all> escape sequence handlers.

The 'g', 'c', and 'o' flags now evoke "Useless ..." warnings when used
in flag and non-capturing group constructs.

=back

=head2 0.01 -- June 29, 2004

=over 4

=item First Release

Documentation not complete, etc.

=back

=head1 CAVEATS

=over 4

=item * Bugs...?

I'd like to say this module doesn't have bugs.  I don't know of any in
this current version, because I've tried to fix those I've already
found. Those who find bugs should email me.  Messages should include the
code you ran that contains the bug, and your opinion on what's wrong
with it.

=item * Variable interpolation

This module parses I<regexes>, not Perl.  If you send a single-quoted
string as a regex with a variable in it, that '$' will be interpreted as
an anchor. If you want to include variables, use C<qr//>, or mix single-
and double-quoted strings in building your regex.

=back

=head1 AUTHOR

Jeff C<japhy> Pinyan, F<japhy@perlmonk.org>

=head1 COPYRIGHT

Copyright (c) 2004 Jeff Pinyan F<japhy@perlmonk.org>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
