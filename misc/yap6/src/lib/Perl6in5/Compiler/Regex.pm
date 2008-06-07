#
# This software is Copyright 2005 by Elsevier Inc.  You may use it
# under the terms of the license at http://perl.plover.com/hop/LICENSE.txt .
#



###
### Regex.pm
###

## Chapter 6 section 5

package Regex;
use Stream ':all';
use base 'Exporter';
@EXPORT_OK = qw(literal union concat star plus charclass show
                matches);
sub literal {
  my $string = shift;
  node($string, undef);
}


## Chapter 6 section 5

sub mingle2 {
  my ($s, $t) = @_;
  return $t unless $s;
  return $s unless $t;
  node(head($s), 
       node(head($t), 
                promise { mingle2(tail($s), 
                                  tail($t)) 
                                }
      ));
}
sub union {
  my ($h, @s) = grep $_, @_;
  return unless $h;
  return $h unless @s;
  node(head($h),
       promise {
         union(@s, tail($h));
       });
}


## Chapter 6 section 5

sub concat {
  my ($S, $T) = @_;
  return unless $S && $T;

  my ($s, $t) = (head($S), head($T));

  node("$s$t", promise {
    union(postcat(tail($S), $t),
           precat(tail($T), $s),
          concat(tail($S), tail($T)),
         )
  });
}


## Chapter 6 section 5

sub precat {
  my ($s, $c) = @_;
  transform {"$c$_[0]"} $s;
}

sub postcat {
  my ($s, $c) = @_;
  transform {"$_[0]$c"} $s;
}


## Chapter 6 section 5

sub star {
  my $s = shift;
  my $r;
  $r = node("", promise { concat($s, $r) });
}


## Chapter 6 section 5

sub show {
  my ($s, $n) = @_;
  while ($s && (! defined $n || $n-- > 0)) {
    print qq{"}, drop($s), qq{"\n};
  }
  print "\n";
}


## Chapter 6 section 5

# charclass('abc') = /^[abc]$/
sub charclass {
  my ($s, $class) = @_;
  union(map literal($_), split(//, $class));
}

# plus($s) = /^s+$/
sub plus {
  my $s = shift;
  concat($s, star($s));
}

1;


## Chapter 6 section 5.1

sub union {
  my (@s) = grep $_, @_;
  return unless @s;
  return $s[0] if @s == 1;
  my $si = index_of_shortest(@s);
  node(head($s[$si]),
              promise {
                union(map $_ == $si ? tail($s[$_]) : $s[$_], 
                          0 .. $#s);
              });
}
sub index_of_shortest {
  my @s = @_;
  my $minlen = length(head($s[0]));
  my $si = 0;
  for (1 .. $#s) {
    my $h = head($s[$_]);
    if (length($h) < $minlen) {
      $minlen = length($h);
      $si = $_;
    }
  }
  $si;
}


## Chapter 6 section 5.2

sub matches {
  my ($string, $regex) = @_;
  while ($regex) {
    my $s = drop($regex);
    return 1 if $s eq $string;
    return 0 if length($s) > length($string);
  }
  return 0;
}


## Chapter 6 section 5.2

sub bal {
  my $contents = shift;
  my $bal;
  $bal = node("", promise {
    concat($bal,
           union($contents,
                 transform {"($_[0])"} $bal,
                )
          )
       });
}

my ($regex, $alternative, $atom, $qatoms);

sub regex_to_stream {
  my @rinput = @_;
  my $rinput = sub { shift @rinput };
  
  my $lexer = iterator_to_stream(
          make_lexer($input,
                     ['PAREN',      qr/[()]/,],
                     ['QUANT',      qr/[*+?]/                     ],
                     ['BAR',        qr/[|]/,                      ],
                     ['ATOM',       qr/\\x[0-9a-fA-F]{0,2} # hex escape
                                      |\\\d+               # octal escape
                                      |\\.                 # other \
                                      |.                   # other char
                                      /x, ],
                    )
          );

    my ($result) =  $regex->($lexer);
    return $result;
  }


## Chapter 8 section 5

use Parser ':all';

my $Regex        = parser { $regex      ->(@_) };
my $Alternative  = parser { $alternative->(@_) };
my $Atom         = parser { $atom       ->(@_) };
my $QAtom        = parser { $qatom      ->(@_) };


## Chapter 8 section 5

use Regex;

# regex -> alternative 'BAR' regex  |  alternative
$regex = alternate(T(concatenate($Alternative,
                                 lookfor('BAR'),
                                 $Regex),
                     sub { Regex::union($_[0], $_[2]) }),
                   $Alternative);
# alternative -> qatom alternative | (nothing)
$alternative = alternate(T(concatenate($QAtom, $Alternative),
                           \&Regex::concat),
                         T(\&nothing, sub { Regex::literal("") }));
my %quant;

# qatom -> atom ('QUANT' | (nothing))
$qatom  = T(concatenate($Atom,
                        alternate(lookfor('QUANT'),
                                  \&nothing),
                       ),
            sub { my ($at, $q) = @_;
                  defined $q ? $quant{$q}->($at) : $at });

%quant = ('*' => \&Regex::star,
          '+' => \&Regex::plus,
          '?' => \&Regex::query,
         );
sub query {
  my $s = shift;
  union(literal(""), $s);
}
# atom -> 'ATOM' | '(' regex ')'
$atom = alternate(lookfor("ATOM", sub { Regex::literal($_[0][1]) }),
                  T(concatenate(lookfor(["PAREN", "("]),
                                $Regex,
                                lookfor(["PAREN", ")"]),
                               ),
                    sub { $_[1] }),
                 );



1;