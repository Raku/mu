package Perl6in5::Compiler::Parser;

use warnings;
use strict;

use Perl6in5::Compiler::Stream ':all';
use Data::Structure::Util qw(unbless);
use base 'Exporter';

our @EXPORT_OK = qw(lookfor l $End_of_Input $nothing T error handle_error
                operator star option concatenate alternate
                display_failures %N parser checkval action test say);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Data::Dumper;
# so that debug output is on one line:
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;

# for our convenience :)
sub say (@) { foreach (@_) {print $_.$/ if $_} }

sub debug($);
# for syrupy syntax
use overload
                '-'  => \&concatenate2,
                '|'  => \&alternate2,
                '>>' => \&T,
                '>'  => \&V,
                '/'  => \&checkval,
                '""' => \&overload::StrVal,
#                '""' => \&parser_name,
  ;

our %N; # parser name storage

sub parser (&) { bless $_[0] => __PACKAGE__ }

# mere shortcuts
sub Perl6in5::Compiler::Parser::l { @_ = [@_]; goto &lookfor }
#sub Perl6in5::Compiler::Parser::c { goto &concatenate }
#sub Perl6in5::Compiler::Parser::a { goto &alternate }
#sub Perl6in5::Compiler::Parser::o { goto &operator }

sub parser_name { 
    my $parser = shift; 
    my $key = overload::StrVal($parser); 
    exists($N{$key}) ? $N{$key} : $key; 
} 

sub list_of {
  my ($element, $separator) = @_;
  $separator = lookfor('COMMA') unless defined $separator;

  return concatenate($element,
                     star($separator, $element));
}

sub null_list {
  my $input = shift;
  return ([], $input);
}

sub action {
  my $action = shift;
  return parser {
    my $input = shift;
    $action->($input);
    return (undef, $input);
  };
}

sub lookfor {
  my $wanted = shift;
  my $value = shift || sub { $_[0][1] };
  my $u = shift;
  $wanted = [$wanted] unless ref $wanted;
  my $parser = parser {
    my $input = shift;
    debug "Looking for token ".Dumper($wanted);
    unless (defined $input) {
      debug "Premature end of input";
      die ['TOKEN', $input, $wanted];
    }
    my $next = head($input);
    debug "Next token is ".Dumper($next);
    for my $i (0 .. $#$wanted) {
      next unless defined $wanted->[$i];
      unless ($wanted->[$i] eq $next->[$i]) {
        debug "Token mismatch";
        die ['TOKEN', $input, $wanted];
      }
    }
    my $wanted_value = $value->($next, $u);
    debug "Token matched";
    my @arr = ($wanted_value, tail($input));
    debug Dumper(\@arr);
    return @arr;
  };
  $N{$parser} = "[@$wanted]";
  return $parser;
}

sub End_of_Input {
  my $input = shift;
  debug "Looking for End of Input";
  unless (defined $input) {
    debug "Found the End of Input";
    return (undef, undef);
  }
  my $errmsg = "Syntax Error near ".Dumper($input);
  $errmsg = substr($errmsg,0,70) if length($errmsg) > 70;
  debug "Hint: the last statement must be followed by a semi-colon or newline";
  die $errmsg;
}
our $End_of_Input = \&End_of_Input;
bless $End_of_Input => __PACKAGE__;
$N{$End_of_Input} = "EOI";

sub nothing {
  my $input = shift;
  debug "Looking for nothing";
  if (defined $input) {
    debug "(Next token is ".Dumper($input->[0])."})";
  } else {
    debug "(At end of input)";
  }
  return (undef, $input);
}

our $nothing = \&nothing;
bless $nothing => __PACKAGE__;
$N{$nothing} = "(nothing)";

sub alternate2 {
  my ($A, $B) = @_;
  alternate($A, $B);
}

my $ALT = 'A';
sub alternate {
  my @p = @_;
  return parser { return () } if @p == 0;
  return $p[0]             if @p == 1;

  my $p;
  $p = parser {
    my $input = shift;
    debug "Looking for ".((exists $N{$p} && defined $N{$p})?$N{$p}:'unknown parser').".";
    if (defined $input) {
      debug "(Next token is ".Dumper($input->[0]).")";
    } else {
      debug "(At end of input)";
    }
    my ($q, $np) = (0, scalar @p);
    my ($v, $newinput);
    my @failures;
    for (@p) {
      $q++;
      debug "Trying alternative $q/$np";
      eval { ($v, $newinput) = $_->($input) };
      if ($@) {
        debug "Unhandled exception when trying alternative $q/$np :".Dumper(\$@) unless ref $@;
        exit(1) unless ref $@;
        debug "Failed alternative $q/$np";
        push @failures, $@;
      } else {
        debug "Matched alternative $q/$np";
        return ($v, $newinput);
      }
    }
    debug "No alternatives matched in ".((exists $N{$p} && defined $N{$p})?$N{$p}:'unknown parser')."; failing";
    die ['ALT', $input, \@failures];
  };
  $N{$p} = "(" . join(" | ", map $N{$_}, @p) . ")";
  return $p;
}

sub concatenate2 {
  my ($A, $B) = @_;
  concatenate($A, $B);
}

sub concatenate {
  my @p = @_;
  return $nothing if @p == 0;
  return $p[0]  if @p == 1;
  my $p;
  $p = parser {
    my $input = shift;
    debug "Looking for $N{$p}";
#    debug "(input is: ".Dumper($input).")";
    if (defined $input) {
      debug "(Next token is ".Dumper($input->[0]).")";
    } else {
      debug "(At end of input)";
    }
    my $v;
    my @values;
    my ($q, $np) = (0, scalar @p);
    my @succeeded;
    for (@p) {
      $q++;
      eval { ($v, $input) = $_->($input) };
      if ($@) {
        say "Syntax Error: Unhandled exception when trying".
            "parser component $q/$np near ", Dumper($input), $@ unless ref $@;
        die unless ref $@;
        die ['CONC', $input, [\@succeeded, $@]];
      } else {
        debug "Matched concatenated component $q/$np";
        push @succeeded, $N{$_};
        push @values, $v;
      }
    }
    debug "Finished matching $N{$p}";
    while (ref $values[0] eq 'Tuple') {
      splice @values, 0, 1, @{$values[0]};
    }
    return (bless(\@values => 'Tuple'), $input);
  };
  $N{$p} = join " ", map $N{$_} || "", @p;
  return $p;
}

my $null_tuple = [];
bless $null_tuple => 'Tuple';
sub star {
  my $p = shift;
  my ($p_star, $conc);
  $p_star = alternate(T($conc = concatenate($p, sub { $p_star->($_[0]) }),
                        sub { 
#                          debug "STAR(".Dumper($_[0]).", ".Dumper($_[1]).")";
                          [$_[0], @{$_[1]}]
                        }),
                      T($nothing,
                        sub { $null_tuple }),
                     );
  $N{$p_star} = (exists $N{$p} && defined $N{$p}) ? 'star('.$N{$p}.')' : "star(unknown)";
  $N{$conc} = (exists $N{$p} && defined $N{$p}) ? "$N{$p} $N{$p_star}" : "conc(unknown) star(unknown)";
  $p_star;
}

sub option {
  my $p = shift;
  my $p_opt = alternate($p, $nothing);
  $N{$p_opt} = "option($N{$p})";
  $p_opt;
}

# Only suitable for applying to concatenations
sub T {
  my ($parser, $transform) = @_;
#  return $parser;
  my $p = parser {
    my $input = shift;
    my ($value, $newinput) = $parser->($input);
    debug "Transforming value produced by $N{$parser}";
    debug "Input to $N{$parser}:  ". Dumper($value);
    my @values;
    while (ref($value) eq 'Tuple') {
       unbless ( $value );
    }
    $value = $transform->(@$value);
    debug "Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser};
  return $p;
}

sub V {
  my ($parser, $transform) = @_;
#  return $parser;
  my $p = parser {
    my $input = shift;
    my ($value, $newinput) = $parser->($input);
    debug "Vransforming value produced by $N{$parser}";
    debug "Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->($value); 
    debug "Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser};
  return $p;
}

sub checkval {
  my ($parser, $condition) = @_;
  my $label = "$N{$parser} condition";
  return parser {
    my $input = shift;
    my ($val, $newinput) = $parser->($input);
    return ($val, $newinput) if ($condition->($val));
    die ['CONDITION', $label, $val];
  }
}

sub test {
  my $action = shift;
  return parser {
    my $input = shift;
    my $result = $action->($input);
    return $result ? (undef, $input) : ();
  };
}

sub error {
  my ($checker, $continuation) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    debug "Error in $N{$continuation}";
    debug "Discarding up to $N{$checker}";
    my @discarded;

    while (defined($input)) {
      my $h = head($input);
      if (my (undef, $result) = $checker->($input)) {
        debug "Discarding $N{$checker}";
        push @discarded, $N{$checker};
        $input = $result;
        last;
      } else {
        debug "Discarding token [@$h]";
        push @discarded, $h->[1];
        drop($input);
      }
    }

    warn "Erroneous input: ignoring '@discarded'" if @discarded;
    return unless defined $input;

    debug "Continuing with $N{$continuation} after error recovery";
    return $continuation->($input);
  };
  $N{$p} = "errhandler($N{$continuation} -> $N{$checker})";
  return $p;
}


sub handle_error {
  my ($try) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    my @result = eval { $try->($input) };
    if ($@) {
      display_failures($@) if ref $@;
      die;
    }
    return @result;
  };
}

sub debug ($) {
  return unless $ENV{DEBUG};
  my $msg = shift;
  my $i = 0;
  $i++ while caller($i);
  my $I = " " x ($i-2);
  $I =~ s/../ |/g;
  $I .= ' ' unless length($I) % 2;
  say $I.$msg;
}

sub display_failures {
  my ($fail, $depth) = @_;
  $depth ||= 0;
  my $I = "  " x $depth;
  $I .= ' ' unless length($I) % 2;
  unless (ref $fail) { die $fail }
  my ($type, $position, $data) = @$fail;
  my $pos_desc = "";
  while (length($pos_desc) < 40) {
    if ($position) {
      my $h = head($position);
      $pos_desc .= Dumper($h)." ";
    } else {
      $pos_desc .= "End of input ";
      last;
    }
    $position = tail($position);
  }
  chop $pos_desc;
  $pos_desc .= "..." if defined $position;

  if ($type eq 'TOKEN') {
    say $I."Wanted ".Dumper($data)."] instead of '$pos_desc'";
  } elsif ($type eq 'End of input') {
    say $I."Wanted EOI instead of '$pos_desc'";
  } elsif ($type eq 'ALT') {
    say $I.($depth ? "Or any" : "Any"), " of the following:";
    for (@$data) {
      display_failures($_, $depth+1);
    }
  } elsif ($type eq 'CONC') {
    my ($succeeded, $subfailure) = @$data;
    say $I."Following (".Dumper($succeeded)."), got '$pos_desc' instead of:";
    display_failures($subfailure, $depth+1);
  } else {
    die "Unknown failure type '$type'";
  }
}

sub operator {
  my ($subpart_parser, @ops) = @_;
  my (@alternatives);
  my $opdesc;
  
  for my $op (@ops) {
    my ($operator, $op_func) = @$op;
    my ($rest_op, $t_rest_op);
    push @alternatives,
      $t_rest_op = T($rest_op = concatenate($operator,
                                            $subpart_parser),
                     sub {
                       my $rest = $_[1];
                       sub { $op_func->($_[0], $rest) }
                     });
    $N{$rest_op} = $N{$t_rest_op} = (exists $N{$operator})?
        "($N{$operator}":"(unknown op"." ".(exists $N{$subpart_parser})?
            "$N{$subpart_parser})":"unknown subpartparser)";
    $opdesc .= (exists $N{$operator})?"$N{$operator} ":"unknown op ";
  }
  chop $opdesc;

  my $alts = alternate(@alternatives);
  $N{$alts} = "some operation $opdesc ".(exists $N{$subpart_parser} && defined $N{$subpart_parser})?
    $N{$subpart_parser}:"unknown subpartparser";
  
  my $result = 
    T(concatenate($subpart_parser,
                  star($alts)),
      sub { my ($total, $funcs) = @_;
            for my $f (@$funcs) {
              $total = $f->($total);
            }
            $total;
          });
  $N{$result} = "(operations {$opdesc} on ".((exists $N{$subpart_parser} && defined $N{$subpart_parser})?$N{$subpart_parser}:"unknown subpartparser")."s)";
  $result;
}

1;

# COPYRIGHT NOTICE:
# The contents of this file are Copyright (c) 2008, Matthew Wilson
# and any other contributors whose commits are recorded by the
# "pugscode" subversion source control repository.  The contributors'
# names/handles of are listed in the "pugsroot/AUTHORS" file).
# See licenses/Artistic2.txt for the 'Artistic License 2.0',
# under which this code is distributed and which may be found
# at http://www.opensource.org/licenses/artistic-license-2.0.php
# or http://www.perlfoundation.org/artistic_license_2_0

# ORIGIN:
# Nearly all of this code is unmodified directly
#       from Higher-Order Perl by Mark Dominus,
#       published by Morgan Kaufmann Publishers,
#       Copyright 2005 by Elsevier Inc
# Because of the origin, this file is also subject to the license
# agreement at http://hop.perl.plover.com/LICENSE.txt