use strict 'refs';
use warnings;
no warnings 'recursion';

# Library based on chap09/arith25.pl

package Perl6in5::Compiler::Parser;
our ($nothing, $End_of_Input);

use Exporter;
@EXPORT_OK = qw(lookfor _ $End_of_Input $nothing T error handle_error
                operator star option concatenate alternate
                display_failures labeledblock commalist 
                termilist trace %N l parser checkval);
@ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output
use Perl6in5::Compiler::Stream 'node', 'head', 'tail', 'promise';

use overload
                '-' => \&concatenate,
                '|' => \&alternate,
                '>>' => \&T,
                '>' => \&V,
                '/' => \&checkval,
                '""' => \&overload::StrVal,
#                '""' => \&parser_name
  ;

$| = 1; # trace

use Data::Dumper;
# so that trace output is on one line:
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Quotekeys = 0;
#$Data::Dumper::Deparse = 1; # trace

sub trace ($) { # trace
  my $msg = (shift) . "\n"; # trace
  my $i = 0; # trace
  $i++ while caller($i); # trace
  $I = " " x ($i-2); # trace
  $I =~ s/../ |/g; # trace
  $I .= ' ' unless length($I) % 2; # trace
  print $I, $msg; # trace
} # trace

sub parser (&) { bless $_[0] => __PACKAGE__ }

sub l { @_ = [@_]; goto &lookfor }

sub lookfor {
  my $wanted = shift;
  my $value = shift || sub { $_[0][1] };
  my $u = shift;
  $wanted = [$wanted] unless ref $wanted;
  my $parser;
  $parser = parser {
    my $input = shift;
    trace "Looking for token ".Dumper($wanted);
    unless (defined $input) {
      trace "Premature end of input";
      die ['TOKEN', $input, $wanted];
    }
    my $next = head($input);
    unless (defined $next) {
      trace "Premature end of input";
      die ['TOKEN', $next, $wanted];
    }
    $next = [$next] unless ref $next;
    trace "Next token is ".Dumper($next);
    for my $i (0 .. $#$wanted) {
      next unless defined $wanted->[$i];
      unless ($wanted->[$i] eq $next->[$i]) {
        trace "Token mismatch";
        die ['TOKEN', $input, $wanted];
      }
    }
    my $wanted_value = $value->($next, $u);
    trace "Token matched";
    return ($wanted_value, tail($input));
  };
  $N{$parser} = "[@$wanted]"; # trace
  return $parser;
}

sub End_of_Input {
  my $input = shift;
  trace "Looking for End of Input";
  return (undef, undef) unless defined($input->[0]);
  die ["End of input", $input];
}

$End_of_Input = \&End_of_Input;
bless $End_of_Input => __PACKAGE__;

sub nothing {
  my $input = shift;
  trace "Looking for nothing";
  if (defined $input) { # trace
    trace "Next token is ".Dumper($input->[0]);
  } else { # trace
    trace "(At end of input)";
  } # trace
  return (undef, $input);
}

$nothing = \&nothing;
bless $nothing => __PACKAGE__;

sub alternate {
  my @p = grep $_,@_;
  return parser { return () } if @p == 0;
  return $p[0]             if @p == 1;
  my $p;
  $p = parser {
    my $input = shift;
    trace "Looking for alt $N{$p}";
    if (defined $input) { # trace
      trace "Next token is ".Dumper($input->[0]);
    } else { # trace
      trace "At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my ($q, $np) = (0, scalar @p); # trace
    my ($v, $newinput);
    my @failures;
    for (@p) {
      $q++; # trace
      trace "Trying alternative $q/$np";
      eval { ($v, $newinput) = $_->($input) };
      if ($@) {
        die unless ref $@;
        trace "Failed alternative $q/$np";
        push @failures, $@;
      } else {
        trace "Matched alternative $q/$np";
        return ($v, $newinput);
      }
    }
    trace "No alternatives matched in $N{$p}; failing";
    die ['ALT', $input, \@failures];
  };
  $N{$p} = "(" . join(" | ", map eval{$N{$_}}, @p) . ")"; # trace
  return $p;
}

sub concatenate {
  my @p = grep $_,@_;
  return $nothing if @p == 0;
  return $p[0]  if @p == 1;
  my $p;
  $p = parser {
    my $input = shift;
    trace "Looking for $N{$p}";
    if (defined $input) { # trace
      trace "Next token is ".Dumper($input->[0]);
    } else { # trace
      trace "At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my $v;
    my @values;
    my ($q, $np) = (0, scalar @p); # trace
    my @succeeded; # trace
    for (@p) {
      $q++; # trace
      eval { ($v, $input) = $_->($input) };
      if ($@) {
        die unless ref $@;
        die ['CONC', $input, [\@succeeded, $@]]; # trace
        die ['CONC', $input, [[], $@]]; # sneaky :)
      } else {
        trace "Matched concatenated component $q/$np";
        push @succeeded, $N{$_}; # trace
        push @values, $v;
      }
    }
    trace "Finished matching $N{$p}";
    while (ref $values[0] eq 'Tuple') {
      splice @values, 0, 1, @{$values[0]};
    }
    return (bless(\@values => 'Tuple'), $input);
  };
  $N{$p} = join " ", map $N{$_}, @p; # trace
  return $p;
}

my $null_tuple = [];
bless $null_tuple => 'Tuple';

sub star {
  my $p = shift;
  my ($p_star, $conc);
  my $p_starexec = parser { $p_star->(@_) };
  $N{$p_starexec} = ""; # trace
  $p_star = alternate(T($conc = concatenate($p, $p_starexec),
                        sub { 
                          [$_[0], @{$_[1]}] 
                        }),
                      T($nothing,
                        sub { $null_tuple }),
                     );
  $N{$p_star} = "star($N{$p})"; # trace
  $N{$conc} = "$N{$p} $N{$p_star}"; # trace
  $p_star;
}

sub option {
  my $p = shift;
  $p_opt = alternate($p, $nothing);
  $N{$p_opt} = "option($N{$p})"; # trace
  $p_opt;
}

# commalist(p, sep) = p star(sep p) option(sep)
sub commalist {
  my ($p, $separator, $sepstr) = @_;

  if (defined $separator) {
    $sepstr ||= $N{$separator};
  } else {
    $separator ||= lookfor('COMMA');
    $sepstr ||= ", ";
  }
  my $parser = T(concatenate($p,
                             star(T(concatenate($separator, $p),
                                    sub { $_[1] }
                                   )),
                             option($separator)),
                 sub { [$_[0], @{$_[1]}] }
                );
  $N{$parser} = "$N{$p}$sepstr $N{$p}$sepstr ..."; # trace
  return $parser;
}

sub termilist {
  my ($p) = shift;
  commalist($p, lookfor('TERMINATOR'), "; ");
}

sub labeledblock {
  my ($label, $contents) = @_;
  my $t;
  my $p = concatenate(concatenate(concatenate($label, 
                                              lookfor('LBRACE'),
                                             ),
                                  $t = star($contents),
                                 ),
                      lookfor('RBRACE'),
                     );
  $N{$p} = "$N{$label} { $N{$t} }"; # trace
  T($p, sub { [$_[0], @{$_[2]}] });
}

# Only suitable for applying to concatenations
sub T {
  my ($parser, $transform) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    trace "Going to transform with $N{$parser}";
    my ($value, $newinput) = $parser->($input);
    trace "Transforming value produced by $N{$parser}";
    trace "Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->(@$value);
    trace "Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser}; # trace
  return $p;
}

sub V {
  my ($parser, $transform) = @_;
#  return $parser;
  my $p = parser {
    my $input = shift;
    my ($value, $newinput) = $parser->($input);
    trace "Vransforming value produced by $N{$parser}";
    trace "Input to $N{$parser}:  ". Dumper($value);
    $value = $transform->($value); 
    trace "Output from $N{$parser}: ". Dumper($value);
    return ($value, $newinput);
  };
  $N{$p} = $N{$parser}; # trace
  return $p;
}

sub checkval {
  my ($parser, $condition) = @_;
  $label = "$N{$parser} condition"; # trace
  return parser {
    my $input = shift;
    my ($val, $newinput) = $parser->($input);
    return ($val, $newinput) if ($condition->($val));
    die ['CONDITION', $label, $val]; # trace
    die ['CONDITION', "trace mode off", $val]; # sneaky
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
  my ($try) = @_;
  my $p;
  $p = parser {
    my $input = shift;
    my @result = eval { $try->($input) };
    if ($@) {
      my $trace = 0;
      $trace = 1; # trace
      my $msg = Dumper($@);
      if ($msg =~ / at /) {
          if (!$trace) {
          $msg =~ s/ at .*//;
          $msg =~ s/^.*propagated.*$//mg;
          $msg =~ s/\n//mg; }
          print "Illegal usage: ".$msg;
      }
      display_failures($@) if ref $@;
      die $msg;
    }
    return @result;
  };
  $N{$p} = $N{$try}; # trace
  $p;
}

sub display_failures {
  my ($fail, $depth) = @_;
  my $xx = 0;
  $xx = 1; # trace
  return unless $xx;
  $depth ||= 0;
  my $I = "  " x $depth;
  unless (ref $fail) { die $fail }
  my ($type, $position, $data) = @$fail;
  my $pos_desc = "";
  while (length($pos_desc) < 40) {
    if ($position) {
      my $h = head($position);
      if (!defined $h) {
        $pos_desc .= "End of input ";
        last;
      }
      $h = [$h] unless ref $h;
      $pos_desc .= "@$h ";
    } else {
      $pos_desc .= "End of input ";
      last;
    }
    $position = tail($position);
  }
  chop $pos_desc;
  $pos_desc .= "..." if defined $position;
  if ($type eq 'TOKEN') {
    print $I, "Wanted [@$data] instead of '$pos_desc'";
  } elsif ($type eq 'End of input') {
    print $I, "Wanted EOI instead of '$pos_desc'";
  } elsif ($type eq 'ALT') {
    print $I, ($depth ? "Or any" : "Any"), " of the following:\n";
    for (@$data) {
      display_failures($_, $depth+1);
    }
  } elsif ($type eq 'CONC') {
    my ($succeeded, $subfailure) = @$data;
    print $I, "Following (@$succeeded), got '$pos_desc' instead of:\n";
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
    my $rest_op;
    push @alternatives,
      $t_rest_op = T($rest_op = concatenate($operator,
                                            $subpart_parser),
                     sub {
                       my $rest = $_[1];
                       sub { $op_func->($_[0], $rest) }
                     });
    $N{$rest_op} = $N{$t_rest_op} = "($N{$operator} $N{$subpart_parser})"; # trace
    $opdesc .= "$N{$operator} "; # trace
  }
  chop $opdesc; # trace
  my $alts = alternate(@alternatives);
  $N{$alts} = "some operation {$opdesc} $N{$subpart_parser}"; # trace
  my $result = 
    T(concatenate($subpart_parser,
                  star($alts)),
      sub { my ($total, $funcs) = @_;
            for my $f (@$funcs) {
              $total = $f->($total);
            }
            $total;
          });
  $N{$result} = "(operations {$opdesc} on $N{$subpart_parser}s)"; # trace
  $result;
}


1;
