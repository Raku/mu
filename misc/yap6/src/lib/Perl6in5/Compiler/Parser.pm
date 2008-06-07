use strict 'refs';
use warnings;
no warnings qw{ reserved closure recursion };

# Library based on chap09/arith25.pl

package Perl6in5::Compiler::Parser;
our ($nothing, $End_of_Input);

use Exporter;
@EXPORT_OK = qw(lookfor $End_of_Input $nothing T error debug
                operator star option concatenate alternate
                display_failures labeledblock commalist o say
                termilist trace %N l parser checkval execnow
                adn ch w keyword keywords clist gt0 word panic
                semilist unspace concatoptws through p6ws
                newline concatmanws);
@ISA = 'Exporter';
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output
# disregarding leading whitespace, lines that start with trace
# or end with trace are discarded by the source filter in 
# Perl6in5::Compiler::Trace.  This allows execution time to be
# much faster than otherwise, since the strings to be traced
# are not only not sent to STDOUT; they aren't even built.

use Perl6in5::Compiler::Stream 'node', 'head', 'tail', 'promise', 'drop';

$|=0; # trace

my $toodeep = 5;

use overload
                '-'  => \&concatoptws,
                '+'  => \&concatmanws,
                '.'  => \&concatenate,
                '|'  => \&alternate,
                '&'  => \&alterlong,
                '>>' => \&T,
                '>'  => \&V,
                '/'  => \&checkval,
                '""' => \&overload::StrVal,
  ;

sub normalizer {
    "@_"
};

# Here's a fun trick; memoize the parser generator functions :)
use Memoize;
map { memoize $_, NORMALIZER=>'normalizer' unless (/^\W/ || /^trace$/) } qw{ newline p6ws nothing };

$| = 1; # trace

use Data::Dumper;
# so that trace output is on one line:
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Quotekeys = 0;
#$Data::Dumper::Deparse = 1; # trace
#$Data::Dumper::Deparse = 1; # debug

sub execnow (&) { $_[0]->() }

sub say (@) { print($_,"\n") for @_ }

sub trace ($) { # trace
  my $msg = (shift) . "\n"; # trace
  my $i = 0; # trace
  $i++ while caller($i); # trace
  $I = "-" x int($i/2-1); # trace
  warn $i.$I." ", $msg; # trace
} # trace

sub callstack () { # trace
  my $i = 1; # trace
  my $msg; # trace
  while (my @stack = caller($i)) { # trace
    $i++; # trace
    $msg .= "\t".join(",",@stack)."\n" if @stack; # trace
  } # trace
  trace $msg;
} # trace

sub debug ($) { # debug
  my $msg = (shift) . "\n"; # debug
  my $i = 0; # debug
  $i++ while caller($i); # debug
  $I = "-" x int($i/2-1); # debug
  warn $i.$I." ", $msg; # debug
} # debug

sub parser (&) { bless $_[0] => __PACKAGE__ }

sub l { @_ = [@_]; goto &lookfor }
sub o { goto &option }

sub lookfor {
  my $wanted = shift;
  my $value = shift || sub { $_[0][1] };
  my $u = shift;
  $wanted = [$wanted] unless ref $wanted;
  my $parser;
  $parser = parser {
    my $input = shift;
    trace "Looking for token ".Dumper($wanted)." in ".Dumper(head($input));
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
    for my $i (0 .. $#$wanted) {
      trace "trying subtoken $i";
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
  unless (defined($input->[0])) {
    trace "Found End of Input";
    return (undef, undef);
  } else {
    trace "Found more input: ".Dumper($input);
    die ["End of input", $input];
  }
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
  $p;
}

my $cdepth = {};

sub p6ws () {
  my $p;
  #trace "generating p6ws".Dumper([caller()]);
  $p = alternate(newline(),gt0(alternate(l("C"," "),
                     concatenate(l("C","#"),
                     # yes, I realize the below is incorrect; a special
                     # function will need to be created eventually.
                                 alternate(concatenate(gt0(l("C","{"),1),
                                                       through(gt0(l("C","}"),1),1)),
                                           concatenate(gt0(l("C","("),1),
                                                       through(gt0(l("C",")"),1),1)),
                                           concatenate(gt0(l("C","["),1),
                                                       through(gt0(l("C","]"),1),1)),
                                           concatenate(gt0(l("C","<"),1),
                                                       through(gt0(l("C",">"),1),1)))),
                     concatenate(l("C","#"),
                                 through(l("C","\n"),1))
                                 # XXX need to add heredoc here soon.
                                 ),1));
  $p = gt0(alternate(l("C"," "),l("C","\n")),1);
#  $p = l("C"," ");
  $N{$p} = 'ws'; # trace
  $p;
}

# slurp up stuff until a stopper parser matches. basically, the {*} signifier.
sub through {
  #trace 'generating through'.Dumper([caller()]);
    my $stop = shift;
    my $p;
    $p = parser {
        my $input = shift;
        my $v;
        my @values;
        while (defined $input) {
            eval { ($v, $input) = $stop->($input) };
            if ($@) {
                unless (ref $@) {
                    $Data::Dumper::Deparse = 1; # trace
                    trace "Dying in through; current parser: ".Dumper($stop);
                    die;
                }
                trace "through $N{$stop} still not matched";
                push @values, drop($input);
                die ['CONC', $input, [\@succeeded, $@]] unless defined $input; # trace
                next;
            } else {
                trace "through $N{$stop} matched";
                push @values, $v;
                last;
            }
        }
        while (ref $values[0] eq 'Tuple') {
            splice @values, 0, 1, @{$values[0]};
        }
        return (bless(\@values => 'Tuple'), $input);
    };
    $N{$p} = "through($N{$stop})"; # trace
    $p;
}

sub concatmanws {
  my @p = grep $_,@_;
  return $nothing if @p == 0;
  return $p[0]  if @p == 1;
  my ($p,$o);
  $o = p6ws;
  $p = parser {
    my $input = shift;
    my $i = 0;
    $i++ while caller($i);
    #trace "concatenate $p on $input at depth $i"; # trace
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser before
        && defined($input) && exists $cdepth->{$p}->{$input}
        # our current depth is lower than before
        && $cdepth->{$p}->{$input} > $toodeep) {
        # we're in an infinite recursion.
        trace "$p ($N{$p}) was too deep (>$toodeep) in itself.";
        die ['CONC', $input, [[], $@]];
    } else { # trace
        #trace "we're not in an infinite recursion"; # trace
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p}->{$input}++;
    trace "Looking for $N{$p}";
    #trace "concatenate: ".Dumper(\@p);
    if (defined $input) { # trace
      trace "Next token is ".Dumper($input->[0]);
    } else { # trace
      trace "At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my $v;
    my @values;
    my ($q, $np) = (0, scalar @p);
    my @succeeded; # trace
    for (@p) {
      $q++;
      eval { ($v, $input) = $_->($input) };
      if ($@) {
        unless (ref $@) {
            $Data::Dumper::Deparse = 1; # trace
            trace "Dying in CONC+; current parser: $N{$p}\n";
            die;
        }
        die ['CONC', $input, [\@succeeded, $@]]; # trace
        die ['CONC', $input, [[], $@]]; # sneaky :)
      } else {
        trace "Matched some whitespace after $q/$np: ".Dumper($v).Dumper(\@succeeded);
        push @succeeded, $N{$_}; # trace
        push @values, $v;
      }
      unless ($q == $np) {
          trace "in concatmanws, finding whitespace after $q/$np";
          eval { ($v, $input) = $o->($input) };
          if ($@) {
            unless (ref $@) {
                $Data::Dumper::Deparse = 1; # trace
                trace "Dying in CONC-; current parser: $N{$p}\n";
                die;
            }
            die ['CONC', $input, [\@succeeded, $@]]; # trace
            die ['CONC', $input, [[], $@]]; # sneaky :)
          } else {
            trace "Matched concatws whitespace $q/$np";
            push @succeeded, ' '; # trace
            push @values, $v;
          }
      }
    }
    trace "Finished matching $N{$p}";
    while (ref $values[0] eq 'Tuple') {
      splice @values, 0, 1, @{$values[0]};
    }
    return (bless(\@values => 'Tuple'), $input);
  };
  $N{$p} = join "+", map $N{$_}, @p; # trace
  return $p;
}

sub concatoptws {
  my @p = grep $_,@_;
  return $nothing if @p == 0;
  return $p[0]  if @p == 1;
  my ($p,$o);
  $o = p6ws;
  $p = parser {
    my $input = shift;
    my $i = 0;
    $i++ while caller($i);
    #trace "concatenate $p on $input at depth $i"; # trace
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser before
        && defined($input) && exists $cdepth->{$p}->{$input}
        # our current depth is lower than before
        && $cdepth->{$p}->{$input} > $toodeep) {
        # we're in an infinite recursion.
        trace "$p ($N{$p}) was too deep (>$toodeep) in itself.";
        die ['CONC', $input, [[], $@]];
    } else { # trace
        #trace "we're not in an infinite recursion"; # trace
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p}->{$input}++;
    trace "Looking for $N{$p}";
    #trace "concatenate: ".Dumper(\@p);
    if (defined $input) { # trace
      trace "Next token is ".Dumper($input->[0]);
    } else { # trace
      trace "At end of input";
    } # trace
    $input = [$input] unless ref $input;
    my $v;
    my @values;
    my ($q, $np) = (0, scalar @p);
    my @succeeded; # trace
    for (@p) {
      $q++;
      eval { ($v, $input) = $_->($input) };
      if ($@) {
        unless (ref $@) {
            $Data::Dumper::Deparse = 1; # trace
            trace "Dying in CONC-; current parser: $N{$p}\n";
            die;
        }
        die ['CONC', $input, [\@succeeded, $@]]; # trace
        die ['CONC', $input, [[], $@]]; # sneaky :)
      } else {
        trace "Possibly matched concatopt component $q/$np";
        push @succeeded, $N{$_}; # trace
        push @values, $v;
      }
      unless ($q == $np) {
          eval { ($v, $input) = $o->($input) };
          if ($@) {
            unless (ref $@) {
                $Data::Dumper::Deparse = 1; # trace
                trace "Dying in CONC-; current parser: $N{$p}\n";
                die;
            }
          } else {
            trace "Matched some whitespace after $q/$np";
            push @succeeded, ' '; # trace
            push @values, $v;
          }
      }
    }
    trace "Finished matching $N{$p}";
    while (ref $values[0] eq 'Tuple') {
      splice @values, 0, 1, @{$values[0]};
    }
    return (bless(\@values => 'Tuple'), $input);
  };
  $N{$p} = join "-", map $N{$_}, @p; # trace
  return $p;
}

sub newline {
  my $p;
  $p = alternate(panic(word("\n#{"),"\\n#{ is illegal"),gt0(l("C","\n"),1));
  $p = gt0(l("C","\n"),1);
  $N{$p} = "nl"; # trace
  return $p;
}

sub concatenate {
  my @p = grep $_,@_;
  return $nothing if @p == 0;
  return $p[0]  if @p == 1;
  my $p;
  $p = parser {
    my $input = shift;
    my $i = 0;
    $i++ while caller($i);
    #trace "concatenate $p on $input at depth $i"; # trace
    if (# we've been in this parser before
        exists $cdepth->{$p}
        # we've seen this input for this parser before
        && defined($input) && exists $cdepth->{$p}->{$input}
        # our current depth is lower than before
        && $cdepth->{$p}->{$input} > $toodeep) {
        # we're in an infinite recursion.
        trace "$p ($N{$p}) was too deep (>$toodeep) in itself.";
        die ['CONC', $input, [[], $@]];
    } else { # trace
        #trace "we're not in an infinite recursion"; # trace
    }
    # store the coderef addresses for this
    # parser so we can detect infinite loops
    $cdepth->{$p}->{$input}++;
    trace "Looking for $N{$p}";
    #trace "concatenate: ".Dumper(\@p);
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
        unless (ref $@) {
            $Data::Dumper::Deparse = 1; # trace
            trace "Dying in CONC.; current parser: $N{$p}\n";
            die;
        }
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
  $N{$p} = join ".", map $N{$_}, @p; # trace
  return $p;
}

my $null_tuple = [];
bless $null_tuple => 'Tuple';

sub star {
  #trace $_[1].'generating star'.Dumper([caller()]);
  my ($p,$nows) = @_;
  my ($p_star, $conc);
  # concatenate using whitespace by default.
  my $concat = sub { concatmanws(@_) };
  if ($nows) { $concat = sub { concatenate(@_) }; };
  #trace "star concat is: ".Dumper($concat);
  my $p_starexec = parser { $p_star->(@_) };
  $N{$p_starexec} = ""; # trace
  $p_star = alternate(T($conc = $concat->($p, $p_starexec),
                        sub { 
                          [$_[0], @{$_[1]}] 
                        }),
                      T($nothing,
                        sub { $null_tuple }),
                     );
  $N{$p_star} = "($N{$p})*"; # trace
  $N{$conc} = "conc $N{$p} $N{$p_star}"; # trace
  $p_star;
}

sub option {
  my $p = shift;
  $p_opt = alternate($p, $nothing);
  $N{$p_opt} = "(?$N{$p})"; # trace
  $p_opt;
}

# commalist(p, sep) = p star(sep p) option(sep)
sub commalist {
  my ($p, $separator, $sepstr) = @_;
  my $parser = T(concatoptws($p,
                             star(T(concatoptws(star($separator,1), $p),
                                    sub { $_[1] }
                                   ),1),
                             star($separator,1)),
                 sub { [$_[0], @{$_[1]}] }
                );
  $N{$parser} = "$N{$p}$sepstr$N{$p}$sepstr..."; # trace
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
  my $I = " " x $depth;
  unless (ref $fail) { die $fail }
  my ($type, $position, $data) = @$fail;
  my $pos_desc = "";
  while (length($pos_desc) < 80) {
    if ($position) {
      my $h = head($position);
      if (!defined $h) {
        $pos_desc .= "End of input ";
        last;
      }
      $h = [$h] unless ref $h;
      $pos_desc .= "$h->[1]";
    } else {
      $pos_desc .= "End of input ";
      last;
    }
    $position = tail($position);
  }
  chop $pos_desc;
  $pos_desc .= "..." if defined $position;
  if ($type eq 'TOKEN') {
    print $I, "Wanted [@$data] instead of ".Dumper($pos_desc);
  } elsif ($type eq 'End of input') {
    print $I, "Wanted EOI instead of ".Dumper($pos_desc);
  } elsif ($type eq 'ALT') {
    print $I, ($depth ? "Or any" : "Any"), " of the following:\n";
    for (@$data) {
      display_failures($_, $depth+1);
    }
  } elsif ($type eq 'CONC') {
    my ($succeeded, $subfailure) = @$data;
    print $I, "After ".Dumper("@$succeeded").", got ".Dumper($pos_desc)." instead of: ";
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


sub adn (@) {
    print "Adding AST node: "; # trace
    say join('',map(Dumper($_)." ",@_));
    Dumper([map("$_",@_)]);
}

sub ch { # parse for a single (normal) character.
  #trace 'generating ch'.Dumper([caller()]);
    # Because of the weirdness of unspace, grammars that
    # need to look for backslashes will need to use l("C",'\\')
    my $p;
    my $char = $_[0];
    # through(option(something)) is an interesting construct.
    # It means chew/slurp stuff through something if something
    # is the first token (series).  Since option() succeeds with
    # nothing(), through will only ever chew 1 match.
    $p = concatenate(through(option(unspace())),l("C",$char));
    $N{$p} = Dumper($char); # trace
    $p;
}

sub unspace () {
  #trace 'generating unspace'.Dumper([caller()]);
    my $p;
    $p = concatenate(l("C","\\"),star(p6ws(),1));
    $N{$p} = 'unspace'; # trace
    $p;
}

sub w { # look for a wrapped entity.  first parm is split into the wrappers.
    my ($d,$e) = split(//,$_[0]);
    my $p;
    my $item = $_[1];
    $p = concatoptws(ch($d),$item,ch($e));
    $N{$p} = "$d$N{$item}$e"; # trace
    $p;
}

sub keyword {
    my $ins = shift;
    my $p;
    $p = l('ID',$ins);
    $N{$p} = "$ins"; # trace
    $p;
}

sub keywords {
    my @args = @_;
    my $p;
    $p = alternate(map(keyword($_),@args));
    $N{$p} = join("|",@args); # trace
    $p;
}

sub clist {
    my $ins = shift;
    my $p;
    $p = commalist($ins,ch(','),',');
    $N{$p} = "clist($N{$ins})"; # trace
    $p;
}

sub semilist {
    my $ins = shift;
    my $p;
    $p = commalist($ins,alternate(ch(','),ch(';')),',;');
    $N{$p} = "semilist($N{$ins})"; # trace
    $p;
}

sub panic {
    my ($ins,$msg) = @_;
    my $p;
    $p = parser {
        my $input = shift;
        trace "Dying if find $N{$p}";
        if (defined $input) { # trace
            trace "Next token is ".Dumper($input->[0]);
        } else { # trace
            trace "At end of input";
        } # trace
        $input = [$input] unless ref $input;
        my ($v, $newinput);
        eval { ($v, $newinput) = $ins->($input) };
        if ($@) {
            die ['panic', $input, [[], $@]]; 
        } else {
            trace "Matched $N{$ins}, so dying";
            die $msg;
        }
    };
    $N{$p} = "panic($N{$ins})"; # trace
    $p;
}

sub gt0 { # hit on 1 or more of the contained. (gt0 == greater than zero)
  #trace $_[1].'generating gt0'.Dumper([caller()]);
    my $p;
    my ($item,$nows) = @_;
    $nows ||= 0;
    my $conc = sub { concatmanws(@_) };
    if ($nows ==1) { $conc = sub { concatenate(@_) }; };
    if ($nows ==2) { $conc = sub { concatoptws(@_) }; };
    #trace "gt0 conc is: ".Dumper($conc);
    $p = $conc->($item,star($item,$nows));
    $N{$p} = ">=1($N{$item})"; # trace
    $p;
}

sub word {
    my $p;
    my $word = $_[0];
    $p = concatenate(map(l('C',$_),split(//, $word)));
    $N{$p} = Dumper($word); # trace
    $p;
}

1;
