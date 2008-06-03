package Perl6in5::Compiler::Iterator;
use Exporter;
@ISA = 'Exporter';
@EXPORT = qw(Iterator NEXTVAL
             imap igrep imap_l igrep_l 
             list_iterator upto
             flatten append  );

sub NEXTVAL { $_[0]->() }

my %creator;


sub Iterator (&) {
  return $_[0] unless $DEBUG;

  my $creator = (caller(1))[3];
  if (exists $creator{$_[0]}) {
    warn "This iterator was already created by $creator{$_[0]}!!\n";
  }
  $extant{$creator}++;
  bless $_[0] => 'Iter';
  $creator{$_[0]} = $creator;
  print STDERR "+ $creator ($extant{$creator}) $_[0]\n";
  return $_[0];
}


sub Iter::DESTROY {
  my $iterator = shift;
  my $creator = delete $creator{$iterator};
  $extant{$creator}--;
  print STDERR "- $creator ($extant{$creator}) $iterator\n";
}

sub imap (&$) {
  my ($transform, $it) = @_;
  return Iterator {
    local $_ = NEXTVAL($it);
    undef $it, return unless defined $_;
    return $transform->();
  }
}

sub imap_l (&$) {
  my ($transform, $it) = @_;
  return Iterator {
    my @a = NEXTVAL($it);
    return unless @a;
    return $transform->(@a);
  }
}

sub igrep (&$) {
  my ($is_interesting, $it) = @_;
  return Iterator {
    local $_;
    while (defined ($_ = NEXTVAL($it))) {
      return $_ if $is_interesting->();
    }
    return;
  }
}

sub igrep_l (&$) {
  my ($is_interesting, $it) = @_;
  return Iterator {
    while (my @a = NEXTVAL($it)) {
      return @a if $is_interesting->(@a);
    }
    return;
  }
}


sub append {
  my @its = @_;
  return Iterator {
    my $val;
    until (@its == 0 || defined($val = NEXTVAL($its[0]))) {
      shift @its;
    }
    return if @its == 0;
    return $val;
  };
}


sub flatten {
  my @stack = @_;
  return Iterator {
    print STDERR ">> In flatten: \n";
    print STDERR ">>  $creator{$_} $_\n" for @stack;
    while (@stack) {
      unless (UNIVERSAL::isa($stack[0], 'CODE')) { return shift @stack }
      my $val = NEXTVAL($stack[0]);
      if (@stack < 2 and UNIVERSAL::isa($val, 'CODE')) {
        unshift @stack, $val;
      } elsif (not defined $val) {
        shift @stack;
      } else {
        return $val
      }
    }
    return undef;
  }
}


sub list_iterator {
  my @items = @_;
  Iterator { return shift @items }
}

sub upto {
  my ($m, $n) = @_;
  return Iterator {
    return $m <= $n ? $m++ : undef;
  };
}

1;


