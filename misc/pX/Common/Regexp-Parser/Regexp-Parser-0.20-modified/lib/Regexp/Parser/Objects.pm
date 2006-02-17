use NEXT;

{
  package Regexp::Parser::__object__;  

  sub class {
    my $self = shift;
    Carp::carp("class() deprecated; use family() instead");
    $self->family(@_);
  }

  sub flags {
    my $self = shift;
    $self->{flags};
  }

  sub family {
    my $self = shift;
    $self->{family};
  }

  sub type {
    my $self = shift;
    $self->{type};
  }

  sub qr {
    my $self = shift;
    $self->visual(@_);
  }

  sub visual {
    my $self = shift;
    exists $self->{vis} ? $self->{vis} : '';
  }

  sub raw {
    my $self = shift;
    exists $self->{raw} ? $self->{raw} : $self->visual(@_);
  }

  sub data {
    my $self = shift;
    return $self->{data};
  }

  sub ender {
    my $self = shift;
    unless ($self->{down}) {
      Carp::carp("ender() ignored for ", $self->family, "/", $self->type);
      return;
    }
    [ 'tail' ];
  }

  sub walk {
    my $self = shift;
    return;
  }

  sub omit {
    my $self = shift;
    $self->{omit} = shift if @_;
    $self->{omit};
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    my $merged = 0;
    return if $self->omit;
    push @$tree, $self;
    $self->merge;
  }

  sub merge {
    my ($self) = @_;
    return;
  }
}


{
  # \A ^ \B \b \G \Z \z $
  package Regexp::Parser::anchor;
  push @ISA, qw( Regexp::Parser::__object__ );
  push @Regexp::Parser::bol::ISA, __PACKAGE__;
  push @Regexp::Parser::bound::ISA, __PACKAGE__;
  push @Regexp::Parser::gpos::ISA, __PACKAGE__;
  push @Regexp::Parser::eol::ISA, __PACKAGE__;

  sub new {
    my ($class, $rx, $type, $vis) = @_;
    Carp::croak("anchor is an abstract class") if $class =~ /::anchor$/;

    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'anchor',
      type => $type,
      vis => $vis,
      zerolen => 1,
    }, $class;
    return $self;
  }
}


{
  # . \C
  package Regexp::Parser::reg_any;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $type, $vis) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'reg_any',
      type => $type,
      vis => $vis,
    }, $class;
    return $self;
  }
}


{
  # \w \W
  package Regexp::Parser::alnum;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $neg) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      neg => $neg,
    }, $class;
    return $self;
  }

  sub neg {
    my $self = shift;
    $self->{neg} = shift if @_;
    $self->{neg};
  }

  sub family { 'alnum' }

  sub type {
    my $self = shift;
    ($self->{neg} ? 'n' : '') . $self->family;
  }

  sub visual {
    my $self = shift;
    $self->{neg} ? '\W' : '\w';
  }
}


{
  # \s \S
  package Regexp::Parser::space;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $neg) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      neg => $neg,
    }, $class;
    return $self;
  }

  sub neg {
    my $self = shift;
    $self->{neg} = shift if @_;
    $self->{neg};
  }

  sub type {
    my $self = shift;
    ($self->{neg} ? 'n' : '') . $self->family;
  }

  sub family { 'space' }

  sub visual {
    my $self = shift;
    $self->{neg} ? '\S' : '\s';
  }
}


{
  # \d \D
  package Regexp::Parser::digit;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $neg) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      neg => $neg,
    }, $class;
    return $self;
  }

  sub neg {
    my $self = shift;
    $self->{neg} = shift if @_;
    $self->{neg};
  }

  sub type {
    my $self = shift;
    ($self->{neg} ? 'n' : '') . $self->family;
  }

  sub family { 'digit' }

  sub visual {
    my $self = shift;
    $self->{neg} ? '\D' : '\d';
  }
}


{
  package Regexp::Parser::anyof;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $neg, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'anyof',
      type => 'anyof',
      neg => $neg,
      data => \@data,
      down => 1,
    }, $class;
    return $self;
  }

  sub qr {
    my $self = shift;
    join "", $self->raw, map($_->qr, @{ $self->{data} }), "]";
  }

  sub visual {
    my $self = shift;
    join "", $self->raw, map($_->visual, @{ $self->{data} }), "]";
  }

  sub raw {
    my $self = shift;
    join "", "[", $self->{neg} ? "^" : "";
  }

  sub neg {
    my $self = shift;
    $self->{neg} = shift if @_;
    $self->{neg};
  }

  sub ender {
    my $self = shift;
    [ 'anyof_close' ];
  }

  sub data {
    my $self = shift;
    if (@_) {
      my $how = shift;
      if ($how eq '=') { $self->{data} = \@_ }
      elsif ($how eq '+') { push @{ $self->{data} }, @_ }
      else {
        my $t = $self->type;
        Carp::croak("\$$t->data([+=], \@data)");
      }
    }
    $self->{data};
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, $self->{rx}->object(@{ $self->ender });
    unshift @$ws, sub { -1 }, @{ $self->{data} }, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    push @$tree, $self;
    push @{ $rx->{stack} }, $tree;
    $rx->{tree} = $self->{data};
  }
}


{
  package Regexp::Parser::anyof_char;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $data, $vis) = @_;
    $vis = $data if not defined $vis;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'anyof_char',
      type => 'anyof_char',
      data => $data,
      vis => $vis,
    }, $class;
  }
}


{
  package Regexp::Parser::anyof_range;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $lhs, $rhs) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'anyof_range',
      type => 'anyof_range',
      data => [$lhs, $rhs],
    }, $class;
  }

  sub qr {
    my $self = shift;
    join "-", $self->{data}[0]->qr, $self->{data}[1]->qr;
  }

  sub visual {
    my $self = shift;
    join "-", $self->{data}[0]->visual, $self->{data}[1]->visual;
  }
}


{
  package Regexp::Parser::anyof_class;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $type, $neg, $how) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'anyof_class',
    }, $class;

    if (ref $type) {
      $self->{data} = $type;
    }
    else {
      $self->{type} = $type;
      $self->{data} = 'POSIX';
      $self->{neg} = $neg;
      $self->{how} = $how;
    }

    return $self;
  }

  sub type {
    my $self = shift;
    if (ref $self->{data}) {
      $self->{data}->type;
    }
    else {
      join "", $self->{how}, ($self->{neg} ? '^' : ''),
               $self->{type}, $self->{how};
    }
  }

  sub neg {
    my $self = shift;
    if (ref $self->{data}) {
      $self->{data}->neg = shift if @_;
      $self->{data}->neg;
    }
    else {
      $self->{neg} = shift if @_;
      $self->{neg};
    }
  }

  sub visual {
    my $self = shift;
    if (ref $self->{data}) {
      $self->{data}->visual;
    }
    else {
      join "", "[", $self->{how}, ($self->{neg} ? '^' : ''),
           $self->{type}, $self->{how}, "]";
    }
  }
}


{
  package Regexp::Parser::anyof_close;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'close',
      type => 'anyof_close',
      raw => ']',
      omit => 1,
      up => 1,
    }, $class;
    return $self;
  }

  sub insert {
    my $self = shift;
    my $rx = $self->{rx};
    $rx->{tree} = pop @{ $rx->{stack} };
    return $self;
  }
}


{
  package Regexp::Parser::prop;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $type, $neg) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'prop',
      type => $type,
      data => '',
      neg => ($neg ? 1 : 0),
    }, $class;
    return $self;
  }

  sub type {
    my $self = shift;
    $self->{type};
  }

  sub neg {
    my $self = shift;
    $self->{neg} = shift if @_;
    $self->{neg};
  }

  sub visual {
    my $self = shift;
    sprintf "\\%s{%s}", $self->{neg} ? 'P' : 'p', $self->type;
  }
}


{
  package Regexp::Parser::clump;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $vis) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'clump',
      type => 'clump',
      vis => $vis,
    }, $class;
  }
}


{
  package Regexp::Parser::branch;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      data => [[]],
      family => 'branch',
      type => 'branch',
      raw => '|',
      branch => 1,
    }, $class;
  }

  sub qr {
    my $self = shift;
    join $self->raw, map join("", map $_->qr, @$_), @{ $self->{data} };
  }

  sub visual {
    my $self = shift;
    join $self->raw, map join("", map $_->visual, @$_), @{ $self->{data} };
  }

  sub merge {
    my ($self) = @_;
    my $tree = $self->{rx}{tree};
    return unless @$tree;

    push @$tree, $self unless $tree->[-1] == $self;
    return unless @$tree > 1;
    my $prev = $tree->[-2];
    return unless $prev->type eq $self->type;
    push @{ $prev->{data} }, @{ $self->{data} };
    pop @$tree;
    return 1;
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    if ($d) {
      my $br = $self->{rx}->object($self->type);
      $br->omit(1);
      for (reverse @{ $self->data }) {
        unshift @$ws, $br, sub { -1 }, @$_, sub { +1 };
      }
      shift @$ws;
    }
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    my $st = $rx->{stack};

    # this is a branch inside an IFTHEN
    if (@$st and @{ $st->[-1] } and $st->[-1][-1]->type eq 'ifthen') {
      my $ifthen = $st->[-1][-1];
      my $cond = shift @{ $ifthen->{data} };
      $ifthen->{data} = [ [ @{ $ifthen->{data} } ], $cond ];
      $rx->{tree} = $ifthen->{data};
    }

    # if this is the 2nd or 3rd (etc) branch...
    elsif (@$st and @{ $st->[-1] } and $st->[-1][-1]->family eq $self->family) {
      my $br = $st->[-1][-1];
      $br->{data}[-1] = [ @$tree ];
      for (@{ $br->{data}[-1] }) {
        last unless $br->{zerolen} &&= $_->{zerolen};
      }
      push @{ $br->{data} }, [];
      $rx->{tree} = $br->{data}[-1];
    }

    # if this is the first branch
    else {
      $self->{data}[-1] = [ @$tree ];
      push @{ $self->{data} }, [];
      @$tree = $self;
      $tree->[-1]{zerolen} = 1;
      for (@{ $tree->[-1]{data}[0] }) {
        last unless $tree->[-1]{zerolen} &&= $_->{zerolen};
      }
      push @$st, $tree;
      $rx->{tree} = $self->{data}[-1];
    }
  }
}


{
  package Regexp::Parser::exact;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $data, $vis) = @_;
    $vis = $data if not defined $vis;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'exact',
      data => [$data],
      vis => [$vis],
    }, $class;
    return $self;
  }

  sub visual {
    my $self = shift;
    join "", @{ $self->{vis} };
  }

  sub type {
    my $self = shift;
    $self->{flags} & $self->{rx}->FLAG_i ? "exactf" : "exact";
  }

  sub data {
    my $self = shift;
    join "", @{ $self->{data} };
  }

  sub merge {
    my ($self) = @_;
    my $tree = $self->{rx}{tree};
    return unless @$tree;

    push @$tree, $self unless $tree->[-1] == $self;
    return unless @$tree > 1;
    my $prev = $tree->[-2];
    return unless $prev->type eq $self->type;
    
    push @{ $prev->{data} }, @{ $self->{data} };
    push @{ $prev->{vis} }, @{ $self->{vis} };
    pop @$tree;
    return 1;
  }
}


{
  package Regexp::Parser::quant;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $min, $max, $data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'quant',
      data => $data,
      min => $min,
      max => $max,
    }, $class;
    return $self;
  }

  sub min {
    my $self = shift;
    $self->{min};
  }

  sub max {
    my $self = shift;
    $self->{max};
  }

  sub type {
    my $self = shift;
    my ($min, $max) = ($self->min, $self->max);
    if ($min == 0 and $max eq '') { 'star' }
    elsif ($min == 1 and $max eq '') { 'plus' }
    else { 'curly' }
  }

  sub raw {
    my $self = shift;
    my ($min, $max) = ($self->min, $self->max);
    if ($min == 0 and $max eq '') { '*' }
    elsif ($min == 1 and $max eq '') { '+' }
    elsif ($min == 0 and $max == 1) { '?' }
    elsif ($max ne '' and $min == $max) { "{$min}" }
    else { "{$min,$max}" }
  }

  sub qr {
    my $self = shift;
    join "", $self->{data}->qr, $self->raw;
  }

  sub visual {
    my $self = shift;
    join "", $self->{data}->visual, $self->raw;
  }

  sub data {
    my $self = shift;
    if (@_) {
      my $how = shift;
      if ($how eq '=') { $self->{data} = \@_ }
      elsif ($how eq '+') { push @{ $self->{data} }, @_ }
      else {
        my $t = $self->type;
        Carp::croak("\$$t->data([+=], \@data)");
      }
    }
    $self->{data};
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, sub { -1 }, $self->{data}, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};

    # quantifiers must follow something
    $rx->error($rx->RPe_EQUANT)
      if @$tree == 0 or $tree->[-1]->family eq "flags";

    # quantifiers must NOT follow quantifiers
    $rx->error($rx->RPe_NESTED)
      if $tree->[-1]->family eq "quant";

    # on /abc+/, we extract the 'c' from the 'exact' node
    if ($tree->[-1]->family eq "exact" and @{ $tree->[-1]->{data} } > 1) {
      my $d = pop @{ $tree->[-1]->{data} };
      my $v = pop @{ $tree->[-1]->{vis} };
      my $q = $rx->object(exact => $d, $v);
      $q->{flags} = $tree->[-1]->{flags};
      $self->{data} = $q;
      push @$tree, $self;
    }
    else {
      # quantifier on (?{ ... }) is pointless;
      # bounded quantifier (but not ?) on a
      # zero-width assertion is unexpected
      if (
        ($tree->[-1]->family eq "assertion" and $tree->[-1]->type eq "eval") or
        ($tree->[-1]->{zerolen} and !($self->{min} == 0 and $self->{max} == 1))
      ) {
        $rx->awarn($rx->RPe_ZQUANT);
      }

      # unbounded quantifier on a zero-width
      # assertion can match a null string a lot
      elsif ($tree->[-1]->{zerolen} and $self->{max} eq '') {
        $rx->awarn($rx->RPe_NULNUL, $tree->[-1]->visual . $self->raw);
      }

      $self->{data} = $tree->[-1];
      $tree->[-1] = $self;
    }
  }
}


{
  # ( non-capturing
  package Regexp::Parser::group;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $on, $off, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'group',
      type => 'group',
      data => \@data,
      on => $on,
      off => $off,
      down => 1,
    }, $class;
  }

  sub on {
    my $self = shift;
    $self->{on};
  }

  sub off {
    my $self = shift;
    $self->{off};
  }

  sub raw {
    my $self = shift;
    join "", "(?", $self->{on},
      (length $self->{off} ? "-" : ""), $self->{off}, ":";
  }

  sub qr {
    my $self = shift;
    join "", $self->raw, map($_->qr, @{ $self->{data} }), ")";
  }

  sub visual {
    my $self = shift;
    join "", $self->raw, map($_->visual, @{ $self->{data} }), ")";
  }

  sub data {
    my $self = shift;
    if (@_) {
      my $how = shift;
      if ($how eq '=') { $self->{data} = \@_ }
      elsif ($how eq '+') { push @{ $self->{data} }, @_ }
      else {
        my $t = $self->type;
        Carp::croak("\$$t->data([+=], \@data)");
      }
    }
    $self->{data};
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, $self->{rx}->object(@{ $self->ender });
    unshift @$ws, sub { -1 }, @{ $self->{data} }, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    push @$tree, $self;
    push @{ $rx->{stack} }, $tree;
    $rx->{tree} = $self->{data};
  }
}


{
  # ( capturing
  package Regexp::Parser::open;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $nparen, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'open',
      nparen => $nparen,
      data => \@data,
      raw => '(',
      down => 1,
    }, $class;
    $self->{rx}{captures}[$nparen - 1] = $self;
    return $self;
  }

  sub type {
    my $self = shift;
    $self->family . $self->nparen;
  }

  sub nparen {
    my $self = shift;
    $self->{nparen};
  }

  sub qr {
    my $self = shift;
    join "", $self->raw, map($_->qr, @{ $self->{data} }), ")";
  }

  sub visual {
    my $self = shift;
    join "", $self->raw, map($_->visual, @{ $self->{data} }), ")";
  }

  sub ender {
    my $self = shift;
    [ close => $self->nparen ];
  }

  sub data {
    my $self = shift;
    if (@_) {
      my $how = shift;
      if ($how eq '=') { $self->{data} = \@_ }
      elsif ($how eq '+') { push @{ $self->{data} }, @_ }
      else {
        my $t = $self->type;
        Carp::croak("\$$t->data([+=], \@data)");
      }
    }
    $self->{data};
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, $self->{rx}->object(@{ $self->ender });
    unshift @$ws, sub { -1 }, @{ $self->{data} }, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    push @$tree, $self;
    push @{ $rx->{stack} }, $tree;
    $rx->{tree} = $self->{data};
  }
}


{
  # ) closing
  package Regexp::Parser::close;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $nparen) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'close',
      nparen => $nparen,
      raw => ')',
      omit => 1,
      up => 1,
    }, $class;
    return $self;
  }

  sub type {
    my $self = shift;
    $self->family . $self->nparen;
  }

  sub nparen {
    my $self = shift;
    $self->{nparen};
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};

    do {
      $tree = pop @{ $rx->{stack} }
        or $rx->error($rx->RPe_RPAREN)
    } until $tree->[-1]->{down};

    $rx->{tree} = $tree;

    $self->{nparen} = $tree->[-1]->nparen
      if $self->family eq 'close' and $tree->[-1]->can('nparen');

    if ($tree->[-1]->{ifthen}) {
      my $ifthen = $tree->[-1];
      my $br = $rx->object(branch =>);
      my $cond;

      if (ref $ifthen->{data}[0] eq "ARRAY") {
        (my($true), $cond) = splice @{ $ifthen->{data} }, 0, 2;
        $br->{data} = [ $true, $ifthen->{data} ];
      }
      else {
        $cond = shift @{ $ifthen->{data} };
        $br->{data} = [ $ifthen->{data} ];
      }

      $ifthen->{data} = [ $cond, $br ];
      $ifthen->{zerolen} =
        !grep !(grep $_->{zerolen}, @$_), @{ $ifthen->{data}[1]{data} };
    }
    else {
      $tree->[-1]->{zerolen} ||=
        !grep !$_->{zerolen}, @{ $tree->[-1]->{data} };
    }

    push @$tree, $self unless $self->omit;
  }
}


{
  # ) for non-captures
  package Regexp::Parser::tail;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'close',
      type => 'tail',
      raw => ')',
      omit => 1,
      up => 1,
    }, $class;
    return $self;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};

    do {
      $rx->{tree} = pop @{ $rx->{stack} }
        or $rx->error($rx->RPe_RPAREN)
    } until $tree->[-1]->{down};

    $self->{nparen} = $tree->[-1]->nparen
      if $self->family eq 'close' and $tree->[-1]->can('nparen');

    if ($tree->[-1]->{ifthen}) {
      my $ifthen = $tree->[-1];
      my $br = $rx->object(branch =>);
      my $cond;

      if (ref $ifthen->{data}[0] eq "ARRAY") {
        (my($true), $cond) = splice @{ $ifthen->{data} }, 0, 2;
        $br->{data} = [ $true, $ifthen->{data} ];
      }
      else {
        $cond = shift @{ $ifthen->{data} };
        $br->{data} = [ $ifthen->{data} ];
      }

      $ifthen->{data} = [ $cond, $br ];
      $ifthen->{zerolen} =
        !grep !(grep $_->{zerolen}, @$_), @{ $ifthen->{data}[1]{data} };
    }
    else {
      $tree->[-1]->{zerolen} ||=
        !grep !$_->{zerolen}, @{ $tree->[-1]->{data} };
    }

    push @$tree, $self unless $self->omit;
  }
}


{
  # \1 (backrefs)
  package Regexp::Parser::ref;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $nparen) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'ref',
      nparen => $nparen,
    }, $class;
    return $self;
  }

  sub type {
    my $self = shift;
    ($self->{flags} & $self->{rx}->FLAG_i ? 'reff' : 'ref') . $self->nparen;
  }

  sub nparen {
    my $self = shift;
    $self->{nparen};
  }

  sub visual {
    my $self = shift;
    "\\$self->{nparen}";
  }
}


{
  package Regexp::Parser::assertion;
  push @ISA, qw( Regexp::Parser::__object__ );

  push @Regexp::Parser::ifmatch::ISA, __PACKAGE__;
  push @Regexp::Parser::unlessm::ISA, __PACKAGE__;
  push @Regexp::Parser::suspend::ISA, __PACKAGE__;
  push @Regexp::Parser::ifthen::ISA, __PACKAGE__;
  push @Regexp::Parser::eval::ISA, __PACKAGE__;
  push @Regexp::Parser::logical::ISA, __PACKAGE__;

  sub qr {
    my $self = shift;
    join "", $self->raw, map($_->qr, @{ $self->{data} }), ")";
  }

  sub visual {
    my $self = shift;
    join "", $self->raw, map($_->visual, @{ $self->{data} }), ")";
  }

  sub data {
    my $self = shift;
    if (@_) {
      my $how = shift;
      if ($how eq '=') { $self->{data} = \@_ }
      elsif ($how eq '+') { push @{ $self->{data} }, @_ }
      else {
        my $t = $self->type;
        Carp::croak("\$$t->data([+=], \@data)");
      }
    }
    $self->{data};
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, $self->{rx}->object(@{ $self->ender });
    unshift @$ws, sub { -1 }, @{ $self->{data} }, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    my $rx = $self->{rx};
    push @$tree, $self;
    push @{ $rx->{stack} }, $tree;
    $rx->{tree} = $self->{data};
  }
}


{
  # (?=) (?<=)
  package Regexp::Parser::ifmatch;

  sub new {
    my ($class, $rx, $dir, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'ifmatch',
      dir => $dir,
      data => \@data,
      down => 1,
      zerolen => 1,
    }, $class;
    return $self;
  }

  sub dir {
    my $self = shift;
    $self->{dir};
  }

  sub raw {
    my $self = shift;
    join "", "(?", ($self->{dir} < 0 ? '<' : ''), "=";
  }
}


{
  # (?!) (?<!)
  package Regexp::Parser::unlessm;

  sub new {
    my ($class, $rx, $dir, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'unlessm',
      dir => $dir,
      data => \@data,
      down => 1,
      zerolen => 1,
    }, $class;
    return $self;
  }

  sub dir {
    my $self = shift;
    $self->{dir};
  }

  sub raw {
    my $self = shift;
    join "", "(?", ($self->{dir} < 0 ? '<' : ''), "!";
  }
}


{
  # (?>)
  package Regexp::Parser::suspend;

  sub new {
    my ($class, $rx, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'suspend',
      data => \@data,
      down => 1,
    }, $class;
    return $self;
  }

  sub raw {
    my $self = shift;
    "(?>";
  }
}

{
  # (?(n)t|f)
  package Regexp::Parser::ifthen;

  sub new {
    my ($class, $rx, @data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'ifthen',
      data => [],
      down => 1,
      ifthen => 1,
    }, $class;
    return $self;
  }

  sub raw {
    my $self = shift;
    "(?";
  }

  sub qr {
    my $self = shift;
    join "", $self->raw, $self->{data}[0]->qr, $self->{data}[1]->qr, ")";
  }

  sub visual {
    my $self = shift;
    join "", $self->raw, $self->{data}[0]->visual, $self->{data}[1]->visual, ")";
  }
}


{
  # the N in (?(N)t|f) when N is a number
  package Regexp::Parser::groupp;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $nparen) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'groupp',
      nparen => $nparen,
    }, $class;
    return $self;
  }

  sub type {
    my $self = shift;
    $self->family . $self->nparen;
  }

  sub nparen {
    my $self = shift;
    $self->{nparen};
  }

  sub visual {
    my $self = shift;
    "($self->{nparen})";
  }
}


{
  # (?{ ... })
  package Regexp::Parser::eval;

  sub new {
    my ($class, $rx, $code) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'eval',
      data => $code,
      zerolen => 1,
    }, $class;
    return $self;
  }

  sub visual {
    my $self = shift;
    "(?{$self->{data}})";
  }

  sub qr {
    my $self = shift;
    $self->visual;
  }

  sub insert {
    my ($self, $tree) = @_;
    push @$tree, $self;
  }

  sub walk {
    my $self = shift;
    return;
  }
}


{
  # (??{ ... })
  package Regexp::Parser::logical;

  sub new {
    my ($class, $rx, $code) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'assertion',
      type => 'logical',
      data => $code,
      zerolen => 1,
    }, $class;
    return $self;
  }

  sub visual {
    my $self = shift;
    "(??{$self->{data}})";
  }

  sub qr {
    my $self = shift;
    $self->visual;
  }

  sub insert {
    my ($self, $tree) = @_;
    push @$tree, $self;
  }

  sub walk {
    my $self = shift;
    return;
  }
}


{
  package Regexp::Parser::flags;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $on, $off) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'flags',
      type => 'flags',
      on => $on,
      off => $off,
      zerolen => 1,
    }, $class;
    return $self;
  }

  sub on {
    my $self = shift;
    $self->{on};
  }

  sub off {
    my $self = shift;
    $self->{off};
  }

  sub visual {
    my $self = shift;
    join "", "(?", $self->{on},
      (length $self->{off} ? "-" : ""), $self->{off}, ")";
  }
}


{
  package Regexp::Parser::minmod;
  push @ISA, qw( Regexp::Parser::__object__ );

  sub new {
    my ($class, $rx, $data) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      family => 'minmod',
      type => 'minmod',
      raw => '?',
      data => $data,
    }, $class;
    return $self;
  }

  sub qr {
    my $self = shift;
    join "", $self->{data}->qr, $self->raw;
  }

  sub visual {
    my $self = shift;
    join "", $self->{data}->visual, $self->raw;
  }

  sub walk {
    my ($self, $ws, $d) = @_;
    unshift @$ws, sub { -1 }, $self->{data}, sub { +1 } if $d;
  }

  sub insert {
    my ($self, $tree) = @_;
    $self->{data} = $tree->[-1];
    $tree->[-1] = $self;
  }
}

1;

__END__

=head1 NAME

Regexp::Parser::Objects - objects for Perl 5 regexes

=head1 DESCRIPTION

This module contains the object definitions for F<Regexp::Parser>.

=head2 Inheritance

All F<Regexp::Parser::*> objects inherit from
F<Regexp::Parser::__object__>, the global object base class.  All
user-defined F<MyRx::*> objects inherit from F<MyRx::__object__> first,
then from the F<Regexp::Parser::*> object of the same name, and finally
from F<Regexp::Parser::__object__>.  Don't worry -- if you don't define
a base class for your module's objects, or the object you create isn't
a modification of a standard object, no warnings will be issued.

=head2 The F<__object__> Base Class

All nodes inherit from F<Regexp::Parser::__object__> the following
methods:

=over 4

=item my $d = $obj->data()

The object's data.  This might be an array reference (for a 'branch'
node), another object (for a 'quant' node), or it might not exist at all
(for an 'anchor' node).

=item my $e = $obj->ender()

The arguments to object() to create the ending node for this object.
This is used by the walk() method.  Typically, a capturing group's ender
is a C<close> node, any other assertion's ender is a C<tail> node, and a
character class's ender is an C<anyof_close> node.

=item my $c = $obj->family()

The general family of this object.  These are any of: alnum, anchor,
anyof, anyof_char, anyof_class, anyof_range, assertion, branch, close,
clump, digit, exact, flags, group, groupp, minmod, prop, open, quant, ref,
reg_any.

=item my $f = $obj->flags()

The flag value for this object.  This value is a number created by OR'ing
together the flags that are enabled at the time.

=item $obj->insert()

Inserts this object into the tree.  It returns a value that says whether
or not it ended up being merged with the previous object in the tree.

=item my $m = $obj->merge()

Merges this node with the previous one, if they are of the same type. If
it is called I<after> $obj has been added to the tree, $obj will be
removed from the tree.  Most node types don't merge.  Returns true if
the node was merged with the previous one.

=item my $o = $obj->omit()

=item my $o = $obj->omit(VALUE)

Whether this node is omitted from the parse tree.  Certain objects
do not need to appear in the tree, but are needed when inspecting
the parsing, or walking the tree.

You can also set this attribute by passing a value.

=item my $q = $obj->qr()

The regex representation of this object.  It includes the regex
representation of any children of the object.

=item my $r = $obj->raw()

The raw representation of this object.  It does not look at the
children of the object, just itself.  This is used primarily when
inspecting the parsing of the regex.

=item my $t = $obj->type()

The specific type of this object.  See the object's documentation for
possible values for its type.

=item my $v = $obj->visual()

The visual representation of this object.  It includes the visual
representation of any children of the object.

=item $obj->walk()

"Walks" the object.  This is used to dive into the node's children
when using a walker (see L<Regexp::Parser/"Walking the Tree">).

=back

Objects may override these methods (as objects often do).

=head3 Using F<NEXT::> instead of F<SUPER::>

You can't use C<< $obj->SUPER::method() >> inside the F<__object__> class,
because F<__object__> doesn't inherit from anywhere.  You want to go along
the I<object>'s inheritance tree.  Use Damian Conway's F<NEXT> module
instead.  This module is standard with Perl 5.8.

=head2 Object Attributes

All objects share the following attributes (accessible via
C<< $obj->{...} >>):

=over 4

=item rx

The parser object with which it was created.

=item flags

The flags for the object.

=back

The following attributes may also be set:

=over 4

=item branch

Whether this object has branches (like C<|>).

=item family

The general family of this object.

=item data

The data or children of this object.

=item dir

The direction of this object (for look-ahead/behind assertions).  If
less than 0, it is behind; otherwise, it is ahead.

=item down

Whether this object creates a deeper scope (like an OPEN).

=item ifthen

Whether this object has a true/false branch (like the C<(?(...)T|F)>
assertion).

=item max

The maximum repetition count of this object (for quantifiers).

=item min

The minimum repetition count of this object (for quantifiers).

=item neg

Whether this object is negated (like a look-ahead or a character class).

=item nparen

The capture group related to this object (like for OPEN and back
references).

=item off

The flags specifically turned off for this object (for flag assertions
and C<(?:...)>).

=item omit

Whether this object is omitted from the actual tree (like a CLOSE).

=item on

The flags specifically turned on for this object (for flag assertions
and C<(?:...)>).

=item raw

The raw representation of this object.

=item type

The specific type of this object.

=item up

Whether this object goes into a shallower scope (like a CLOSE).

=item vis

The visual representation of this object.

=item zerolen

Whether this object does is zero-width (like an anchor).

=back

If there is a method with the name of one of these attributes, it is
I<imperative> you use the method to access the attribute when I<outside>
the class, and it's a good idea to do so I<inside> the class as well.

=head1 OBJECTS

All objects are prefixed with F<Regexp::Parser::>, but that is omitted
here for brevity.  The headings are object I<classes>.  The field
"family" represents the general category into which that object falls.

This is very sparse.  Future versions will have more complete
documentation.  For now, read the source (!).

=head2 bol

Family: anchor

Types: bol (C<^>), sbol (C<^> with C</s> on, C<\A>), mbol (C<^> with
C</m> on)

=head2 bound

Family: anchor

Types: bound (C<\b>), nbound (C<\B>)

Neg: 1 if negated

=head2 gpos

Family: anchor

Types: gpos (C<\G>)

=head2 eol

Family: anchor

Types: eol (C<$>), seol (C<$> with C</s> on, C<\Z>), meol (C<$> with
C</m> on), eos (C<\z>)

=head2 reg_any

Family: reg_any

Types: reg_any (C<.>), sany (C<.> with C</s> on), cany (C<\C>)

=head2 alnum

Family: alnum

Types: alnum (C<\w>), nalnum (C<\W>)

Neg: 1 if negated

=head2 space

Family: space

Types: space (C<\s>), nspace (C<\S>)

Neg: 1 if negated

=head2 digit

Family: digit

Types: digit (C<\d>), ndigit (C<\D>)

Neg: 1 if negated

=head2 anyof

Family: anyof

Types: anyof (C<[>)

Data: array reference of I<anyof_char>, I<anyof_range>, I<anyof_class>

Neg: 1 if negated

Ender: I<anyof_close>

=head2 anyof_char

Family: anyof_char

Types: anyof_char (C<X>)

Data: actual character

=head2 anyof_range

Family: anyof_range

Types: anyof_range (C<X-Y>)

Data: array reference of lower and upper bounds, both I<anyof_char>

=head2 anyof_class

Family: anyof_class

Types: via C<[:NAME:]>, C<[:^NAME:]>, C<\p{NAME}>, C<\P{NAME}>: alnum
(C<\w>, C<\W>), alpha, ascii, cntrl, digit (C<\d>, C<\D>), graph, lower,
print, punct, space (C<\s>, C<\S>), upper, word, xdigit; others are
possible (Unicode properties and user-defined POSIX classes)

Data: 'POSIX' if C<[:NAME:]>, C<[^:NAME:]> (or other POSIX notations, like
C<[=NAME=]> and C<[.NAME.]>); otherwise, reference to I<alnum>, I<digit>,
I<space>, or I<prop> object

Neg: 1 if negated

=head2 anyof_close

Family: close

Types: anyof_close (C<]> when in C<[...>)

Omitted

=head2 prop

Family: prop

Types: name of property (C<\p{NAME}>, C<\P{NAME}>); any Unicode property
defined by Perl or elsewhere

Neg: 1 if negated

=head2 clump

Family: clump

Types: clump (C<\X>)

=head2 branch

Family: branch

Types: branch (C<|>)

Data: array reference of array references, each representing one
alternation, holding any number of objects

Branched

=head2 exact

Family: exact

Types: exact (C<abc>), exactf (C<abc> with C</i> on)

Data: array reference of actual characters

=head2 quant

Family: quant

Types: star (C<*>), plus (C<+>), curly (C<?>, C<{n}>, C<{n,}>, C<{n,m}>)

Data: one object

=head2 group

Family: group

Types: group (C<(?:>, C<(?i-s:>)

Data: array reference of any number of objects

Ender: I<tail>

=head2 open

Family: open

Types: open1, open2 ... openN (C<(>)

Data: array reference of any number of objects

Ender: I<close>

=head2 close

Family: close

Types: close1, close2 ... closeN (C<)> when in C<(...>)

Omitted

=head2 tail

Family: close

Types: tail (C<)> when not in C<(...>)

Omitted

=head2 ref

Family: ref

Types: ref1, ref2 .. refN (C<\1>, C<\2>, etc.); reff1, reff2 .. reffN
(C<\1>, C<\2>, etc. with C</i> on)

=head2 ifmatch

Family: assertion

Types: ifmatch (C<(?=)>, C<< (?<= >>)

Data: array reference of any number of objects

Dir: -1 if look-behind, 1 if look-ahead

Ender: tail

=head2 unlessm

Family: assertion

Types: unlessm (C<(?!>, C<< (?<! >>)

Data: array reference of any number of objects

Dir: -1 if look-behind, 1 if look-ahead

Ender: tail

=head2 suspend

Family: assertion

Types: suspend (C<< (?> >>)

Data: array reference of any number of objects

Ender: tail

=head2 ifthen

Family: assertion

Types: ifthen (C<(?(>)

Data: array reference of two objects; first: I<ifmatch>, I<unlessm>,
I<eval>, I<groupp>; second: I<branch>

Ender: tail

=head2 groupp

Family: groupp

Types: groupp1, groupp2 .. grouppN (C<1>, C<2>, etc. when in C<(?(>)

=head2 eval

Family: assertion

Types: eval (C<(?{>)

Data: string with contents of assertion

=head2 logical

Family: assertion

Types: logical (C<(??{>)

Data: string with contents of assertion

=head2 flags

Family: flags

Types: flags (C<(?i-s)>)

=head2 minmod

Family: minmod

Types: minmod (C<?> after I<quant>)

Data: an object in the I<quant> family

=head1 SEE ALSO

L<Regexp::Parser>, L<Regexp::Parser::Handlers>.

=head1 AUTHOR

Jeff C<japhy> Pinyan, F<japhy@perlmonk.org>

=head1 COPYRIGHT

Copyright (c) 2004 Jeff Pinyan F<japhy@perlmonk.org>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
