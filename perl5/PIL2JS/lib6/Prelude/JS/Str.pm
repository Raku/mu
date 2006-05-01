method JS::Root::substr(Str $str is rw: Int $a, Int $b = chars $str) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var str  = args[1],
        a    = Number(args[2].toNative()),
        b    = Number(args[3].toNative()),
        cc   = args.pop();
    if(str.FETCH().referencee && str.FETCH().autoderef) str = str.FETCH().referencee;

    var proxy = new PIL2JS.Box.Proxy(
      function () {
        var str_ = String(PIL2JS.cps2normal(
          _26main_3a_3aprefix_3a_7e.FETCH(),
          [ PIL2JS.Context.ItemAny, str ]
        ).toNative());

        return str_.substr(a, b);
      },
      function (n) {
        var str_ = String(PIL2JS.cps2normal(
          _26main_3a_3aprefix_3a_7e.FETCH(),
          [ PIL2JS.Context.ItemAny, str ]
        ).toNative());
        var repl = String(PIL2JS.cps2normal(
          _26main_3a_3aprefix_3a_7e.FETCH(),
          [ PIL2JS.Context.ItemAny, n ]
        ).toNative());

        str.STORE(new PIL2JS.Box.Constant(
          str_.substr(0, a) +
          repl +
          str_.substr(a + b)
        ));
      }
    );

    cc(proxy);
  })')($str, +$a, +$b < 0 ?? +$b - $a + chars $str !! +$b);
}

method split(Str $self: Str $splitter) { split $splitter, $self }
sub JS::Root::split(Str $splitter, Str $str) is primitive {
  JS::inline('(
    function (splitter, str) {
      return String(str).split(String(splitter));
    }
  )')(~$splitter, ~$str);
}

# XXX! $self = $CALLER::_ is a hack!!
method uc(Str $self = $CALLER::_:) { JS::inline('(function (str) { return str.toUpperCase() })')(~$self) }
method lc(Str $self = $CALLER::_:) { JS::inline('(function (str) { return str.toLowerCase() })')(~$self) }

method lcfirst(Str $self = $CALLER::_:) { lc(substr $self, 0, 1) ~ substr($self, 1) }
method ucfirst(Str $self = $CALLER::_:) { uc(substr $self, 0, 1) ~ substr($self, 1) }

# Of course, &bytes, &codes, &graphs will have to change. Dunno how to do
# different Unicode levels in browsers.
method bytes  (Str $self:) { JS::inline('(function (str) { return str.length })')(~$self) }
method chars  (Str $self:) { JS::inline('(function (str) { return str.length })')(~$self) }
method codes  (Str $self:) { JS::inline('(function (str) { return str.length })')(~$self) }
method graphs (Str $self:) { JS::inline('(function (str) { return str.length })')(~$self) }

method index(Str $self: Str $substr, Int $pos = 0) {
  JS::inline('(function (str, substr, pos) {
    return str.indexOf(substr, pos);
  })')(~$self, ~$substr, +$pos);
}
method rindex(Str $self: Str $substr, Int $pos = chars $self) {
  if $self eq "" and $substr ne "" {
    -1;
  } else {
    JS::inline('(function (str, substr, pos) {
      return str.lastIndexOf(substr, pos);
    })')(~$self, ~$substr, +$pos);
  }
}

method chomp(Str $self:) {
  if substr($self, -1, 1) eq "\n" {
    substr $self, 0, -1;
  } else {
    ~$self;
  }
}

method chop(Str $self:) { substr $self, 0, -1 }

sub infix:<x>    (Str $a, Int $count) is primitive {
  my $ret = "";
  $ret ~= $a for 1..$count;
  $ret;
}

sub infix:<xx>   (*@a) is primitive {
  my Int $count := pop @a;
  my @ret;
  push @ret, @a for 1..$count;
  @ret;
}

sub sprintf (Str $format, *@parts) is primitive {
  "&sprintf not yet implemented in PIL2JS";
}

# From src/perl6/Prelude.pm:7632
method trans (Str $self: Pair *@intable) {
    # Motto: If in doubt use brute force!
    my sub expand (Str $string is copy) {
        my @rv;

        my $add_dash;
        my $idx;

        if (substr($string,0,1) eq '-') {
            push @rv, '-';
            $string = substr($string,1);
        }

        if (substr($string,-1,1) eq '-') {
            $add_dash = 1;
            $string = substr($string,0,-1)
        }

        while (($idx = index($string,'-')) != -1) {
            my $pre = substr($string,0,$idx-1);
            my $start = substr($string,$idx-1,1);
            my $end = substr($string,$idx+1,1);

            push @rv, $pre.split('');
            push @rv, (~ $start)..(~ $end);

            $string = substr($string,$idx+2);
        }

        push @rv, $string.split('');
        push @rv, '-' if $add_dash;

        @rv;
    }

    my %transtable;
    for @intable -> Pair $pair {
        my ($k, $v) = $pair.kv;
        # $k is stringified by the => operator.
        my @ks = $k.isa(Str) ?? expand($k) !! $k.values;
        my @vs = $v.isa(Str) ?? expand($v) !! $v.values;
        %transtable{@ks} = @vs;
    }

    [~] map { %transtable{$_} // $_ }, $self.split('');
}
