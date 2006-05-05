# EVIL hacks here! E.g. method map and sub JS::Root::map!

method JS::Root::shift(@self:) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].FETCH(), cc = args.pop();
    var ret   = array.shift();
    cc(ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret);
  })')(@self);
}

method JS::Root::pop(@self:) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].FETCH(), cc = args.pop();
    var ret   = array.pop();
    cc(ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret);
  })')(@self);
}

method JS::Root::unshift($self is rw: *@things) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].FETCH(), add = args[2].FETCH(), cc = args.pop();
    if(array == undefined) args[1].STORE(new PIL2JS.Box.Constant(array = []));
    if(array.referencee && array.autoderef) array = array.referencee.FETCH();

    for(var i = add.length - 1; i >= 0; i--) {
      array.unshift(new PIL2JS.Box(add[i].FETCH()));
    }
    cc(new PIL2JS.Box.Constant(array.length));
  })')($self, @things);
}

method JS::Root::push($self is rw: *@things) is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].FETCH(), add = args[2].FETCH(), cc = args.pop();
    if(array == undefined) args[1].STORE(new PIL2JS.Box.Constant(array = []));
    if(array.referencee && array.autoderef) array = array.referencee.FETCH();

    for(var i = 0; i < add.length; i++) {
      array.push(new PIL2JS.Box(add[i].FETCH()));
    }
    cc(new PIL2JS.Box.Constant(array.length));
  })')($self, @things);
}

method join(@self: Str $sep) { join $sep, *@self }
sub JS::Root::join(Str $sep, *@things) is primitive {
  JS::inline('(
    function (arr, sep) {
      return arr.join(String(sep));
    }
  )')(@things.map:{ ~$_ }, $sep);
}

method JS::Root::elems(@self:) {
  JS::inline('(function (arr) { return arr.length })')(@self);
}

method JS::Root::end(@self:) {
  JS::inline('(function (arr) { return arr.length - 1 })')(@self);
}

method map(@self is rw: Code $code) { map $code, *@self }
sub JS::Root::map(Code $code, *@array is rw) is primitive {
  die "&map needs a Code as first argument!" unless $code.isa("Code");
  my $arity = $code.arity;
  # die "Can't use 0-ary subroutine as \"map\" body!" if $arity == 0;
  $arity ||= 1;

  my @res;
  while +@array > 0 {
    my @args = ();
    my $i; loop ($i = 0; $i < $arity; $i++) {
      # Slighly hacky
      push @args: undef;
      @args[-1] := @array.shift;
    }
    push @res, $code(*@args);
  }

  @res;
}

# XXX XXX XXX XXX ("luckily", the fully qualified name of a method doesn't
# matter. XXX XXX evil hack)
method sort(@self: Code $cmp = &infix:<cmp>) { sort $cmp, @self }
method PIL2JS::Internals::This::Is::A::Truly::Horrible::Hack::sort(%self: Code $cmp = &infix:<cmp>) { sort $cmp, %self.pairs }
sub JS::Root::sort(Code $cmp is copy = &infix:<cmp>, *@array) is primitive {
  # Hack
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:<cmp>;
  }

  die "&sort needs a Code as first argument!" unless $cmp.isa("Code");
  my $arity = $cmp.arity;
  $arity ||= 2; # hack
  die "Can't use $arity-ary subroutine as comparator block for &sort!"
    unless $arity == 2;

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    // [].concat(...): Defeat modifying of the original array.
    var array = [].concat(args[1].FETCH()), cmp = args[2].FETCH(), cc = args.pop();
    var jscmp = function (a, b) {
      return PIL2JS.cps2normal(cmp, [PIL2JS.Context.ItemAny, a, b]).toNative();
    };
    array.sort(jscmp);
    cc(new PIL2JS.Box.Constant(array));
  })')(@array, $cmp);
}

method reduce(@self: Code $code) { reduce $code, *@self }
sub JS::Root::reduce(Code $code, *@array) is primitive {
  die "&reduce needs a Code as first argument!" unless $code.isa("Code");
  my $arity = $code.arity;
  die "Can't use an unary or nullary block for &reduce!" if $arity < 2;

  my $ret = @array.shift;
  while +@array > 0 {
    my @args;
    my $i; loop ($i = 0; $i < $arity - 1; $i++) {
      # Slighly hacky
      push @args: undef;
      @args[-1] := @array.shift;
    }
    $ret = $code($ret, *@args);
  }

  $ret;
}

method min(@self: Code $cmp = &infix:«<=>») { min $cmp, *@self }
method max(@self: Code $cmp = &infix:«<=>») { max $cmp, *@self }
sub JS::Root::min(Code $cmp = &infix:«<=>», *@array) is primitive {
  # Hack, see comment at &sort.
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:«<=>»;
  }
  @array.max:{ $cmp($^b, $^a) };
}
sub JS::Root::max(Code $cmp = &infix:«<=>», *@array) is primitive {
  # Hack, see comment at &sort.
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:«<=>»;
  }

  my $max = @array.shift;
  $max = ($cmp($max, $_)) < 0 ?? $_ !! $max for @array;
  $max;
}

method grep(@self: Code $code) { grep $code, *@self }
sub JS::Root::grep(Code $code, *@array) is primitive {
  #die "Code block for \"grep\" must be unary!" unless $code.arity == 1;

  my @res;
  for @array -> $item is rw {
    push @res, $item if $code($item);
  }
  @res;
}

method sum(@self:) { sum *@self }
sub JS::Root::sum(*@vals) is primitive {
  my $sum = 0;
  $sum += +$_ for @vals;
  @vals ?? $sum !! undef;
  # We should return undef if we haven't been giving @vals to sum.
}

method uniq(@self: Code $cmp = &infix:<===>) { uniq $cmp, @self }
sub JS::Root::uniq(Code $cmp is copy = &infix:<cmp>, *@array) is primitive {
  # Hack
  unless $cmp.isa("Code") {
    unshift @array, @$cmp;
    $cmp := &infix:<===>;
  }

  # XXX O(n²) implementation, needing .id or === hashes for a better
  # implementation
  my @res;
  for @array -> $elem {
    unless $cmp($elem, any(@res)) {
      push @res, $elem;
    }
  }

  @res;
}

sub JS::Root::zip(Array *@arrays) is primitive is rw {
  my $maxlen = max map { +$_ }, @arrays;  # XXX wanting hyperops
  map {
    my $i := $_;
    map { @arrays[$_][$i] }, 0..@arrays.end;
  }, 0..$maxlen-1;
}

method reverse(*@things is copy:) {
  # Hack, should of course use context info, but that's not here yet.
  if @things == 1 {
    JS::inline('(function (str) { return str.split("").reverse().join("") })')(@things[0]);
  } else {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var arr = [].concat(args[1].FETCH()), cc = args.pop();
      arr.reverse();
      cc(new PIL2JS.Box.Constant(arr));
    })')(@things);
  }
}

sub infix:<..>(Num $from, Num $to) is primitive {
  my $i;
  my @res;

  loop ($i = $from; $i <= $to; $i++) {
    push @res, $i;
  }

  @res;
}
sub infix:<^..>  (Num $from, Num $to) is primitive { ($from + 1)..$to }
sub infix:<..^>  (Num $from, Num $to) is primitive { $from..($to - 1) }
sub infix:<^..^> (Num $from, Num $to) is primitive { ($from + 1)..($to - 1) }

sub infix:<,>(*@xs is rw) is primitive is rw {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var cc    = args.pop();
    var iarr  = args[0].FETCH();

    // We don\'t create new containers (new PIL2JS.Boxes) *here* -- lists
    // don\'t create new containers. Assigning to an array will take care of
    // this.

    var mk_magicalarray = function () {
      var marray = [];

      for(var i = 0; i < iarr.length; i++) {
        marray[i] = new PIL2JS.Box(undefined).BINDTO(
          // Slighly hacky way to determine if iarr[i] is undef, i.e.
          // it\'s needed to make
          //   my ($a, undef, $b) = (3,4,5);
          // work.
          iarr[i].isConstant && iarr[i].FETCH() == undefined
            ? new PIL2JS.Box(undefined)
            : iarr[i]
        );
      }

      return marray;
    };

    // Proxy needed for ($a, $b) = (3, 4) which really is
    // &infix:<,>($a, $b) = (3, 4);
    var proxy = new PIL2JS.Box.Proxy(
      function ()  { return iarr },
      function (n) {
        var marray = mk_magicalarray();
        var arr    = new PIL2JS.Box([]).STORE(n).FETCH();

        for(var i = 0; i < arr.length; i++) {
          if(marray[i]) marray[i].STORE(arr[i]);
        }

        return this;
      }
    );

    proxy.BINDTO = function (other) {
      var arr = other.FETCH();

      if(!(arr instanceof Array)) {
        PIL2JS.die("Can\'t bind list literal to non-array object!");
      }

      var backup_arr = [];
      for(var i = 0; i < arr.length; i++) {
        backup_arr[i]        = new PIL2JS.Box;
        backup_arr[i].FETCH  = arr[i].FETCH;
        backup_arr[i].STORE  = arr[i].STORE;
        backup_arr[i].BINDTO = arr[i].BINDTO;
      }

      for(var i = 0; i < backup_arr.length; i++) {
        if(iarr[i].isConstant && iarr[i].FETCH() == undefined) {
          // ($a, **undef**, $b) := (1,2,3);
          // (i.e., do nothing)
        } else {
          iarr[i].BINDTO(backup_arr[i]);
        }
      }

      return this;
    };

    cc(proxy);
  })')(@xs);
}
our &list := &infix:<,>;
our &pair := &infix:<,>;  # XXX wrong

sub circumfix:<[]>(*@xs is rw) is primitive { my @copy; @copy = @xs; \@copy }
method postcircumfix:<[]>(@self: Int *@idxs) is rw {
  die "Can't use object of type {@self.ref} as an array!"
    unless @self.isa("Array");

  # *Important*: We have to calculate the idx only *once*:
  #   my @a  = (1,2,3,4);
  #   my $z := @a[-1];
  #   say $z;               # 4
  #   push @a, 5;
  #   say $z;               # 4 (!!)

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var cc    = args.pop();
    var array = args[0].FETCH();
    var idxs  = args[1].toNative();

    var orig_value = [];
    for(var i = 0; i < idxs.length; i++) {
      idxs[i] = Number(idxs[i]);
      if(idxs[i] < 0) {
        var orig = Number(idxs[i]);
        idxs[i] = array.length + idxs[i];
        orig_value[idxs[i]] = orig;
      }
    }

    if(idxs.length == 0) PIL2JS.die("No indices given to &postcircumfix:<[ ]>!");

    // Relay .FETCH and .STORE to array[idx].
    var proxy_for = function (idx) {
      var ret = new PIL2JS.Box.Proxy(
        function () {
          var ret = array[idx];
          return ret == undefined ? undefined : ret.FETCH();
        },
        function (n) {
          if(idx < 0)
            PIL2JS.die("Modification of non-creatable array value attempted, subscript " + orig_value[idx]);

          // Support (in a slightly hacky manner) ($a, undef, $b) = (3,4,5).
          if(
            array[idx] == undefined || (
              array[idx].isConstant &&
              array[idx].FETCH() == undefined
            )
          ) {
            array[idx] = new PIL2JS.Box(undefined);
          }
          array[idx].STORE(n);
          return n;
        }
      );

      ret.uid = array[idx] == undefined ? undefined : array[idx].uid;

      // @a[$idx] := $foo should autovivify @a[$idx] if necessary.
      ret.BINDTO = function (other) {
        if(idx < 0)
          PIL2JS.die("Modification of non-creatable array value attempted, subscript " + orig_value[idx]);

        if(array[idx] == undefined)
          array[idx] = new PIL2JS.Box(undefined);

        return array[idx].BINDTO(other);
      };

      return ret;
    };

    if(idxs.length == 1) {
      cc(proxy_for(idxs[0]));
    } else {
      var ret = [];
      for(var i = 0; i < idxs.length; i++) {
        ret.push(proxy_for(idxs[i]));
      }

      // Needed for @a[1,2] = (3,4).
      var proxy = new PIL2JS.Box.Proxy(
        function ()  { return ret },
        function (n) {
          var arr = new PIL2JS.Box([]).STORE(n).FETCH();
          for(var i = 0; i < arr.length; i++) {
            if(ret[i]) ret[i].STORE(arr[i]);
          }

          return this;
        }
      );
      proxy.BINDTO = function (other) {
        var arr = other.FETCH();

        if(!(arr instanceof Array)) {
          PIL2JS.die("Can\'t bind array slice to non-array object!");
        }

        var backup_arr = [];
        for(var i = 0; i < arr.length; i++) {
          backup_arr[i]        = new PIL2JS.Box;
          backup_arr[i].FETCH  = arr[i].FETCH;
          backup_arr[i].STORE  = arr[i].STORE;
          backup_arr[i].BINDTO = arr[i].BINDTO;
        }

        for(var i = 0; i < backup_arr.length; i++) {
          ret[i].BINDTO(backup_arr[i]);
        }

        return this;
      };

      cc(proxy);
    }
  })')(@self, @idxs);
}

# Array autovification
# Needs PIL2 and MMD to be done without hacks
sub PIL2JS::Internals::Hacks::array_postcircumfix_for_undefs (
  $array is rw, Int *@idxs,
) is primitive is rw {
  if defined $array {
    die "\"$array\" can't be autovivified to an array!";
  }

  $array = [];
  $array[*@idxs];
}

sub PIL2JS::Internals::Hacks::init_undef_array_postcircumfix_method () is primitive {
  JS::inline('(function () {
    PIL2JS.addmethod(
      _3amain_3a_3aItem,
      "postcircumfix:[]",
      _26PIL2JS_3a_3aInternals_3a_3aHacks_3a_3aarray_postcircumfix_for_undefs
    );
  })')();
}

# Code from Prelude::PIR
sub splice (@a is rw, $offset=0, $length?, *@list) is primitive {
    my $off = +$offset;
    my $len = $length;
    my $size = +@a;

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

    my $listlen = +@list;
    my $size_change = $listlen - $len;
    my @result;

    if 1 {
        my $i = $off;
        my $stop = $off + $len;
        while $i < $stop {
            push(@result,@a[$i]);
            $i++;
        }
    }

    if $size_change > 0 {
        my $i = $size + $size_change -1;
        my $final = $off + $size_change;
        while $i >= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @a[$i], see t/operators/binding/arrays.t.
            @a.delete($i);
            @a[$i] = @a[$i-$size_change];
            $i--;
        }
    } elsif $size_change < 0 {
        my $i = $off;
        my $final = $size + $size_change -1;
        while $i <= $final {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @a[$i], see t/operators/binding/arrays.t.
            @a.delete($i);
            @a[$i] = @a[$i-$size_change];
            $i++;
        }
        # +@a = $size + $size_change;
        #   doesnt exist yet, so...
        my $n = 0;
        while $n-- > $size_change {
            pop(@a);
        }
    }

    if $listlen > 0 {
        my $i = 0;
        while $i < $listlen {
            # The .delete here is necessary to destroy all possible bindings
            # user code has to @a[$off+$i], see t/operators/binding/arrays.t.
            @a.delete($off+$i);
            @a[$off+$i] = @list[$i];
            $i++;
        }
    }

    #  want.List ?? *@result !! pop(@result)
    #  want.List ?? *@result !! +@result ?? @result[-1] !! undef;
    #  *@result;
    @result;
}
