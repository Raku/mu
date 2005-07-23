# EVIL hacks here! E.g. method map and sub JS::Root::map!

method JS::Root::shift(Array $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var ret = args[1].GET().shift();
    if(ret instanceof PIL2JS.Ref) ret = ret.referencee.GET();
    return ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret;
  })')($self);
}

method JS::Root::pop(Array $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var ret = args[1].GET().pop();
    if(ret instanceof PIL2JS.Ref) ret = ret.referencee.GET();
    return ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret;
  })')($self);
}

method JS::Root::unshift(Array $self: *@things) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].GET(), add = args[2].GET();
    if(array instanceof PIL2JS.Ref) array = array.referencee.GET();
    for(var i = add.length - 1; i >= 0; i--) {
      array.unshift(new PIL2JS.Box(add[i].GET()));
    }
    return new PIL2JS.Box.Constant(array.length);
  })')($self, @things);
}

method JS::Root::push(Array $self: *@things) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].GET(), add = args[2].GET();
    if(array instanceof PIL2JS.Ref) array = array.referencee.GET();
    for(var i = 0; i < add.length; i++) {
      array.push(new PIL2JS.Box(add[i].GET()));
    }
    return new PIL2JS.Box.Constant(array.length);
  })')($self, @things);
}

method join(Array $self: Str $sep) { join $sep, *$self }
sub JS::Root::join(Str $sep, *@things) is primitive {
  JS::inline('
    function (arr, sep) {
      return arr.join(sep);
    }
  ')(@things, $sep);
}

method JS::Root::elems(Array $self:) {
  JS::inline('function (arr) { return arr.length }')($self);
}

method JS::Root::end(Array $self:) {
  JS::inline('function (arr) { return arr.length - 1 }')($self);
}

method map(Array $self: Code $code) { map $code, *$self }
sub JS::Root::map(Code $code, *@array) is primitive {
  my $arity = $code.arity;
  # die "Can't use 0-ary subroutine as \"map\" body!" if $arity == 0;
  $arity ||= 1;

  my @res;
  while(+@array > 0) {
    my @args = ();
    my $i; loop $i = 0; $i < $arity; $i++ {
      push @args: @array.shift;
    }
    push @res, *$code(*@args);
  }

  @res;
}

method grep(Array $self: Code $code) { grep $code, *$self }
sub JS::Root::grep(Code $code, *@array) is primitive {
  die "Code block for \"grep\" must be unary!" unless $code.arity == 1;

  my @res;
  for @array -> $item {
    push @res, $item if $code($item);
  }
  @res;
}

method sum(Array $self:) { sum *$self }
sub JS::Root::sum(*@vals) is primitive {
  my $sum = 0;
  $sum += +$_ for @vals;
  $sum;
}

sub infix:<..>(Num $from is copy, Num $to) is primitive {
  my @array = ($from,);

  while(($from += 1) <= $to) {
    push @array: $from;
  }

  @array;
}
sub infix:<^..>  (Num $from, Num $to) is primitive { ($from + 1)..$to }
sub infix:<..^>  (Num $from, Num $to) is primitive { $from..($to - 1) }
sub infix:<^..^> (Num $from, Num $to) is primitive { ($from + 1)..($to - 1) }

sub infix:<,>(*@xs) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var array = [];
    for(var i = 0; i < args[0].GET().length; i++) {
      // The extra new PIL2JS.Box is necessary to make the contents of arrays
      // readwrite, i.e. my @a = (0,1,2); @a[1] = ... should work.
      array[i] = new PIL2JS.Box(args[0].GET()[i].GET());
    }
    return new PIL2JS.Box.Constant(array);
  })')(@xs);
}

sub circumfix:<[]>(*@xs) is primitive { \@xs }
method postcircumfix:<[]>(Array $self: Int $idx is copy) is rw {
  # *Important*: We have to calculate the idx only *once*:
  #   my @a  = (1,2,3,4);
  #   my $z := @a[-1];
  #   say $z;               # 4
  #   push @a, 5;
  #   say $z;               # 4 (!!)
  $idx = +$self + $idx if $idx < 0;
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var array = args[0].GET();
    if(array instanceof PIL2JS.Ref) array = array.referencee.GET();
    var idx   = args[1].toNative();

    // Relay .GET and .STORE to array[idx].
    var ret = new PIL2JS.Box.Proxy(
      function () {
        var ret = array[idx];
        return ret == undefined ? undefined : ret.GET();
      },
      function (n) {
        if(array[idx] == undefined)
          array[idx] = new PIL2JS.Box(undefined);
        array[idx].STORE(n);
        return n;
      }
    );

    ret.uid = array[idx] == undefined ? undefined : array[idx].uid;

    // .BINDTO is special: @a[$idx] := $foo should work.
    ret.BINDTO = function (other) {
      if(array[idx] == undefined)
        PIL2JS.die("Can\'t rebind undefined!");

      return array[idx].BINDTO(other);
    };

    return ret;
  })')($self, $idx);
}
