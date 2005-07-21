method JS::Root::shift(Array $self:) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var ret = args[1].GET().shift();
    return ret == undefined ? new PIL2JS.Box.Constant(undefined) : ret;
  })')($self);
}

method JS::Root::push(Array $self: *@things) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var array = args[1].GET(), add = args[2].GET();
    for(var i = 0; i < add.length; i++) {
      array.push(add[i]);
    }
    return new PIL2JS.Box.Constant(array.length);
  })')($self, @things);
}

method JS::Root::join(Array $self: Str $sep) {
  JS::inline('
    function (arr, sep) {
      return arr.join(sep);
    }
  ')($self, $sep);
}


method JS::Root::elems(Array $self:) {
  JS::inline('function (arr) { return arr.length }')($self);
}

sub infix:<,>(*@xs)            is primitive {
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

sub circumfix:<[]>(*@xs)       is primitive { @xs }
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

