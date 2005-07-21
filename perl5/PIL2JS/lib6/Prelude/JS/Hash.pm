sub hash(Array *@pairs) is primitive { circumfix:<{}>(@pairs) }
sub circumfix:<{}>(Array $pairs) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var pairs = args[0].GET();
    var hash  = new PIL2JS.Hash();

    for(var i = 0; i < pairs.length; i++) {
      var key = pairs[i].GET().key.toNative(), value = pairs[i].GET().value;
      // Note sure -- see thread "Hash creation with duplicate keys" started by
      // Ingo Blechschmidt on p6l:
      // http://www.nntp.perl.org/group/perl.perl6.language/22379
      if(hash[key] == undefined) {
        // The extra new PIL2JS.Box is necessary to make the contents of hashes
        // readwrite, i.e. my %a = (a => 1); %a<a> = ... should work.
        hash[key] = new PIL2JS.Box(value.GET());
      }
    }

    return new PIL2JS.Box.Constant(hash);
  })')($pairs);
}

method postcircumfix:<{}>(Hash $self: $key) {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt  = args.shift();
    var hash = args[0].GET();
    var key  = args[1].toNative();

    // Relay .GET and .STORE to hash[key].
    var ret = new PIL2JS.Box.Proxy(
      function () {
        var ret = hash[key];
        return ret == undefined ? undefined : ret.GET();
      },
      function (n) {
        if(hash[key] == undefined)
          hash[key] = new PIL2JS.Box(undefined);
        hash[key].STORE(n);
        return n;
      }
    );

    ret.uid = hash[key] == undefined ? undefined : hash[key].uid;

    // .BINDTO is special: %hash{$key} := $foo should work.
    ret.BINDTO = function (other) {
      if(hash[key] == undefined)
        PIL2JS.die("Can\'t rebind undefined!");

      return hash[key].BINDTO(other);
    };

    return ret;
  })')($self, $key);
}

sub infix:«=>»($key, $value)  is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt = args.shift();
    return new PIL2JS.Box.Constant(
      new PIL2JS.Pair(args[0], args[1])
    );
  })')($key, $value);
}
