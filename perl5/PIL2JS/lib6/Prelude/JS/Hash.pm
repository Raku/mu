sub circumfix:<{}>(Array $pairs) is primitive { \hash(*$pairs) }
sub hash(Pair *@pairs) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var pairs = args[0].GET();
    var hash  = new PIL2JS.Hash();

    for(var i = 0; i < pairs.length; i++) {
      var pair = pairs[i].GET();

      // my %hash = ("a", 1, "b", 2);
      if(!(pair instanceof PIL2JS.Pair)) {
        // pair is in fact the key; extract the value.
        i++;
        var value = pairs[i]
          ? pairs[i]
          : new PIL2JS.Box.Constant(undefined);
        pair = new PIL2JS.Pair(new PIL2JS.Box.Constant(pair), value);
      }

      // See thread "Hash creation with duplicate keys" started by Ingo
      // Blechschmidt on p6l:
      // http://www.nntp.perl.org/group/perl.perl6.language/22379
      if(!hash.exists(pair.key)) {
        // The extra new PIL2JS.Box is necessary to make the contents of hashes
        // readwrite, i.e. my %a = (a => 1); %a<a> = ... should work.
        hash.add_pair(new PIL2JS.Pair(
          new PIL2JS.Box.ReadOnly(pair.key),
          new PIL2JS.Box(pair.value.GET())
        ));
      }
    }

    return new PIL2JS.Box.Constant(hash);
  })')(@pairs);
}

method postcircumfix:<{}>(Hash $self: $key) {
  return PIL2JS::Internals::generic_deref($self){$key}
    if $self.isa("Ref");
  die "Can't use object of type {$self.ref} as a hash!"
    unless $self.isa("Hash");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt  = args.shift();
    var hash = args[0].GET();
    if(hash instanceof PIL2JS.Ref) hash = hash.referencee.GET();
    var key  = args[1];

    // Relay .GET and .STORE to hash.entries[key].
    var ret = new PIL2JS.Box.Proxy(
      function () {
        var ret = hash.get_value(key);
        return ret == undefined ? undefined : ret.GET();
      },
      function (n) {
        if(!hash.exists(key)) {
          hash.add_pair(new PIL2JS.Pair(
            new PIL2JS.Box.ReadOnly(key),
            new PIL2JS.Box(undefined)
          ));
        }
        hash.get_value(key).STORE(n);
        return n;
      }
    );

    ret.uid = hash.exists(key) ? hash.get_value(key).uid : undefined;

    // .BINDTO is special: %hash{$key} := $foo should work.
    ret.BINDTO = function (other) {
      if(!hash.exists(key))
        PIL2JS.die("Can\'t rebind undefined!");

      return hash.get_value(key).BINDTO(other);
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
