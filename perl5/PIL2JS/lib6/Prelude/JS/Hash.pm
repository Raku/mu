sub circumfix:<{}>(*@pairs) is primitive { \hash(*@pairs) }
sub hash(*@pairs) is primitive {
  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt   = args.shift();
    var cc    = args.pop();
    var pairs = args[0].FETCH();
    var hash  = new PIL2JS.Hash();

    for(var i = 0; i < pairs.length; i++) {
      var pair = pairs[i].FETCH();

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
          new PIL2JS.Box(pair.value.FETCH())
        ));
      }
    }

    cc(new PIL2JS.Box.Constant(hash));
  })')(@pairs);
}

method postcircumfix:<{}>(%self: *@keys) is rw {
  die "Can't use object of type {%self.ref} as a hash!"
    unless %self.isa("Hash");

  JS::inline('new PIL2JS.Box.Constant(function (args) {
    var cxt  = args.shift();
    var cc   = args.pop();
    var hash = args[0].FETCH();
    var keys = args[1].FETCH();

    if(keys.length == 0) PIL2JS.die("No keys given to &postcircumfix:<{ }>!");

    var proxy_for = function (key) {
      // Relay .FETCH and .STORE to hash.entries[key].
      var ret = new PIL2JS.Box.Proxy(
        function () {
          var ret = hash.get_value(key);
          return ret == undefined ? undefined : ret.FETCH();
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

      // %hash{$key} := $foo should autovivify %hash{$key} if necessary.
      ret.BINDTO = function (other) {
        if(!hash.exists(key))
          hash.add_pair(new PIL2JS.Pair(
            new PIL2JS.Box.ReadOnly(key),
            new PIL2JS.Box(undefined)
          ));

        return hash.get_value(key).BINDTO(other);
      };

      return ret;
    };

    if(keys.length == 1) {
      cc(proxy_for(keys[0]));
    } else {
      var ret = [];
      for(var i = 0; i < keys.length; i++) {
        ret.push(proxy_for(keys[i]));
      }

      // Needed for %a<a b> = <c d>.
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
          PIL2JS.die("Can\'t bind hash slice to non-array object!");
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
  })')(%self, @keys);
}

# Hash autovification
# Needs PIL2 and MMD to be done without hacks
sub PIL2JS::Internals::Hacks::hash_postcircumfix_for_undefs (
  $hash is rw, *@keys
) is primitive is rw {
  if defined $hash {
    die "\"$hash\" can't be autovivified to a hash!";
  }

  $hash = hash();
  $hash{*@keys};
}

sub PIL2JS::Internals::Hacks::init_undef_hash_postcircumfix_method () is primitive {
  JS::inline('(function () {
    PIL2JS.addmethod(
      _3amain_3a_3aItem,
      "postcircumfix:{}",
      _26PIL2JS_3a_3aInternals_3a_3aHacks_3a_3ahash_postcircumfix_for_undefs
    );
  })')();
}
