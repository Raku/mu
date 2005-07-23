# No MMD yet.
method exists (Hash|Array $self: $idx) {
  return PIL2JS::Internals::generic_deref($self).exists($idx)
    if $self.isa("Ref");

  if($self.isa("Hash")) {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].GET(), key = args[2];
      return new PIL2JS.Box.Constant(hash.exists(key));
    })')($self, $idx);
  } elsif($self.isa("Array")) {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].GET(), idx = args[2].toNative();
      return new PIL2JS.Box.Constant(
        array[idx >= 0 ? idx : array.length + idx] != undefined
      );
    })')($self, $idx);
  } else {
    die ".exists does not work on objects of type {$self.ref}!";
  }
}

method delete (Hash|Array $self: *@idx) {
  return PIL2JS::Internals::generic_deref($self).delete(*@idx)
    if $self.isa("Ref");

  if($self.isa("Hash")) {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].GET(), keys = args[2].GET();
      var ret  = [];
      for(var i = 0; i < keys.length; i++) {
        var deleted = hash.delete_key(keys[i]);
        ret.push(deleted == undefined ? new PIL2JS.Box.Constant(undefined) : deleted);
      }
      return new PIL2JS.Box.Constant(ret);
    })')($self, @idx);
  } elsif($self.isa("Array")) {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].GET(), idxs = args[2].toNative();
      var ret  = [];
      for(var i = 0; i < idxs.length; i++) {
        var idx = idxs[i] >= 0 ? idxs[i] : array.length + idxs[i];
        ret.push(array[idx] == undefined ? new PIL2JS.Box.Constant(undefined) : array[idx]);
        delete array[idx];
      }
      return new PIL2JS.Box.Constant(ret);
    })')($self, @idx);
  } else {
    die ".exists does not work on objects of type {$self.ref}!";
  }
}
