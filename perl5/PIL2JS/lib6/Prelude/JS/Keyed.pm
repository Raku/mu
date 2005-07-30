# No MMD yet.
method exists (Hash|Pair|Array $self: $idx) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].GET(), key = args[2];
      return new PIL2JS.Box.Constant(hash.exists(key));
    })')(%$self, $idx);
  } elsif $self.isa("Pair") {
    $self.key eq $idx;
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].GET(), idx = Number(args[2].toNative());
      return new PIL2JS.Box.Constant(
        array[idx >= 0 ? idx : array.length + idx] != undefined
      );
    })')(@$self, $idx);
  } else {
    die ".exists does not work on objects of type {$self.ref}!";
  }
}

method delete (Hash|Array $self: *@idx) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].GET(), keys = args[2].GET();
      var ret  = [];
      for(var i = 0; i < keys.length; i++) {
        var deleted = hash.delete_key(keys[i]);
        ret.push(deleted == undefined ? new PIL2JS.Box.Constant(undefined) : deleted);
      }
      return new PIL2JS.Box.Constant(ret);
    })')(%$self, @idx);
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].GET(), idxs = args[2].toNative();
      var ret   = [];
      for(var i = 0; i < idxs.length; i++) {
        var idx = Number(idxs[i]) >= 0 ? Number(idxs[i]) : array.length + Number(idxs[i]);
        ret.push(array[idx] == undefined ? new PIL2JS.Box.Constant(undefined) : array[idx]);
        delete array[idx];
        if(idx == array.length - 1) array.length--;
      }
      return new PIL2JS.Box.Constant(ret);
    })')($self, @idx);
  } else {
    die ".delete does not work on objects of type {$self.ref}!";
  }
}

method keys (Hash|Pair|Array $self:) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var hash = args[1].GET();
      var keys = hash.keys();
      return new PIL2JS.Box.Constant(keys);
    })')(%$self);
  } elsif $self.isa("Pair") {
    ($self.key,);
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    JS::inline('new PIL2JS.Box.Constant(function (args) {
      var array = args[1].GET();
      var ret   = [];
      for(var i = 0; i < array.length; i++) {
        ret.push(new PIL2JS.Box.Constant(i));
      }
      return new PIL2JS.Box.Constant(ret);
    })')(@$self);
  } else {
    die ".keys does not work on objects of type {$self.ref}!";
  }
}

method values (Hash|Pair|Array $self:) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    %$self.keys.map:{ $self{$_} };
  } elsif $self.isa("Pair") {
    ($self.value,);
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    @$self;
  } else {
    die ".values does not work on objects of type {$self.ref}!";
  }
}

method kv (Hash|Pair|Array $self:) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    $self.keys.map:{ $_, $self{$_} };
  } elsif $self.isa("Pair") {
    ($self.key, $self.value);
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    $self.keys.map:{ $_, $self[$_] };
  } else {
    die ".kv does not work on objects of type {$self.ref}!";
  }
}

method pairs (Hash|Pair|Array $self:) {
  if $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Hash") {
    $self.keys.map:{ $_ => $self{$_}; };    # Don't remove the ";" inside the map!
  } elsif $self.isa("Pair") {
    ($self,);
  } elsif $self.isa("Ref") and PIL2JS::Internals::generic_deref($self).isa("Array") {
    $self.keys.map:{ $_ => $self[$_]; };    # ditto
  } else {
    die ".pairs does not work on objects of type {$self.ref}!";
  }
}
