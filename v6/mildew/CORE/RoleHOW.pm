knowhow RoleHOW {
  method add_attribute($object, $privname, $attribute) {
    $object.^!attributes.{$privname} = $attribute;
  }
  method add_method($object, $name, $code) {
    $object.^!methods.{$name} = $code
  }
  method dispatch($object, $identifier, \$capture) {
    if PRIMITIVES::idconst_eq($identifier,'FETCH') {
      # in item context, returns itself.
      return $object;
    } else {
      # Roles are not classes! so we're going to delegate this to a
      # punned class that does this role. For now, we're going to pun a
      # new class every time, then we'll think in some sort of caching.
      my $class = ::p6opaque.^!CREATE;
      $class.^!how = ::PrototypeHOW;
      $class.^compose_role(::LowObject);
      $class.^compose_role($object);
      return $class.^dispatch($identifier, (|$capture));
    }
  }
}

role LowObject {
  method new {
    return self.^clone;
  }
}

$LexicalPrelude.{'RoleHOW'} := ::RoleHOW;

