knowhow ClassHOW {
  method add_method($how: $object, $name, $code) {
      if $object.^!methods.exists($name) {
          if $object.^!methods.{$name}.multi {
              $object.^!methods.{$name}.variants.push($code);
          } elsif $object.^!methods.{$name}.yada {
              $object.^!methods.{$name} = $code;
          } else {
              warn 'Method ', $name, ' redefined.';
              $object.^!methods.{$name} = $code;
          }
      } else {
          $object.^!methods.{$name} = $code;
      }
  }
  method dispatch($responder, $identifier, $capture) {
  }
}
class Foo {
  method bar {
  }
}
1;