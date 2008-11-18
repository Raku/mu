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
      my $invocant = $capture.invocant();
      if $invocant.^!methods.exists($identifier) {
         $invocant.^!methods.{$identifier}.($capture);
      } else {
         die 'No method ',$identifier;
      }
  }
}
class Foo {
  method bar {
    $OUT.print("ok 1 - method called.\n");
  }
}
$OUT.print("1..1\n");
Foo.bar;
1;