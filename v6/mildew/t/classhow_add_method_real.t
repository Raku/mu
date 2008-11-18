knowhow ClassHOW {
  method add_method($how: $object, $name, $code) {
      $object.^!methods.{$name} = $code;
  }
  method dispatch($responder, $identifier, $capture) {  
      my $invocant = $capture.invocant();
      if $invocant.^!methods.exists($identifier) {
         $invocant.^!methods.{$identifier}.postcircumfix:<( )>($capture);
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