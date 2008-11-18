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
  my $inner_var;
  method bar {
    $OUT.print($inner_var.FETCH);
  }
  method foo($value) {
    $inner_var = $value;
  }
}
$OUT.print("1..1\n");
Foo.foo("ok 1 - methods are called.\n");
Foo.bar;
1;