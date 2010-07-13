knowhow ClassHOW {
  method add_method($object, $name, $code) {
      $object.^!methods{$name.FETCH} = $code;
  }
  method dispatch($object, $identifier, \$capture) {  
      if $object.^!methods.exists($identifier.FETCH) {
         $object.^!methods{$identifier.FETCH}.postcircumfix:<( )>( (|$capture) );
      } else {
         say 'No method ',$identifier;
      }
  }
}
class Foo {
  my $inner_var;
  method bar {
    say $inner_var;
  }
  method foo($value) {
    $inner_var = $value;
  }
}
say "1..1";
Foo.foo("ok 1 - methods are called.");
Foo.bar;
1;
