say "1..2";
my $multi = ::Multi.new;
$multi.variants = ::Array.new;
{
  my $code = sub ($arg1) {
    say $arg1;
  }
  my $sig = ::Signature.new();
  $sig.positionals = ::Array.new;

  my $param1 = ::RefParam.new;
  $param1.name = '$arg1';
  $sig.positionals.push($param1.FETCH);

  $multi.variants.push(::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig))));
}.();
{
  my $code = sub ($arg1,$arg2) {
    say "ok 2";
  }
  my $sig = ::Signature.new();
  $sig.positionals = ::Array.new;

  my $param1 = ::ReadonlyParam.new;
  $param1.name = '$arg1';
  $sig.positionals.push($param1.FETCH);

  my $param2 = ::ReadonlyParam.new;
  $param2.name = '$arg2';
  $sig.positionals.push($param2.FETCH);

  $multi.variants.push(::Code.new(:outer($code.outer),:mold($code.mold),:signature((|$sig))));
}.();
$multi.("ok 1");
$multi.(1,2);
