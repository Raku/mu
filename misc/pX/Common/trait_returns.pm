# verbatim from S12
role returns {
  has ReturnType $.returns;
  multi sub trait_verb:<returns>($container: ReturnType $arg) {
    $container does returns($arg);
  }
  #... #QQQ - what goes here, if anything?
}
