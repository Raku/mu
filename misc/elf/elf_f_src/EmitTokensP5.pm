class EmitTokensP5 is EmitFasterP5 {
  sub rx ( $s ) {
      '/\G' ~ $s ~ '/g';
  }
  method cb__RegexDef ($n) {
      'sub ' ~ $n.ident ~ '{local $_ = shift;' ~ $.e($n.pattern) ~ '}';
  };
  method cb__Regex ($n) {
      $.e($n.patterns);
  };
  method cb__RxSequence ($n) {
      $.e($n.patterns).join('&&');
  };
  method cb__RxLiteral ($n) {
      rx($n.text);
  };
  method cb__RxFirst ($n) {
      'do{ my $_pos = pos(); ( ' 
            ~ $.e($n.patterns).join(
                    ' ) || ( ( ( pos() = $_pos ) || 1 ) && '
                ) 
      ~ ' ) }';
  }
  method cb__RxBlock ($n) {
      'do {' ~ $.e($n.block) ~ '1;}';
  }

};
