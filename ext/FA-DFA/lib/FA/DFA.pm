use v6;

class FA::DFA::Node;

has %.arc;
has bool $.final;

multi method addarc(Str $trigger,:$node = FA::DFA::Node.new --> FA::DFA::Node) {
  %.arc{$trigger} = $node;
  $node;
}

multi method next(Str $trigger --> FA::DFA::Node) {
  %.arc{$trigger};
}

multi method finalize() {
  $.final=bool::true;
}

multi method finalq() {
  $.final;
}


class FA::DFA;

# An extreemly simple DFA.

has $.state;
has $.net;

multi method BUILD {
  $.net   //= FA::DFA::Node.new;
  $.state //= $.net;
}

multi method reset {
  $.state = $.net;
}

multi method addarc(Str $trigger,FA::DFA::Node $node --> FA::DFA::Node) {
  $.state.addarc($trigger,$node);
}

multi method addarc(Str $trigger --> FA::DFA::Node) {
  $.state.addarc($trigger);
}

multi method next(Str $trigger) {
  $.state = $.state.next($trigger);
}

multi method final() {
  $.state.final;
}

multi method final($final) {
  $.state.final = $final;
}


