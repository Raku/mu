module re;

use v6-alpha;

sub import (Str $pkg: $input_engine) {
  my $engine = uc $input_engine;
  unless($engine eq 'PGE' or $engine eq 'PCR') {
    warn "Unknown re engine: $engine"; return %ENV<PUGS_REGEX_ENGINE>;
  }

  %ENV<PUGS_REGEX_ENGINE> = $engine;
}

1;
