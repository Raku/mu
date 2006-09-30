use v6-alpha;
use Test;

plan 5;

{
  eval_ok((eval q{use re 'pge'; 1}), 'can import the re module');
  is(%ENV<PUGS_REGEX_ENGINE>, 'PGE', 're module can set engine to PGE');
}

{
  eval q{use re 'PCR'};
  is(%ENV<PUGS_REGEX_ENGINE>, 'PCR', 're module can set engine to PCR');
}

{
  eval q[use re];
  is(%ENV<PUGS_REGEX_ENGINE>, 'PCR', 'argument must be provided');
}

{
  eval q[use re 'garbage'];
  is(%ENV<PUGS_REGEX_ENGINE>, 'PCR', 'garbage value not accepted');
}

