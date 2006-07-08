sub japh (Str $lang) { say "just another $lang hacker"; }
my &perl6Japh := &japh.assuming("Perl6");  
perl6Japh()
