say "1..1";
::p6opaque.^!CREATE;
say "ok 1 # lives after destroying p6opaque without a HOW";
my $obj = ::p6opaque.^!CREATE;
