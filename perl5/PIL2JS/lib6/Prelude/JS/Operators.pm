# Standard operators
my @subs = (
  "infix:«<»",    "Number(a)  < Number(b)",
  "infix:«>»",    "Number(a)  > Number(b)",
  "infix:«<=»",   "Number(a) <= Number(b)",
  "infix:«>=»",   "Number(a) >= Number(b)",
  "infix:«==»",   "Number(a) == Number(b)",
  "infix:«!=»",   "Number(a) != Number(b)",
  "infix:«lt»",   "String(a)  < String(b)",
  "infix:«gt»",   "String(a)  > String(b)",
  "infix:«le»",   "String(a) <= String(b)",
  "infix:«ge»",   "String(a) >= String(b)",
  "infix:«eq»",   "String(a) == String(b)",
  "infix:«ne»",   "String(a) != String(b)",
  "infix:«+»",    "Number(a)  + Number(b)",
  "infix:«-»",    "Number(a)  - Number(b)",
  "infix:«*»",    "Number(a)  * Number(b)",
  "infix:«/»",    "Number(a)  / Number(b)",
  "infix:«%»",    "Number(a)  % Number(b)",
  "infix:«**»",   "Math.pow(Number(a), Number(b))",
  "infix:«<=>»",  "Number(a) < Number(b) ? -1 : Number(a) == Number(b) ? 0 : 1",
  "infix:«cmp»",  "String(a) < String(b) ? -1 : String(a) == String(b) ? 0 : 1",
  "prefix:«-»",   "-a",
);

# First, we generate the code to eval later.
# Why don't eval the sub declarations immediately?
#   Because then we can't use them anymore. E.g.:
#     sub infix:<~> ($a, $b) { JS::inline(...) }
#     my $foo = $bar ~ $baz; # won't work!
# Ok, so why don't you use JS::Root::infix:<~> then?
#   Because the following doesn't parse currently:
#     sub JS::Root::infix:<~> ($a, $b) {...}
my $eval;
for @subs -> $name, $body {
  my $arity  = $name ~~ rx:P5/^infix:/ ?? 2 :: 1;
  my $jsbody = "function ({$arity == 1 ?? "a" :: "a, b"}) \{
    return($body);
  \}";

  my $args = $arity == 1 ?? '$a' :: '$a, $b';
  $eval ~= "
    sub $name ($args) is primitive \{
      JS::inline('$jsbody').($args);
    \}
  ";
}

# From here on, most normal things won't work any longer, as all the standard
# operators are overloaded with calls to JS::inline.
Pugs::Internals::eval $eval;
die $! if $!;

sub infix:<//>   ($a, Code $b) is primitive { defined($a) ?? $a :: $b() }
sub infix:<||>   ($a, Code $b) is primitive { $a ?? $a :: $b() }
sub infix:<&&>   ($a, Code $b) is primitive { $a ?? $b() :: $a }
sub infix:<err>  ($a, Code $b) is primitive { infix:<//>($a, $b()) } # XXX! hack
sub infix:<or>   ($a, Code $b) is primitive { infix:<||>($a, $b()) } # XXX! hack
sub infix:<and>  ($a, Code $b) is primitive { infix:<&&>($a, $b()) } # XXX! hack
sub prefix:<++>  ($a is rw)    is primitive { $a = $a + 1 }
sub postfix:<++> ($a is rw)    is primitive { my $cur = $a; $a = $a + 1; $cur }
sub prefix:<-->  ($a is rw)    is primitive { $a = $a - 1 }
sub postfix:<--> ($a is rw)    is primitive { my $cur = $a; $a = $a - 1; $cur }
