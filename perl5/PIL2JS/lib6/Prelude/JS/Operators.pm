# Standard operators
my @subs = (
  "infix:«<»",    "N", "Number(a)  < Number(b)",
  "infix:«>»",    "N", "Number(a)  > Number(b)",
  "infix:«<=»",   "N", "Number(a) <= Number(b)",
  "infix:«>=»",   "N", "Number(a) >= Number(b)",
  "infix:«==»",   "N", "Number(a) == Number(b)",
  "infix:«!=»",   "N", "Number(a) != Number(b)",
  "infix:«lt»",   "S", "String(a)  < String(b)",
  "infix:«gt»",   "S", "String(a)  > String(b)",
  "infix:«le»",   "S", "String(a) <= String(b)",
  "infix:«ge»",   "S", "String(a) >= String(b)",
  "infix:«eq»",   "S", "String(a) == String(b)",
  "infix:«ne»",   "S", "String(a) != String(b)",
  "infix:«+»",    "N", "Number(a)  + Number(b)",
  "infix:«-»",    "N", "Number(a)  - Number(b)",
  "infix:«*»",    "N", "Number(a)  * Number(b)",
  "infix:«/»",    "N", "Number(a)  / Number(b)",
  "infix:«%»",    "N", "Number(a)  % Number(b)",
  "infix:«**»",   "N", "Math.pow(Number(a), Number(b))",
  "infix:«<=>»",  "N", "Number(a) < Number(b) ? -1 : Number(a) == Number(b) ? 0 : 1",
  "infix:«cmp»",  "S", "String(a) < String(b) ? -1 : String(a) == String(b) ? 0 : 1",
  "prefix:«-»",   "N", "-a",
  "abs",          "N", "Math.abs(a)",
  "sqrt",         "N", "Math.sqrt(a)",
  "sign",         "N", "a > 0 ? +1 : a == 0 ? 0 : -1",
  "exp",          "N", "Math.exp(a)",
  "log",          "N", "Math.log(a)",
  "log10",        "N", "Math.log(a) / Math.log(10)",
  "int",          "N", "parseInt(String(a))",
  "chr",          "N", "String.fromCharCode(a)",
  "ord",          "S", "a.length > 0 ? a.charCodeAt(0) : undefined",
  "hex",          "S", "parseInt(a, 16)",
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
for @subs -> $name, $type, $body {
  my $arity         = $name ~~ rx:P5/^infix:/ ?? 2 :: 1;
  my $undef         = $type eq "S" ?? '""' :: 0;
  my $jsbody        = "function ({$arity == 1 ?? "a" :: "a, b"}) \{
    if(a == undefined) a = $undef;
    {$arity == 2 ?? "if(b == undefined) b = $undef;" :: ""}
    return($body);
  \}";

  # XXX! HACK! See the end of Prelude::JS for explanation.
  my $args  = $arity == 1 ?? '$__a'   :: '$__a, $__b';
  my $type  = $arity == 1 ?? "method" :: "sub";
  my $colon = $arity == 1 ?? ":"      :: "";
  my $trait = $arity == 1 ?? ""       :: "is primitive";
  $eval ~= "
    $type $name ($args$colon) $trait \{
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
sub JS::Root::rand (?$a = 1)   is primitive { $JS::Math.random() * $a }

sub infix:<x>    (Str $a, Int $count) is primitive {
  my $ret = "";
  $ret ~= $a for 1..$count;
  $ret;
}

sub infix:<xx>   (*@a) is primitive {
  my Int $count := pop @a;
  my @ret;
  push @ret, @a for 1..$count;
  @ret;
}

sub infix:<^^>   ($a, $b) is primitive {
     if  $a and  $b { ?0 }
  elsif  $a and !$b { $a }
  elsif !$a and  $b { $b }
  else              { ?0 }
}
our &infix:<xor> = &infix:<^^>;
