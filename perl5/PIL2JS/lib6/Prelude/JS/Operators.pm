my $mapStr = -> Str $x, Code $f {
  "
    (function () \{
      var res = \"\";
      for(var i = 0; i < {$x}.length; i++) \{
        res += String.fromCharCode($f("{$x}.charCodeAt(i)"));
      \}
      return res;
    \})()
  ";
};

my $mapStr2 = -> Str $op, Str $x, Str $y {
  "
    (function () \{
      var res    = \"\";
      var minlen = {$x}.length < {$y}.length ? {$x}.length : {$y}.length;
      for(var i = 0; i < minlen; i++) \{
        res += String.fromCharCode({$x}.charCodeAt(i) $op {$y}.charCodeAt(i));
      \}
      return res;
    \})()
  ";
};

my $mapStr2Fill = -> Str $op, Str $x, Str $y {
  "
    (function () \{
      var res = \"\";

      for(var i = {$x}.length; i < {$y}.length; i++)
        $x += \"\\000\";
      for(var i = {$y}.length; i < {$x}.length; i++)
        $y += \"\\000\";

      for(var i = 0; i < {$x}.length; i++) \{
        res += String.fromCharCode({$x}.charCodeAt(i) $op {$y}.charCodeAt(i));
      \}
      return res;
    \})()
  ";
};

# Standard operators
my @subs = (
  "infix:«<»",    2, "N", "Number(a)  < Number(b)",
  "infix:«>»",    2, "N", "Number(a)  > Number(b)",
  "infix:«<=»",   2, "N", "Number(a) <= Number(b)",
  "infix:«>=»",   2, "N", "Number(a) >= Number(b)",
  "infix:«==»",   2, "N", "Number(a) == Number(b)",
  "infix:«!=»",   2, "N", "Number(a) != Number(b)",
  "infix:«lt»",   2, "S", "String(a)  < String(b)",
  "infix:«gt»",   2, "S", "String(a)  > String(b)",
  "infix:«le»",   2, "S", "String(a) <= String(b)",
  "infix:«ge»",   2, "S", "String(a) >= String(b)",
  "infix:«eq»",   2, "S", "String(a) == String(b)",
  "infix:«ne»",   2, "S", "String(a) != String(b)",
  "infix:«+»",    2, "N", "Number(a)  + Number(b)",
  "infix:«-»",    2, "N", "Number(a)  - Number(b)",
  "infix:«*»",    2, "N", "Number(a)  * Number(b)",
  "infix:«/»",    2, "N", "Number(a)  / Number(b)",
  "infix:«/»",    2, "N", "Number(b) == 0 ? eval(\"throw(new Error(\\\"Division by zero\\\"))\") : Number(a)  / Number(b)",
  "infix:«%»",    2, "N", "Number(b) == 0 ? eval(\"throw(new Error(\\\"Modulo zero\\\"))\") : Number(a)  % Number(b)",
  "infix:«**»",   2, "N", "Math.pow(Number(a), Number(b))",
  "infix:«+|»",   2, "N", "Number(a)  | Number(b)",
  "infix:«+&»",   2, "N", "Number(a)  & Number(b)",
  "infix:«~&»",   2, "S", "$mapStr2("&", "String(a)", "String(b)")",
  "infix:«~|»",   2, "S", "a = String(a), b = String(b), $mapStr2Fill("|", "a", "b")",
  "infix:«~^»",   2, "S", "a = String(a), b = String(b), $mapStr2Fill("^", "a", "b")",
  "prefix:«~^»",  1, "S", $mapStr("String(a)", { "255 - $^ord" }),
  "prefix:«+^»",  1, "N", "~Number(a)",
  "infix:«+^»",   2, "N", "Number(a)  ^ Number(b)",
  "infix:«+<»",   2, "N", "Number(a) << Number(b)",
  "infix:«+>»",   2, "N", "Number(a) >> Number(b)",
  "infix:«<=>»",  2, "N", "Number(a) < Number(b) ? -1 : Number(a) == Number(b) ? 0 : 1",
  "infix:«cmp»",  2, "S", "String(a) < String(b) ? -1 : String(a) == String(b) ? 0 : 1",
  "prefix:«-»",   1, "N", "-a",
  "abs",          1, "N", "Math.abs(a)",
  "sqrt",         1, "N", "Math.sqrt(a)",
  "sign",         1, "N", "a > 0 ? +1 : a == 0 ? 0 : -1",
  "exp",          1, "N", "Math.exp(a)",
  "log",          1, "N", "Math.log(a)",
  "log10",        1, "N", "Math.log(a) / Math.log(10)",
  "int",          1, "N", "a == Infinity || a == -Infinity || a != a ? a : parseInt(String(a))",
  "chr",          1, "N", "String.fromCharCode(a)",
  "ord",          1, "S", "a.length > 0 ? a.charCodeAt(0) : undefined",
  "hex",          1, "S", "parseInt(a, 16)",
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
for @subs -> $name, $arity, $type, $body {
  my $undef  = $type eq "S" ?? '""' !! 0;
  my $jsbody = "function ({$arity == 1 ?? "a" !! "a, b"}) \{
    if(a == undefined) a = $undef;
    {$arity == 2 ?? "if(b == undefined) b = $undef;" !! ""}
    return($body);
  \}";

  # XXX! minor hack. See the end of Prelude::JS for explanation.
  my $args  = $arity == 1  ?? '$__a = $CALLER::_' !! '$__a, $__b';
  my $c     = $type eq "S" ?? "~"                 !! "+";
  my $args_ = $arity == 1  ?? "$c\$__a"           !! "$c\$__a, $c\$__b";
  my $type  = $arity == 1  ?? "method"            !! "sub";
  my $colon = $arity == 1  ?? ":"                 !! "";
  my $trait = $arity == 1  ?? ""                  !! "is primitive";
  $eval ~= "
    $type $name ($args$colon) $trait \{
      JS::inline('($jsbody)').($args_);
    \}
  ";
}

# [...] reduce metaoperator
# XXX This implementation is, of course, incorrect. There is *no* attention
# paid to the associativity of the original operator and auto-metaed versions
# of user-defined ops are not generated.
for «
  <  >  <= >= == !=
  lt gt le ge eq ne
  + - * / % **
  ~
  <=> cmp
  &&  || //
  and or err
» -> $op {
  $eval ~= "
    sub prefix:«[$op]» (*\@things) is primitive \{
      if \@things \{
        reduce \{ \$^a $op \$^b \} \@things;
      \} else \{
        # We should fail() here, but as &fail isn't yet implemented...
        undef;
      \}
    \}
  ";
#  XXX: currying doesn't work properly
#  $eval ~= "
#    our &infix:\{\"»$op«\"\} := &__hyper.assuming( 'op' => &infix«$op» );
#    our &infix:\{\">>$op<<\"\} := &__hyper.assuming( 'op' => &infix«$op» );
#  ";
}

sub infix:{">>+<<"} (Array @a, Array @b) {
  __hyper(&infix:<+>, @a, @b);
}

our &infix:{"»+«"} := &infix:{">>+<<"};

sub infix:{">>~<<"} (Array @a, Array @b) {
  __hyper(&infix:<~>, @a, @b);
}

our &infix:{"»~«"} := &infix:{">>~<<"};

sub __hyper (Code $op, Array @a is copy, Array @b is copy) {
  my Array @ret;
  if (@a.elems == 1) {
      @a = @a[0] xx @b.elems;
  }
  if (@b.elems == 1) {
      @b = @b[0] xx @a.elems;
  }
  for 0..(@a.end, @b.end).max -> $i {
    if $i > @a.end {
      push @ret, @b[$i];
    }
    elsif $i > @b.end {
      push @ret, @a[$i];
    }
    else {
      push @ret, $op(@a[$i], @b[$i]);
    }
  }
  return @ret;
}

# From here on, most normal things won't work any longer, as all the standard
# operators are overloaded with calls to JS::inline.
Pugs::Internals::eval $eval;
die $! if $!;


sub prefix:<++>  ($a is rw)    is primitive { $a = $a + 1 }
sub postfix:<++> ($a is rw)    is primitive { my $cur = $a; $a = $a + 1; $cur }
sub prefix:<-->  ($a is rw)    is primitive { $a = $a - 1 }
sub postfix:<--> ($a is rw)    is primitive { my $cur = $a; $a = $a - 1; $cur }
sub JS::Root::rand ($a = 1)    is primitive { $JS::Math.random() * $a }

# The following line also installs &infix:<=>. (hack!)
method infix:<=> (Item $a is rw: $b) is rw          { $a = $b }
#sub infix:<=>    ($a is rw, $b) is primitive is rw { $a = $b }

sub prefix:<[.{}]> (*$head is copy, *@rest is copy) is primitive {
  while @rest {
    $head = $head{shift @rest};
  }

  $head;
}

sub prefix:<[.[]]> (*$head is copy, *@rest is copy) is primitive {
  while @rest {
    $head = $head[shift @rest];
  }

  $head;
}

sub prefix:«[=>]» (*@args) is primitive {
  # XXX copying necessary because PIL2JS's => currently captures *containers*,
  # not values.
  reduce -> $a, $b {; my $B = $b; my $A = $a; $B => $A } reverse @args;
}

sub prefix:«[=]» (*@vars is copy) is primitive is rw {
  my $dest := pop @vars;
  $_ = $dest for @vars;
  @vars[0];
}

our &prefix:«[,]» := &list;

sub infix:«Y» (Array *@arrays) is primitive is rw { zip *@arrays }
sub infix:«¥» (Array *@arrays) is primitive is rw { zip *@arrays }
