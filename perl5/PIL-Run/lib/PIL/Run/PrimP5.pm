#!/usr/bin/perl -w

=kwid

Note that an alternative to writing primitives in p5 is to write them
in p6 and add them to Prelude.

=cut


package PrimFilter;
sub gen {
    my($what,$name,$args)=@_;
    $what =~ tr/A-Z/a-z/;
    my $args5 = $args;
    my $args6 = $args;
    $args5 =~ s/(?<!\*)\@/\$/g; $args5 =~ s/(?<!\*)\%/\$/g;
    $args5 =~ s/\*\@/\@/g; $args5 =~ s/\*\%/\%/g;
    $args6 =~ s/\A\s+//; # for split.
    $args6 = join(",",map{"'$_'"} split(/\s*,\s*/,$args6));
    my $my_args = $args =~ /\A\s*\Z/ ? "" : "my($args5)=\@_; ";
    "def '$what','$name', [$args6], sub {my \$_fn ='$name'; $my_args";
}

use Filter::Simple sub {
    s/\#.+//g;
    s/{\.\.\.}/{p6_die("\$_fn: unimplemented");}/g;
    s/(MULTI SUB|MACRO)\s+(\S+)\s+\((.*?)\)\s+{/gen($1,$2,$3)/ge;
    #print; #print STDERR;
    $_;
};
# BEGIN { FILTER_ONLY code => sub {} }; also works?

package PrimP5;
BEGIN {PrimFilter::import};
use Math::Trig;
use PIL::Run::ApiX;
sub def {
    my($what,$name,$argl,$f)=@_;
    PIL::Run::ApiX::def_prim($what,$name,$argl,$f);
};

# a first few - dont add more here?
MULTI SUB pi () {p6_from_n(Math::Trig::pi)};
MULTI SUB say (*@args) {
    p6_new(Int => print( (map{p6_to_s($_)}@args),"\n"));
};
# MULTI SUB prefix:<,> (*@a) {@a};
MULTI SUB infix:<,>  (*@a) { p6_from_a(@a) };

# Things which dont appear in Prim.hs
MACRO     statement_control:<if> ($xx0,$xx1,$xx2) {
    "if (p6_to_b($xx0)) $xx1 else $xx2";
};

# From Prim.hs
# op0
#MULTI SUB XXX:<&> () {...}; - ???
#MULTI SUB XXX:<^> () {...}; - ???
#MULTI SUB XXX:<|> () {...}; - ???
MULTI SUB want () {...};
MULTI SUB bool::true () {...};
MULTI SUB bool::false () {...};
MULTI SUB time () {...};
MULTI SUB times () {...};
MULTI SUB so () {...};
#MULTI SUB ¥ () {...}; - need protective unicode mangling/encoding first.
MULTI SUB Y () {...};
MULTI SUB File::Spec::cwd () {...};
MULTI SUB File::Spec::tmpdir () {...};
# pi say - placed above, as a temporary dev hack.
MULTI SUB print () {...};
MULTI SUB return () {...};
MULTI SUB yield () {...};
MULTI SUB take () {...};
# nothing - in PrimP6

# op1
MULTI SUB prefix:<!> ($xx) {...};
MULTI SUB id ($xx) {...};
MULTI SUB clone ($xx) {...};
MULTI SUB chop ($xx) {...};
MULTI SUB chomp ($xx) {...};
MULTI SUB Str::split (*@xxa) {...};
MULTI SUB lc ($xx) {...};
MULTI SUB lcfirst ($xx) {...};
MULTI SUB uc ($xx) {...};
MULTI SUB ucfirst ($xx) {...};
MULTI SUB capitalize ($xx) {...};
MULTI SUB undef ($xx) {...};
MULTI SUB undefine ($xx) {...};
MULTI SUB prefix:<+> ($xx) {...};
MULTI SUB abs ($xx) {...};
MULTI SUB Pugs::Internals::truncate ($xx) {...};
MULTI SUB Pugs::Internals::round ($xx) {...};
MULTI SUB Pugs::Internals::floor ($xx) {...};
MULTI SUB Pugs::Internals::ceiling ($xx) {...};
MULTI SUB cos ($xx) {...};
MULTI SUB sin ($xx) {...};
MULTI SUB tan ($xx) {...};
MULTI SUB sqrt ($xx) {...};
MULTI SUB atan (*@xxa) {...};
MULTI SUB postfix:<++> ($xx) { p6_set($xx,p6_from_n(p6_to_n($xx)+1)) };
MULTI SUB prefix:<++> ($xx) { p6_set($xx,p6_from_n(p6_to_n($xx)+1)) };
MULTI SUB postfix:<--> ($xx) {...};
MULTI SUB prefix:<--> ($xx) {...};
MULTI SUB prefix:<-> ($xx) {...};
MULTI SUB scalar ($xx) {...};
MULTI SUB sort (*@xxa) {...};
MULTI SUB reverse ($xx) {...};
MULTI SUB list ($xx) {...};
MULTI SUB pair ($xx) {...};
MULTI SUB prefix:<~> ($xx) {...};
MULTI SUB prefix:<?> ($xx) {...};
MULTI SUB int ($xx) {...};
MULTI SUB prefix:<+^> ($xx) {...};
MULTI SUB prefix:<~^> ($xx) {...};
MULTI SUB prefix:<?^> ($xx) {...};
MULTI SUB prefix:<\\> ($xx) {...};
MULTI SUB postfix:<...> ($xx) {...};
MULTI SUB true ($xx) {...};
MULTI SUB any ($xx) {...};
MULTI SUB all ($xx) {...};
MULTI SUB one ($xx) {...};
MULTI SUB none ($xx) {...};
MULTI SUB perl ($xx) {...};
MULTI SUB require_haskell ($xx) {...};
MULTI SUB require_parrot ($xx) {...};
MULTI SUB require_perl5 ($xx) {...};
MULTI SUB Pugs::Internals::eval_parrot ($xx) {...};
MULTI SUB use ($xx) {...};
MULTI SUB require ($xx) {...};
MULTI SUB Pugs::Internals::eval ($xx) {...};
MULTI SUB evalfile ($xx) {...};
MULTI SUB Pugs::Internals::eval_perl5 ($xx) {...};
MULTI SUB Pugs::Internals::eval_haskell ($xx) {...};
MULTI SUB Pugs::Internals::eval_yaml ($xx) {...};
MULTI SUB try ($xx) {...};
MULTI SUB lazy ($xx) {...};
MULTI SUB defined ($xx) {...};
MULTI SUB last ($xx) {...};
MULTI SUB next ($xx) {...};
MULTI SUB redo ($xx) {...};
# return - see op0
# yield - see op0
# take - see op0
MULTI SUB sign ($xx) {...};
MULTI SUB rand ($xx) {...};
# say - see op0
# print - see op0
MULTI SUB IO::say (*@xxa) {...};
MULTI SUB IO::print (*@xxa) {...};
MULTI SUB IO::next ($xx) {...};
MULTI SUB Pugs::Safe::safe_print ($xx) {...};
MULTI SUB die ($xx) {...};
MULTI SUB warn ($xx) {...};
MULTI SUB fail_ ($xx) {...};
MULTI SUB exit ($xx) {...};
MULTI SUB readlink ($xx) {...};
MULTI SUB sleep ($xx) {...};
MULTI SUB mkdir ($xx) {...};
MULTI SUB rmdir ($xx) {...};
MULTI SUB chdir ($xx) {...};
MULTI SUB prefix:<-r> ($xx) {...};
MULTI SUB prefix:<-w> ($xx) {...};
MULTI SUB prefix:<-x> ($xx) {...};
MULTI SUB prefix:<-e> ($xx) {...};
MULTI SUB prefix:<-z> ($xx) {...};
MULTI SUB prefix:<-s> ($xx) {...};
MULTI SUB prefix:<-f> ($xx) {...};
MULTI SUB prefix:<-d> ($xx) {...};
MULTI SUB end ($xx) {...};
MULTI SUB elems ($xx) {...};
MULTI SUB graphs ($xx) {...};
MULTI SUB codes ($xx) {...};
MULTI SUB chars ($xx) {...};
MULTI SUB bytes ($xx) {...};
MULTI SUB unlink ($xx) {...};
MULTI SUB readdir ($xx) {...};
MULTI SUB slurp ($xx) {...};
MULTI SUB opendir ($xx) {...};
MULTI SUB IO::Dir::closedir ($xx) {...};
MULTI SUB IO::Dir::rewinddir ($xx) {...};
MULTI SUB IO::Dir::readdir ($xx) {...};
MULTI SUB Pugs::Internals::runInteractiveCommand ($xx) {...};
MULTI SUB Pugs::Internals::check_for_io_leak ($xx) {...};
MULTI SUB system (*@xxa) {...};
MULTI SUB accept ($xx) {...};
MULTI SUB detach ($xx) {...};
MULTI SUB kill (*@xxa) {...};
MULTI SUB join (*@xxa) {...};
MULTI SUB async ($xx) {...};
MULTI SUB listen ($xx) {...};
MULTI SUB flush ($xx) {...};
MULTI SUB close ($xx) {...};
MULTI SUB key ($xx) {...};
MULTI SUB value ($xx) {...};
MULTI SUB pairs ($xx) {...};
MULTI SUB List::kv ($xx) {...}; 
MULTI SUB Pair::kv ($xx) {...};
MULTI SUB keys ($xx) {...};
MULTI SUB values ($xx) {...};
MULTI SUB prefix:<=> ($xx) {...};
MULTI SUB readline ($xx) {...};
MULTI SUB getc ($xx) {...};
MULTI SUB ref ($xx) {...};
MULTI SUB pop ($xx) {...};
MULTI SUB shift ($xx) { $xx->shift };
MULTI SUB pick ($xx) {...};
MULTI SUB sum ($xx) {...};
MULTI SUB min ($xx) {...};
MULTI SUB max ($xx) {...};
MULTI SUB uniq ($xx) {...};
MULTI SUB chr ($xx) {...};
MULTI SUB ord ($xx) {...};
MULTI SUB hex ($xx) {...};
MULTI SUB log ($xx) {...};
MULTI SUB log10 ($xx) {...};
MULTI SUB from ($xx) {...};
MULTI SUB to ($xx) {...};
MULTI SUB matches ($xx) {...};
MULTI SUB gather ($xx) {...};
MULTI SUB Thread::yield ($xx) {...};
MULTI SUB DESTROYALL ($xx) {...};
# prefix:<,> - see above.
MULTI SUB Code::assoc ($xx) {...};
MULTI SUB Code::name ($xx) {...};
MULTI SUB Code::arity ($xx) {...};
MULTI SUB Code::body ($xx) {...};
MULTI SUB Code::pos ($xx) {...};
MULTI SUB IO::tell ($xx) {...};
MULTI SUB Pugs::Internals::hIsOpen ($xx) {...};
MULTI SUB Pugs::Internals::hIsClosed ($xx) {...};
MULTI SUB Pugs::Internals::hIsReadable ($xx) {...};
MULTI SUB Pugs::Internals::hIsWritable ($xx) {...};
MULTI SUB Pugs::Internals::hIsSeekable ($xx) {...};

# op2
MULTI SUB rename ($xx0,$xx1) {...};
MULTI SUB symlink ($xx0,$xx1) {...};
MULTI SUB link ($xx0,$xx1) {...};
MULTI SUB infix:<*> ($xx0,$xx1) {...};
MULTI SUB infix:</> ($xx0,$xx1) {...};
MULTI SUB infix:<%> ($xx0,$xx1) {...};
MULTI SUB x ($xx0,$xx1) {...};
MULTI SUB xx ($xx0,$xx1) {...};
MULTI SUB infix:<+&> ($xx0,$xx1) {...};
MULTI SUB infix:[+<] ($xx0,$xx1) {...};
MULTI SUB infix:[+>] ($xx0,$xx1) {...};
MULTI SUB infix:<~&> ($xx0,$xx1) {...};
MULTI SUB infix:[~<] ($xx0,$xx1) {...};
MULTI SUB infix:[~>] ($xx0,$xx1) {...};
MULTI SUB infix:<**> ($xx0,$xx1) {...};
MULTI SUB infix:<+> ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) + p6_to_n($xx1)) };
MULTI SUB infix:<-> ($xx0,$xx1) {...};
# atan - see op1
MULTI SUB infix:<~> ($xx0,$xx1) { p6_from_s(p6_to_s($xx0) . p6_to_s($xx1)) };
MULTI SUB infix:<+|> ($xx0,$xx1) {...};
MULTI SUB infix:<+^> ($xx0,$xx1) {...};
MULTI SUB infix:<~|> ($xx0,$xx1) {...};
MULTI SUB infix:<?|> ($xx0,$xx1) {...};
MULTI SUB infix:<~^> ($xx0,$xx1) {...};
MULTI SUB infix:[=>] ($xx0,$xx1) {...};
MULTI SUB infix:<=> ($xx0,$xx1) {...};
MULTI SUB infix:<cmp> ($xx0,$xx1) { p6_from_n(p6_to_s($xx0) cmp p6_to_s($xx1)) };
MULTI SUB infix:[<=>] ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) <=> p6_to_n($xx1)) };
MULTI SUB infix:<..> ($xx0,$xx1) { 
    p6_from_a(
        p6_from_l( 
            Perl6::Value::List->from_num_range( start => $xx0->unboxed, end => $xx1->unboxed, step => 1 ) 
        ) 
    ) 
};
MULTI SUB infix:<..^> ($xx0,$xx1) {...};
MULTI SUB infix:<^..> ($xx0,$xx1) {...};
MULTI SUB infix:<^..^> ($xx0,$xx1) {...};
MULTI SUB infix:<!=> ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) != p6_to_n($xx1)) };
MULTI SUB infix:<==> ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) == p6_to_n($xx1)) };
MULTI SUB infix:[<=] ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) <= p6_to_n($xx1)) };
MULTI SUB infix:[>=] ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) >= p6_to_n($xx1)) };
MULTI SUB infix:[<] ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) < p6_to_n($xx1)) };
MULTI SUB infix:[>] ($xx0,$xx1) { p6_from_b(p6_to_n($xx0) > p6_to_n($xx1)) };
MULTI SUB infix:<ne> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) ne p6_to_s($xx1)) };
MULTI SUB infix:<eq> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) eq p6_to_s($xx1)) };
MULTI SUB infix:<lt> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) lt p6_to_s($xx1)) };
MULTI SUB infix:<le> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) le p6_to_s($xx1)) };
MULTI SUB infix:<gt> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) gt p6_to_s($xx1)) };
MULTI SUB infix:<ge> ($xx0,$xx1) { p6_from_b(p6_to_s($xx0) ge p6_to_s($xx1)) };
MULTI SUB infix:<~~> ($xx0,$xx1) {...};
MULTI SUB infix:<!~> ($xx0,$xx1) {...};
MULTI SUB infix:<=:=> ($xx0,$xx1) {...};
MACRO     infix:<&&> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? ('.$xx1.') : $_v1 }' };
MACRO     infix:<||> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? $_v1 : ('.$xx1.') }' };
MACRO     infix:<^^> ($xx0,$xx1) {...};
MACRO     infix:<//> ($xx0,$xx1) {...};
MACRO     infix:<!!> ($xx0,$xx1) {...};
MULTI SUB infix:<.[]> ($xx0,$xx1) {...};
MULTI SUB infix:<.{}> ($xx0,$xx1) {...};
MULTI SUB infix:<and> ($xx0,$xx1) {...};
MULTI SUB infix:<or> ($xx0,$xx1) {...};
MULTI SUB infix:<xor> ($xx0,$xx1) {...};
MULTI SUB infix:<err> ($xx0,$xx1) {...};
MULTI SUB infix:<nor> ($xx0,$xx1) {...};
MULTI SUB grep ($xx0,$xx1) {...};
MULTI SUB map ($xx0,$xx1) {...};
# join - see op1
MULTI SUB reduce ($xx0,$xx1) {...};
# kill - see op1
MULTI SUB does ($xx0,$xx1) {...};
MULTI SUB isa ($xx0,$xx1) {...};
MULTI SUB delete ($xx0,$xx1) {...};
MULTI SUB exists ($xx0,$xx1) {...};
MULTI SUB unshift ($xx0,$xx1) {...};
MULTI SUB push ($xx0,$xx1) {...};
MULTI SUB split (*@xxa) {...};
# Str::split - see op1
MULTI SUB connect ($xx0,$xx1) {...};
MULTI SUB Pugs::Internals::hSetBinaryMode ($xx0,$xx1) {...};
MULTI SUB Pugs::Internals::openFile ($xx0,$xx1) {...};
MULTI SUB exp ($xx0,$xx1) {...};
MULTI SUB Pugs::Internals::sprintf ($xx0,$xx1) {...};
MULTI SUB exec ($xx0,$xx1) {...};
# system - see op1
MULTI SUB chmod ($xx0,$xx1) {...};
MULTI SUB splice (*@xxa) {...};
# sort - see op1
# IO::say - see op1
# IO::print - see op1
MULTI SUB BUILDALL ($xx0,$xx1) {...};

# op3
MULTI SUB Pugs::Internals::caller ($xx0,$xx1,$xx2) {...};
MULTI SUB index ($xx0,$xx1,$xx2) {...};
MULTI SUB rindex ($xx0,$xx1,$xx2) {...};
# splice - see op2
# split - see op2
# Str::split - see op1
MULTI SUB Any::new ($xx0,$xx1,$xx2) {...};
MULTI SUB Pugs::Internals::localtime ($xx0,$xx1,$xx2) {...};
MULTI SUB Pugs::Internals::hSeek ($xx0,$xx1,$xx2) {...};

# op4
MULTI SUB substr ($xx0,$xx1,$xx2,$xx3) {...};
# splice - see op2

1;
__END__
