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
    s/(MULTI SUB|MACROP5)\s+(.*?(?:[\]\>]|\w))\s+\((.*?)\)\s+{/gen($1,$2,$3)/ge;
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

sub smart_match {
    my($a,$b)=@_;
    my $ret = attempt_rx_match($a,$b);
    return $ret if defined $ret;
    p6_from_b(p6_to_s($a) eq p6_to_s($b));
}


# a first few - dont add more here?
MULTI SUB pi () {p6_from_n(Math::Trig::pi)};
MULTI SUB say (*@args) {
    p6_new(Int => print( (map{p6_to_s($_)}@args),"\n"));
};

MULTI SUB circumfix:<{}> (*@a) {
    # param is Pair or Array of Pair
    my $h = Hash->new();
    my $p = shift @a;
    if ( $p->isa( 'Pair' ) ) {
        $h->store( $p->key, $p->value );
    }
    elsif ( $p->isa( 'Array' ) ) {
        for ( 0 .. $p->elems->unboxed -1 ) {
            my $pp = $p->fetch($_)->fetch;
            # warn "pp = $pp ". $pp->str->unboxed;
            $h->store( $pp->key, $pp->value );
        }
    }
    else {
        warn "Not implemented - Hash from $p";
    }
    return $h;
};

MULTI SUB circumfix:<[]> (*@a) {
    my $h = Array->new();
    if ( @a ) {
        my $p = shift @a;
        if ( UNIVERSAL::isa( $p, 'Array' ) ) {
            for ( $p->items ) {
                my $pp = $_; # ->clone; XXX
                # warn "pp = $pp ". $pp->str->unboxed;
                $h->push( $pp );
            }
        }
        else {
            $h->push( $p );
        }
    }
    $h = Ref->new( '$.referred' => $h );
    return $h;
};

MULTI SUB coerce:as ($x, $to) { 
    my $tmp;
    my $class = $to->unboxed;
    eval { $tmp = $x->$class };
    if ( $@ ) {
        my $c = lc($class);
        eval { $tmp = $x->$c };
    } 
    if ( $@ ) {
        eval { $tmp = $class->new( '$.unboxed' => $x->unboxed ) };
    } 
    if ( $@ ) {
        no warnings 'numeric';
        return p6_from_b($tmp) if $class eq 'Bit' && ! ref( $tmp );
        warn "can't coerce $x to $class ($@)";
        $tmp = p6_from_s("");
    }
    return $tmp;
};

MULTI SUB infix:<,>  (*@a) {
    # warn "COMMA: @a\n";
    my $a = p6_from_a( @a );
    # warn "elems: ". p6_to_n( $a->elems );
    my $idx = Array->new();
    # TODO - split index into @a.each.elems parts
    if ( p6_to_n( $a->elems ) == &Perl6::Value::Num::Inf ) {
        $idx->push( Perl6::Value::List->from_num_range( start => 0, end => p6_to_n( $a->elems ) - 1 ) )
    }
    else {
        for ( 0 .. p6_to_n( $a->elems ) - 1 ) {
            $idx->push( $_ )
        }
    }
    $a->slice( $idx );
};
# MULTI SUB Array::fetch ($a,$i) {
#    # implements (1,2,3)[1] - see PrimP6.pm - postcircumfix:<[ ]>
#    $a->fetch( $i->unboxed )
#};
MULTI SUB Array::slice ($a,$i) {
    # implements (1,2,3)[1] - see PrimP6.pm - postcircumfix:<[ ]>
    if ( ref($i) eq 'Array' ) {
        # warn "SLICE: ".$i->perl->unboxed;
        $a->slice( $i )
    }
    else {
        $a->fetch( $i->unboxed )
    }
};

# Things which dont appear in Prim.hs
MACROP5   statement_control:<if> ($xx0,$xx1,$xx2) {
    "if (p6_to_b($xx0)) $xx1 else { $xx2 }";
    # XXX - the {} on xx2 are because thunks are being mishandled.
};
MACROP5   statement_control:<unless> ($xx0,$xx1,$xx2) {
    "if (!p6_to_b($xx0)) $xx1 else { $xx2 }";
    # XXX - the {} on xx2 are because thunks are being mishandled.
};
MACROP5   statement_control:<while> ($xx0,$xx1) {
    "while (p6_to_b(p6_apply($xx0))) { p6_apply($xx1) }";
};
MACROP5   Class::_create ($xx0) {""};


# From Prim.hs
# op0
#MULTI SUB XXX:<&> () {...}; - ???
#MULTI SUB XXX:<^> () {...}; - ???
#MULTI SUB XXX:<|> () {...}; - ???
MULTI SUB want () {...};
MULTI SUB bool::true () { p6_from_b(1) };
MULTI SUB bool::false () { p6_from_b(0) };
MULTI SUB time () { use Time::HiRes; p6_from_n(time)};
MULTI SUB times () {p6_from_l(times)};
MULTI SUB so () {...};
#MULTI SUB ¥ () {...}; - need protective unicode mangling/encoding first.
#MULTI SUB Y () {...};
MULTI SUB File::Spec::cwd () {p6_from_s(cwd)};
MULTI SUB File::Spec::tmpdir () {...};
# pi say - placed above, as a temporary dev hack.
MULTI SUB print (*@xxa) {print(map{p6_to_s($_)} @xxa);};
MACROP5   return (*@xxa) {
    "return (".join(",",@xxa).")";
};
MULTI SUB yield () {...};
MULTI SUB take () {...};
# nothing - in PrimP6

# op1
#MULTI SUB prefix:<!> ($xx) {...}; # in PrimP6
# MULTI SUB id ($xx)    { Perl6::Value::identify($xx) };
# MULTI SUB clone ($xx) { $xx->clone };
MULTI SUB chop ($xx)  {...};
MULTI SUB chomp ($xx) {...};
MULTI SUB Str::split (*@xxa) {...};
MULTI SUB lc ($xx)      { p6_from_s( lc( $xx->unboxed ) ) };
MULTI SUB lcfirst ($xx) { p6_from_s( lcfirst( $xx->unboxed ) ) };
MULTI SUB uc ($xx)      { p6_from_s( uc( $xx->unboxed ) )  };
MULTI SUB ucfirst ($xx) {  p6_from_s( ucfirst( $xx->unboxed ) ) };
MULTI SUB capitalize ($xx) {
    my $string = $xx->unboxed;
    # from the Perl FAQ
    $string =~ s/ (
                 (^\w)    #at the beginning of the line
                   |      # or
                 (\s\w)   #preceded by whitespace
                   )
                /\U$1/xg;
    $string =~ s/([\w\']+)/\u\L$1/g;
    p6_from_s($string);
};
MULTI SUB undef ($xx) {p6_undef};
MULTI SUB undefine ($xx) {p6_set($xx,p6_undef)};
#MULTI SUB prefix:<+> ($xx) {...}; # in PrimP6
#MULTI SUB abs ($xx) {...};
MULTI SUB Pugs::Internals::truncate ($xx) {...};
MULTI SUB Pugs::Internals::round ($xx) {...};
MULTI SUB Pugs::Internals::floor ($xx) {...};
MULTI SUB Pugs::Internals::ceiling ($xx) {...};
MULTI SUB cos ($xx)  { p6_from_n( cos( $xx->unboxed ) ) };
MULTI SUB sin ($xx)  { p6_from_n( sin( $xx->unboxed ) ) };
MULTI SUB tan ($xx)  { p6_from_n( tan( $xx->unboxed ) ) };
MULTI SUB sqrt ($xx) { p6_from_n( sqrt( $xx->unboxed ) ) };
MULTI SUB atan (*@xxa) {...};

MULTI SUB postfix:<++> ($xx) { 
    my $old = p6_new(Num => p6_to_n($xx)); # XXX - use clone() - needs MM2
    p6_set($xx,p6_from_n(p6_to_n($xx)+1)); 
    $old };
MULTI SUB prefix:<++> ($xx)  { p6_set($xx,p6_from_n(p6_to_n($xx)+1)) };
MULTI SUB postfix:<--> ($xx) { 
    my $old = p6_new(Num => p6_to_n($xx)); # XXX - use clone() - needs MM2
    p6_set($xx,p6_from_n(p6_to_n($xx)-1)); 
    $old };
MULTI SUB prefix:<--> ($xx)  { p6_set($xx,p6_from_n(p6_to_n($xx)+1)) };

MULTI SUB scalar ($xx) {...};
MULTI SUB sort (*@xxa) {p6_from_l(sort map{p6_to_s($_)} @xxa)};
#MULTI SUB reverse (@xx) { $xx->reverse };
MULTI SUB reverse ($xx) { 
    if ( UNIVERSAL::isa( $xx, 'Array' ) ) {
        my $ret = Array->new();
        $ret->store( $xx );  # unbind slice
        #warn "reversing ".p6_to_s($ret)." -- $ret\n";
        # return $ret->reverse;
        $ret = $ret->reverse;
        #warn "      got ".p6_to_s($ret)." -- $ret\n";
        $ret;
    }
    else {
        my $tmp = Perl6::Value::stringify( $xx );
        $tmp = reverse( $tmp );
        Str->new( '$.unboxed' => $tmp ); 
    }
};
MULTI SUB zip (@x0,*@x1) { 
    # warn "x0 ".ref($x0)." ".$x0->perl->unboxed;
    my $a;
    if ( @x1 == 1 && ref( $x1[0] ) eq 'Array' ) {
        $a = $x1[0];
        # warn ref( $a );
        # warn $a->perl->unboxed;
    }
    else {
        $a = p6_from_a( @x1 );
    }
    # warn ref( $a );
    # warn $a->perl->unboxed;
    my $res = $x0->zip( $a );
    # warn "res ".ref($res)." ".$res->perl->unboxed;
    return $res;
};
MULTI SUB list (*@xx) { p6_from_a( @xx ) };
MULTI SUB pair ($xx0,$xx1) { Pair->new( '$.key' => $xx0, '$.value' => $xx1 ) };
#MULTI SUB prefix:<~> ($xx) {...}; # in PrimP6
#MULTI SUB prefix:<?> ($xx) {...}; # in PrimP6
#MULTI SUB int ($xx) {...}; # in PrimP6
MULTI SUB prefix:<+^> ($xx) {...};
MULTI SUB prefix:<~^> ($xx) {...};
MULTI SUB prefix:<?^> ($xx) {...};
MULTI SUB prefix:<\\> ($xx) { p6_new(Ref => $xx) };
# MULTI SUB postfix:<...> ($xx) {...}; # in PrimP6
# MULTI SUB true ($xx) {...}; # in PrimP6
MULTI SUB any ($xx) {...};
MULTI SUB all ($xx) {...};
MULTI SUB one ($xx) {...};
MULTI SUB none ($xx) {...};
MULTI SUB perl ($xx) { $xx->perl };
MULTI SUB require_haskell ($xx) {...};
MULTI SUB require_parrot ($xx) {...};
MULTI SUB require_perl5 ($xx) { p6_from_x(eval("require ".p6_to_s($xx).";"));};
MULTI SUB Pugs::Internals::eval_parrot ($xx) {...};
MULTI SUB use_avoiding_pugs ($xx) {help_require_use($xx,1)};
MULTI SUB require ($xx) {help_require_use($xx,0)};
sub help_require_use {
    my($xx,$use)=@_;
    use FindBin;
    use File::Spec;
    my $name = p6_to_s($xx);
    if ($name =~ /^v6\b/) {
	return;
    }
    if ($name =~ /^perl5:(.+)/) {
	my $pkg = $1;
	eval "require $pkg;";
	die "require($pkg) - $@\n" if $@;
	if($use) {
	    my $tmp = "Temp".int(rand(10000000));
	    my $code = "package $tmp; use $pkg; \\\%${tmp}::;";
	    print STDERR $code,"\n";
	    my $sym = eval $code; die "bug $@" if $@;
	    for my $key (keys(%$sym)) {
		next if $key =~ /BEGIN/;
		# XXX - *@a doesnt work yet
#		my $sub = "sub $key (*\@a) { PIL::Run::Internals::call_perl5('${tmp}::$key',*\@a) }";
		my $sub = "sub $key (*\@a) { PIL::Run::Internals::call_perl5('${tmp}::$key',\@a) }";
	    print STDERR $sub,"\n";
		PIL::Run::EvalX::p6_eval($sub);
	    }
	}
	return;
    }
    $name = $name.".pm" if $name !~ /\.pm/; # help out use();
    my $fn = File::Spec->catfile(split(/::/,$name));
    my @candidates;
    push(@candidates,
         $fn,
         File::Spec->catfile('lib6',$fn),
         File::Spec->catfile($FindBin::Bin,'lib6',$fn));
    for my $f (@candidates) {
        next if !-e $f;
        return PIL::Run::EvalX::p6_eval_file($f);
    }
    die "require($name) - file not found";
}
MULTI SUB Pugs::Internals::eval ($xx) {PIL::Run::EvalX::p6_eval(p6_to_s($xx))};
MULTI SUB evalfile ($xx) {PIL::Run::EvalX::p6_eval_file(p6_to_s($xx))};
MULTI SUB Pugs::Internals::eval_perl5 ($xx) {p6_from_x(eval(p6_to_s($xx)))};
MULTI SUB Pugs::Internals::eval_haskell ($xx) {...};
MULTI SUB Pugs::Internals::eval_yaml ($xx) {...};
#MULTI SUB PIL::Run::Internals::call_perl5 ($xx0,*@xxa) { # XXX - see help_require_use() above
MULTI SUB PIL::Run::Internals::call_perl5 ($xx0,@xxa) {
    p6_from_x(p6_to_s($xx0)->(map{p6_to_s($_)}@$xxa)); #XXX p6_to_x
}; 
MULTI SUB try ($xx) {
    use Error qw(:try);
    try { p6_apply($xx); } otherwise { p6_set(p6_var('$!',2),$_[0]); };
};
{ package Error::Simple;
  use PIL::Run::ApiX;
  sub bit {my $e=shift; $e ? p6_from_b(1) : p6_from_b(0); }
}
MULTI SUB lazy ($xx) {...};
MULTI SUB defined ($xx) { $xx->defined };
MACROP5   last (*@xx) {"last";};# XXX - ?$xx
MACROP5   next (*@xx) {"next";};
MULTI SUB redo ($xx) {...};
# return - see op0
# yield - see op0
# take - see op0
# MULTI SUB sign ($xx) {...};
MULTI SUB rand ($xx) { p6_from_n(rand(defined $xx ? p6_to_n($xx) : 1)) };
# say - see op0
# print - see op0
MULTI SUB IO::say (*@xxa) {...};
MULTI SUB IO::print (*@xxa) {...};
MULTI SUB IO::next ($xx) {...};
MULTI SUB Pugs::Safe::safe_print ($xx) {...};
MULTI SUB die (*@xxa) { die map{p6_to_s($_)}@xxa; };
MULTI SUB warn ($xx) { 
    # TODO - add line number, if string doesn't terminate in "\n"
    warn $xx->unboxed . " at ...\n";
};
MULTI SUB fail_ ($xx) {...};
MULTI SUB exit ($xx) {...};
MULTI SUB readlink ($xx) {...};
MULTI SUB sleep ($xx) { sleep ( p6_to_n($xx) ) };
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
MULTI SUB end ($xx) { $xx->end };
MULTI SUB elems ($xx) { $xx->elems };
MULTI SUB graphs ($xx) {...};
MULTI SUB codes ($xx) {...};
MULTI SUB chars ($xx) {...};
MULTI SUB bytes ($xx) {};
MULTI SUB unlink ($xx) {unlink(p6_to_s($xx0))};
MULTI SUB readdir ($xx) {...};
MULTI SUB slurp ($xx) {...};
MULTI SUB opendir ($xx) {...};
MULTI SUB IO::Dir::closedir ($xx) {...};
MULTI SUB IO::Dir::rewinddir ($xx) {...};
MULTI SUB IO::Dir::readdir ($xx) {...};
MULTI SUB Pugs::Internals::runInteractiveCommand ($xx) {...};
MULTI SUB Pugs::Internals::check_for_io_leak ($xx) {...};
MULTI SUB system (*@xx) {
    #for ( $PIL::Run::Root::main::hash_ENV->keys->items ) {
    #    $ENV{ Perl6::Value::stringify( $_ ) } = 
    #        Perl6::Value::stringify( $PIL::Run::Root::main::hash_ENV->fetch( $_ ) ) ;
    #}
    @xx = map{ Perl6::Value::stringify( $_ ) } @xx;
    p6_from_n( system @xx );
};
MULTI SUB accept ($xx) {...};
MULTI SUB detach ($xx) {...};
MULTI SUB kill (*@xxa) {...};
MULTI SUB join (*@xxa) {...};
MULTI SUB async ($xx) {...};
MULTI SUB listen ($xx) {...};
MULTI SUB flush ($xx) {...};
MULTI SUB close ($xx) {...};
MULTI SUB key ($xx) { $xx->key };
MULTI SUB value ($xx) { $xx->value };
MULTI SUB pairs ($xx) { $xx->pairs };
MULTI SUB List::kv ($xx) { $xx->kv }; 
MULTI SUB Pair::kv ($xx) { p6_from_a($xx->key, $xx->value) };
MULTI SUB keys ($xx) { $xx->keys };
MULTI SUB values ($xx) { $xx->values };
MULTI SUB prefix:<=> ($xx) {...};
MULTI SUB readline ($xx) {...};
MULTI SUB getc ($xx) {...};
MULTI SUB ref ($xx) { $xx->ref }; 
MULTI SUB pop ($xx) { $xx->pop };
MULTI SUB shift ($xx) { $xx->shift };
#MULTI SUB pick ($xx) {...};
#MULTI SUB sum ($xx) {...};
MULTI SUB min ($xx) {...};
MULTI SUB max ($xx) {...};
#MULTI SUB uniq ($xx) {...};
MULTI SUB chr ($xx) { p6_from_s( chr( p6_to_n( $xx ) ) ) };
MULTI SUB ord ($xx) { p6_from_n( ord( p6_to_s( $xx ) ) ) };
MULTI SUB hex ($xx) { p6_from_n( eval '0x' . $xx->unboxed ) };
MULTI SUB log ($xx)   { p6_from_n( log( p6_to_s( $xx ) ) ) };
MULTI SUB log10 ($xx) { p6_from_n( log( p6_to_s( $xx ) ) / log(10) ) };
#MULTI SUB from ($xx) {...}; - implemented as methods.
#MULTI SUB to ($xx) {...};
MULTI SUB matches ($xx) {...};
MULTI SUB gather ($xx) {...};
MULTI SUB Thread::yield ($xx) {...};
MULTI SUB DESTROYALL ($xx) {...};
# MULTI SUB prefix:<,> (*@a) {@a}; # ??? - unneeded?
MULTI SUB Code::assoc ($xx) {...};
#MULTI SUB Code::name ($xx) {...};
#MULTI SUB Code::arity ($xx) {...};
#MULTI SUB Code::body ($xx) {...};
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
MULTI SUB infix:<*> ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) * p6_to_n($xx1)) };
MULTI SUB infix:</> ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) / p6_to_n($xx1)) };
MULTI SUB infix:<%> ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) % p6_to_n($xx1)) };
MULTI SUB infix:<x> ($xx0,$xx1) {
    p6_from_s( Perl6::Value::stringify($xx0) x  $xx1->unboxed ) 
};
MULTI SUB infix:<xx> ($xx0,$xx1) {
    p6_from_a( Perl6::Value::List->from_x( item => $xx0, count => $xx1->unboxed ) ) 
};
MULTI SUB infix:<+&> ($xx0,$xx1) {...};
MULTI SUB infix:[+<] ($xx0,$xx1) {...};
MULTI SUB infix:[+>] ($xx0,$xx1) {...};
MULTI SUB infix:<~&> ($xx0,$xx1) {...};
MULTI SUB infix:[~<] ($xx0,$xx1) {...};
MULTI SUB infix:[~>] ($xx0,$xx1) {...};
MULTI SUB infix:<**> ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) ** p6_to_n($xx1)) };
MULTI SUB infix:<+>  ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) + p6_to_n($xx1)) };
MULTI SUB infix:<->  ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) - p6_to_n($xx1)) };
# atan - see op1
MULTI SUB infix:<~>  ($xx0,$xx1) { p6_from_s(p6_to_s($xx0) . p6_to_s($xx1)) };
MULTI SUB infix:<+|> ($xx0,$xx1) {...};
MULTI SUB infix:<+^> ($xx0,$xx1) {...};
MULTI SUB infix:<~|> ($xx0,$xx1) {...};
MULTI SUB infix:<?|> ($xx0,$xx1) {...};
MULTI SUB infix:<~^> ($xx0,$xx1) {...};
MULTI SUB infix:[=>] ($xx0,$xx1) { Pair->new( '$.key' => $xx0, '$.value' => $xx1 ) };
MULTI SUB infix:<=>  ($xx0,$xx1) {...};
MULTI SUB infix:<cmp> ($xx0,$xx1) { p6_from_n(p6_to_s($xx0) cmp p6_to_s($xx1)) };
MULTI SUB infix:[<=>] ($xx0,$xx1) { p6_from_n(p6_to_n($xx0) <=> p6_to_n($xx1)) };
MULTI SUB infix:<..> ($xx0,$xx1) { 
    my $n = Perl6::Value::numify( $xx0 );
    if ( $n eq $xx0->unboxed ) {
        return p6_from_a(
            Perl6::Value::List->from_num_range( start => $xx0->unboxed, end => $xx1->unboxed )
        )         
    }
    return p6_from_a(
        Perl6::Value::List->from_range( start => $xx0->unboxed, end => $xx1->unboxed )
    )         
};
#MULTI SUB infix:<..^> ($xx0,$xx1) {...}; # in PrimP6
#MULTI SUB infix:<^..> ($xx0,$xx1) {...}; # in PrimP6
#MULTI SUB infix:<^..^> ($xx0,$xx1) {...}; # in PrimP6
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
MULTI SUB infix:<~~> ($xx0,$xx1) { smart_match($xx0,$xx1) };
MULTI SUB infix:<!~> ($xx0,$xx1) { p6_from_b(!p6_to_b(smart_match($xx0,$xx1))) };
MULTI SUB infix:<=:=> ($xx0,$xx1) { Perl6::Value::identify($xx0) eq Perl6::Value::identify($xx1) };
MACROP5   infix:<&&> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? ('.$xx1.') : $_v1 }' };
MACROP5   infix:<||> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? $_v1 : ('.$xx1.') }' };
MACROP5   infix:<^^> ($xx0,$xx1) {...};
MACROP5   infix:<//> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; defined($_v1) ? $_v1 : ('.$xx1.') }' };
MACROP5   infix:<!!> ($xx0,$xx1) {...};
MULTI SUB infix:<.[]> ($xx0,$xx1) {...};
MULTI SUB infix:<.{}> ($xx0,$xx1) {...};
MACROP5 infix:<and> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? ('.$xx1.') : $_v1 }' };
MACROP5 infix:<or> ($xx0,$xx1) { 'do{my $_v1 = '.$xx0.'; p6_to_b($_v1) ? $_v1 : ('.$xx1.') }' };
MULTI SUB infix:<xor> ($xx0,$xx1) {...};
MULTI SUB infix:<err> ($xx0,$xx1) {...};
MULTI SUB infix:<nor> ($xx0,$xx1) {...};
#MULTI SUB grep ($xx0,$xx1) {...};
#MULTI SUB map ($xx0,$xx1) {...};
# join - see op1
MULTI SUB reduce ($xx0,$xx1) {...};
# kill - see op1
MULTI SUB does ($xx0,$xx1) { p6_from_b($xx0->does( $xx1->unboxed )) };
MULTI SUB isa  ($xx0,$xx1) { p6_from_b($xx0->isa( $xx1->unboxed )) };
#MULTI SUB delete ($xx0,$xx1) {...}; -- implemented in Array, Hash
#MULTI SUB exists ($xx0,$xx1) {...};
MULTI SUB unshift (@xx0,*@xxa) { $xx0->unshift( @xxa ) };
MULTI SUB push (@xx0,*@xxa) { $xx0->push( @xxa ) };
MULTI SUB split (*@xxa) {
    my($splitstr,$str) = map{p6_to_s($_)} @xxa;
    p6_from_l(split(/$splitstr/,$str));
};
# Str::split - see op1
MULTI SUB connect ($xx0,$xx1) {...};
MULTI SUB Pugs::Internals::hSetBinaryMode ($xx0,$xx1) {...};
MULTI SUB Pugs::Internals::openFile ($xx0,$xx1) {...};
MULTI SUB exp ($xx0,$xx1) {p6_from_n(p6_to_n($xx0)**p6_to_n($xx1))};
MULTI SUB sprintf ($xx0,*@xxa) {
    p6_from_s(sprintf(p6_to_s($xx0), map{p6_to_s($_)} @xxa));
};
MULTI SUB exec (*@xx) {
    #for ( $PIL::Run::Root::main::hash_ENV->keys->items ) {
    #    # warn "SETENV $_ = ".Perl6::Value::stringify( $PIL::Run::Root::main::hash_ENV->fetch( $_ ))."\n";
    #    $ENV{ Perl6::Value::stringify( $_ ) } = 
    #        Perl6::Value::stringify( $PIL::Run::Root::main::hash_ENV->fetch( $_ ) ) ;
    #}
    @xx = map{ Perl6::Value::stringify( $_ ) } @xx;
    # warn "EXEC @xx";
    exec @xx;
};
# system - see op1
MULTI SUB chmod ($xx0,@xxa) {chmod(p6_to_n($xx0),map{p6_to_s($_)}@xxa)};
MULTI SUB splice ($xx0,*@xxa) { 
    for (0,1) {
        $xxa[$_] = Perl6::Value::numify( $xxa[$_] ) if defined $xxa[$_]; 
    }
    $xx0->splice( @xxa );
};
# sort - see op1
# IO::say - see op1
# IO::print - see op1
MULTI SUB BUILDALL ($xx0,$xx1) {...};

# op3
MULTI SUB Pugs::Internals::caller ($xx0,$xx1,$xx2) {...};
MULTI SUB index ($xx0,$xx1,$xx2) {
    my($v0,$v1,$v2)=(p6_to_s($xx0),p6_to_s($xx1),undef);
    $v2 = p6_to_n($xx2) if p6_defined($xx2);
    (defined $v2
     ? p6_from_n(index($v0,$v1,$v2))
     : p6_from_n(index($v0,$v1)));
};
MULTI SUB rindex ($xx0,$xx1,$xx2) {
    my($v0,$v1,$v2)=(p6_to_s($xx0),p6_to_s($xx1),undef);
    $v2 = p6_to_n($xx2) if p6_defined($xx2);
    (defined $v2
     ? p6_from_n(rindex($v0,$v1,$v2))
     : p6_from_n(rindex($v0,$v1)));
};
# splice - see op2
# split - see op2
# Str::split - see op1
MULTI SUB Any::new ($xx0,$xx1,$xx2) {...};
MULTI SUB Pugs::Internals::localtime ($xx0,$xx1,$xx2) {...};
MULTI SUB Pugs::Internals::hSeek ($xx0,$xx1,$xx2) {...};

# op4
MULTI SUB substr ($xx0,$xx1,*@xxa) {
    my($s,$v1) = (p6_to_s($xx0),p6_to_n($xx1));
    my($v2,$v3)= map { defined $_ ? p6_to_n($_) : undef } @xxa;
    p6_from_s(substr($s,$v1,$v2)); # XXX - doesnt handle replacement.
};
# splice - see op2



sub rx_common {
    my($mods6,$pat6,$qo6,$qc6)=@_;
    my $pat = p6_to_s($pat6);
    my $qo = defined $qo ? p6_to_s($qo6) : '/';
    my $qc = defined $qc ? p6_to_s($qc6) : '/';
    my $m = "";
    $m = join("",$mods->keys) if defined $mods;
    eval "qr$qo$pat$qc$m";
}
MULTI SUB rxbare_ ($pat) { rx_common(undef,$pat,undef,undef); };
MULTI SUB rx_ ($mods,$pat,$qo,$qc) { rx_common($mods,$pat,$qo,$qc) };
MULTI SUB m_ ($mods,$pat,$qo,$qc) { rx_common($mods,$pat,$qo,$qc) };

sub attempt_rx_match {
    my($a,$b)=@_;
    my $ret;
    eval {
	my $m;
	my $as = p6_to_s($a);
	if ($as =~ $b) {
	    my $i = 0;
	    my @cap;
	    for(my $i=1;$i < @+; $i++) {
		my $ci = $$i;
		push(@cap,
		     Match->new(defined $ci ? 1 : 0,
				defined $ci ? $ci : "",
				[],{},@-[$i],@+[$i]));
	    }
	    $m = Match->new(1,"$&",[@cap],{});
	} else {
	    $m = Match->new_failed();
	}
	p6_set(p6_var('$/',2),$m);
	$ret = $m;
    };
    return $ret;
}

package Match;
use overload
    'bool' => 'as_bool',
    '""'   => 'as_string',
    '@{}'  => 'as_array',
    '%{}'  => 'as_hash',
    fallback => 1
    ;

sub as_bool   {my($o)=@_;$$o->{'val_bool'}}
sub as_string {my($o)=@_;$$o->{'val_string'}}
sub as_array  {my($o)=@_;$$o->{'val_array'}}
sub as_hash   {my($o)=@_;$$o->{'val_hash'}}

sub from {my($o)=@_;$$o->{'from'}}
sub to   {my($o)=@_;$$o->{'to'}}

sub new {
    my($cls,$b,$s,$a,$h,$from,$to)=@_;
    my $h = {};
    my $o = (bless \$h,$cls)->set($b,$s,$a,$h,$from,$to);
    return $o;
}
sub new_failed {
    my($cls)=@_;
    $cls->new()->set_as_failed();
}

sub init {
    my($o)=@_;
    $o->set(1,"",[],{});
    return $o;
}
sub set {
    my($o,$b,$s,$a,$h,$from,$to)=@_;
    $$o->{'val_bool'}   = $b;
    $$o->{'val_string'} = $s;
    $$o->{'val_array'}  = $a;
    $$o->{'val_hash'}   = $h;
    $$o->{'from'}  = $from;
    $$o->{'to'}    = $to;
    return $o;
}
sub set_as_failed {
    my($o)=@_;
    $o->set(0,"",[],{});
    return $o;
}
sub describe {
    my($o)=@_;
    my $s = overload::StrVal($o)."<".($o?"1":"0").",\"$o\",[";
    for (@{$o}) { $s .= "\n".$o->_indent($_->describe())."," }
    $s .= "\n " if @{$o};
    $s .= "],{";
    for (keys(%{$o})) {
        $s .= "\n$_ => " .$o->_indent_except_top($o->describe())."," }
    $s .= "\n " if %{$o};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to>";
    return $s;
}
sub _indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
sub _indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}

1;
__END__
