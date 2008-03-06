#!/usr/bin/perl -w
use strict; no strict 'subs';
use warnings;
sub _ ($$);
=begin

This is a deconstruction of src/perl6/STD.pm,
targeted at parser implementers.

It is intended to
 help clarify STD.pm,
 help clarify what implementers need to do,
 support the metaprograming of maintainable Perl 6 parsers.

=cut

=begin

 Categories (of tokens and rules)

=cut

my @categories=eval('qw{'.remove_comments(<<'END').'}');
  category
  special_variable
  version module_name

  terminator

  sigil twigil

  term

  quote quote_mod q_backslash qq_backslash

  dotty

  infix prefix postfix circumfix postcircumfix

  infix_prefix_meta_operator
  infix_postfix_meta_operator
  infix_circumfix_meta_operator
  prefix_postfix_meta_operator
  prefix_circumfix_meta_operator
  postfix_prefix_meta_operator

  trait_verb
  trait_auxiliary

  type_declarator
  scope_declarator
  package_declarator
  routine_declarator
  plurality_declarator

  statement_prefix
  statement_control
  statement_mod_cond
  statement_mod_loop

  regex_assertion
  regex_backslash
  regex_declarator
  regex_metachar
  regex_mod_internal
  regex_quantifier
END

=begin

Categories - which are rules, not tokens

=cut

my @categories_which_are_rules_not_tokens=qw(
  statement_prefix
  statement_control
  statement_mod_cond
  statement_mod_loop
);

=begin

Categories with symbol constraints

=cut

my %category_symbol_constraints = qw{
  quote              nofat
  dotty              unspacey
  trait_verb         nofat_space
  trait_auxiliary    nofat_space

  type_declarator    nofat
  scope_declarator   nofat
  package_declarator nofat
  routine_declarator nofat

  statement_prefix   nofat
  statement_control  nofat_space
  statement_mod_cond nofat
  statement_mod_loop nofat
};

=begin

Operator precedence

=cut

my $precedence_table = <<'END';
  hyper           transparent      
  term            z=               
  methodcall      y=               
  autoincrement   x=               
  exponentiation  w=  right assign 
  symbolic_unary  v=               
  multiplicative  u=  left  assign 
  additive        t=  left  assign 
  replication     s=  left  assign 
  concatenation   r=  left  assign 
  junctive_and    q=  list  assign 
  junctive_or     p=  list  assign 
  named_unary     o=               
  nonchaining     n=  non          
  chaining        m=  chain bool   
  tight_and       l=  left  assign 
  tight_or        k=  left  assign 
  conditional     j=  right        
  item_assignment i=  right        
  loose_unary     h=               
  comma           g=  list         
  list_infix      f=  list  assign 
  list_assignment i=  right sub:e=
  list_prefix     e=                 
  loose_and       d=  left  assign  
  loose_or        c=  left  assign 
  LOOSEST         a=!              
  terminator      a=  list         
END
#do_precedence_table(remove_comments($precedence_table));

my %precedence_aliases=qw{
  prefix  symbolic_unary
  infix   additive
  postfix autoincrement
};

=begin

Very simple tokens

=cut

my $very_simple_tokens_without_precedence=<<'END';

prefix_postfix_meta_operator «
prefix_postfix_meta_operator <<
postfix_prefix_meta_operator »
postfix_prefix_meta_operator >>

sigil     $ @@ @ % & ::
twigil    . ! ^ : * + ? =
quote_mod w ww x to s a h f c b

regex_metachar . ^^ ^ $$
qq_backslash \\ a b e f n r t 0
regex_assertion . ,

regex_mod_internal :!i

regex_metachar <( )> << >> « »

END

my $very_simple_tokens_with_precedence=<<'END';

term    term            self * 
infix   methodcall      . 
postfix methodcall      -> 
postfix autoincrement   ++ -- i 
prefix  autoincrement   ++ -- 
infix   exponentiation  ** 
prefix  symbolic_unary  ! + - ~ ? = * ** ~^ +^ ?^ ^ | 
infix   multiplicative  * / % +& +< << >> +> ~&> ~< ~> 
infix   additive        + - +| +^ ~| ~^ ?| ?^ 
infix   replication     x xx 
infix   concatenation   ~ 
infix   junctive_and    & 
infix   junctive_or     | ^ 
prefix  named_unary     rand sleep abs 
infix   nonchaining     <=> cmp is but does .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^ 
infix   chaining        == != < <= > >= ~~ !~ =~ eq ne lt le gt ge =:= === 
infix   tight_and       && 
infix   tight_or        || // 
infix   item_assignment := ::= 
infix   item_assignment .= 
prefix  loose_unary     true not 
infix   comma           , p5=> 
infix   list_infix      X Z minmax 
infix   loose_and       and andthen 
infix   loose_or        or xor orelse 

END

=begin

 Simple tokens

=end






=begin

Typenames kludge.

=cut

my @typenames = qw(
    Bit Int Str Num Complex Bool Rat
    Exception Code Block List Seq Range Set Bag Junction Pair
    Mapping Signature Capture Blob Whatever Undef Failure
    StrPos StrLen Version P6opaque
    bit int uint buf num complex bool rat
    Scalar Array Hash KeyHash KeySet KeyBag Buf IO Routine Sub Method
    Submethod Macro Regex Match Package Module Class Role Grammar Any Object
);


=begin

A copy of the unicode bracket pairs.

=cut

my @open2close = qw{
    0028 0029  003C 003E  005B 005D   007B 007D  00AB 00BB  0F3A 0F3B
    0F3C 0F3D  169B 169C  2039 203A   2045 2046  207D 207E  208D 208E
    2208 220B  2209 220C  220A 220D   2215 29F5  223C 223D  2243 22CD
    2252 2253  2254 2255  2264 2265   2266 2267  2268 2269  226A 226B
    226E 226F  2270 2271  2272 2273   2274 2275  2276 2277  2278 2279
    227A 227B  227C 227D  227E 227F   2280 2281  2282 2283  2284 2285
    2286 2287  2288 2289  228A 228B   228F 2290  2291 2292  2298 29B8
    22A2 22A3  22A6 2ADE  22A8 2AE4   22A9 2AE3  22AB 2AE5  22B0 22B1
    22B2 22B3  22B4 22B5  22B6 22B7   22C9 22CA  22CB 22CC  22D0 22D1
    22D6 22D7  22D8 22D9  22DA 22DB   22DC 22DD  22DE 22DF  22E0 22E1
    22E2 22E3  22E4 22E5  22E6 22E7   22E8 22E9  22EA 22EB  22EC 22ED
    22F0 22F1  22F2 22FA  22F3 22FB   22F4 22FC  22F6 22FD  22F7 22FE
    2308 2309  230A 230B  2329 232A   23B4 23B5  2768 2769  276A 276B
    276C 276D  276E 276F  2770 2771   2772 2773  2774 2775  27C3 27C4
    27C5 27C6  27D5 27D6  27DD 27DE   27E2 27E3  27E4 27E5  27E6 27E7
    27E8 27E9  27EA 27EB  2983 2984   2985 2986  2987 2988  2989 298A
    298B 298C  298D 298E  298F 2990   2991 2992  2993 2994  2995 2996
    2997 2998  29C0 29C1  29C4 29C5   29CF 29D0  29D1 29D2  29D4 29D5
    29D8 29D9  29DA 29DB  29F8 29F9   29FC 29FD  2A2B 2A2C  2A2D 2A2E
    2A34 2A35  2A3C 2A3D  2A64 2A65   2A79 2A7A  2A7D 2A7E  2A7F 2A80
    2A81 2A82  2A83 2A84  2A8B 2A8C   2A91 2A92  2A93 2A94  2A95 2A96
    2A97 2A98  2A99 2A9A  2A9B 2A9C   2AA1 2AA2  2AA6 2AA7  2AA8 2AA9
    2AAA 2AAB  2AAC 2AAD  2AAF 2AB0   2AB3 2AB4  2ABB 2ABC  2ABD 2ABE
    2ABF 2AC0  2AC1 2AC2  2AC3 2AC4   2AC5 2AC6  2ACD 2ACE  2ACF 2AD0
    2AD1 2AD2  2AD3 2AD4  2AD5 2AD6   2AEC 2AED  2AF7 2AF8  2AF9 2AFA
    2E02 2E03  2E04 2E05  2E09 2E0A   2E0C 2E0D  2E1C 2E1D  3008 3009
    300A 300B  300C 300D  300E 300F   3010 3011  3014 3015  3016 3017
    3018 3019  301A 301B  301D 301E   FD3E FD3F  FE17 FE18  FE35 FE36
    FE37 FE38  FE39 FE3A  FE3B FE3C   FE3D FE3E  FE3F FE40  FE41 FE42
    FE43 FE44  FE47 FE48  FE59 FE5A   FE5B FE5C  FE5D FE5E  FF08 FF09
    FF1C FF1E  FF3B FF3D  FF5B FF5D   FF5F FF60  FF62 FF63
};   
{package O2cP6;
 sub wrap{shift;'"\u'.$_[0].'"'}
 sub link{shift;$_[0].' => '.$_[1]}
 sub comma{shift;my $s=""; while(@_){$s.=join(", ",splice(@_,0,3)).",\n  "}; $s}
 sub context{shift;'constant %open2close = ('."\n  ".$_[0].");\n"}
 sub no_high_bit_codes{0}
 sub code{
     my($cls)=@_;
     $cls->context($cls->comma(
        map{
            my($o,$c)=@$_;
            (($cls->no_high_bit_codes and $o !~ /^00/)
             ? ()
             : $cls->link(map{$cls->wrap($_)}($o,$c)));
        } array_2_pairs(@open2close)));
 }
}
_ O2C, O2cP6->code;

# Helper subs
sub remove_comments {my($s)=@_;$s=~s/\#.*//g;$s}
sub array_to_pairs {my @pairs; push(@pairs,[splice(@_,0,2)]) while @_; @pairs;}

#============================================================
my(%c,$last_n);
sub _ ($$) {
    my($n,$s)=@_;
    ($n,$s)=($last_n,$n) if !defined $s;
    my($p,$f,$l)=caller; $l++;
    $s =~ /^(\s*)/; my $remove_indent = $1;
    $s =~ s/^$remove_indent//mg;
    $c{$n}.="#line $l:$f\n".$s;
}
sub ceval {
    my $code = $c{Main};
    $code =~ s/^(\s*)<<(\w+)>>/
              {my($sp,$n)=($1,$2);
               my $s=$c{$n};
               $s=~s{^(?!#line)}{$sp}mg;
               $s}/megx; #/
    print $code;
    eval($code) or die $!;
}
ceval;

__END__
_ Foo,<<'^';
print 3,"\n";
print 4,"\n";
print 5,"\n";
^
_ Main,<<'^';
print 1,"\n";
<<Foo>>
<<O2C>>
^

