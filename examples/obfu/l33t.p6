class l33t-0.0.1;

use v6;

=kwid

l33t interpreter

This isn't supposed to be particularly obfuscated code; but it is an
interpreter for the esoteric language l33t:

L<http://electrod.ifreepages.com/l33t.htm>

Which I think warrants its inclusion in this directory.

=cut

#our $MEMSIZE  = 64 * 1024;
our $MEMSIZE  = 200; # use this till pugs becomes faster. :-(
our $CELLSIZE = 256;
our $INSULT   = 'j00 4r3 teh 5ux0r';

has int8 @.mem;   # program and data memory space
has Int $.ip;     # instruction pointer
has Int $.mp;     # memory pointer
has Socket $con;  # "connection"; stdio when undef
has Bool $:debug;

submethod BUILD {
   #say "BUILD";
   @.mem[$_] = 0 for 0 .. ($MEMSIZE-1);
   $.ip = 0;
   $.mp = 0;
   #$:debug = bool::true;
   $:debug = bool::false;
   #say "BUILD done";
}

submethod debug(: Str $msg) {
    return unless $:debug;
    say $msg;
}

method load($self: Str $program is copy) {
    ./debug("loading >>\n $program\n<<");
    $program ~~ s:perl5<s>/^\s*//;
    for split rx:perl5/\s+/, $program -> $word {
        my $val = ([+] $word ~~ rx:P5<g>/(\d)/) // 0;
        ./debug("word [$word] = $val");
        @.mem[$.mp++] = $val;
        $.mp %= $MEMSIZE;
    }
    $.mp++; $.mp %= $MEMSIZE;
    return $self;
}

submethod IIP() { # increment instruction pointer
    $.ip++;
    $.ip %= $MEMSIZE;
}

submethod DIP() { # decrement instruction pointer
    $.ip--;
    $.ip %= $MEMSIZE;
}

method run() {
    loop {
        ./debug("IP: $.ip => @.mem[$.ip]  MP: $.mp => @.mem[$.mp]");
        given @.mem[$.ip] {
            when 0 { ./IIP };                  # NOP
            when 1 { ./write; ./IIP };         # WRT
            when 2 { ./read;  ./IIP };         # RD
            when 3|4 { ./bracket($_) };        # IF / EIF
            when 5|6 {                         # FWD / BAK
                ./IIP;
                $.mp += (($_ == 5) ?? 1 :: -1) * (@.mem[$.ip] + 1);
                $.mp %= $MEMSIZE;
                ./IIP;
            };
            when 7|8 {                         # INC / DEC
                ./IIP;
                @.mem[$.mp] += (($_ == 7) ?? 1 :: -1) * (@.mem[$.ip] + 1);
                @.mem[$.mp] %= $CELLSIZE;
                ./IIP;
            };
            when 9 {                         # CON
                ./IIP;
                $.mp += 6;
                $.mp %= $MAXSIZE;
                say "C0N n0t 1mp13m3nt3D, j0!";
            };
            when 10 {                        # END
                $.con.close if defined $.con;
                last;
            };
            ./debug($INSULT);
            ./IIP;
        }
    }
}

method bracket(: $own) {
    my $move     = (($own == 3) ?? {./IIP} :: {./DIP}); # mover in the right direction
    my $matching = (($own == 3) ?? 4    :: 3);    # matching bracket instruction
    if (($own == 3 && @.mem[$.mp] == 0) || ($own == 4 && @.mem[$.mp] != 0)) {
        my $iflevel = 1;
        loop {
             $move();
             given @.mem[$.ip] {
                 when $own      { $iflevel++ };
                 when $matching { ./IIP, last unless --$iflevel };
             }
         }
    } else {
        ./IIP;
    }
}

method write() {
    print chr @.mem[$.mp];
}

method read() {
    @.mem[$.mp] = ord getc;
}

method demo(Class $class: ) {
    #say("demo starting");
    #$class.new.load(l33t::Samples.hello).run;
    $class.new.load($class.hello).run;
}

if !caller() { l33t.demo }

#class l33t::Samples-0.0.1;

method crazy_eights(Class $class :) {
    return q{
        7hink y0uR t0uGh?
        Ar3 y0U 5ure?
        5tEp uP 7hEn!!
        f3e11 t3h buRn!!
        7akE it!!!1
        y0u l1kE?
        We'LL PlAy CrAzY 8's bItCh!!!11
        I'll 0wN yOuR f4t A55!!!!
    };
}

method ascii_loop(Class $class :) {
    return q{
        ph34r my l3Et 5kIlLZ!!!!!!
        nErDs 41n't cool 3v3ry1!!!
        y0u b1g g33kS r teh g33kY sux0rs!
        PHE4R! LOLOLOLOLOLOL!!!
    };
}


# Sayeth the original source code:
# "Hello World" by Stephen McGreal.
# Note that the views expressed in this source code do not necessarily
# coincide with those of the author :o)
method hello(Class $class :) {
    return q{

    Gr34t l33tN3$$? 
    M3h...
    iT 41n't s0 7rIckY.

    l33t sP33k is U8er keWl 4nD eA5y wehn u 7hink 1t tHr0uGh.
    1f u w4nn4be UB3R-l33t u d3f1n1t3lY w4nt in 0n a b4d4sS h4xX0r1ng s1tE!!! ;p
    w4r3Z c0ll3cT10n2 r 7eh l3Et3r!

    Qu4k3 cL4nS r 7eh bE5t tH1ng 1n teh 3nTIr3 w0rlD!!!
    g4m3s wh3r3 u g3t to 5h00t ppl r 70tAl1_y w1cK1d!!
    I'M teh fr4GM4stEr aN I'lL t0t41_1Ly wIpE teh phr34k1ng fL00r ***j3d1 5tYlE*** wItH y0uR h1dE!!!! L0L0L0L!
    t3lEphR4gG1nG l4m3rs wit mY m8tes r34lLy k1kK$ A$$

    l33t hAxX0r$ CrE4t3 u8er- k3wL 5tUff lIkE n34t pR0gR4mm1nG lAnguidGe$...
    s0m3tIm3$ teh l4nGu4gES l00k jUst l1k3 rE41_ 0neS 7o mAkE ppl Th1nk th3y'r3 ju$t n0rMal lEE7 5pEEk but th3y're 5ecRetLy c0dE!!!!
    n080DY unDer5tAnD$ l33t SpEaK 4p4rT fr0m j3d1!!!!!
    50mE kId 0n A me$$4gEb04rD m1ghT 8E a r0xX0r1nG hAxX0r wH0 w4nT2 t0 bR34k 5tuFf, 0r mAyb3 ju5t sh0w 7eh wAy5 l33t ppl cAn 8E m0re lIkE y0d4!!! hE i5 teh u8ER!!!!
    1t m1ght 8E 5omE v1rus 0r a Pl4ySt4tI0n ch34t c0dE.
    1t 3v3n MiTe jUs7 s4y "H3LL0 W0RLD!!!" u ju5t cAn'T gu3s5.
    tH3r3's n3v3r anY p0iNt l00KiNg sC3pT1c4l c0s th4t, be1_1Ev3 iT 0r n0t, 1s whAt th1s 1s!!!!!

    5uxX0r5!!!L0L0L0L0L!!!!!!!
    };
}

