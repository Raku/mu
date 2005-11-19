class l33t-0.0.3;

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

has Bool $!trace;
has Bool $!debug;
has Bool $!step;
has Bool %!breakpoints;
has Str  $!last_db_command;
has Str  $!coninfo;

submethod BUILD {
   #say "BUILD";
   @.mem[$_] = 0 for 0 .. ($MEMSIZE-1);
   $.ip = 0;
   $.mp = 0;
   $!trace = $!step = $!debug;
   say "BUILD done" if $!debug;
}

submethod trace( Str $msg) {
    say $msg if $!trace;
}

method load($self: Str $program is copy) {
    ./trace("loading >>\n $program\n<<");
    $program ~~ s:perl5<s>/^\s*//;
    for split rx:perl5/\s+/, $program -> $word {
        my $val = ([+] $word ~~ rx:P5<g>/(\d)/) // 0;
        ./trace("word [$word] = $val");
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
    say "\nl33t d3bu993r - h1t 'h' 4 h31p / 'i' 4 inf0\n" if $!debug;
    #try {
      loop {
        ./debug if $!debug; # XXX: figure out how to bind this once
        given @.mem[$.ip] {
            when 0 { ./IIP };                                      # NOP
            when 1 { ./write; };                                   # WRT
            when 2 { ./read; };                                    # RD
            when 3 { ./bracket(3, 4) };                            # IF
            when 4 { ./bracket(4, 3) };                            # EIF
            when 5 { ./mem($.mp,        wrap=>$MEMSIZE) };         # FWD
            when 6 { ./mem($.mp,        wrap=>$MEMSIZE,  :down) }; # BAK
            when 7 { ./mem(@.mem[$.mp], wrap=>$CELLSIZE) };        # INC
            when 8 { ./mem(@.mem[$.mp], wrap=>$CELLSIZE, :down) }; # DEC
            when 9 { ./con };                                      # CON
            when 10 {                                              # END
                $.con.close if defined $.con;
                last;
            };

            # unknown opcode. this is NOT a (fatal) syntax error.
            say "$INSULT: wtf iz $_?";
            ./IIP;
        }
      }
    #CATCH "Debugger::QUIT" { say "qu1t" }
    #};
    #die $! if $!;
}

method con() {
    @.mem[$MEMSIZE .. $MEMSIZE+5] = @.mem[0 .. 5]; # ch33tz! 101
    my $ip = join ".", @.mem[$.mp .. $.mp+3];
    my $port = @.mem[$.mp+4] * 256 + @.mem[$.mp+5]; # >> f1x0rz v1m

    my $newcon = connect($ip, $port);
    if $newcon {
        $.con.close if $.con;
        $.con = $newcon;
        $!coninfo = "$ip:$port";
    }

    ./IIP;
    $.mp += 6;
    $.mp %= $MAXSIZE;
};

method mem($target is rw, :$wrap!, $down?) {
    ./IIP;
    $target += (@.mem[$.ip] + 1) * ($down ?? -1 !! 1);
    $target %= $wrap;
    ./IIP;
}

method bracket(: $own, $matching) {
    my $move     = (($own == 3) ?? {./IIP} !! {./DIP}); # mover in the right direction
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
    ./IIP;
}

method read() {
    @.mem[$.mp] = ord getc;
    ./IIP;
}

method demo(Class $class: ) {
    #say("demo starting");
    #$class.new.load(l33t::Samples.hello).run;
    $class.new(:debug).load($class.hello).run;
}

method debug() {
    loop { ./debug_trace } until ./debug_interactive;
}

# true return == stay at this line
method debug_interactive() returns Bool {
    if %!breakpoints{$.ip} {
        say "<6r34k>";
    } elsif !$!step {
        return bool::true;
    }
    loop { print "$.ip> " } until (./debug_action(=<>) || $!runnable);
    return $!runnable;
}


method debug_help {
    say qq:to/END/
    h (help)   - print this message
    B          - list breakpoints
    b (break)  - toggle breakpoint here
    b ADDR     - toggle breakpoint at location ADDR
    C (clear)  - clear all breakpoints
    i (info)   - print misc. info (connection, etc.)
    ip ADDR    - change IP to ADDR
    ip +|-NUM  - move IP NUM positions back or forward
    l (list)   - list program (near context, starting here)
    l ADDR     - list program starting at ADDR
    mp ADDR    - change MP to ADDR
    mp +|-NUM  - move MP NUM positions back or forward
    q (quit)   - end program
    r (run)    - continue running until next breakpoint
    s (step)   - single step [enter to keep stepping]
    t (trace)  - toggle trace prints [currently {$!trace ?? "ON" !! "OFF"}]
    w PROG     - write program fragment PROG beginning at MP (changes MP)
    END;
} # : f1x0rz v1m

method debug_action(Str $cmd is copy) returns Bool {
    $!runnable = bool::false;
    $cmd .= chomp;
    $cmd ||= $!last_db_command;
    $!last_db_command = $cmd;
    given $cmd {
        when rx:perl5<i>/^\s*h|\?/ {        # h help
            ./debug_help;
        };
        #when 'B' { say %!breakpoints.keys.sort:{$^a<=>$^b} };
        when 'B' {
            say %!breakpoints.keys.join(" ");
        };
        when rx:perl5/^\s*b\s*(\d+)?$/ {
            my $addr = $0 // $.ip;
            %!breakpoints{$addr} ^^= 1;
            %!breakpoints.delete($addr) unless %!breakpoints{$addr};
        };
        when 'C' {                          # C (clear)
            undefine %!breakpoints;
            say "6r34p01ntz (134r3";
        };
        when 'i' {
            say "(urr3nt (0nn3xxx10n: " ~ ($!coninfo // "stdio");
            say "tr4(3 m0de: " ~ ($!trace ?? "0n" !! "0ff");
            return bool::true;
        };
        when rx:perl5<i>/^\s*ip\s*(([-+])?\d+)/ {
            if $1 { $.ip += $0 }
            else  { $.ip  = $0 }
            $.ip %= $MEMSIZE;
            return bool::true;
        };
        when rx:perl5<i>/^\s*mp\s*(([-+])?\d+)/ {
            if $1 { $.mp += $0 }
            else  { $.mp  = $0 }
            $.mp %= $MEMSIZE;
            return bool::true;
        };
        when rx:perl5/^\s*r/ {              # r run
            $!step     = bool::false;
            $!runnable = bool::true;
        };
        when rx:perl5<i>/^\s*l\s*(\d+)?/ {  # l list
            my $from = $0 // $.ip;
            @.mem[$MEMSIZE .. $MEMSIZE+63] = @.mem[0 .. 63]; # ch33tz! 101
            for 0 .. 3 -> $off {
                say "[{($from+$off*16)%$MEMSIZE}] " ~
                    @.mem[($from+$off*16) .. (($from+$off*16)+15)].join(" ");
            }
        };
        when 's' {                          # s step
            $!step     = bool::true;
            $!runnable = bool::true;
        };
        when 't' {                          # t trace
            $!trace ^^= 1;
        };
        when 'q' { die "Debugger::QUIT" };  # q quit
        when rx:perl5<i>/^\s*w\s*(.+)/ {    # w write
            ./load($0);
            return bool::true;
        };
        say "$INSULT: wft iz $_?";
        return bool::true;
    }
    return bool::false;
}

method debug_trace() {
    return unless $!trace;

    say("IP: $.ip => @.mem[$.ip]  MP: $.mp => @.mem[$.mp]");
    my $msg; # I want rvalue given.
    given @.mem[$.ip] {
        when 0 { $msg = "NOP" };
        when 1 { $msg = "WRT @.mem[$.mp]" };
        when 2 { $msg = "RD => $.mp [01d v4l = @.mem[$.mp]" };
        when 3 { $msg = "IF [{@.mem[$.mp] ?? 'tru3' !! 'f4l53'}]" };
        when 4 { $msg = "EIF [{@.mem[$.mp] ?? 'f4l53' !! 'tru3'}]" };
        when 5 {
            my $nmp = ($.mp + @.mem[($.ip+1) % $MEMSIZE] + 1) % $MEMSIZE;
            $msg = "FWD {@.mem[($.ip+1) % $MEMSIZE]} => $nmp [@.mem[$.mp]]" };
        when 6 {
            my $nmp = ($.mp - @.mem[($.ip+1) % $MEMSIZE] + 1) % $MEMSIZE;
            $msg = "BAK {@.mem[($.ip+1) % $MEMSIZE]} => $nmp [@.mem[$.mp]]" };
        when 7 {
            my $val = @.mem[($.ip+1) % $MEMSIZE];
            $msg = "INC $val => {(@.mem[$.mp]+$val+1) % $CELLSIZE}" };
        when 8 {
            my $val = @.mem[($.ip+1) % $MEMSIZE];
            $msg = "INC $val => {(@.mem[$.mp]-$val+1) % $CELLSIZE}" };
        when 9 {
            @.mem[$MEMSIZE .. $MEMSIZE+5] = @.mem[0 .. 5]; # ch33tz! 101
            my $ip = join ".", @.mem[$.mp .. $.mp+3];
            my $port = @.mem[$.mp+4] * 256 + @.mem[$.mp+5]; # >> f1x0rz v1m
            $msg = "CON $ip:$port"; # 41nt 1 t3h sw33t
        };
        when 10 { $msg = "END" };
        $msg = "$_ [unknown opcode]";
     };
     say $msg;
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

