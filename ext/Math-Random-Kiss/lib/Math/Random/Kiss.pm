use v6-alpha;

=begin DOCS

Google for --

 From: George Marsaglia <geo@stat.fsu.edu>
 Subject: Random numbers for C: End, at last?
 Date: 1999/01/21
 Message-ID: <36A5BB98.F2561DFF@stat.fsu.edu>
 Approved: mmcirvin@world.std.com (sci.physics.research)
 Sender: mmcirvin@world.std.com (Matthew J McIrvin)
 Organization: Florida State University
 Newsgroups: sci.stat.math,sci.math,sci.math.num-analysis,
 	sci.crypt,sci.physics.research,comp.os.msdos.djgpp

for details.

=end DOCS

=cut

role Rand
{
    our multi method srand ( Num $seed? ) {...}
    our multi method rand ( Num $x = 1 --> Num ) {...}
}


class Math::Random::Kiss
    does Rand
{
    has int $.z;
    has int $.w;
    has int $.jsr;
    has int $.jcong;

    submethod BUILD ( Int $z?, Int $w?, Int $jsr?, Int $jcong? ) {
        my $mask = self.default_seed_algorithm();

        $.z     = ( 362436069 +^ ($z     // $mask) ) +& 0x7FFFFFFF;
        $.w     = ( 521288629 +^ ($w     // $mask) ) +& 0x7FFFFFFF;
        $.jsr   = ( 123456789 +^ ($jsr   // $mask) ) +& 0x7FFFFFFF;
        $.jcong = ( 380116160 +^ ($jcong // $mask) ) +& 0x7FFFFFFF;
    }

    our multi method srand ( Num $seed? ) {
        my $s = $seed // self.default_seed_algorithm();
        self.BUILD($s, $s, $s, $s);
    }

    our multi method rand ( Num $x = 1 --> Num ) {
        $x * self.kiss();
    }

    # Marsaglia's kiss()
    submethod kiss () {
        my int $mym = self.mwc();
        my int $myc = self.cong();
        my int $mys = self.shr3();

        ( (($mym +^ $myc) + $mys) +& 0x7FFFFFFF ) / 2147483648.0;
    }

    method default_seed_algorithm () {
        int(time) +^ ( $?PID + ($?PID +< 15) );
    }

    submethod znew () {
        $.z = 36969 * ( $.z +&65535 ) + ( ( $.z +> 16 ) +& 0x0000FFFF );
        $.z +&= 0x7FFFFFFF;
    }

    submethod wnew () {
        $.w = 18000 * ( $.w +& 65535 ) + ( ( $.w +> 16 ) +& 0x0000FFFF );
        $.w +&= 0x7FFFFFFF;
    }

    submethod mwc () {
        my int $myz = self.znew();
        my int $myw = self.wnew();

        ( ( $myz +< 16 ) + $myw ) +& 0x7FFFFFFF;
    }

    submethod shr3 () {
        $.jsr +^= $.jsr +< 17;
        $.jsr +^= ( $.jsr +> 13 ) +& 0x0007FFFF;
        $.jsr +^= $.jsr +< 5;

        $.jsr +&= 0x7FFFFFFF;
    }

    submethod cong () {
        $.jcong = ( 69069 * $.jcong + 1234567 ) +& 0x7FFFFFFF;
    }
}
