module MIME::Base64;


# See http://www.ietf.org/rfc/rfc3548.txt

#      Value Encoding  Value Encoding  Value Encoding  Value Encoding
#          0 A            17 R            34 i            51 z
#          1 B            18 S            35 j            52 0
#          2 C            19 T            36 k            53 1
#          3 D            20 U            37 l            54 2
#          4 E            21 V            38 m            55 3
#          5 F            22 W            39 n            56 4
#          6 G            23 X            40 o            57 5
#          7 H            24 Y            41 p            58 6
#          8 I            25 Z            42 q            59 7
#          9 J            26 a            43 r            60 8
#         10 K            27 b            44 s            61 9
#         11 L            28 c            45 t            62 +
#         12 M            29 d            46 u            63 /
#         13 N            30 e            47 v
#         14 O            31 f            48 w         (pad) =
#         15 P            32 g            49 x
#         16 Q            33 h            50 y

#        1716151413121110 F E D C B A 9 8 7 6 5 4 3 2 1 0
#        +--0     octet--+-1      octet--+--2     octet--+
#        |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
#        +-----------+---+-------+-------+---+-----------+
#        |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
#        +--0.index--+--1.index--+--2.index--+--3.index--+
#######################################################################


multi encode (Int $value) returns Str {
#  say "Int";
  ("A".."Z","a".."z",0..9,"+","/")[$value];
}

multi encode (Str $string) {
  my @oct = split('',$string);
  [~] encode(@oct);
}

multi encode (*@string is copy) returns Array {
#  say "Array";

  gather {
    while (@string>=3) {

      my Int @octect = map { ord $_} splice @string,0,3;

      take encode(index(0         ,0,@octect[0],2,0x3F)); # 0.index
      take encode(index(@octect[0],4,@octect[1],4,0x3F)); # 1.index
      take encode(index(@octect[1],2,@octect[2],6,0x3F)); # 2.index
      take encode(index(@octect[2],0,         0,0,0x3F)); # 3.index
    }

    if (@string == 2) {
      my Int @octect = map { ord $_} splice @string,0,3;

      take encode(index(0         ,0,@octect[0],2,0x3F)); # 0.index
      take encode(index(@octect[0],4,@octect[1],4,0x3F)); # 1.index
      take encode(index(@octect[1],2,         0,0,0x3F)); # 2.index
      take "=";

    } elsif (@string == 1) {
      my Int @octect = map { ord $_} splice @string,0,3;

      take encode(index(0         ,0,@octect[0],2,0x3F)); # 0.index
      take encode(index(@octect[0],4,         0,0,0x3F)); # 1.index
      take "=";
      take "=";
    }
  }
}

sub index (Int $a, Int $ashift, Int $b, Int $bshift, Int $mask) returns Int {
  (($a +& ($mask +>$ashift)) +< $ashift) +| ($b +> $bshift);
}

sub decode (Str $didget) {
#  say "decode Str ->" ~ $didget ~ "<-";
  given $didget {
		when /^<[A..Z]>$/ {     ord($didget)-ord("A")}
		when /^<[a..z]>$/ {26 + ord($didget)-ord("a")}
		when /^<[0..9]>$/ {52 + $didget}
		when /^   \+   $/ {62}
                when /^   \/   $/ {63}
		when /^   \=   $/ {-1}
		when /^<-[A..Za..z0..9+\/=]>$/ {fail "This should never happen"}
 		default {
		  my @index = split('',$didget);
		  @index = grep {$_ ~~ /^<[A..Za..z0..9+\/=]>+$/},@index;
		  [~] map {chr ($_)} decode(@index)}
  }
}

#        1716151413121110 F E D C B A 9 8 7 6 5 4 3 2 1 0
#        +--0     octet--+-1      octet--+--2     octet--+
#        |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
#        +-----------+---+-------+-------+---+-----------+
#        |5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|5 4 3 2 1 0|
#        +--0.index--+--1.index--+--2.index--+--3.index--+
#######################################################################

sub decode (*@string is copy) {
#  say "decode Array ->" ~ @index.perl ~ "<-";
  @string = map{ decode($_) } @string;
  gather {
    while (@string>0 and all(@string[0..3])>0) {
      my Int @index = splice @string,0,4;
      take index(@index[0],2,@index[1],4,0xFF); # 0.octet
      take index(@index[1],4,@index[2],2,0xFF); # 1.octet
      take index(@index[2],6,@index[3],0,0xFF); # 2.octet
    }

    if (all(@string[0..2])>0) {
      my Int @index = splice @string,0,4;
      take index(@index[0],2,@index[1],4,0xFF); # 0.octet
      take index(@index[1],4,@index[2],2,0xFF); # 1.octet
#      take index(@index[2],6,@index[3],0,0xFF); # 2.octet
    } elsif (all(@string[0..1])>0) {
      my Int @index = splice @string,0,4;
      take index(@index[0],2,@index[1],4,0xFF); # 0.octet
#      take index(@index[1],4,@index[2],2,0xFF); # 1.octet
#      take index(@index[2],6,@index[3],0,0xFF); # 2.octet
    } elsif (@string[0]>0) {
      my Int @index = splice @string,0,4;
      take index(@index[0],2,@index[1],4,0xFF); # 0.octet
#      take index(@index[1],4,@index[2],2,0xFF); # 1.octet
#      take index(@index[2],6,@index[3],0,0xFF); # 2.octet
    }
  }
}



=head1 NAME

MIME::Base64 - base64 encoding and decoding for Pugs

=head1 SYNOPSIS

  use MIME::Base64;

  my $encode = MIME::Base64::encode("A string");
  my $decode = MIME::Base64::decode("QSBzdHJpbmc=");

=head1 DESCRIPTION

C<MIME::Base64> is a base64 encoder/decoder for pugs it is I<not> a port
of Perl5's MIME::Base64.

=head1 FUNCTIONS

XXX

=cut
