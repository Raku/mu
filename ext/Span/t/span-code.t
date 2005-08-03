#!/usr/bin/pugs

use v6;
use Test;

plan 20;

use_ok( 'Span::Code' );
use Span::Code;   # XXX should not need this
use Recurrence;
use Span::Num;

my $universe = Recurrence.new( 
    closure_next =>     sub ( $x is copy ) { return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return $x + 1 },
    closure_previous => sub ( $x is copy ) { return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return $x - 1 },
    :is_universe(1) );
my $span = Span::Num.new( start => -Inf, end => Inf, start_is_open => bool::true, end_is_open => bool::true );

my $u = Span::Code.new( recurrence => $universe, span => $span );

isa_ok( $u, 'Span::Code', 
    'created a Span::Code' );

is( $u.start, -Inf, "start" );
is( $u.end  ,  Inf, "end" );
is( $u.stringify, '-Infinity..Inf', "stringify" );
# XXX - is( $universe.universe.stringify, '-Infinity..Inf', "universe accessor" );

is( $u.start_is_open,   bool::false, "start_is_open" );
is( $u.end_is_open,     bool::false, "end_is_open" );

is( $u.start_is_closed, bool::true, "start_is_closed" );
is( $u.end_is_closed,   bool::true, "end_is_closed" );

is( $u.next( 10 ), 11, 'next' );
is( $u.previous( 10 ), 9, 'previous' );

my $even_rec = Recurrence.new( 
    closure_next =>     sub { return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return 2 * int( $_ / 2 ) + 2 },
    closure_previous => sub { return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return 2 * int( ( $_ - 1 ) / 2 ) },
    universe => $universe );
my $even_numbers = Span::Code.new( recurrence => $even_rec, span => $span );
is( $even_numbers.next( 10 ), 12, 'next even' );
is( $even_numbers.previous( 10 ), 8, 'previous even' );

{
    # union
    my $each_3_rec = Recurrence.new( 
        closure_next =>     sub { return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return 3 * int( $_ / 3 ) + 3 },
        closure_previous => sub { return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return 3 * int( ( $_ - 1 ) / 3 ) },
        universe => $universe );
    my $each_3_span = Span::Num.new( start => 10, end => 30 );
    my $each_3_spancode = Span::Code.new( recurrence => $each_3_rec, span => $each_3_span );
    is( $each_3_span.stringify, '[10,30]', '10 to 30' );
    is( $each_3_spancode.stringify, '12,15,18..24,27,30', 'each 3 from 10 to 30' );

    my $even_span = Span::Num.new( start => 20, end => 40 );
    my $even_spancode = Span::Code.new( recurrence => $even_rec, span => $even_span );
    is( $even_spancode.stringify, '20,22,24..36,38,40', 'each 2 from 20 to 40' );

    my @union = $each_3_spancode.union( $even_spancode );
    my $result = @union.map:{ $_.stringify }.join(',');
    is( $result, '12,15,18,20,21,22..27,28,30,32,34,36,38,40', 'Span::Code union Span::Code' );

    # intersection
    my @inter = $each_3_spancode.intersection( $even_spancode );
    $result = @inter.map:{ $_.stringify }.join(',');
    is( $result, '24,30', 'Span::Code intersection Span::Code' );

    # difference
    my @diff = $each_3_spancode.difference( $even_spancode );
    $result = @diff.map:{ $_.stringify }.join(',');
    is( $result, '12,15,18,21,27', 'Span::Code difference Span::Code' );

    # complement
    my @compl = $each_3_spancode.complement();
    $result = @compl.map:{ $_.stringify }.join(',');
    is( $result, 
        '-Infinity..7,8,9,10,11,13..26,28,29,31,32,33..Inf',
        # '(-Infinity,10),10,11,13..26,28,29,(30,Inf)', 
        'Span::Code complement' );
}

=for later

my $odd_numbers = $even_numbers.complement;
is( $odd_numbers.next( 10 ), 11, 'odd even' );
is( $odd_numbers.previous( 10 ), 9, 'odd even' );

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => Inf, :end_is_open(bool::true) );
    is( $continuous.stringify, '[10,Inf)', 'continuous 10-Inf' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..Inf', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '-Infinity..7,8,9', 'range complement' );
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => -Inf, end => 10, :start_is_open(bool::true) );
    is( $continuous.stringify, '(-Infinity,10]', 'continuous (-Inf,10]' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '-Infinity..8,9,10', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '11,12,13..Inf', 'range complement' );
}

{
    # -- intersection of odd numbers with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => -Inf, end => 10, :start_is_open(bool::true) );
    # continuous (-Inf,10]
    my $range = $odd_numbers.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'odd range from continuous' );
    is( $range.stringify, '-Infinity..5,7,9', 'odd range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '-Infinity..Inf', 'odd range complement' );

    {
    my $continuous = Span::Num.new( start => 6, end => 12 );
    # continuous [6..12]
    my $range = $complement.intersection( $continuous );
    is( $range.stringify, '6,8,10,11,12', 'odd range complement' );
    }

    {
    my $continuous = Span::Num.new( start => 4, end => 5 );
    my $range = $even_numbers.union( $continuous );
    my $range2 = Span::Num.new( start => 2, end => 9 );
    my $range3 = $range.intersection( $range2 );
    is( $range3.stringify, '2,4,5,6,8', 'even range union' );
    }

    {
    my $continuous = Span::Num.new( start => 4, end => 5 );
    my $range = $even_numbers.difference( $continuous );
    my $range2 = Span::Num.new( start => 2, end => 9 );
    my $range3 = $range.intersection( $range2 );
    is( $range3.stringify, '2,6,8', 'even range difference' );
    }
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous 10-20' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..18,19,20', 'range from continuous' );

    $continuous = Span::Num.new( start => 10, end => 20, end_is_open => 1 );
    is( $continuous.stringify, '[10,20)', 'continuous' );
    $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..17,18,19', 'range from continuous semi' );
    
    my $complement = $range.complement;
    # XXX - universe slice should be written '(-Infinity,Inf)'
    is( $complement.stringify, '-Infinity..Inf', 'range complement' );

    my $complement1 = $complement.intersection( Span::Num.new( start => -Inf, end => 15 ) );
    is( $complement1.stringify, '-Infinity..7,8,9', 'complement 1' );
    $complement1 = $complement.intersection( Span::Num.new( start => 15, end => Inf ) );
    is( $complement1.stringify, '20,21,22..Inf', 'complement 2' );
}
{
    # -- intersection with a discrete span
    use Span::Int;
    my $continuous = Span::Int.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous' );
    my $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..18,19,20', 'range from discrete' );
}
{
    my $set = $universe.complement;
    is( $set.start,  undef, "start empty set" );
    is( $set.end  ,  undef, "end" );
    is( $set.stringify, '', "stringify" );
}
{
    # 0 .. Inf
    my $span1 = Span::Code.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 ::    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 :: -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 ::  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 ::   -1 },
        universe => $universe );
    
    is( $span1.start,    0, "start" );
    is( $span1.end  ,  Inf, "end" );

    # -Inf .. 10
    my $span3 = Span::Code.new( 
        closure_next =>         sub { $_ < 10 ??  $_ + 1 ::  Inf },
        closure_previous =>     sub { $_ < 11 ??  $_ - 1 ::   10 },
        complement_next =>      sub { $_ >= 10 ?? $_ + 1 ::   11 },
        complement_previous =>  sub { $_ > 11 ??  $_ - 1 :: -Inf },
        universe => $universe );
    
    is( $span3.start, -Inf, "start" );
    is( $span3.end  ,   10, "end" );

    is( $span1.intersects( $span3 ), bool::true, 'intersects' );

    {
        my $span2 = $span1.complement;
        is( $span2.start, -Inf, "start" );
        is( $span2.end  ,   -1, "end" );
    }
    {
        my $span4 = $span3.complement;
        is( $span4.start,   11, "start" );
        is( $span4.end  ,  Inf, "end" );
        is( $span4.stringify, '11,12,13..Inf', "stringify" );
    }
    {
        my $span5 = $span1.intersection( $span3 );
        is( $span5.start,  0, "start" );
        is( $span5.end  , 10, "end" );
        is( $span5.stringify, '0,1,2..8,9,10', "stringify" );
    }
    {
        my $span5 = $span1.difference( $span3 );
        is( $span5.start,  11, "start" );
        is( $span5.end  , Inf, "end" );
    }
    {
        my $span5 = $span3.difference( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,   -1, "end" );
    }
    {
        my $span5 = $span3.union( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,  Inf, "end" );
    }
}

=cut

=for later

is( $span.size, 2, "real size" );
# is( $span.size( density => 1 ), 3, "integer size" );

my $span2 = Span::Num.new( 
    start => 2, end => 4, start_is_open => bool::false, end_is_open => bool::false );

my $span3 = Span::Num.new( 
    start => 4, end => 6, start_is_open => bool::false, end_is_open => bool::false );

is( $span.intersects( $span2 ), bool::true, 'intersects' );

is( $span.intersects( $span3 ), bool::false, 'doesn\'t intersect' );

{
    my @a = $span.complement;
    # XXX inconsistent stringification of -Inf
    is( @a[0].stringify ~ ' ' ~ @a[1].stringify, '(-Infinity,1) (3,Inf)', 'complement' );
}

is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

is( $span.union( $span2 ).stringify, '[1,4]', 'union' );

=cut
