
class Time::Zone;

has Str $.tz;

sub tz(Str $tz) returns Time::Zone is export {
    Time::Zone.new(:tz($tz));
}

