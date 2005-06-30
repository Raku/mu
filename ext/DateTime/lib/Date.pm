class Date;

has $.year;
has $.month;
has $.day;

multi method new ($class) returns Date {
    return $class.SUPER::new();
#    return $class.new( :epoch => time() );
}
