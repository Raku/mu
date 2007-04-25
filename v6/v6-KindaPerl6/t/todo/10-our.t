class Main {

our $v1 = 1;
my $v2 = 2;

BEGIN {
    my $v3 = 10;
    my $v4 := $v3;
    $v3 := $v4;
    $v3 := $v1;
    $v2 = $v3;
}

}

