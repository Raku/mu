module Main {
    my %a = { a => 4, b => 2 };
    say '1..3';
    %a{'a'} = 1;
    say 'ok ' ~ %a{'a'};
    my $var = 2;
    %a{'a'} := $var;
    say 'ok ' ~ %a{'a'};
    $var = 3;
    say 'ok ' ~ %a{'a'};
}