sub head(*$head, *@tail)         { return $head }
sub neck(*$head, *$neck, *@tail) { return $neck }
sub tail(*$head, *@tail)         { return @tail }

my @args = (1..5);

    # $head parameter receives 1
    # @tail parameter receives [2, 3, 4, 5]
head(@args).say;


    # $head parameter receives 1
    # $neck parameter receives 2
    # @tail parameter receives [3, 4, 5]
neck(@args).say;

    # $head parameter receives 1
    # @tail parameter receives [2, 3, 4, 5]
tail(@args).say;


# -------------------------------
# infinite data is handled lazily - not
# -------------------------------

# my @infinite_list = (1 .. Inf) ;
# my $head = head(@infinite_list);


