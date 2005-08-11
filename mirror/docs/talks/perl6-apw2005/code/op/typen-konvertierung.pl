#=Typen-Konvertierung ('coerce')
multi sub *coerce:as (Wolf $wolf, Schafspelz ::pelz) {...}

# Der berüchtigte Wolf im Schafspelz:
my $böse = $wolf as Schafspelz;

# mit obiger Funktion geht dann auch das:
my Schafspelz $pelz = $wolf;
