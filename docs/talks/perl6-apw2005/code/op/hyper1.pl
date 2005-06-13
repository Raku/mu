#=Hyper-Operatoren
@x = @a >>+<< @b;    # elementweise Addition
@a>>--;              # alle Elemente decrementieren
@x = $x +<<@a;       # $x zu allen @a addieren
                     # und als @x speichern
#=andere Hyper-Operatoren
$a = @b + @c;
@a = @b + @c;
@a = @b >>+ @c;
@a >>=~ s/A/B/;
