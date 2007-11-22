typedef struct {
    int from;
    int to;
    int boolean;
    char *match_str;
    AV* array;
    HV* hash;
    SV* result;
} match;
void dump_match(match* m) {
    printf("boolean = %d,from %d to %d on %s\n",m->boolean,m->from,m->to,m->match_str);
}
match* new_match(char *match_str,int pos) {
    match* m = malloc(sizeof(match));
    m->match_str = match_str;
    m->from = pos;
    m->array = newAV();
    m->hash  = newHV();
    return m;
}

/*

AUTHORS

The Pugs Team perl6-compiler@perl.org.

SEE ALSO

The Perl 6 homepage at http://dev.perl.org/perl6.

The Pugs homepage at http://pugscode.org/.

COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

*/