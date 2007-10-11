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
