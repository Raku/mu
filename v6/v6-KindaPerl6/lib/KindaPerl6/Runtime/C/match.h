    /*
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;
    has $.array;
    has $.hash;
   void *result;*/
typedef struct {
    int from;
    int to;
    int boolean;
    char *match_str;
} match;
void dump_match(match* m) {
    printf("bool = %d,from %d to %d on %s\n",m->boolean,m->from,m->to,m->match_str);
}
