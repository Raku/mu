#include <stdio.h>
#include <EXTERN.h>
#include <perl.h>
#include <parrot/parrot.h>
void send_msg(void) {
	SV *msg = get_sv("Inline::Parrot::msg",FALSE);
	if (msg == NULL) {
		printf("not ok 2 - No such var:Inline::Parrot::msg\n");
	} else {
		sv_setpv(msg,"ok 2 - simple var set\n"); 
	}
}
int send_pmc(Parrot_Interp i,PMC *what,char *where) {
	SV *sv = get_sv(where,TRUE);
	sv_setref_pv(sv, "PMC", (void*)what);
	return 0;
}
