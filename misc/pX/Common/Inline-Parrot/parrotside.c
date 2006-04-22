#include <stdio.h>
#include <EXTERN.h>
#include <perl.h>
void hello_world(void) {
	SV *msg = get_sv("Inline::Parrot::msg",FALSE);
	if (msg == NULL) {
		printf("not ok 2 - No such var:Inline::Parrot::msg\n");
	} else {
		sv_setpv(msg,"ok 2 - simple var set\n"); 
	}
}
