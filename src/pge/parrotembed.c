
static int exit_count = 0;

void Pugs_trap_exit (void *i, int status, void* arg) {
    if (exit_count == ((Parrot_exception*)arg)->system) {
        exit_count++;
        longjmp(((Parrot_exception*)arg)->destination, -1);
    }
}

void* Pugs_callSubSSS(void *i, void *p, void *x, void *y) {
    void *rv;
    Parrot_exception jb;


    if (setjmp(jb.destination)) {
        return(NULL);
    }
    else {
	push_new_c_exception_handler(i, &jb);
        Parrot_on_exit(i, (exit_handler_f)Pugs_trap_exit, &jb);

        exit_count++;
        jb.system = exit_count;
        rv = Parrot_call_sub(i, p, "SSS", x, y);

        pop_exception(i);
    }

    return rv;
}
