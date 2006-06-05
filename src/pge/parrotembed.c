void Pugs_trap_exit (int status, void* arg) {
    longjmp(((Parrot_exception*)arg)->destination, -1);
}

void* Pugs_callSubSSS(void *i, void *p, void *x, void *y) {
    void *rv;
    Parrot_exception jb;

    if (setjmp(jb.destination)) {
        Parrot_destroy(i);
        return(NULL);
    }
    else {
	push_new_c_exception_handler(i, &jb);
        Parrot_on_exit(Pugs_trap_exit, &jb);
        rv = Parrot_call_sub(i, p, "SSS", x, y);
        pop_exception(i);
    }

    return rv;
}
