sub JS::Root::scalar ($thing)   is primitive is rw { $thing }
sub JS::Root::list   (*@things) is primitive       { @things }
