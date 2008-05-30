SMOP__Object* SMOP__S1P__method_fib;

void SMOP__S1P__method_fib_boot() {
  SMOP__Object* SMOP__ID__fib = SMOP__NATIVE__idconst_create("fib");
  fib = SMOP__OO__LOWL__Method_create(0, SMOP__ID__fib, SMOP__NATIVE__bool_false, SMOP__S1P__method_fib_run);
};

void SMOP__S1P__method_fib_run(SMOP__Object* interpreter, SMOP__Object* method, SMOP__Object* capture) {
  SMOP__Object* n = SMOP__NATIVE__capture_positional(interpreter,capture,0);
  SMOP__Object* then1,else1,then2,else2,frame;

  then1 = q:sm0p {
    0;
  };
  then2 = q:sm0p {
    1;
  };
  else2 = q:sm0p {
    $n;
    SMOP__SLIME__CurrentFrame.copy(1);
    1;
    2;
    SMOP__SLIME__CurrentFrame.capturize(0,(4,2),(),2);
    SMOP__SLIME__CurrentFrame.capturize(0,(4,2),(),2);
    SMOP__OP__minus.call();
    SMOP__OP__minus.call();
    SMOP__SLIME__CurrentFrame.capturize(0,(2),(),2);
    SMOP__SLIME__CurrentFrame.capturize(0,(2),(),2);
    SMOP__S1P__method_fib.call();
    SMOP__S1P__method_fib.call();
    SMOP__SLIME__CurrentFrame.capturize(0,(1,2),(),1);
    SMOP__OP__plus.call();
  };
  else1 = q:sm0p {
    $n;
    1;
    SMOP__SLIME__CurrentFrame.capturize(0,(1,2),(),2);
    "cond";
    SMOP__OP__numeq.call();
    "then";
    $then2;
    "else";
    $else2;
    SMOP__SLIME__CurrentFrame.capturize(0,(),(6,5,4,3,2,1),1);
    SMOP__OP__if.call();
  };
  SMOP__Object* current = SMOP_DISPATCH(interpreter,interpreter,SMOP__ID__continuation);
  frame = q:sm0p {
    $n;
    0;
    SMOP__SLIME__CurrentFrame.capturize(0,(1,2),(),2);
    "cond";
    SMOP__OP__numeq.call();
    "then";
    $then1;
    "else";
    $else1;
    SMOP__SLIME__CurrentFrame.capturize(0,(),(6,5,4,3,2,1),1);
    SMOP__OP__if.call();
    SMOP__SLIME__CurrentFrame.capturize(0,(1),(),1);
    $current.setr()
    interpreter.goto(|$current);
  };
  SMOP_DISPATCH(interpreter, interpreter, SMOP__ID__goto, frame);
};
