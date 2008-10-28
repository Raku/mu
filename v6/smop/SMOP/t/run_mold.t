use SMOP;

# this mold code was brought here from the 23_code.m0ld
# I took the C compiled version and rewritten it.

my $submold = SMOP::Mold->create
  (10,
   [ '$OUT','$_','FETCH','back','continuation',
     'goto','lookup','postcircumfix:{ }','print' ],
   59,
   [ 1,11,10,7,1,1,0,1,12,11,2,0,0,1,13,12,2,0,0,1,
     15,10,6,1,0,0,1,16,15,2,0,0,1,14,16,8,1,13,0,1,
     17,9,4,0,0,1,18,17,3,0,0,1,14,9,5,1,18,0,0 ]);


my $mold = SMOP::Mold->create
  (13,
   [ SMOP::S1P->Capturize,
     SMOP::S1P->LexicalScope,
     SMOP::S1P->Scalar,
     '$OUT','$_',"1..3\n",'Code','FETCH','STORE',
     'capturize','lookup','mold','new',"ok 1\n",
     "ok 2\n","ok 3\n",'outer',
     'postcircumfix:( )','postcircumfix:{ }','print',
     $submold,
     SMOP::S1P->LexicalPrelude ],
   146,
   [ 1,23,21,10,1,3,0,1,24,23,7,0,0,1,22,24,19,1,5,0,1,25,21,
     10,1,6,0,1,26,25,7,0,0,1,27,1,12,0,2,16,21,1,28,27,16,0,
     0,1,22,28,8,1,21,0,1,29,27,10,1,3,0,1,30,23,7,0,0,1,31,27,
     18,1,4,0,1,32,2,12,0,0,1,22,31,8,1,32,0,1,22,32,8,1,15,0,
     1,33,26,12,0,4,16,27,11,20,1,34,0,9,1,13,0,1,22,33,17,1,34,
     0,1,34,0,9,1,14,0,1,22,33,17,1,34,0,1,34,0,9,0,0,1,22,33,
     17,1,34,0,0 ]);


my $frame = SMOP::MoldFrame->create($mold);

SMOP::Interpreter->run($frame);

