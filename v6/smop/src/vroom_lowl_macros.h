/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */

#define SMOP_APPLY(value,arguments,wants) (value->dispatcher?\
                                           value->dispatcher->APPLY(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__List*)arguments,\
                                              (SMOP__CORE__Value*)wants\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->APPLY(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__List*)arguments,\
                                              (SMOP__CORE__Value*)wants))

#define SMOP_DESTR(value)                (value->dispatcher?\
                                           value->dispatcher->DESTR(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->DESTR(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_NEW(prototype,arguments) (prototype->NEW(prototype,arguments))


#define SMOP_STRNG(value)                (value->dispatcher?\
                                           value->dispatcher->STRNG(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->STRNG(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_NUMBR(value)                (value->dispatcher?\
                                           value->dispatcher->NUMBR(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->NUMBR(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_BOOLN(value)                (value->dispatcher?\
                                           value->dispatcher->BOOLN(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->BOOLN(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_SCALAR(value)                (value->dispatcher?\
                                           value->dispatcher->SCALAR(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->SCALAR(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_LIST(value)                (value->dispatcher?\
                                           value->dispatcher->LIST(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->LIST(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_HASH(value)                (value->dispatcher?\
                                           value->dispatcher->HASH(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->HASH(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_WHICH(value)                (value->dispatcher?\
                                           value->dispatcher->WHICH(\
                                              value->dispatcher,\
                                              (SMOP__CORE__Value*)value):\
                                           ((SMOP__CORE__Dispatcher*)value)->WHICH(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_WHAT(value)                 (value->dispatcher?\
                                           smop_value_refcnt_inc((SMOP__CORE__Value*)(value->dispatcher)):\
                                           smop_value_refcnt_inc((SMOP__CORE__Value*)value))

#define SMOP_LIST_LOOKP(value,index) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((SMOP__CORE__ListDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->LOOKP(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index))

#define SMOP_LIST_EXIST(value,index) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((SMOP__CORE__ListDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->EXIST(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index))

#define SMOP_LIST_STORE(value,index,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((SMOP__CORE__ListDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index,\
                                              (SMOP__CORE__Value*)new\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->STORE(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index,\
                                              (SMOP__CORE__Value*)new))

#define SMOP_LIST_DELET(value,index) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((SMOP__CORE__ListDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->DELET(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index))

#define SMOP_LIST_ELEMS(value,index) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((SMOP__CORE__ListDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index\
                                           ):\
                                           ((SMOP__CORE__ListDispatcher*)value)->ELEMS(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__int*)index))

#define SMOP_SCALAR_FETCH(value,wants) (value->dispatcher?\
                                           ((SMOP__CORE__ScalarDispatcher*)value->dispatcher)->FETCH(\
                                              ((SMOP__CORE__Dispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)wants\
                                           ):\
                                           ((SMOP__CORE__ScalarDispatcher*)value)->FETCH(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)wants))

#define SMOP_SCALAR_STORE(value,newvalue) (value->dispatcher?\
                                           ((SMOP__CORE__ScalarDispatcher*)value->dispatcher)->STORE(\
                                              ((SMOP__CORE__ScalarDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)newvalue\
                                           ):\
                                           ((SMOP__CORE__ScalarDispatcher*)value)->STORE(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)newvalue))

#define SMOP_PAIR_GTKEY(value) (value->dispatcher?\
                                           ((SMOP__CORE__PairDispatcher*)value->dispatcher)->GTKEY(\
                                              ((SMOP__CORE__Dispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value\
                                           ):\
                                           ((SMOP__CORE__PairDispatcher*)value)->GTKEY(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_PAIR_GTVAL(value) (value->dispatcher?\
                                           ((SMOP__CORE__PairDispatcher*)value->dispatcher)->GTVAL(\
                                              ((SMOP__CORE__Dispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value\
                                           ):\
                                           ((SMOP__CORE__PairDispatcher*)value)->GTVAL(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value))

#define SMOP_PAIR_STVAL(value,newval) (value->dispatcher?\
                                           ((SMOP__CORE__PairDispatcher*)value->dispatcher)->STVAL(\
                                              ((SMOP__CORE__Dispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)newval\
                                           ):\
                                           ((SMOP__CORE__PairDispatcher*)value)->STVAL(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)newval))

#define SMOP_HASH_LOOKP(value,key) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((SMOP__CORE__HashDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key\
                                           ):\
                                           ((SMOP__CORE__HashDispatcher*)value)->LOOKP(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key))

#define SMOP_HASH_EXIST(value,key) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((SMOP__CORE__HashDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key\
                                           ):\
                                           ((SMOP__CORE__HashDispatcher*)value)->EXIST(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key))

#define SMOP_HASH_STORE(value,key,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((SMOP__CORE__HashDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key,\
                                              (SMOP__CORE__Value*)new\
                                           ):\
                                           ((SMOP__CORE__HashDispatcher*)value)->STORE(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key,\
                                              (SMOP__CORE__Value*)new))

#define SMOP_HASH_DELET(value,key) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((SMOP__CORE__HashDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key\
                                           ):\
                                           ((SMOP__CORE__HashDispatcher*)value)->DELET(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key))

#define SMOP_HASH_ELEMS(value,key) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((SMOP__CORE__HashDispatcher*)value)->dispatcher,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key\
                                           ):\
                                           ((SMOP__CORE__HashDispatcher*)value)->ELEMS(\
                                              (SMOP__CORE__Dispatcher*)value,\
                                              (SMOP__CORE__Value*)value,\
                                              (SMOP__CORE__Value*)key))
