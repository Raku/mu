/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */

#define VROOM_APPLY(value,arguments,wants) (value->dispatcher?\
                                           value->dispatcher->APPLY(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__List*)arguments,\
                                              (VROOM__CORE__Value*)wants\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->APPLY(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__List*)arguments,\
                                              (VROOM__CORE__Value*)wants))

#define VROOM_DESTR(value)                (value->dispatcher?\
                                           value->dispatcher->DESTR(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->DESTR(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_NEW(prototype,arguments) (prototype->NEW(prototype,arguments))


#define VROOM_STRNG(value)                (value->dispatcher?\
                                           value->dispatcher->STRNG(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->STRNG(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_NUMBR(value)                (value->dispatcher?\
                                           value->dispatcher->NUMBR(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->NUMBR(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_BOOLN(value)                (value->dispatcher?\
                                           value->dispatcher->BOOLN(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->BOOLN(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_SCALAR(value)                (value->dispatcher?\
                                           value->dispatcher->SCALAR(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->SCALAR(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_LIST(value)                (value->dispatcher?\
                                           value->dispatcher->LIST(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->LIST(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_HASH(value)                (value->dispatcher?\
                                           value->dispatcher->HASH(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->HASH(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_WHICH(value)                (value->dispatcher?\
                                           value->dispatcher->WHICH(\
                                              value->dispatcher,\
                                              (VROOM__CORE__Value*)value):\
                                           ((VROOM__CORE__Dispatcher*)value)->WHICH(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_WHAT(value)                 (value->dispatcher?\
                                           vroom_value_refcnt_inc((VROOM__CORE__Value*)(value->dispatcher)):\
                                           vroom_value_refcnt_inc((VROOM__CORE__Value*)value))

#define VROOM_LIST_LOOKP(value,index) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((VROOM__CORE__ListDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->LOOKP(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index))

#define VROOM_LIST_EXIST(value,index) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((VROOM__CORE__ListDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->EXIST(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index))

#define VROOM_LIST_STORE(value,index,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((VROOM__CORE__ListDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index,\
                                              (VROOM__CORE__Value*)new\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->STORE(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index,\
                                              (VROOM__CORE__Value*)new))

#define VROOM_LIST_DELET(value,index) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((VROOM__CORE__ListDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->DELET(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index))

#define VROOM_LIST_ELEMS(value,index) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((VROOM__CORE__ListDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index\
                                           ):\
                                           ((VROOM__CORE__ListDispatcher*)value)->ELEMS(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__int*)index))

#define VROOM_SCALAR_FETCH(value,wants) (value->dispatcher?\
                                           ((VROOM__CORE__ScalarDispatcher*)value->dispatcher)->FETCH(\
                                              ((VROOM__CORE__Dispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)wants\
                                           ):\
                                           ((VROOM__CORE__ScalarDispatcher*)value)->FETCH(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)wants))

#define VROOM_SCALAR_STORE(value,newvalue) (value->dispatcher?\
                                           ((VROOM__CORE__ScalarDispatcher*)value->dispatcher)->STORE(\
                                              ((VROOM__CORE__ScalarDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)newvalue\
                                           ):\
                                           ((VROOM__CORE__ScalarDispatcher*)value)->STORE(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)newvalue))

#define VROOM_PAIR_GTKEY(value) (value->dispatcher?\
                                           ((VROOM__CORE__PairDispatcher*)value->dispatcher)->GTKEY(\
                                              ((VROOM__CORE__Dispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value\
                                           ):\
                                           ((VROOM__CORE__PairDispatcher*)value)->GTKEY(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_PAIR_GTVAL(value) (value->dispatcher?\
                                           ((VROOM__CORE__PairDispatcher*)value->dispatcher)->GTVAL(\
                                              ((VROOM__CORE__Dispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value\
                                           ):\
                                           ((VROOM__CORE__PairDispatcher*)value)->GTVAL(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value))

#define VROOM_PAIR_STVAL(value,newval) (value->dispatcher?\
                                           ((VROOM__CORE__PairDispatcher*)value->dispatcher)->STVAL(\
                                              ((VROOM__CORE__Dispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)newval\
                                           ):\
                                           ((VROOM__CORE__PairDispatcher*)value)->STVAL(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)newval))

#define VROOM_HASH_LOOKP(value,key) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((VROOM__CORE__HashDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key\
                                           ):\
                                           ((VROOM__CORE__HashDispatcher*)value)->LOOKP(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key))

#define VROOM_HASH_EXIST(value,key) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((VROOM__CORE__HashDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key\
                                           ):\
                                           ((VROOM__CORE__HashDispatcher*)value)->EXIST(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key))

#define VROOM_HASH_STORE(value,key,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((VROOM__CORE__HashDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key,\
                                              (VROOM__CORE__Value*)new\
                                           ):\
                                           ((VROOM__CORE__HashDispatcher*)value)->STORE(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key,\
                                              (VROOM__CORE__Value*)new))

#define VROOM_HASH_DELET(value,key) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((VROOM__CORE__HashDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key\
                                           ):\
                                           ((VROOM__CORE__HashDispatcher*)value)->DELET(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key))

#define VROOM_HASH_ELEMS(value,key) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((VROOM__CORE__HashDispatcher*)value)->dispatcher,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key\
                                           ):\
                                           ((VROOM__CORE__HashDispatcher*)value)->ELEMS(\
                                              (VROOM__CORE__Dispatcher*)value,\
                                              (VROOM__CORE__Value*)value,\
                                              (VROOM__CORE__Value*)key))
