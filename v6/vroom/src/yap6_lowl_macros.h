/* Dispatching mechanism... This can be rewritten in the future,
   but for now, it's as simple as it gets */

#define YAP6_APPLY(value,arguments,wants) (value->dispatcher?\
                                           value->dispatcher->APPLY(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__List*)arguments,\
                                              (YAP6__CORE__Value*)wants\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->APPLY(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__List*)arguments,\
                                              (YAP6__CORE__Value*)wants))

#define YAP6_DESTR(value)                (value->dispatcher?\
                                           value->dispatcher->DESTR(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->DESTR(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_NEW(prototype,arguments) (prototype->NEW(prototype,arguments))


#define YAP6_STRNG(value)                (value->dispatcher?\
                                           value->dispatcher->STRNG(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->STRNG(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_NUMBR(value)                (value->dispatcher?\
                                           value->dispatcher->NUMBR(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->NUMBR(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_BOOLN(value)                (value->dispatcher?\
                                           value->dispatcher->BOOLN(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->BOOLN(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_SCALAR(value)                (value->dispatcher?\
                                           value->dispatcher->SCALAR(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->SCALAR(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_LIST(value)                (value->dispatcher?\
                                           value->dispatcher->LIST(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->LIST(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_HASH(value)                (value->dispatcher?\
                                           value->dispatcher->HASH(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->HASH(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_WHICH(value)                (value->dispatcher?\
                                           value->dispatcher->WHICH(\
                                              value->dispatcher,\
                                              (YAP6__CORE__Value*)value):\
                                           ((YAP6__CORE__Dispatcher*)value)->WHICH(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_WHAT(value)                 (value->dispatcher?\
                                           yap6_value_refcnt_inc((YAP6__CORE__Value*)(value->dispatcher)):\
                                           yap6_value_refcnt_inc((YAP6__CORE__Value*)value))

#define YAP6_LIST_LOOKP(value,index) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->LOOKP(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

#define YAP6_LIST_EXIST(value,index) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->EXIST(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

#define YAP6_LIST_STORE(value,index,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index,\
                                              (YAP6__CORE__Value*)new\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->STORE(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index,\
                                              (YAP6__CORE__Value*)new))

#define YAP6_LIST_DELET(value,index) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->DELET(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

#define YAP6_LIST_ELEMS(value,index) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((YAP6__CORE__ListDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index\
                                           ):\
                                           ((YAP6__CORE__ListDispatcher*)value)->ELEMS(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__int*)index))

#define YAP6_SCALAR_FETCH(value,wants) (value->dispatcher?\
                                           ((YAP6__CORE__ScalarDispatcher*)value->dispatcher)->FETCH(\
                                              ((YAP6__CORE__Dispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)wants\
                                           ):\
                                           ((YAP6__CORE__ScalarDispatcher*)value)->FETCH(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)wants))

#define YAP6_SCALAR_STORE(value,newvalue) (value->dispatcher?\
                                           ((YAP6__CORE__ScalarDispatcher*)value->dispatcher)->STORE(\
                                              ((YAP6__CORE__ScalarDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newvalue\
                                           ):\
                                           ((YAP6__CORE__ScalarDispatcher*)value)->STORE(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newvalue))

#define YAP6_PAIR_GTKEY(value) (value->dispatcher?\
                                           ((YAP6__CORE__PairDispatcher*)value->dispatcher)->GTKEY(\
                                              ((YAP6__CORE__Dispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value\
                                           ):\
                                           ((YAP6__CORE__PairDispatcher*)value)->GTKEY(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_PAIR_GTVAL(value) (value->dispatcher?\
                                           ((YAP6__CORE__PairDispatcher*)value->dispatcher)->GTVAL(\
                                              ((YAP6__CORE__Dispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value\
                                           ):\
                                           ((YAP6__CORE__PairDispatcher*)value)->GTVAL(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value))

#define YAP6_PAIR_STVAL(value,newval) (value->dispatcher?\
                                           ((YAP6__CORE__PairDispatcher*)value->dispatcher)->STVAL(\
                                              ((YAP6__CORE__Dispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newval\
                                           ):\
                                           ((YAP6__CORE__PairDispatcher*)value)->STVAL(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)newval))

#define YAP6_HASH_LOOKP(value,key) (value->dispatcher?\
                                           value->dispatcher->LOOKP(\
                                              ((YAP6__CORE__HashDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key\
                                           ):\
                                           ((YAP6__CORE__HashDispatcher*)value)->LOOKP(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key))

#define YAP6_HASH_EXIST(value,key) (value->dispatcher?\
                                           value->dispatcher->EXIST(\
                                              ((YAP6__CORE__HashDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key\
                                           ):\
                                           ((YAP6__CORE__HashDispatcher*)value)->EXIST(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key))

#define YAP6_HASH_STORE(value,key,new) (value->dispatcher?\
                                           value->dispatcher->STORE(\
                                              ((YAP6__CORE__HashDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key,\
                                              (YAP6__CORE__Value*)new\
                                           ):\
                                           ((YAP6__CORE__HashDispatcher*)value)->STORE(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key,\
                                              (YAP6__CORE__Value*)new))

#define YAP6_HASH_DELET(value,key) (value->dispatcher?\
                                           value->dispatcher->DELET(\
                                              ((YAP6__CORE__HashDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key\
                                           ):\
                                           ((YAP6__CORE__HashDispatcher*)value)->DELET(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key))

#define YAP6_HASH_ELEMS(value,key) (value->dispatcher?\
                                           value->dispatcher->ELEMS(\
                                              ((YAP6__CORE__HashDispatcher*)value)->dispatcher,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key\
                                           ):\
                                           ((YAP6__CORE__HashDispatcher*)value)->ELEMS(\
                                              (YAP6__CORE__Dispatcher*)value,\
                                              (YAP6__CORE__Value*)value,\
                                              (YAP6__CORE__Value*)key))
