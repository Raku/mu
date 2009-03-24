#ifndef SMOP_UTIL_H
#define SMOP_UTIL_H
#define ___VALUE_FETCH___\
        SMOP_REFERENCE(interpreter,(SMOP__Object*) invocant);\
    ret = invocant;
#define ___VALUE_STORE___ \
        { \
                fprintf(stderr, "can't call STORE on an read-only value at %s line %d file %s\n",__func__,__LINE__,__FILE__); \
                abort(); \
                } 

#define ___UNKNOWN_METHOD___ \
        { \
                int identifier_size;\
                char* s = SMOP__NATIVE__idconst_fetch(identifier,&identifier_size);\
                fprintf(stderr,"unknown method \"%.*s\" at %s line %d file %s\n",identifier_size,s,__func__,__LINE__,__FILE__); \
                abort(); \
                } 
#endif

