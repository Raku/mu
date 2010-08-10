#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <dlfcn.h>

#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <smop/mold.h>
#include <smop/s1p.h>
#include <smop/lost.h>
#include <smop/p6opaque.h>
#include <smop/s1p-oo.h>
#include <smop/yeast.h>
#include <smop/profile.h>
#include <smop/dump.h>
#include <smop/nagc.h>
#include <stdio.h>

void smop_p5_init(SMOP__Object* interpreter);
void smop_p5_destr(SMOP__Object* interpreter);

void* load_so(char* filename) {
  void* handle = dlopen(filename,RTLD_LAZY | RTLD_GLOBAL);
  if (!handle) {
    printf("Loader.load(%s): %s\n",filename,dlerror());
    abort();
  }
}

MODULE = SMOP::Embed		PACKAGE = SMOP::Embed

void
load(char* filename)
  CODE:
    load_so(filename);

void
load_and_run(char* filename)
  CODE:
    void* handle = load_so(filename);
    void* (*run)() = dlsym(handle,"run");
    run();
