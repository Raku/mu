#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <dlfcn.h>

#include <smop/base.h>

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
