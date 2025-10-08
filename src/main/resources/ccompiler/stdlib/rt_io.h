#ifndef __rt_io__
#define __rt_io__

#include "rt_primitives.h"
#include "native_io.h"

Void print(String s) {
    native_print(s->chars);
    return NULL;
}

#endif
