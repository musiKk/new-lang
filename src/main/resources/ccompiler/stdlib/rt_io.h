#ifndef __rt_io__
#define __rt_io__

#include "rt_primitives.h"
#include "native_io.h"

void print(String s) {
    native_print(s->chars);
}

#endif
