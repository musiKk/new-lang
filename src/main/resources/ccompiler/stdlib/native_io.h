#ifndef  __native_io__
#define __native_io__

#import<stdio.h>

static inline void native_print(char* arg) {
    printf("%s", arg);
}

#endif
