#ifndef __rt_string__
#define __rt_string__

#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include "rt.h"

struct String__struct {
    char *chars;
    size_t len;
};
typedef struct String__struct* String;

String String__native_new(char *buf) {
    String ret = NEW(String);
    ret -> chars = buf;
    ret -> len = strlen(buf);
    return ret;
}

String String__native_new_copy(char *buf) {
    size_t len = strlen(buf);
    char *target_buf = malloc(sizeof(char) * (len + 1));
    strcpy(target_buf, buf);

    String ret = NEW(String);
    ret -> chars = target_buf;
    ret -> len = len;
    return ret;
}

#define Int int_fast64_t

String Int__toString(int i) {
    int len = snprintf(NULL, 0, "%d", i);
    char *buf = malloc(sizeof(char) * (len + 1));
    sprintf(buf, "%d", i);
    return String__native_new(buf);
}

#endif
