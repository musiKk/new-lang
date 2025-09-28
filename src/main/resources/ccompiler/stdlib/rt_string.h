#ifndef __rt_string__
#define __rt_string__

#include <string.h>
#include "rt.h"

struct String__struct {
    char *chars;
    size_t length;
};
typedef struct String__struct* String;

String String__native_new_copy(char *buf) {
    size_t len = strlen(buf);
    char *target_buf = malloc(sizeof(char) * (len + 1));
    strcpy(target_buf, buf);

    String ret = NEW(String);
    ret -> chars = target_buf;
    ret -> length = len;
    return ret;
}

#endif
