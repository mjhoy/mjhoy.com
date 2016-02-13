#include <stdio.h>
#include "foo.h"

int foo(char *msg)
{
    printf("hi from foo: %s\n", msg);

    return 0;
}
