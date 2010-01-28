#include <stdio.h>
#include <time.h>

int main()
{
    time_t t;

    t = 0;
    printf("0x%08x: %s", (unsigned)t, ctime(&t));
    t = time(NULL);
    printf("0x%08x: %s", (unsigned)t, ctime(&t));
    t = 0x7fffffff;
    printf("0x%08x: %s", (unsigned)t, ctime(&t));
    t = 0x80000000;
    printf("0x%08x: %s", (unsigned)t, ctime(&t));
    t = 0xffffffff;
    printf("0x%08x: %s", (unsigned)t, ctime(&t));
    return 0;
}
