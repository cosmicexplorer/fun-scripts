#include <stdio.h>
#include <pthread.h>

#include "tlclasses.h"

TL_EXPOSE_VAR int x1234;

TL_EXPOSE_VAR char test_char;

void *thread_func(void *p)
{
	++test_char;
	++*((int *)p); // example of where this is unhelpful
        return NULL;
}

int main(int argc, char **argv)
{
	x1234 = 2;
	test_char = 'd';
	printf("%d\n", x1234);
	pthread_t p;
	pthread_create(&p, NULL, thread_func, &x1234); // but the access may be
                                                       // caught here
	++test_char;
        pthread_join(p, NULL);
	printf("hello world!\n");
}
