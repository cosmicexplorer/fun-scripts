#include <stddef.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

#define STACK_SIZE 16384

volatile int unoptimisable_ = 1;

struct cr_ {
    struct cr_ *next;
    jmp_buf ctx;
};

struct cr_ main_cr_;

struct cr_ *first_cr_ = &main_cr_;
struct cr_ *last_cr_ = &main_cr_;

#define go(fn) \
    do {\
        if(!setjmp(first_cr_->ctx)) {\
            char *stack = malloc(STACK_SIZE);\
            int anchor_[unoptimisable_];\
            char filler_[(char*)&anchor_ - (char*)(stack + STACK_SIZE)];\
            struct cr_ cr[unoptimisable_];\
            cr->next = first_cr_;\
            first_cr_ = cr;\
            char *stack_[unoptimisable_];\
            stack_[0] = stack;\
            fn;\
            free(stack_[0]);\
            first_cr_ = first_cr_->next;\
            longjmp(first_cr_->ctx, 1);\
        }\
    } while(0)

void yield(void) {
    if(first_cr_ == last_cr_)
        return;
    if(setjmp(first_cr_->ctx))
        return;
    struct cr_ *cr = first_cr_;
    first_cr_ = cr->next;
    cr->next = NULL;
    last_cr_->next = cr;
    last_cr_ = cr;
    longjmp(first_cr_->ctx, 1);
}

void foo(int count, const char *text) {
    int i;
    for(i = 0; i != count; ++i) {
        printf("%s\n", text);
        yield();
    }
}

int main() {
    go(foo(3, "a"));
    go(printf("Hello, %s!\n", "world"));
    go(foo(2, "b"));
    foo(5, "c");
    return 0;
}
