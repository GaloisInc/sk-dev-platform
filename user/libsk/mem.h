
#ifndef __LIBSK_MEM_H__
#define __LIBSK_MEM_H__

#include <sys/types.h>

struct mem {
    void *base;
    size_t size;
};

typedef struct mem* mem_p;

#endif
