#ifndef JK_ARENA_H
#define JK_ARENA_H

#ifdef __clang__
#define JK_ARENA_UNUSED __attribute__((__unused__))
#else
#define JK_ARENA_UNUSED
#endif

#ifndef JK_ARENA_API
#define JK_ARENA_API JK_ARENA_UNUSED static
#endif

typedef struct Arena {
    char            *ptr;
    char            *end;
    size_t          blockCount;
    size_t          blockCountMax;
    char            **blocks;
} Arena;

JK_ARENA_API void *arena_alloc(Arena *arena, size_t size);
JK_ARENA_API void  arena_free(Arena *arena);

#endif /* JK_ARENA_H */

#ifdef JK_ARENA_IMPLEMENTATION

#ifndef JK_ARENA_ALIGNMENT
#define JK_ARENA_ALIGNMENT 8
#endif

#ifndef JK_ARENA_BLOCK_SIZE
#define JK_ARENA_BLOCK_SIZE 4096
#endif

#ifndef JK_ARENA_MALLOC
#define JK_ARENA_MALLOC malloc
#endif

#ifndef JK_ARENA_REALLOC
#define JK_ARENA_REALLOC realloc
#endif

#ifndef JK_ARENA_FREE
#define JK_ARENA_FREE free
#endif

#ifndef JK_ARENA_ASSERT
#define JK_ARENA_ASSERT(cond)
#endif

#define JK_ARENA_MAX(a,b) ((a) > (b) ? (a) : (b))
#define JK_ARENA_ALIGN_DOWN(n,a) ((n) & ~((a) -1))
#define JK_ARENA_ALIGN_UP(n,a) JK_ARENA_ALIGN_DOWN((n)+(a)-1, (a))
#define JK_ARENA_ALING_DOWN_PTR(p,a) ((void*)(JK_ARENA_ALIGN_DOWN((uintptr_t)(p), (a))))
#define JK_ARENA_ALING_UP_PTR(p,a) ((void*)(JK_ARENA_ALIGN_UP((uintptr_t)(p), (a))))

JK_ARENA_API void *arena_alloc(Arena *arena, size_t size) {
    void *ptr;
    if(size > (size_t)(arena->end - arena->ptr)) {
        size_t allocSize = JK_ARENA_ALIGN_UP(JK_ARENA_MAX(size, JK_ARENA_BLOCK_SIZE), JK_ARENA_ALIGNMENT);
        arena->ptr = JK_ARENA_MALLOC(allocSize);
        arena->end = arena->ptr + allocSize;
        JK_ARENA_ASSERT(JK_ARENA_ALING_DOWN_PTR(arena->ptr, JK_ARENA_ALIGNMENT) == arena->ptr);
        if(arena->blockCount >= arena->blockCountMax) {
            arena->blockCountMax = JK_ARENA_MAX(arena->blockCountMax*2, arena->blockCount+1);
            arena->blocks = JK_ARENA_REALLOC(arena->blocks, arena->blockCountMax * sizeof(char*));
        }
        arena->blocks[arena->blockCount++] = arena->ptr;
    }
    ptr = arena->ptr;
    arena->ptr = JK_ARENA_ALING_UP_PTR(arena->ptr + size, JK_ARENA_ALIGNMENT);
    JK_ARENA_ASSERT(arena->ptr >= ((char*)ptr + size) && arena->ptr < ((char*)ptr + size + JK_ARENA_ALIGNMENT));
    return ptr;
}

JK_ARENA_API void arena_free(Arena *arena) {
    size_t i;

    if(arena) {
        for(i = 0; i < arena->blockCount; i++) {
            JK_ARENA_FREE(arena->blocks[i]);
        }
        JK_ARENA_FREE(arena->blocks);

        arena->ptr = NULL;
        arena->end = NULL;
        arena->blocks = NULL;
        arena->blockCount = 0;
        arena->blockCountMax = 0;
    }
}

#endif /* JK_ARENA_IMPLEMENTATION */
