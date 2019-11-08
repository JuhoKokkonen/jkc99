#ifndef JK_DYNAMIC_ARRAY_H
#define JK_DYNAMIC_ARRAY_H

#ifndef JK_DYNAMIC_ARRAY_MAX
#define JK_DYNAMIC_ARRAY_MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef JK_DYNAMIC_ARRAY_MIN
#define JK_DYNAMIC_ARRAY_MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef JK_DYNAMIC_ARRAY_ASSERT
#define JK_DYNAMIC_ARRAY_ASSERT(cond) ((cond) ? 0 : ((*(volatile int*)0) = 123), 0)
#endif

#ifndef JK_DYNAMIC_ARRAY_SIZE_T
#ifndef __cplusplus
#if __STDC_VERSION__ < 199901L
#define JK_DYNAMIC_ARRAY_SIZE_T unsigned int
#else
#define JK_DYNAMIC_ARRAY_SIZE_T size_t
#endif
#endif
#endif

#ifdef __clang__
#define JK_DYNAMIC_ARRAY_UNUSED __attribute__((__unused__))
#else
#define JK_DYNAMIC_ARRAY_UNUSED
#endif

#ifdef JK_DYNAMIC_ARRAY_EXTERN
#define JK_DYNAMIC_ARRAY_DEF JK_DYNAMIC_ARRAY_UNUSED extern
#else
#define JK_DYNAMIC_ARRAY_DEF JK_DYNAMIC_ARRAY_UNUSED static
#endif

#ifndef JK_DYNAMIC_ARRAY_REALLOC
#define JK_DYNAMIC_ARRAY_REALLOC realloc
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMCPY
#include <string.h>
#define JK_DYNAMIC_ARRAY_MEMCPY memcpy
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMSET
#include <string.h>
#define JK_DYNAMIC_ARRAY_MEMSET memset
#endif

#ifndef JK_DYNAMIC_ARRAY_NO_PRINTF
#ifndef JK_DYNAMIC_ARRAY_VSNPRINTF
#include <stdio.h>
#define JK_DYNAMIC_ARRAY_VSNPRINTF vsnprintf
#endif
#endif

#define da_count_internal(arr) ((((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 2)[0])
#define da_allocated_internal(arr) ((((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 1)[0])
#define da_need_to_grow_internal(arr,n) (!(arr) || (da_count_internal(arr) + (n)) >= da_allocated_internal(arr))
#define da_maybe_grow_internal(arr,n) (da_need_to_grow_internal((arr), (n)) ? (*((void**)&(arr)) = da_grow_internal((void*)(arr), (n), sizeof(*(arr)))) : 0)

#define da_init(arr,n) ((arr) = da_grow_internal((arr), (n), sizeof(*(arr))))
#define da_count(arr) ((arr) ? da_count_internal(arr) : 0)
#define da_allocated(arr) ((arr) ? da_allocated_internal(arr) : 0)
#define da_last(arr) ((arr)[da_count_internal(arr) - 1])
#define da_push(arr,...) (da_maybe_grow_internal((arr), 1), (arr)[da_count_internal(arr)++] = (__VA_ARGS__))
#define da_pushn(arr,n) (da_maybe_grow_internal((arr), n), da_count_internal(arr) += n)
#define da_pop(arr) (da_count(arr) ? JK_DYNAMIC_ARRAY_MEMSET((arr) + (--da_count_internal(arr)), 0, sizeof(*(arr))), 0 : 0)
#define da_popn(arr,n) (da_count(arr) ? JK_DYNAMIC_ARRAY_MEMSET((arr) + (da_count_internal(arr) - JK_DYNAMIC_ARRAY_MIN((n), da_count_internal(arr))), 0, sizeof(*(arr))*(JK_DYNAMIC_ARRAY_MIN((n), da_count_internal(arr)))), da_count_internal(arr) -= JK_DYNAMIC_ARRAY_MIN((n), da_count_internal(arr)), 0 : 0)
#define da_free(arr) ((arr) ? free(((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 2), (arr) = 0, 0 : 0)
#define da_clear(arr) (da_count(arr) ? JK_DYNAMIC_ARRAY_MEMSET((arr), 0, sizeof(*(arr))*da_count_internal(arr)), da_count_internal(arr) = 0, 0 : 0);
#define da_grow(arr,n) (((arr) = da_grow_internal((arr), (n), sizeof(*(arr)))), da_count_internal(arr) += (n))
#define da_remove_swap_last(arr,index) ((((index) >= 0) && (index) < da_count_internal(arr)) ? (JK_DYNAMIC_ARRAY_MEMCPY((arr) + (index), &da_last(arr), sizeof(*(arr))), --da_count_internal(arr)) : 0)
#define da_sizeof(arr) (sizeof(*(arr)) * da_count(arr))

JK_DYNAMIC_ARRAY_DEF void *da_grow_internal(void *arr, JK_DYNAMIC_ARRAY_SIZE_T count, JK_DYNAMIC_ARRAY_SIZE_T size) {
    void *res = 0;
    JK_DYNAMIC_ARRAY_SIZE_T allocSize;
    JK_DYNAMIC_ARRAY_SIZE_T allocCount;

    allocCount = JK_DYNAMIC_ARRAY_MAX(2 * da_count(arr), da_count(arr) + count);
    allocSize = 2 * sizeof(JK_DYNAMIC_ARRAY_SIZE_T) + allocCount*size;
    if(arr) {
#if 0
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)JK_DYNAMIC_ARRAY_REALLOC(((JK_DYNAMIC_ARRAY_SIZE_T*)arr)-2, allocSize);
#else
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)malloc(allocSize);
        if(ptr) {
            JK_DYNAMIC_ARRAY_MEMCPY(ptr, ((JK_DYNAMIC_ARRAY_SIZE_T*)arr) - 2, da_count(arr)*size + 2*sizeof(JK_DYNAMIC_ARRAY_SIZE_T));
            //free(((JK_DYNAMIC_ARRAY_SIZE_T*)arr) - 2);
            da_free(arr);
        }
#endif
        if(ptr) {
            JK_DYNAMIC_ARRAY_SIZE_T zeroSize = allocSize - 2*sizeof(JK_DYNAMIC_ARRAY_SIZE_T) - ptr[0]*size;
            JK_DYNAMIC_ARRAY_MEMSET(((char*)ptr) + (allocSize - zeroSize), 0, zeroSize);
            res = ptr + 2;
            da_allocated_internal(res) = allocCount;
        } else {
            JK_DYNAMIC_ARRAY_ASSERT(0);
        }
    } else {
#if 0
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)JK_DYNAMIC_ARRAY_REALLOC(0, allocSize);
#else
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)malloc(allocSize);
#endif
        if(ptr) {
            res = ptr + 2;
            JK_DYNAMIC_ARRAY_MEMSET(ptr, 0, allocSize);
            da_count_internal(res) = 0;
            da_allocated_internal(res) = allocCount;
        } else {
            JK_DYNAMIC_ARRAY_ASSERT(0);
        }
    }

    JK_DYNAMIC_ARRAY_ASSERT(res);
    return res;
}

#ifndef JK_DYNAMIC_ARRAY_NO_PRINTF
//TODO This needs to checked for portability. I'm not sure what the status of va_copy is on msvc for example.
JK_DYNAMIC_ARRAY_DEF int da_printf_internal(char **buf, const char *format, ...) {
    va_list va, va2;
    va_start(va, format);
    va_copy(va2, va);
    int len = JK_DYNAMIC_ARRAY_VSNPRINTF(NULL, 0, format, va);
    va_end(va);
    if(len > 0) {
        int res;
        da_maybe_grow_internal(*buf, len+1);
        res = JK_DYNAMIC_ARRAY_VSNPRINTF(*buf+da_count(*buf), len+1, format, va2);
        JK_DYNAMIC_ARRAY_ASSERT(res == len);
        da_count_internal(*buf) += res;
        return res;
    } else if(len < 0) {
        JK_DYNAMIC_ARRAY_ASSERT(0);
    }
    va_end(va2);
    return len;
}

#define da_printf(buf,...) da_printf_internal(&(buf), __VA_ARGS__)
#endif

#endif /* JK_DYNAMIC_ARRAY_H */
