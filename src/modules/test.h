/* This header should header should be included in any project that uses the jkc99 test module.
 * Any function of type JKC99_TEST with a name starting with JKC99_TEST_PREFIX will be collected by the test module.
 * All the collected tests can then be run by calling jkc99_test_run wherever appropriate.
 * The test functions can utilise the JKC99T-macros to test various conditions.
 * Success and failure is automatically handled by the test module and JKC99T-macros.
 * See unit_tests.c for some simple examples of usage.
 */

#ifndef JKC99_TEST_H
#define JKC99_TEST_H

#ifndef JKC99_TEST_PREFIX
#define JKC99_TEST_PREFIX jkc99t_
#endif

#define JKC99_TEST_CONCAT2(a,b) a##b
#define JKC99_TEST_CONCAT(a,b) JKC99_TEST_CONCAT2(a,b)

#ifndef JKC99_TEST
#define JKC99_TEST(name) int JKC99_TEST_CONCAT(JKC99_TEST_PREFIX,name)(int condIndex)
#endif

#include <stdio.h>

static inline void jkc99_test_fail(const char *test, int condIndex, const char *cond) {
    printf("    %-48s FAIL    (%s) condition %d\n",test,cond,condIndex);
}

#define JKC99T_FAIL(test,condIndex,cond) jkc99_test_fail(test,condIndex,cond); return 1

#define JKC99T(cond) \
    if(!(cond)) { \
        JKC99T_FAIL(__func__,condIndex,#cond); \
    } \
    condIndex++

#if 0
#define JKC99T_STRCMP(a,b) \
    if(strcmp(a, b) != 0) { \
        JKC99T_FAIL(__func__,condIndex,"STRCMP"); \
    } \
    condIndex++

#define JKC99T_STRNCMP(a,b,n) \
    if(strncmp(a, b, n) != 0) { \
        JKC99T_FAIL(__func__,condIndex,"STRNCMP"); \
    } \
    condIndex++

#define JKC99T_WCSCMP(a,b) \
    if(wcscmp(a, b) != 0) { \
        JKC99T_FAIL(__func__,condIndex,"WCSCMP"); \
    } \
    condIndex++

#define JKC99T_WCSNCMP(a,b,n) \
    if(wcsncmp(a, b, n) != 0) { \
        JKC99T_FAIL(__func__,condIndex,"WCSNCMP"); \
    } \
    condIndex++
#endif


static int jkc99_test_run(void);


#endif /* JKC99_TEST_H */
