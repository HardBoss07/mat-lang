#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#ifdef MAT_USE_GC
#include <gc.h>
#define MAT_MALLOC(sz) GC_malloc_atomic(sz)
#else
#define MAT_MALLOC(sz) malloc(sz)
#endif

// Must be called as the very first instruction in the compiled binary's main()
void _mat_rt_init(void)
{
#ifdef MAT_USE_GC
    GC_INIT();
#endif
}

// Dynamically calculates required string length and allocates buffer
char *_mat_rt_fmt_string(const char *fmt, ...)
{
    if (!fmt)
    {
        char *empty = (char *)MAT_MALLOC(1);
        if (empty)
            empty[0] = '\0';
        return empty;
    }

    va_list args, args_copy;
    va_start(args, fmt);
    va_copy(args_copy, args);

    int len = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    if (len < 0)
    {
        va_end(args_copy);
        char *empty = (char *)MAT_MALLOC(1);
        if (empty)
            empty[0] = '\0';
        return empty;
    }

    char *buf = (char *)MAT_MALLOC((size_t)len + 1);
    if (!buf)
    {
        va_end(args_copy);
        return NULL;
    }

    vsnprintf(buf, len + 1, fmt, args_copy);
    va_end(args_copy);

    return buf;
}

// Null-safe standard output helpers
void _mat_rt_print_str(const char *str)
{
    if (str)
    {
        fputs(str, stdout);
    }
    else
    {
        fputs("<null>", stdout);
    }
}

void _mat_rt_println_str(const char *str)
{
    if (str)
    {
        puts(str);
    }
    else
    {
        puts("<null>");
    }
}

void _mat_rt_println_int(long long val)
{
    printf("%lld\n", val);
}

void _mat_rt_println_float(double val)
{
    printf("%g\n", val);
}

void _mat_rt_println_bool(bool val)
{
    puts(val ? "tru" : "fal");
}
