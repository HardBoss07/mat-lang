#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <locale.h>

#ifdef _WIN32
#include <windows.h>
#endif

#ifdef MAT_USE_GC
#include <gc.h>
#define MAT_MALLOC(sz) GC_malloc_atomic(sz)
#else
#define MAT_MALLOC(sz) malloc(sz)
#endif

void _mat_rt_init(void)
{
#ifdef MAT_USE_GC
    GC_INIT();
#endif

#ifdef _WIN32
    SetConsoleOutputCP(65001);
    SetConsoleCP(65001);
#endif

    setlocale(LC_ALL, ".UTF-8");
}

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
    fputs(str ? str : "<null>", stdout);
}

void _mat_rt_println_str(const char *str)
{
    puts(str ? str : "<null>");
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
