#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
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

char *_mat_rt_fmt_bin(long long val)
{
    char buf[67];
    buf[0] = '0';
    buf[1] = 'b';
    if (val == 0)
    {
        buf[2] = '0';
        buf[3] = '\0';
    }
    else
    {
        unsigned long long uval = (unsigned long long)val;
        char tmp[65];
        int pos = 0;
        while (uval > 0)
        {
            tmp[pos++] = (uval & 1) ? '1' : '0';
            uval >>= 1;
        }
        int out_pos = 2;
        for (int i = pos - 1; i >= 0; i--)
        {
            buf[out_pos++] = tmp[i];
        }
        buf[out_pos] = '\0';
    }

    size_t len = strlen(buf);
    char *res = (char *)MAT_MALLOC(len + 1);
    if (res)
    {
        memcpy(res, buf, len + 1);
    }
    return res;
}

char *_mat_rt_fmt_hex(long long val)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "0x%llX", (unsigned long long)val);
    size_t len = strlen(buf);
    char *res = (char *)MAT_MALLOC(len + 1);
    if (res)
    {
        memcpy(res, buf, len + 1);
    }
    return res;
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

// System I/O Runtime Helpers
char *_mat_rt_read_file(const char *path)
{
    if (!path)
        return NULL;

    FILE *file = fopen(path, "rb");
    if (!file)
        return NULL;

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    rewind(file);

    if (size < 0)
    {
        fclose(file);
        return NULL;
    }

    char *buffer = (char *)MAT_MALLOC((size_t)size + 1);
    if (!buffer)
    {
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(buffer, 1, (size_t)size, file);
    buffer[bytes_read] = '\0';

    fclose(file);
    return buffer;
}

bool _mat_rt_write_file(const char *path, const char *content)
{
    if (!path || !content)
        return false;

    FILE *file = fopen(path, "wb");
    if (!file)
        return false;

    size_t len = strlen(content);
    size_t written = fwrite(content, 1, len, file);
    fclose(file);

    return written == len;
}
