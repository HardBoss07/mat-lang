#include <locale.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#ifdef _WIN32
#include <windows.h>
#endif

#ifdef MAT_USE_GC
#include <gc.h>
#define MAT_MALLOC(sz) GC_malloc_atomic(sz)
#else
#define MAT_MALLOC(sz) malloc(sz)
#endif

typedef struct {
  void *data;
  size_t len;
  size_t capacity;
  size_t elem_size;
} MatVector;

MatVector *_mat_rt_vec_create(size_t elem_size, size_t initial_capacity) {
  MatVector *vec = (MatVector *)MAT_MALLOC(sizeof(MatVector));
  if (!vec)
    return NULL;

  vec->elem_size = elem_size;
  vec->len = 0;
  vec->capacity = initial_capacity > 0 ? initial_capacity : 4;
  vec->data = MAT_MALLOC(vec->capacity * elem_size);
  if (!vec->data && vec->capacity > 0) {
    return NULL;
  }
  return vec;
}

void _mat_rt_vec_push(MatVector *vec, void *elem_ptr) {
  if (!vec)
    return;

  if (vec->len >= vec->capacity) {
    size_t new_cap = vec->capacity == 0 ? 4 : vec->capacity * 2;
    void *new_data = MAT_MALLOC(new_cap * vec->elem_size);
    if (vec->data && vec->len > 0) {
      memcpy(new_data, vec->data, vec->len * vec->elem_size);
    }
    vec->data = new_data;
    vec->capacity = new_cap;
  }

  if (elem_ptr && vec->data) {
    memcpy((char *)vec->data + (vec->len * vec->elem_size), elem_ptr,
           vec->elem_size);
  }
  vec->len++;
}

void _mat_rt_vec_pop(MatVector *vec, void *out_elem_ptr) {
  if (!vec || vec->len == 0) {
    fprintf(stderr, "Runtime error: pop from empty vector\n");
    return;
  }

  vec->len--;
  if (out_elem_ptr && vec->data) {
    memcpy(out_elem_ptr, (char *)vec->data + (vec->len * vec->elem_size),
           vec->elem_size);
  }
}

void *_mat_rt_vec_get(MatVector *vec, size_t index) {
  if (!vec || index >= vec->len) {
    fprintf(stderr,
            "Runtime error: vector index %zu out of bounds (len: %zu)\n", index,
            vec ? vec->len : 0);
    return NULL;
  }
  return (char *)vec->data + (index * vec->elem_size);
}

size_t _mat_rt_vec_len(const MatVector *vec) {
  if (!vec)
    return 0;
  return vec->len;
}

void _mat_rt_init(void) {
#ifdef MAT_USE_GC
  GC_INIT();
#endif

#ifdef _WIN32
  SetConsoleOutputCP(65001);
  SetConsoleCP(65001);
#endif

  setlocale(LC_ALL, ".UTF-8");
}

char *_mat_rt_fmt_string(const char *fmt, ...) {
  if (!fmt) {
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

  if (len < 0) {
    va_end(args_copy);
    char *empty = (char *)MAT_MALLOC(1);
    if (empty)
      empty[0] = '\0';
    return empty;
  }

  char *buf = (char *)MAT_MALLOC((size_t)len + 1);
  if (!buf) {
    va_end(args_copy);
    return NULL;
  }

  vsnprintf(buf, len + 1, fmt, args_copy);
  va_end(args_copy);

  return buf;
}

char *_mat_rt_fmt_bin(long long val) {
  char buf[67];
  buf[0] = '0';
  buf[1] = 'b';
  if (val == 0) {
    buf[2] = '0';
    buf[3] = '\0';
  } else {
    unsigned long long uval = (unsigned long long)val;
    char tmp[65];
    int pos = 0;
    while (uval > 0) {
      tmp[pos++] = (uval & 1) ? '1' : '0';
      uval >>= 1;
    }
    int out_pos = 2;
    for (int i = pos - 1; i >= 0; i--) {
      buf[out_pos++] = tmp[i];
    }
    buf[out_pos] = '\0';
  }

  size_t len = strlen(buf);
  char *res = (char *)MAT_MALLOC(len + 1);
  if (res) {
    memcpy(res, buf, len + 1);
  }
  return res;
}

char *_mat_rt_fmt_hex(long long val) {
  char buf[64];
  snprintf(buf, sizeof(buf), "0x%llX", (unsigned long long)val);
  size_t len = strlen(buf);
  char *res = (char *)MAT_MALLOC(len + 1);
  if (res) {
    memcpy(res, buf, len + 1);
  }
  return res;
}

// Null-safe standard output helpers
void _mat_rt_print_str(const char *str) { fputs(str ? str : "<null>", stdout); }

void _mat_rt_println_str(const char *str) { puts(str ? str : "<null>"); }

void _mat_rt_println_int(long long val) { printf("%lld\n", val); }

void _mat_rt_println_float(double val) { printf("%g\n", val); }

void _mat_rt_println_bool(bool val) { puts(val ? "tru" : "fal"); }

// System I/O Runtime Helpers
char *_mat_rt_read_file(const char *path) {
  if (!path)
    return NULL;

  FILE *file = fopen(path, "rb");
  if (!file)
    return NULL;

  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  rewind(file);

  if (size < 0) {
    fclose(file);
    return NULL;
  }

  char *buffer = (char *)MAT_MALLOC((size_t)size + 1);
  if (!buffer) {
    fclose(file);
    return NULL;
  }

  size_t bytes_read = fread(buffer, 1, (size_t)size, file);
  buffer[bytes_read] = '\0';

  fclose(file);
  return buffer;
}

bool _mat_rt_write_file(const char *path, const char *content) {
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
