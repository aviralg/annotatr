
#define PAGE_SIZE 0x1000ULL

// FIXME: 0x1000 is one page; use sysconf(PAGESIZE) instead.
#define ROUND_DOWN(x)                                                          \
    ((unsigned long long)(x) & ~(unsigned long long)(PAGE_SIZE - 1))
#define ROUND_UP(x)                                                            \
    (((unsigned long long)(x) + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1))
#define PAGE_OFFSET(x) ((x) & (PAGE_SIZE - 1))

typedef char *VA; /* VA = virtual address */

extern char * instruction_buffer;
extern int instruction_buffer_size;
extern void* from_addr;
extern FILE* logfile;

int insert_trampoline(void *from_addr, void *to_addr);

SEXP override_parser();

SEXP reset_parser();
