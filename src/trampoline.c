#define _GNU_SOURCE
#include <sys/mman.h>
#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include "trampoline.h"
#include "gram.h"
#include <errno.h>

#include <dlfcn.h>

char *instruction_buffer = NULL;
int instruction_buffer_size = 0;
void *from_addr;
FILE* logfile = NULL;

/* Returns 0 on success, -1 on failure */
int insert_trampoline(void *from_addr, void *to_addr) {
    int rc;

    if (!from_addr || !to_addr)
        return -1;

#if defined(__x86_64__)
    unsigned char asm_jump[] = {// mov    $0x1234567812345678,%rax
                                0x48, 0xb8, 0x78, 0x56, 0x34, 0x12, 0x78, 0x56,
                                0x34, 0x12,
                                // jmpq   *%rax
                                0xff, 0xe0};
    // Beginning of address in asm_jump:
    const int addr_offset = 2;
#elif defined(__i386__)
    static unsigned char asm_jump[] = {
        0xb8, 0x78, 0x56, 0x34, 0x12, // mov    $0x12345678,%eax
        0xff, 0xe0                    // jmp    *%eax
    };
    // Beginning of address in asm_jump:
    const int addr_offset = 1;
#else
    Rf_error("Architecture not supported");
#endif

    fprintf(logfile, "Here1\n");
    fflush(logfile);
    void *page_base = (void *)ROUND_DOWN(from_addr);
    size_t page_length = PAGE_SIZE;
    if ((VA)from_addr + sizeof(asm_jump) - (VA)page_base > PAGE_SIZE) {
        // The patching instructions cross page boundary. View page as double
        // size.
        page_length = 2 * PAGE_SIZE;
    }

    // Temporarily add write permissions
    //printf("PID: %d\n\n", getpid());
    fprintf(logfile, "Here2\n");
    fflush(logfile);
    rc = mprotect(page_base, page_length, PROT_READ | PROT_WRITE | PROT_EXEC);
    fprintf(logfile, "Here3\n");
    fflush(logfile);
    if (rc < 0) {
      static int loopy = 1;
        while (loopy) {
            ;
        }
        Rf_error("mprotect failed for %p at %zu. Error: %s\n", page_base,
                 page_length, strerror(errno));
        return -1;
    }

    if(instruction_buffer == NULL) {
        fprintf(logfile, "Here4\n");
        fflush(logfile);
        instruction_buffer_size = sizeof(asm_jump);
        instruction_buffer = (char *)malloc(sizeof(asm_jump));
        memcpy(instruction_buffer, from_addr, sizeof(asm_jump));
        fprintf(logfile, "Here5\n");
        fflush(logfile);
    }

    fprintf(logfile, "Here6\n");
    fflush(logfile);
    // Now, do the patching
    memcpy(from_addr, asm_jump, sizeof(asm_jump));
    memcpy((VA)from_addr + addr_offset, (void *)&to_addr, sizeof(&to_addr));
    fprintf(logfile, "Here7\n");
    fflush(logfile);

    // Finally, remove the write permissions
    rc = mprotect(page_base, page_length, PROT_READ | PROT_EXEC);
    fprintf(logfile, "Here8\n");
    fflush(logfile);
    if (rc < 0) {
        static int loopy = 1;
        while (loopy) {
            ;
        }
        Rf_error("mprotect failed: %s\n", strerror(errno));
        return -1;
    }
    return rc;
}

SEXP override_parser() {
    /* Opening a file in r mode*/
    logfile = fopen("/tmp/logfile", "w");
    // fprintf(stderr, "%s\n", dlerror());
    fprintf(logfile, "before\n");
    fflush(logfile);
    from_addr = dlsym(RTLD_DEFAULT, "Rf_yyparse");
    fprintf(logfile, "after\n");
    fflush(logfile);
    //static int loopy = 1;
    //while (loopy) {
    //    ;
    //}
    //fprintf(stderr, "%s\n", dlerror());
    if(from_addr != NULL) {
      /* static int loopy = 1; */
      /*   while (loopy) { */
      /*       ; */
      /*   } */
        insert_trampoline(from_addr, yyparse);
    }
    fprintf(logfile, "Here9\n");
    fflush(logfile);
    return mkString("DONE!");
}

SEXP reset_parser() {
    fprintf(logfile, "Here10\n");
    fflush(logfile);
    memcpy(from_addr, instruction_buffer, instruction_buffer_size);
    fprintf(logfile, "Here11\n");
    fflush(logfile);
    fclose(logfile);
    return mkString("DONE!");
}
