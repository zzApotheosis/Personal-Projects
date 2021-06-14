# ----------------------------------------------------------------------------------------
# Writes "Hello, world" to the console using a C library. Runs on Linux or any other system
# that does not use underscores for symbols in its C library. To assemble and run:
#
#     gcc -c hello.s
#     ld -dynamic-linker /lib/ld-linux-x86-64.so.2 /usr/lib/crt1.o /usr/lib/crti.o -lc hello.o /usr/lib/crtn.o
#     ./a.out
# ----------------------------------------------------------------------------------------

    .global main
    
    .text
main:                               # This is called by C library's startup code
    mov     $message, %rdi          # First integer (or pointer) parameter in %rdi
    call    puts                    # puts(message)
    ret                             # Return to C library code
message:
    .asciz "Hello, world"            # asciz puts a 0 byte at the end
