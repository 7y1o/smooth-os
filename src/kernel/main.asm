org 0x0                 ; set code start address
bits 16                 ; emit 16-bit code

%define ENDL 0x0D, 0x0A ; define line end code

start:                  ; entry point
    mov si, msg_hello
    call puts

.halt:
    cli
    hlt

; Prints a string to the screen. Params:
;   - ds:si points to the string
puts:                   
    push si
    push ax
    push bx

.loop:
    lodsb               ; loads next character in al
    or al, al           ; verify if next character is null?
    jz .done            ; finish the work if next character is null

    mov ah, 0x0e        ; set TTY write function
    mov bh, 0
    int 0x10            ; call video interrupt
    
    jmp .loop           ; call loop to write next character

.done:                  ; loop is finished
    pop bx
    pop ax
    pop si
    ret

msg_hello: db 'Hello, World!', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
