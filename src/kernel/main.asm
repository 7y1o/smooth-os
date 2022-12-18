org 0x7C00              ; set code start address
bits 16                 ; emit 16-bit code

%define ENDL 0x0D, 0x0A ; define line end code

start:                  ; entry point
    jmp main

; Prints a string to the screen. Params:
;   - ds:si points to the string
puts:                   
    push si
    push ax

.loop:
    lodsb               ; loads next character in al
    or al, al           ; verify if next character is null?
    jz .done            ; finish the work if next character is null

    mov ah, 0x0e        ; set TTY write function
    int 0x10            ; call video interrupt
    
    jmp .loop           ; call loop to write next character

.done:                  ; loop is finished
    pop ax
    pop si
    ret

main:                   ; main program section
    ; ------------------- setup data segments
    mov ax, 0           ; can`t write to DS/ES directly
    mov ds, ax
    mov es, ax

    ; ------------------- setup stack
    mov ss, ax
    mov sp, 0x7C00      ; stack grows downwards from where are loaded in mem

    ; ------------------- print message
    mov si, msg_hello
    call puts

    hlt

.halt:                  ; CPU halting section
    jmp .halt

msg_hello: db 'Hello, World!', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
