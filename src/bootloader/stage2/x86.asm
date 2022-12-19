bits 16

section _TEXT class=CODE

; void _cdecl x86_div64_32(uint64_t dividend, uint32_t divisor, uint64_t* qutientOut, uint32_t* remainderOut);

global _x86_div64_32
_x86_div64_32:
    push bp             ; save old call frame
    mov bp, sp          ; initialize new call frame

    push bx

    ; ------------------- divide upper 32 bits
    mov eax, [bp + 8]   ; eax = upper 32 bits of dividend
    mov ecx, [bp + 12]  ; ecx = divisor 
    xor edx, edx
    div ecx             ; eax = quot, edx = remainder

    ; ------------------- store upper 32 bits of quotient
    mov bx, [bp + 16]
    mov [bx + 4], eax

    ; ------------------- divide lower 32 bits
    mov eax, [bp + 4]   ; eax = lower 32 bits of dividend, edx = old remainder
    div ecx

    ; ------------------- store results
    mov [bx], eax
    mov bx, [bp + 18]
    mov [bx], edx

    pop bx

    pop bx              ; restore bx
    mov sp, bp          ; restore old call frame
    pop bp
    ret


; int 10h ah=0Eh. Args:
;   - character
;   - page

global _x86_Video_WriteCharTeletype
_x86_Video_WriteCharTeletype:
    push bp             ; save old call frame
    mov bp, sp          ; initialize new call frame

    push bx             ; save bx

    ; [bp + 0] - old call frame
    ; [bp + 2] - return address (small mem model => 2 bytes)
    ; [bp + 4] - first arg (character)
    ; [bp + 6] - second arg (page)
    ; NOTE: bytes are converted to words (you can`t push a single byte on the stack)
    mov ah, 0Eh
    mov al, [bp + 4]
    mov bh, [bp + 6]

    int 10h
    
    pop bx              ; restore bx
    mov sp, bp          ; restore old call frame
    pop bp
    ret
    
