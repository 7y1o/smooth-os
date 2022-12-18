org 0x7C00                      ; set code start address
bits 16                         ; emit 16-bit code

%define ENDL 0x0D, 0x0A ; define line end code

;
; FAT12 header
;
jmp short start
nop

bdb_oem: db 'MSWIN4.1'          ; 8 bytes
bdb_bps: dw 512                 ; bytes per sector
bdb_spc: db 1                   ; sectors per cluster
bdb_rs:  dw 1                   ; reserved sectors
bdb_fc:  db 2                   ; fat count
bdb_dec: dw 0E0h                ; dir entries count
bdb_ts:  dw 2880                ; total sectors. 2880 * 512 = 1.44MB
bdb_mdt: db 0F0h                ; media descriptor type. F0 = 3.5" floppy disk
bdb_spf: dw 9                   ; sectors per fat
bdb_spt: dw 18                  ; sectors per track
bdb_hd:  dw 2                   ; heads 
bdb_hs:  dd 0                   ; hidden sectors
bdb_lsc: dd 0                   ; large sector count

; extended boot record
ebr_dn:  db 0                   ; driver number. 0x00 = floppy, 0x80 = hdd
         db 0                   ; reserved
ebr_sig: db 29h                 ; signature
ebr_vid: db 12h, 34h, 56h, 78h  ; serial number
ebr_lab: db 'SMOOTHOS   '       ; volume label. 11 bytes, padded with space
ebr_sid: db '        '          ; system id. 8 bytes


;
; Code goes here
;

start:                          ; entry point
    jmp main

; Prints a string to the screen. Params:
;   - ds:si points to the string
puts:                   
    push si
    push ax

.loop:
    lodsb                       ; loads next character in al
    or al, al                   ; verify if next character is null?
    jz .done                    ; finish the work if next character is null

    mov ah, 0x0e                ; set TTY write function
    int 0x10                    ; call video interrupt
    
    jmp .loop                   ; call loop to write next character

.done:                          ; loop is finished
    pop ax
    pop si
    ret

main:                           ; main program section
    ; --------------------------- setup data segments
    mov ax, 0                   ; can`t write to DS/ES directly
    mov ds, ax
    mov es, ax

    ; --------------------------- setup stack
    mov ss, ax
    mov sp, 0x7C00              ; stack grows downwards from where are loaded in mem

    ; read smth from floppy disk
    ; BIOS should set DL to drive number
    mov [ebr_dn], dl

    mov ax, 1                   ; LBA=1, second sector from disk
    mov cl, 1                   ; 1 sector to read
    mov bx, 0x7E00              ; data should be after the bootloader
    call disk_read

    ; --------------------------- print message
    mov si, msg_hello
    call puts

    cli
    hlt

; Error handlers

floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h                     ; wait for keypress
    jmp 0FFFFh:0                 ; jump to beginnig of BIOS, should reboot
    hlt

.halt:                          ; CPU halting section
    cli                         ; disable interrupts
    hlt

;
; Disk routines
;

; Converts an LBA adresses to a CHS addresses. Parameters:
;   - ax: LBA address
;  Returns:
;   - cx [bits 0-5]: sector number
;   - cx [bits 6-15]: cylinder
;   - dh: head

lba_to_chs:
    push ax
    push dx

    xor dx, dx                  ; dx = 0
    div word [bdb_spt]          ; ax = LBA / SectorsPerTrack
                                ; dx = LBA % SectorsPerTrack
    inc dx                      ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx    
    
    xor dx, dx                  ; dx = 0
    div word [bdb_hd]           ; ax = (LBA / SectorsPerTrack) / Heads = cylinder
                                ; dx = (LBA / SectorsPerTrack) % Heads = head
    mov dh, dl                  ; dl = head
    mov ch, al                  ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah                   ; put upper 2 bits of cylinder in CL
    
    pop ax
    mov dl, al                  ; restore DL
    pop ax
    ret

; Reads sectors from a disk. Parameters
;   - ax: LBA address
;   - cl: number of sectors to read (up to 128)
;   - dl: drive number
;   - es:bx: mem address where to store and read data

disk_read:
    push ax
    push bx
    push cx
    push dx
    push di

    push cx                     ; temporarily save CL (number of sectors to read)
    call lba_to_chs             ; compute CHS
    pop ax                      ; AL = number of sectors to read

    mov ah, 02h
    mov di, 3                   ; retry count

.retry:
    pusha                       ; save all registers, we don`t know what BIOS modifies
    stc                         ; set carry flag, some BIOS`es don`t set it
    int 13h                     ; carry flag cleared = success
    jnc .done                   ; jump if carry is not set

    ; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry

.fail:
    ; after all attempts are exhausted
    jmp floppy_error

.done:
    popa

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; Resets disk controller. Parameters:
;   - dl: drive number

disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret


msg_hello:          db 'Hello, World!', ENDL, 0
msg_read_failed:    db 'Read from disk failed', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
