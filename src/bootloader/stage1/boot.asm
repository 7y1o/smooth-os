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
ebr_lab: db 'SMOOTH OS  '       ; volume label. 11 bytes, padded with space
ebr_sid: db '        '          ; system id. 8 bytes


;
; Code goes here
;

start:                          ; entry point
    ; --------------------------- setup data segments
    mov ax, 0                   ; can`t write to DS/ES directly
    mov ds, ax
    mov es, ax

    ; --------------------------- setup stack
    mov ss, ax
    mov sp, 0x7C00              ; stack grows downwards from where are loaded in mem

    ; some BIOS`es might start us at 07C0:0000 instead of 0000:7C00
    push es
    push word .after
    retf

.after:
    ; read smth from floppy disk
    ; BIOS should set DL to drive number
    mov [ebr_dn], dl

    ; --------------------------- show loading message
    mov si, msg_loading
    call puts

    ; --------------------------- read drive parameters
    push es
    mov ah, 08h
    int 13h
    jc floppy_error
    pop es

    and cl, 0x3F                ; remove top 2 bits
    xor ch, ch
    mov [bdb_spt], cx

    inc dh
    mov [bdb_hd], dh            ; head count

    ; --------------------------- read FAT root directory
    mov ax, [bdb_spf]           ; LBA of root dir = reserved + fats * sectors_per_fat
    mov bl, [bdb_fc]
    xor bh, bh
    mul bx                      ; ax = (fats * sectors_per_fat)
    add ax, [bdb_rs]            ; ax = LBA of root dir
    push ax

    ; --------------------------- compute size of root directory = (32 * number_of_entries) / bytes_per_sector
    mov ax, [bdb_dec]
    shl ax, 5                   ; ax *= 32
    xor dx, dx                  ; dx = 0
    div word [bdb_bps]          ; number of sectors we need to read

    test dx, dx
    jz .root_dir_after
    inc ax

.root_dir_after:
    mov cl, al                  ; cl = number of sectors to read = size of root dir
    pop ax                      ; ax = LBA of root dir
    mov dl, [ebr_dn]            ; dl = drive number
    mov bx, buffer              ; es:bx = buffer
    call disk_read

    ; --------------------------- search for kernel.bin
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_kernel_bin
    mov cx, 11                  ; compare up to 11 characters
    push di
    repe cmpsb
    pop di
    je .found_kernel

    add di, 32
    inc bx
    cmp bx, [bdb_dec]
    jl .search_kernel

    ; --------------------------- kernel not found
    jmp kernel_not_found_error

.found_kernel:
    ; --------------------------- di should have the address to the entry
    mov ax, [di + 26]           ; first logical cluster field (offset 26)
    mov [kernel_c], ax

    ; --------------------------- load FAT from disk into memory
    mov ax, [bdb_rs]
    mov bx, buffer
    mov cl, [bdb_spf]
    mov dl, [ebr_dn]
    call disk_read

    ; --------------------------- read kernel and process FAT chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
    mov ax, [kernel_c]

    ; --------------------------- SORRY FOR THIS HARDCODE
    add ax, 31                  ; first cluster = (cluster number - 2) * sectors_per_cluster + start_sector
                                ; start sector = reserved + fats + root dir size = 1 + 18 + 14 = 33
    mov cl, 1
    mov dl, [ebr_dn]
    call disk_read

    add bx, [bdb_bps]

    ; --------------------------- compute location of next cluster
    mov ax, [kernel_c]
    mov cx, 3
    mul cx
    mov cx, 2
    div cx                      ; ax = index of entry in FAT, dx = cluster mod 2

    mov si, buffer
    add si, ax
    mov ax, [ds:si]             ; read entry from FAT table at index ax

    or dx, dx
    jz .even

.odd:
    shr ax, 4
    jmp .next_cluster_after

.even:
    and ax, 0x0FFF

.next_cluster_after:
    cmp ax, 0x0FF8
    jae .read_finish

    mov [kernel_c], ax
    jmp .load_kernel_loop

.read_finish:
    mov dl, [ebr_dn]
    
    mov ax, KERNEL_LOAD_SEGMENT ; set segment registers
    mov ds, ax
    mov es, ax

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET
    jmp wait_key_and_reboot

    cli
    hlt

; Error handlers

floppy_error:
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

kernel_not_found_error:
    mov si, msg_kernel_not_found
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

; Prints a string to the screen. Params:
;   - ds:si points to the string
puts:                   
    push si
    push ax
    push bx

.loop:
    lodsb                       ; loads next character in al
    or al, al                   ; verify if next character is null?
    jz .done                    ; finish the work if next character is null

    mov ah, 0x0e                ; set TTY write function
    mov bh, 0
    int 0x10                    ; call video interrupt
    
    jmp .loop                   ; call loop to write next character

.done:                          ; loop is finished
    pop bx
    pop ax
    pop si
    ret

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


msg_loading:            db 'Loading...', ENDL, 0
msg_read_failed:        db 'Read from disk failed', ENDL, 0
msg_kernel_not_found:   db 'STAGE2.BIN file not found!', ENDL, 0

file_kernel_bin:        db 'STAGE2  BIN'
kernel_c:               dw 0

KERNEL_LOAD_SEGMENT     equ 0x2000
KERNEL_LOAD_OFFSET      equ 0x0000


times 510-($-$$) db 0
dw 0AA55h

buffer: