define	os_lowercaseVar		$D000A4

esp:
    .main:
        ; LibLoad launcher: https://github.com/CE-Programming/libload/blob/master/setup/setup.md  
        ld hl, os_lowercaseVar
		set 3, (hl)

		ld	bc, HelloText ; description
		push	bc
		ld	bc, 75 ; Priority - hooks with a lower value are called first
		push	bc
		ld	bc, HOOK_TYPE_PARSER
		push	bc
		ld	bc, p_hook.size ; Hook size
		push	bc
		ld	bc, p_hook
		push	bc
		ld	bc,$FF0000 ; Globally unique ID - see https://github.com/commandblockguy/capnhook/wiki/Hook-ID-Registry
		push	bc
		call	hook_Install
		pop	bc,bc,bc,bc,bc,bc

		call	hook_Sync ; Apply changes and activate hooks

		ld	iy,ti.flags ; Preserve flags so that the OS doesn't get crashy

        set	0,(iy-flag_continue) ; Don't return a value to TI-OS
        ret  

p_hook:
    db $83

    push hl,de,ix,iy,bc,af

    ; ; open debugger
    ; scf
    ; sbc    hl,hl
    ; ld     (hl),2

    ; check to make sure this is a homescreen equation
    ld hl, (ti.basic_prog)
    or a, a
    ld de, $2305
    sbc hl, de
    jr nz, .earlyExit


    or a, a ; reset carry flag

    ld c, 128
    push bc

    .hookStart:
        jr nc, .loadPC

        pop bc
        inc c
        push bc
        jr z, .earlyExit

        ld b, 1                 ; 2 cycles high for start bit        
        out0	($0C), b        ; 4 cycles  
        
        ; wait for esp32 to wake up then resend message
        ld a, 64
        .delayLoop:
            dec a
            jr nz, .delayLoop

        ld b, 0
        out0    ($0C), b

        .loadPC:
            ld hl, (ti.begPC)  ; begPC is a uint8_t**
            ld a, (hl)
            sub a, $5F ; check if it's a prgm token 
            jr z, .earlyExit
            ld de, (ti.curPC)
            or a, a
            push hl
            sbc hl, de
            pop hl
            jr z, .continueSend

            .earlyExit:
                ; not first parser call, exit hook
                ld b, 0 ; set output pin to low
                out0	($0C), b 
                
                pop bc,af,bc,iy,ix,de,hl
                set	0,(iy-flag_continue) ; Don't return a value to TI-OS
                ret

        .continueSend:    
            di

            ld b, 0 ; set output pin to low
            out0	($0C), b 

            ld de, (ti.endPC)
            inc de
            
            .sendLoop:
                ld b, 1                 ; 2 cycles high for start bit        
                out0	($0C), b        ; 4 cycles  
                ld a, (hl)
                ld c, 8
                nop
                nop
                .sendBit:
                    out0 ($0C), a
                    srl a
                    dec c
                    jr nz, .sendBit
                    ld b, 0                ; 2 cycles low for stop bit
                    out0	($0C), b       ; 4 cycles 
            .afterSend:
                inc hl
                or a, a ; reset carry flag
                push hl
                sbc hl, de
                pop hl
                jr nz, p_hook.sendLoop   
                
                ; PC send finished

                ld de, 1
                ld hl, $FFFFFF - $5000

                ; wait for line to go high
                .inWait:
                    add hl, de
                    jr c, .hookStart   ; wait 5000h cycles and if it hasn't gone high, resend the message
                    in0 b, ($0B)
                    bit 0, b  
                    jr z, .inWait

                pop bc

                ; ; open debugger
                ; scf
                ; sbc    hl,hl
                ; ld     (hl),2

                ; store received data in pixelShadow2. 8400 bytes available
                ; https://wikiti.brandonw.net/index.php?title=Category:84PCE:RAM:By_Address
                ld iy, ti.pixelShadow2 - 2
                ld de, 2 
                ld c, 2

                ; wait for start bit
                .inStartBit:
                    in0 a, ($0B)
                    bit 0, a
                    jr nz, .inStartBit

                .inBitSetup:
                    ld b, 2

                    ; delay to get to center of uart waveform
                    nop
                    nop
                    nop

                .inBit:
                    in0 a, ($0B)
                    and a, 1
                    or a, b
                    ld b, a
                    sla b
                    jr nc, .inBit

                    ; one more time
                    nop
                    in0 a, ($0B)
                    and a, 1
                    or a, b
                    ld b, a
                    nop
                    nop

                ld (iy), b
                inc iy
                ld hl, 0
                dec de
                sbc hl, de                
                jr nz, .inStartBit

                ; de reached 0 for first time, load received packet size back into de   
                dec c
                jr z, .inDone

               
                or a, a ; reset carry flag

                ld e, (iy - 2)
                ld d, (iy - 1)
                dec de

                ; check for invalid packet size
                ld hl, 8400 - 1
                sbc hl, de
                inc de
                jr nc, .inStartBit

            .inDone:
                ld (iy), 0 ; null terminate
                ei
            .end:
                ld b, 0 ; set output pin to low
                out0	($0C), b 

                pop af,bc,iy,ix,de,hl

                ld	hl, ti.pixelShadow2                
                ld	a, $0C
                ld	bc, 0
                ld	iy, ti.flags
                call	$21890 ; https://wikiti.brandonw.net/index.php?title=84PCE:Syscalls:021890


                res	0,(iy-flag_continue)
                res	ti.donePrgm,(iy+ti.doneFlags)

                xor	a,a
	            inc	a

                ret

    .size = $ - p_hook

    HelloText:
        db "Hello world!",0

; Strings for the LibLoad launcher
__missingappvar:
	db	"Need"
__libloadappvar:
	db	" LibLoad",0
