		opt ae+						; set automatic even's to on
		opt w-						; disable warnings
		opt l.						; local lable symbol is . (dot)
		opt v+						; output local labels in .sym file
		opt ws+						; allow white spaces in operand parsing
		opt ow+						; optimize word addressing
		opt op+						; optimize pc relative addressing
		opt os+						; optimize short branches
		opt oz+						; optimize zero displacement

test_vgm =	0
		include "delta/files.mac"			; number of files in reality
		include "equ.mac"				; other equates
		nolist
		include "pcm/lang.asm"				; Z80 is nice
		list
; ==============================================================
; --------------------------------------------------------------
; ROM header
; --------------------------------------------------------------

	org 0
		dc.l 0,      Init,   exBus,  exAddr
		dc.l exIll,  exDiv,  exChk,  Trapv
		dc.l exPriv, exTrace,exLineA,exLineF
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l Hint,   exMisc, Vint,   exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc

hInitAregs:							; address registers for init routine
hZ80_Bus:	dc.l Z80_Bus				; a1	; Z80 bus request
hZ80_Reset:	dc.l Z80_Reset				; a2	; Z80 reset
hZ80_RAM:	dc.l Z80_RAM				; a3	; Z80 RAM start
hPAD_Control1:	dc.l PAD_Control1			; a4	; PAD 1 control
hVDP_Data:	dc.l VDP_Data				; a5	; VDP data port
hVDP_Control:	dc.l VDP_Control			; a6	; VDP control port

		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc, exMisc, exMisc, exMisc
		dc.l exMisc
hPAD_Data1:	dc.l PAD_Data1					; PAD 1 data

		dc.b 'SEGA SSF        '
		dc.b 'AURORA   04-2021'
		dc.b 'BAD APPLE MD                                  v0'
		dc.b 'BAD APPLE MD                                  v0'
		dc.b 'AF-BAD-APPLE-0'
		dc.w 0
		dc.b 'J               '
		dc.l 0
EndOfROM:	dc.l -1
		dc.l $FF0000, $FFFFFF
		dc.l $20202020, $20202020, $20202020
		dc.l 0
		dc.l 0
		dc.b 'CONFIRMED TO WORK ON BLASTEM AND REAL HW    '
		dc.b 'JUE             '
; ==============================================================
; --------------------------------------------------------------
; error handlers
; --------------------------------------------------------------

exBus:
exAddr:		stop	#$2F00
exIll:		stop	#$2F00
exDiv:		stop	#$2F00
exChk:		stop	#$2F00
Trapv:		stop	#$2F00
exPriv:		stop	#$2F00
exTrace:	stop	#$2F00
exLineA:	stop	#$2F00
exLineF:	stop	#$2F00
exMisc:		stop	#$2F00
; ==============================================================
; --------------------------------------------------------------
; int handlers
; --------------------------------------------------------------

Vint:
		addq.b	#1,zcheck.w				; add to zcheck counter
Hint:
		rte
; ==============================================================
; --------------------------------------------------------------
; Hardware initialization data
; --------------------------------------------------------------

InitData:
		dc.w .zdataend-.zdatastart-1		; d0	; driver length
		dc.w $100				; d1	; value for Z80 reset
		dc.w $40				; d2	; value for enabling pads
		dc.w .endregs-.regs-1			; d3	; VDP registers list
		dc.w $8000				; d4	; VDP register increment
		dc.w 16*4/2				; d5	; number of colors to clear
		dc.w 20					; d6	; number of scroll positions to clear
		dc.w VDP_PSG-VDP_Data			; d7	; offset for a5 later on

	; z80 sound driver data
.zdatastart
	z80prog 0
		di						; no interrupts 4 u
		im	1					; but make sure to use im 1!
		ld	de,$4000				; load YM register to de

.reg		macro r,v
		ld	a,\r					; send register
		ld	(de),a					;
		inc	e					;
		ld	a,\v					; send value
		ld	(de),a					;
	endm

	.reg	$27, $00					; disable timer A
		dec	e					;
	.reg	$42, $7F					; Set FM6 TL1 to mute
		dec	e					;
	.reg	$46, $7F					; Set FM6 TL2 to mute
		dec	e					;
	.reg	$4A, $7F					; Set FM6 TL3 to mute
		dec	e					;
	.reg	$4E, $7F					; Set FM6 TL4 to mute
; --------------------------------------------------------------

		ld	de,$4000				; load YM register to de

		ld	a,$2B					; DAC ENABLE
		ld	(de),a					;
		inc	e					;
		ld	a,$80					; send value
		ld	(de),a					;

		dec	e					;
		ld	a,$2A					; send register
		ld	(de),a					;
; --------------------------------------------------------------

		xor	a					; clear a
		ld	h,a					; clear hl
		ld	l,a
		ld	(hl),a					; clear the first bank
		inc	hl					; go to second byte
		ld	(hl),a					; clear the first bank
		inc	hl					; go to second byte
		ld	(hl),(Audio / $80000) + 2		; initialize next bank addres
; --------------------------------------------------------------

		ld	de,$4001				; load YM data to de
		inc	hl					; go to check address

.wait
		xor	a					; clear a
		zor	(hl)					; or address 4
		jr	nz,.wait				; branch if not 0
; --------------------------------------------------------------

.loadnextbank						; 53
		ld	hl,.bank			; 10	; load the bank number address to hl
		inc	(hl)				; 11	; add 1 to it

		ld	b,a				; 4	; prepare bank 1 address
		ld	c,a				; 4	;
; --------------------------------------------------------------

		ld	a,$0F				; 7	; load bank check to a
		zand	(hl)				; 7	; check if divisible by $10
		jp	nz, .initloop			; 10	; if not, skip

		ld	a,$30					; prepare $30
		cp	(hl)					; check if it's the same
		jp	z, .load1				; go to loading bank 1

		ld	a,$40					; prepare $40
		cp	(hl)					; check if it's the same
		jp	nz, .initloop				; if not, skip

		inc	c					; prepare bank 2 address
		ld	(hl),$20				; reset bank
; --------------------------------------------------------------

.load1
		ld	a,(2)					; load the next bank from 2w
		cp	(AudioEnd / $80000) + 1			; check if this is the end of the sample
		jr	z, *					; if so, just halt the driver

		cp	AudioEnd / $80000			; check if this is near the end of the sample
		jr	nc, .nosave				; if so, do not load more banks (Regen bug?)
		ld	(bc),a					; save to next loaded bank

.nosave
		inc	a					; go to next bank
		ld	(2),a					; save it
; --------------------------------------------------------------

.initloop						; 127
		ld	hl,$6000			; 10	; load the bank shift register to hl
.bank = *+1
		ld	a,$1F				; 7	; load bank addr
	rept 8						; 99
		ld	(hl), a					; load all bank bits into hl
		rrca
	endr
		xor	a
		ld	(hl), a					; clear last bit
; --------------------------------------------------------------

		ld	l,a				; 4	; load $8000 to hl
		ld	h,$80				; 7	;

	; waste 71 cycles
		ld	b,4				; 7	; load djnz count to b
		djnz	*				; 60	; spin
		znop					; 4
; --------------------------------------------------------------
	; 144 cycles per loop
.loop							; 38
		ld	a,(hl)				; 7	; load the next sample
		ld	(de),a				; 7	; save the sample
		inc	hl				; 6	; increase the sample address

		ld	a,h				; 4	; load h to a
		zor	l				; 4	; or with l
		jp	z,.loadnextbank			; 10	; jump if next bank is needed
; --------------------------------------------------------------

	; waste 107 cycles here(!)
		znop					; 4
		ld	b,6				; 7	; load djnz count to b
		djnz	*				; 86	; spin
		jp	.loop2				; 10	; jump back to loop
; --------------------------------------------------------------
	; 144 cycles per loop
.loop2							; 38
		ld	a,(hl)				; 7	; load the next sample
		ld	(de),a				; 7	; save the sample
		inc	hl				; 6	; increase the sample address

		ld	a,h				; 4	; load h to a
		zor	l				; 4	; or with l
		jp	z,.loadnextbank			; 10	; jump if next bank is needed
; --------------------------------------------------------------

	; waste 106 cycles here(!)
		znop					; 4
		ld	b,5				; 7	; load djnz count to b
		djnz	*				; 86	; spin
		jr	.n				; 12
.n		jp	.loop				; 10	; jump back to loop
; --------------------------------------------------------------
	z80prog
	even
.zdataend

	; VDP register dump
.regs
		dc.b $04				; $80	; 8-colour mode
		dc.b $74				; $81	; enable DMA, display and MD mode
		dc.b (vPlaneA >> 10)			; $82	; plane A address
		dc.b (vWindow >> 10)			; $83	; window plane address
		dc.b (vPlaneB >> 13)			; $84	; plane B address
		dc.b (vSprites >> 9)			; $85	; sprite table address
		dc.b $00				; $86	; use lower 64k VRAM for sprites
		dc.b $00				; $87	; background colour line 0 index 0
		dc.b $00				; $88	; unused
		dc.b $00				; $89	; unused
		dc.b $FF				; $8A	; h-int line count
		dc.b $00				; $8B	; full hscroll, full vscroll
		dc.b $81				; $8C	; 32 tile display, no S/H
		dc.b (vHscroll >> 10)			; $8D	; hscroll table address
		dc.b $00				; $8E	; use lower 64k VRAM for planes
		dc.b $02				; $8F	; auto-increment
		dc.b $01				; $90	; 64x32 tile plane size
		dc.b $00				; $91	; window horizontal size
		dc.b $00				; $92	; window vertical size
		dc.b $00				; $93	; filler
.endregs

	vdp	dc.l,0,CRAM,WRITE				; CRAM WRITE to 0
	vdp	dc.l,0,VSRAM,WRITE				; VSRAM WRITE to 0
		dc.b $9F, $BF, $DF, $FF				; PSG volume commands
; ==============================================================
; --------------------------------------------------------------
; Hardware startup routine
; --------------------------------------------------------------

Init:
		move	#$2F00,sr				; disable interrupts
		move.l	hVDP_Control.w,a6			; load VDP control port to a6

	;	tst.l	PAD_Control1-1				; test port A and B control
	;	bne.s	.aok					; if enabled, branch
	;	tst.w	PAD_ControlX-1				; test port C control

.aok
	;	bne.w	SoftInit				; if enabled, branch
; --------------------------------------------------------------

		move.l	hZ80_Bus.w,a1				; load Z80 bus request address to a1
		moveq	#$F,d0					; prepare revision ID mask to d0
		and.b	HW_Version-Z80_Bus(a1),d0		; AND with actual revision ID
		beq.s	.rev0					; if 0, skip
		move.l	$100.w,HW_TMSS-Z80_Bus(a1)		; satistify TMSS
; --------------------------------------------------------------

.rev0
		btst	#6,HW_Version-Z80_Bus(a1)		; check if this is a PAL machine
		sne	palflag.w				; if so, set palflag

.wait
		move	(a6),ccr				; check if DMA is taking place and reset latch
		bvs.s	.wait					; if yes, branch

		move.w	#$8114,(a6)				; enable DMA
	vdpfill	0, 0, $10000, 0					; fill entire VRAM with 0 but don't wait
; --------------------------------------------------------------

		movem.l	hInitAregs.w,a1-a6			; load initial register set to a1-a6
		lea	InitData(pc),a0				; load initialization data to a0
		movem.w	(a0)+,d0-d7				; load some register values

		move.b	d2,(a4)					; enable pad1
		move.b	d2,2(a4)				; enable pad2
		move.b	d2,4(a4)				; enable padex
; --------------------------------------------------------------

		move.w	d1,(a1)					; request Z80 bus
		move.w	d1,(a2)					; Z80 reset on

.waitz80
		btst	d1,(a1)					; check if the bus is free
		bne.s	.waitz80				; branch if not

.loadz80
		move.b	(a0)+,(a3)+				; copy driver to Z80 RAM
		dbf	d0,.loadz80				; loop for every byte
; --------------------------------------------------------------

		moveq	#0,d0					; clear d0
		move.w	d0,(a2)					; Z80 reset off
		move.w	d0,(a1)					; enable Z80
		move.w	d1,(a2)					; Z80 reset on
; --------------------------------------------------------------

.fill
		move	(a6),ccr				; check if DMA is taking place and reset latch
		bvs.s	.fill					; if yes, branch

.regs
		move.b	(a0)+,d4				; load next register value
		move.w	d4,(a6)					; send it to VDP control port
		add.w	d1,d4					; go to next register address
		dbf	d3,.regs				; loop for every register
; --------------------------------------------------------------

		move.l	(a0)+,(a6)				; load CRAM WRITE command to VDP

.cram
		move.l	d0,(a5)					; clear CRAM completely
		dbf	d5,.cram				; loop for every entry
; --------------------------------------------------------------

		move.l	(a0)+,(a6)				; load VSRAM WRITE command to VDP

.vsram
		move.l	d0,(a5)					; clear VSRAM completely
		dbf	d6,.vsram				; loop for every entry
; --------------------------------------------------------------

		add.w	d7,a5					; load PSG data port to a5
	rept 4
		move.b	(a0)+,(a5)				; mute PSG channel
	endr
; ==============================================================
; --------------------------------------------------------------
; Software startup routine
; --------------------------------------------------------------

SoftInit:
	; reset vdp
		movem.l	hVDP_Data.w,a5-a6			; load VDP control port to a6 and data port to a5
		lea	Stack.w,sp				; reset stack pointer
		moveq	#0,d0					; clear d0

	vdp	move.l,vHscroll,VRAM,WRITE,(a6)			; write to hscroll
		move.l	d0,(a5)					; clear scrolling

	vdp	move.l,0,VSRAM,WRITE,(a6)			; write to vscroll
		move.l	d0,(a5)					; clear scrolling
; --------------------------------------------------------------

	; reset plane
	vdp	move.l,vPlaneA,VRAM,WRITE,(a6)			; write to planea
		moveq	#0,d0					; counter
		moveq	#28-1,d2				; row count

.rows
		moveq	#hres/8-1,d1				; tile count

.tiles
		move.w	d0,(a5)					; write 1 tile
		addq.w	#1,d0					; go to next tile
		dbf	d1,.tiles				; write full row

		moveq	#(512-hres)/8-1,d1			; write rest of the hres

.tlcl		move.w	#$7FF,(a5)				; write empty tile
		dbf	d1,.tlcl				; write full row
		dbf	d2,.rows				; write full plane
; --------------------------------------------------------------

	; reset palette
	vdp	move.l,2,CRAM,WRITE,(a6)			; write to cram
		move.l	#$00000222,(a5)
		move.l	#$04440666,(a5)
		move.l	#$08880AAA,(a5)
		move.l	#$0CCC0EEE,(a5)

		move.l	#$000E000E,(a5)
		move.w	#$000E,(a5)

		move.l	#$00000EEE,(a5)				; back and white
		move.l	#$04440AAA,(a5)				; grays

	; reset RAM variables
		move.b	#1,mapper1				; load second 512K bank to mapper1
		move.l	#Lyrics,lyricsaddr.w			; reset lyrics address
		move.w	#1,lyricsdelay.w			; force lyrics update next frame
		move.w	#4,lyricsplace.w			; reset lyrics placement

		lea	Palmap,a0				; load PAL mappings to a0
		move.b	(a0)+,pskipbyte.w			; load the first byte to RAM
		move.l	a0,pskipaddr.w				; save next address to RAM
		move.b	#8,pskipbits.w				; reset the number of bits

		move.w	#files,frame.w				; set num of frames
		clr.w	int.w					; clear interrupt counter
		lea	List,a0					; load list of frame lengths to a0
		lea	PtrList,a1				; load pointers list to a1

		move.b	#Audio / $80000,mapper2			; prepare mapper2
		move.b	#(Audio / $80000) + 1,mapper3		; prepare mapper3
		stop	#$2300					; wait for the next frame

	if test_vgm
		jmp	PlayVGM
	endif

	stopz80							; stop z80
		clr.b	Z80_RAM+3				; enable playback
	startz80						; start z80

METHOD =	0
; ==============================================================
; --------------------------------------------------------------
; Main software loop
; --------------------------------------------------------------

.frames
		tst.b	palflag.w				; check if this is a PAL machine
		beq.s	.notPAL					; branch if not

		move.b	pskipbyte.w,d0				; load the skip byte to d0
		add.b	d0,d0					; shift right by 1 bit
		scs	d1					; if carry was set, set d1
; --------------------------------------------------------------

		subq.b	#1,pskipbits.w				; decrease the number of bits left
		bgt.s	.bitsleft				; if some are left, branch
		move.l	pskipaddr.w,a2				; load the list address to a2
		move.b	(a2)+,d0				; load the next byte from it
		move.l	a2,pskipaddr.w				; save as the new address
		move.b	#8,pskipbits.w				; reset the number of bits
; --------------------------------------------------------------

.bitsleft
		move.b	d0,pskipbyte.w				; save byte
		tst.b	d1					; check if skip was initialized
		beq.s	.notPAL					; if not, branch
		bsr.w	.render					; render an extra frame
; --------------------------------------------------------------

.notPAL
		bsr.w	.render					; render a frame

		moveq	#$0F,d0					; load check count
		and.b	zcheck.w,d0				; AND with zcheck
		bne.s	.nochk					; branch if no check

	stopz80							; stop z80
		tst.b	Z80_RAM+0				; check if bank request is enabled
		beq.s	.bank2					; branch if no
		move.b	Z80_RAM+0,mapper2			; load to mapper 2
		clr.b	Z80_RAM+0				; set as updated
; --------------------------------------------------------------

.bank2
		tst.b	Z80_RAM+1				; check if bank request is enabled
		beq.s	.bankd					; branch if no
		move.b	Z80_RAM+1,mapper3			; load to mapper 3
		clr.b	Z80_RAM+1				; set as updated

.bankd
	startz80						; start z80

.nochk
		stop	#$2300					; wait for the next frame
; --------------------------------------------------------------

		lea	PAD_Data1,a2				; load pad1 data to a0
		moveq	#0,d2					; load TH low to d2
		move.b	d2,(a2)					; TH LOW
		moveq	#1<<PAD_TH,d3				; load TH high to d3

		moveq	#(1<<PAD_TL)|(1<<PAD_TR),d0		; get only Start+A mask
		and.b	(a2),d0					; and with the buttons from pad
		move.b	d3,(a2)					; TH HIGH

		moveq	#(1<<PAD_TL)|(1<<PAD_TR)|$F,d1		; get B+C+UDLR mask to d1
		and.b	(a2),d1					; and with the buttons from pad

		btst	#4,d0					; check if A is held
		beq.s	.aheld					; if so, branch

		clr.b	lyricsa.w				; clear A button state
		bra.w	.frames					; execute the next frame
; --------------------------------------------------------------

.aheld
		tst.b	lyricsa.w				; check if A is held
		bne.w	.frames					; if so, ignore
		st	lyricsa.w				; A is held but not ignored this frame

		addq.w	#4,lyricsplace.w			; go to the next lyrics slot
		cmp.w	#16,lyricsplace.w			; check for invalid lyrics placement
		bne.s	.ckupdate				; check if we need to update
		move.w	#4,lyricsplace.w			; reset lyrics placement
; --------------------------------------------------------------

.ckupdate
		cmp.w	#4,lyricsdelay.w			; check if delay is nearly over
		ble.w	.frames					; if so, do not update rn

		pea	.frames(pc)				; return straight to run the next frame
		move.w	lyricsplace.w,d0			; load lyrics placement to d0
		move.l	lyricsaddr.w,a2				; load lyrics address to a2
		move.l	-16(a2,d0.w),a2				; load the real address to a2
		jmp	(a2)					; run the sprite DMA code
; --------------------------------------------------------------

.render
		subq.w	#1,lyricsdelay.w			; decrease lyrics delay
		bcc.s	.nographic				; branch if no carry

		move.w	lyricsplace.w,d0			; load lyrics placement to d0
		move.l	lyricsaddr.w,a2				; load lyrics address to a2
		move.l	(a2,d0.w),a3				; load the real address to a3
		jsr	(a3)					; run the sprite DMA code

		move.w	(a2),d0					; load the upper byte of delay
		move.b	4(a2),d0				; load lower byte of delay
		move.w	d0,lyricsdelay.w			; save delay
; --------------------------------------------------------------

		tst.w	(a2)					; check if this is the end token
		beq.s	.nographic				; if so, do not advance address
		add.w	#16,a2					; skip the DMA addresses
		move.l	a2,lyricsaddr.w				; save lyrics address
; --------------------------------------------------------------

.nographic
		cmp.w	#1,lyricsdelay.w			; should we load the graphics now?
		bne.s	.nolyric				; branch if not exactly 0
		move.l	lyricsaddr.w,a2				; load lyrics address to a2
		move.l	(a2),a2					; load the real address to a2
		jsr	(a2)					; run the tile DMA code
; --------------------------------------------------------------

.nolyric
		move.b	(a1),mapper7				; load new mapper bank
		movem.l	(a1)+,a2-a4				; load data to a2, vram to a3 and len to a4

		move.l	a1,-(sp)				; push to stack
		bsr.s	ProcessFrame				; process this frame
		move.l	(sp)+,a1				; pop from stack
; --------------------------------------------------------------

		subq.w	#1,frame.w				; check if more frames to render
		bne.s	.rts					; if yes, branch
		stop	#$2300					; wait for the next frame
		stop	#$2300					; wait for the next frame
		jmp	PlayVGM					; go play da vgm

.rts
		rts
; ==============================================================
; --------------------------------------------------------------
; Process full frame of video
; --------------------------------------------------------------

_setvram	macro
		moveq	#0,d2
		move.w	(a3)+,d2				; load VRAM address to d2
		swap	d2					; swap into place
		or.l	d5,d2					; or VDP command to the address
		move.l	d2,(a6)					; send VDP command
	endm

_dorows		macro
		moveq	#0,d2
		move.w	(a2)+,d2				; load row data to d2
		add.l	d2,d2					; double (for LUT)
		add.l	d2,d2					; quadruple (for LUT)
		move.l	(a1,d2.l),(a5)				; load row from LUT to vdp
	endm
; --------------------------------------------------------------

ProcessFrame:
		lea	RowLUT(pc),a1				; load row LUT to a1
		moveq	#0,d2

		move.l	(a0)+,d6				; load row lengths to d6
		tst.w	d6					; get length of the plane base to d6
		bmi.s	.bank1					; if no rows, skip
	vdp	move.l,vTiles,VRAM,WRITE,d5			; prepare VDP command to d5
		bsr.s	ProcessRows				; process the upcoming rows
; --------------------------------------------------------------

.bank1
		swap	d6					; swap words
		tst.w	d6					; get length of the plane base to d6
		bmi.s	.bank2					; if no rows, skip
	vdp	move.l,(vTiles+$4000),VRAM,WRITE,d5		; prepare VDP command to d5
		bsr.s	ProcessRows				; process the upcoming rows
; --------------------------------------------------------------

.bank2
		move.w	(a0)+,d6				; load row lengths to d6
		tst.w	d6					; get length of the plane base to d6
		bmi.s	ProcessRowsEnd				; if no rows, skip
	vdp	move.l,(vTiles+$8000),VRAM,WRITE,d5		; prepare VDP command to d5
; --------------------------------------------------------------

ProcessRows:
	_setvram						; prepare VDP request
		moveq	#0,d0					; prepare and mask for len
		move.b	(a4)+,d0				; load length to d0

.loop
		_dorows						; execute a row
		dbf	d0,.loop				; do all loops

		move	(a6),ccr				; check if v-blank is active
		bra.s	ProcessRowsActive2			; branch if yes

ProcessRows2:
		dbf	d6,ProcessRows				; loop until all rows are done

ProcessRowsEnd:
		rts
; --------------------------------------------------------------

ProcessRowsActive:
	_setvram						; prepare VDP request
		moveq	#0,d0					; prepare and mask for len
		move.b	(a4)+,d0				; load length to d0

.loop
		btst	#0,(a6)
		bne.s	.loop
		_dorows						; execute a row
		dbf	d0,.loop				; do all loops

		move	(a6),ccr				; check if v-blank is active
		bmi.s	ProcessRows2				; branch if yes

ProcessRowsActive2:
		dbf	d6,ProcessRowsActive			; loop until all rows are done
		rts
; --------------------------------------------------------------

	if METHOD
DoFastRows:
	rept 16
		_dorows						; execute a row
	endr
		rts
; --------------------------------------------------------------

RowCodeOffs:
x =		$A0
	rept $10
x =		x-$0A
		dc.b x
	endr
	endif
; ==============================================================
; --------------------------------------------------------------
; row look up table to convert between internal format to VDP tile
; --------------------------------------------------------------

RowLUT:
		nolist
v =	0
	rept $10000
		dc.w $CCCC|((v & $C000) >> 2)|((v & $3000) >> 4)|((v & $0C00) >> 6)|((v & $0300) >> 8)
		dc.w $CCCC|((v & $C0) << 6)|((v & $30) << 4)|((v & $0C) << 2)|((v & $03))
v =		v+1
	endr
		list
; ==============================================================
; --------------------------------------------------------------
; include lyrics
; --------------------------------------------------------------

Lyrics:
.ly		macro	id, frames
		dc.l	((\frames&$FF00)<<16)|LyricsTilesLoad\id		; load graphics
		dc.l	((\frames&$FF)<<24)|LyricsSpritesLoad\id		; text on right
		dc.l	LyricsSprites2Load\id					; text on left
		dc.l	LyricsSpritesLoad0					; text disabled
	endm
; --------------------------------------------------------------

		.ly	0, $0040	;
		.ly	81,$0140	; intro
		.ly	0, $0010	;
		.ly	82,$0140	; intro
		.ly	0, $0010	;
		.ly	83,$0140	; intro

		.ly	0, $021C	;
		.ly	1, $0198	; 流れてく　時の中ででも　気だるさが　ほらグルグル廻って
		.ly	0, $000A	;
		.ly	2, $0184	; 私から　離れる心も　見えないわ　そう知らない？
		.ly	0, $000A	;
		.ly	3, $0198	; 自分から　動くこともなく　時の隙間に　流され続けて
		.ly	0, $000A	;
		.ly	4, $00EE	; 知らないわ　周りのことなど　私は私　それだけ
		.ly	0, $0006	;
		.ly	5, $00FA	; 夢見てる？　なにも見てない？　語るも無駄な　自分の言葉
		.ly	0, $0006	;
		.ly	6, $0190	; 悲しむなんて　疲れるだけよ　何も感じず　過ごせばいいの
		.ly	0, $000A	;
		.ly	7, $0178	; 戸惑う言葉　与えられても　自分の心　ただ上の空
		.ly	0, $0008	;
		.ly	8, $017C	; もし私から　動くのならば　すべて変えるのなら　黒にする

		.ly	0, $000A	;
		.ly	9, $0180	; こんな自分に　未来はあるの？　こんな世界に　私はいるの？
		.ly	0, $000A	;
		.ly	10,$0100	; 今切ないの？　今悲しいの？　自分の事も　わからないまま
		.ly	0, $0008	;
		.ly	11,$0170	; 歩むことさえ　疲れるだけよ　人のことなど　知りもしないわ
		.ly	0, $000A	;
		.ly	12,$0160	; こんな私も　変われるのなら　もし変われるのなら　白になる
		.ly	0, $02A8	;

		.ly	13,$0160	; 流れてく　時の中ででも　気だるさがほら　グルグル廻って
		.ly	0, $000A	;
		.ly	14,$0178	; 私から　離れる心も　見えないわそう　知らない？
		.ly	0, $000A	;
		.ly	3, $0192	; 自分から　動くこともなく　時の隙間に　流され続けて
		.ly	0, $000A	;
		.ly	4, $0160	; 知らないわ　周りのことなど　私は私　それだけ
		.ly	0, $000A	;
		.ly	5, $0160	; 夢見てる？　なにも見てない？　語るも無駄な　自分の言葉
		.ly	0, $0006	;
		.ly	6, $0180	; 悲しむなんて　疲れるだけよ　何も感じず　過ごせばいいの
		.ly	0, $000A	;
		.ly	7, $0188	; 戸惑う言葉　与えられても　自分の心　ただ上の空
		.ly	0, $000A	;
		.ly	8, $017A	; もし私から　動くのならば　すべて変えるのなら　黒にする
		.ly	0, $000A	;

		.ly	31,$019A	; 動くのならば　動くのならば　すべて壊すわ　すべて壊すわ
		.ly	0, $000A	;
		.ly	32,$018C	; 悲しむならば　悲しむならば　私の心　白く変われる？
		.ly	0, $000A	;
		.ly	33,$0190	; 貴方の事も　私のことも　全ての事も　まだ知らないの
		.ly	0, $000A	;
		.ly	34,$0178	; 重い目蓋を　開けたのならば　すべて壊すのなら　黒になれ！！！
		.ly	0, $FFFF	;
		dc.w 0			; end of list
; --------------------------------------------------------------

LyricsDo	macro id
	rept narg

LyricsTilesLoad\id:
	if filesize("lyrics/data/\id\ tiles.dat")>0
		dma	.data, $480*32, filesize("lyrics/data/\id\ tiles.dat"), VRAM
	endif
		rts
.data		incbin "lyrics/data/\id\ tiles.dat"

LyricsSpritesLoad\id:
		dma	.data, $F800, filesize("lyrics/data/\id\ sprites.dat"), VRAM
		rts
.data		incbin "lyrics/data/\id\ sprites.dat"

LyricsSprites2Load\id:
		dma	.data, $F800, filesize("lyrics/data/\id\ sprites2.dat"), VRAM
		rts
.data		incbin "lyrics/data/\id\ sprites2.dat"
	shift
	endr
	endm
; --------------------------------------------------------------

	LyricsDo	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
	LyricsDo	31, 32, 33, 34
	LyricsDo	81, 82, 83
; ==============================================================
; --------------------------------------------------------------
; include misc data
; --------------------------------------------------------------

Palmap:		incbin "delta/pal.dat"
		even
List:		incbin "delta/list.dat"
PtrList:	incbin "delta/pointers.dat"

		align $10			; wevy wevy impowowtant
		dc.b "                "
		dc.b " Look! We even  "
		dc.b " have some free "
		dc.b " space here! We "
		dc.b " have a lot of  "
		dc.b "  free space!   "
		dc.b "                "
		dc.b " Wonder what we "
		dc.b " should do with "
		dc.b "  it, hmmm?! I  "
		dc.b "know, lets have "
		dc.b "a little credits"
		dc.b "    section:    "
		dc.b "                "
		dc.b "created by:     "
		dc.b "   AURORA FIELDS"
		dc.b "                "
		dc.b "hardware testing"
		dc.b " & help:        "
		dc.b "          Jorge "
		dc.b "                "
		dc.b "song & ideas:   "
		dc.b "     Nat The    "
		dc.b "       Porcupine"
		dc.b "                "
		dc.b "other testing:  "
		dc.b "    Vladikcomper"
		dc.b "        Soldaten"
		dc.b "           Selbi"
		dc.b "                "
		dc.b "in association w"
		dc.b "  Mega Drive    "
		dc.b "    Developers  "
		dc.b "      Collective"
		dc.b "                "
		dc.b "join our Discord"
		dc.b "   @ dc.mddc.dev"
		dc.b "                "
		dc.b "  we are also   "
		dc.b "going to launch "
		dc.b "forums@ mddc.dev"
		dc.b "                "
		dc.b "talking about MD"
		dc.b " and other SEGA "
		dc.b "  software and  "
		dc.b " hardware! Also "
		dc.b "about research, "
		dc.b "homebrew, hacks,"
		dc.b " and other cool "
		dc.b " ideas we have! "
		dc.b "                "
		dc.b "   thanks for   "
		dc.b "  enjoying the  "
		dc.b " demo, hope you "
		dc.b "were impressed. "
		dc.b "                "
		dc.b "  yes, this is  "
		dc.b "320x224, at 60hz"
		dc.b "most of the time"
		dc.b "                "
		dc.b "sometimes there "
		dc.b "  are skipped   "
		dc.b " frames because "
		dc.b "  this was too  "
		dc.b "  difficult to  "
		dc.b " optimize more! "
		dc.b "                "
		dc.b " the video only "
		dc.b "   uses delta   "
		dc.b "encoding to run "
		dc.b " the video with "
		dc.b "  VRAM writes.  "
		dc.b "                "
		dc.b " this makes DMA "
		dc.b "  impractical.  "
		dc.b "                "
		dc.b " so yes all of  "
		dc.b "this can be done"
		dc.b "without any DMA."
		dc.b "                "
		dc.b "  This project  "
		dc.b "  took about a  "
		dc.b "week to do, but "
		dc.b " was one of the "
		dc.b " most difficult "
		dc.b "things I've ever"
		dc.b "done on the MD! "
		dc.b "                "
		dc.b " See the source "
		dc.b "   code on my   "
		dc.b "  Github page:  "
		dc.b "     github.com/"
		dc.b "NatsumiFox/Bad-Apple-MD         "
		even
; ==============================================================
; --------------------------------------------------------------
; play teh VGM filee
;
; edit: Does not work, abandoning this idea
; --------------------------------------------------------------

LittleEndianSucksW	macro dst, offs
	if narg=2
		move.w	\offs(a0),\dst				; load word into dst
		ror.w	#8,\dst					; swap bytes

	else
		move.b	(a0)+,(sp)				; load the next byte into stack
		move.b	(a0)+,\dst				; load byte value into dst
		lsl.w	#8,\dst					; shift into place
		move.b	(sp),\dst				; get the previous byte back to dst
	endif
	endm
; --------------------------------------------------------------

LittleEndianSucks	macro dst, offs
	if '\0' = 'l'
		if narg=2
			move.l	\offs(a0),\dst			; load longword to dst
			ror.w	#8,\dst				; swap 2 lower bytes
			swap	\dst				; swap words
			ror.w	#8,\dst				; swap 2 lower bytes

		else
			LittleEndianSucksW	\dst		; read a word of little endian bs
			swap	\dst				; swap words
			LittleEndianSucksW	\dst		; read a word of little endian bs
			swap	\dst				; swap words again
		endif
	else
		LittleEndianSucksW	\_			; read a word of little endian bs
	endif
	endm

LittleEndianSucksVar	macro *, value
\* set			(\value >> 24)|(\value << 24)|(\value >> 8)|(\value << 8)
	endm
; --------------------------------------------------------------

YM1		macro reg, val
		move.b	#\reg,(a4)				; send command to port1
		or.l	d0,d0					; waste cycles
		move.b	#\val,1(a4)				; send value to port1
	endm
; --------------------------------------------------------------

WaitYM		macro reg, val
.busy\@
		tst.b	(a4)					; check if YM2612 is busy
		bmi.s	.busy\@					; branch if yes
	endm
; --------------------------------------------------------------

;EXPECTED_PSG_FREQUENCY	LittleEndianSucksVar	3579545		; this is the right PSG freq
;EXPECTED_FM_FREQUENCY	LittleEndianSucksVar	7670454		; this is the right FM freq

PlayVGM:
		move	#$2700,sr				; disable ints
		stopZ80

		lea	VGM,a0					; load VGM source file to a0
		move.w	8(a0),d7				; load VGM version (for example $5001, $7001, etc.)
; --------------------------------------------------------------

	; load start location
		LittleEndianSucks.l	d0, $34			; load the sucky little endian number

		cmp.w	#$5001,d7				; check VGM version
		bhs.s	.normal					; branch if >=1.50
		moveq	#$40-$34,d0				; load the default offset

.normal
		lea	$34(a0,d0.l),a1				; load VGM data to a1 (leave header to a0)
; --------------------------------------------------------------

	; load loop location
		LittleEndianSucks.l	d0, $1C			; load the sucky little endian number
		lea	$1C(a0,d0.l),a2				; load VGM loop address a2
		move.l	a2,vgmloop.w				; save loop point
; --------------------------------------------------------------

	; init
		exg	a0,a1					; swap so a1 is header
		move.l	a0,a2					; pretend data bank is at start

		lea	YM_Reg1,a4				; load YM port 1 to a4
		lea	YM_Reg2,a5				; load YM port 2 to a5
		lea	VDP_PSG,a6				; load PSG port to a6

		move.w	#blocks,blockaddr.w			; reset block address
		move.w	#735,vgmwaitA.w				; wait states for 60hz
		move.w	#882,vgmwaitB.w				; wait states for 50hz

		moveq	#0,d5					; reset the length of the sample
		moveq	#0,d6					; reset the number of wait states
		moveq	#0,d7

	YM1	$24, $FF					; YM Timer A: $3FF
		or.l	d0,d0					; wait so we can safely check the busy flag
	WaitYM							; wait if busy...
	YM1	$25, $03
		or.l	d0,d0					; wait so we can safely check the busy flag
	WaitYM							; wait if busy...
	YM1	$27, $15					; enable Timer A
; ==============================================================
; --------------------------------------------------------------
; VGM playback loop
; --------------------------------------------------------------

VGM_Play:
		move.b	(a0)+,d0				; load the next command
		cmp.b	#$60,d0					; check if this is a $50 command
		blo.s	.c50					; jump if yes
		cmp.b	#$70,d0					; check if this is a $60 command
		blo.s	.c60					; jump if yes
		cmp.b	#$80,d0					; check if this is a wait command
		blo.w	VGM_WaitQ				; jump if yes
		cmp.b	#$90,d0					; check if this is a DAC command
		blo.w	VGM_DAC					; jump if yes

		cmp.b	#$95,d0					; check if this is a Start Stream Fast command
		beq.w	VGM_SSF					; jump if yes
	;	cmp.b	#$96,d0					; check for special sample bleepbloop commands
	;	bhs.w	*					; UNSUPPORTED COMMAND
; --------------------------------------------------------------

		and.w	#7,d0					; remove the upper byte
		move.b	.tab90(pc,d0.w),d0			; load the offset to d0
		add.w	d0,a0					; skip this many bytes
		bra.s	VGM_Play				;

.tab90		dc.b 4, 4, 5, 10, 1, 4,	0, 0			; cool table
; --------------------------------------------------------------

.c50
		cmp.b	#$4F,d0					; check if this is an invalid
		blo.w	*					; jump if yes
		cmp.b	#$50,d0					; check if this is a $4F or $50 command
		bls.w	VGM_PSG					; jump if yes

		cmp.b	#$52,d0					; check if this is a FM port1 command
		beq.w	VGM_FM1					; jump if yes
		cmp.b	#$53,d0					; check if this is a FM port2 command
		beq.w	VGM_FM2					; jump if yes
	;	bra.w	*					; UNSUPPORTED COMMAND
; --------------------------------------------------------------

.c60
		cmp.b	#$61,d0					; check if this is a wait n command
		beq.w	VGM_WaitN				; jump if yes
		cmp.b	#$62,d0					; check if this is a wait a command
		beq.w	VGM_WaitA				; jump if yes
		cmp.b	#$63,d0					; check if this is a wait a command
		beq.w	VGM_WaitB				; jump if yes

		cmp.b	#$67,d0					; check if this is a data block command
		beq.w	VGM_Data				; jump if yes
	;	cmp.b	#$66,d0					; check if this is a stop command
	;	bne.w	*					; jump if not

		move.l	vgmloop.w,a0				; loop VGM
		move.w	#blocks,blockaddr.w			; reset block address
		bra.w	VGM_Play				;
; ==============================================================
; --------------------------------------------------------------
; VMG FM write
; --------------------------------------------------------------

VGM_FM1:
	WaitYM
		move.b	(a0)+,(a4)				; send command to port1
		or.l	d0,d0					; waste cycles
		move.b	(a0)+,1(a4)				; send value to port1
		bra.w	VGM_Play				; jump back to playback code
; --------------------------------------------------------------

VGM_FM2:
	WaitYM
		move.b	(a0)+,(a5)				; send command to port2
		or.l	d0,d0					; waste cycles
		move.b	(a0)+,1(a5)				; send value to port2
		bra.w	VGM_Play				; jump back to playback code
; ==============================================================
; --------------------------------------------------------------
; VGM PSG write
; --------------------------------------------------------------

VGM_PSG:
		move.b	(a0)+,(a6)				; send command to PSG
		bra.w	VGM_Play				; jump back to playback code
; ==============================================================
; --------------------------------------------------------------
; VGM wait states
; --------------------------------------------------------------

VGM_WaitN:
		LittleEndianSucks.w	d0			; load timer wait states from after the command
		bra.s	VGM_WaitCom				;
; --------------------------------------------------------------

VGM_WaitA:
		move.w	vgmwaitA.w,d0				; load timer A wait states
		bra.s	VGM_WaitCom				;
; --------------------------------------------------------------

VGM_WaitB:
		move.w	vgmwaitA.w,d0				; load timer A wait states
		bra.s	VGM_WaitCom				;
; --------------------------------------------------------------

VGM_DAC:
		move.b	#$2A,(a4)				; send command to port1
		and.w	#$F,d0					; get states from the command
		or.l	d0,d0					; waste cycles
		move.b	(a2)+,1(a4)				; send value to port2
		bra.s	VGM_WaitCom				;
; --------------------------------------------------------------

VGM_WaitQ:
		and.w	#$F,d0					; get states from the command
		addq.w	#1,d0					; +1 beccause VGM
; --------------------------------------------------------------

VGM_WaitCom:
		sub.w	d6,d0					; subtract the timer stuffs
		ble.s	.cont					; loop til done
	WaitYM

.loop
.check
		btst	#0,(a4)					; check if Timer A has overflowed
		beq.s	.check					; if not, wait some more

	YM1	$27, $15					; enable Timer A
		moveq	#1,d6					; change by 1
		or.l	d0,d0					; wait so we can safely check the busy flag
	WaitYM
		add.b	#$C0,d7					; increment d7		; <- evil evil hack
		bcc.s	.sub					; if no overflow, set branch

		move.b	#$2A,(a4)				; send DAC command to port1
		subq.w	#1,d5					; decrease sample index
		bne.s	.noreset				; branch if more sample to play

		move.w	#$1000,d5				; reset sample lenght
		lea	VGM_NullPCM(pc),a1			; reset sample address

.noreset
		moveq	#2,d6					; decrease by 2
		move.b	(a1)+,1(a4)				; send value to port1

.sub
		sub.w	d6,d0					; subtract from d0
		bgt.s	.loop					; if still >0, loop

.cont
		move.w	d0,d6					; load the remaining amount to d6
		neg.w	d6					; negative to positive
		bra.w	VGM_Play				; jump back to playback code
; ==============================================================
; --------------------------------------------------------------
; VGM load data
; --------------------------------------------------------------

VGM_Data:
		cmp.b	#$66,(a0)+				; check if valid data
		bne.w	*					; if not, freeze

		move.b	(a0)+,d0				; load the data type to d0
	;	beq.s	VGM_DataUncPCM				; uncompressed YM2612 PCM data block
	;	bra.w	*					; NOT SUPPORTED
; --------------------------------------------------------------

VGM_DataUncPCM:
		LittleEndianSucks.l	d0			; load the sucky little endian number
		move.l	a0,a2					; load the new data section
		lea	(a0,d0.l),a0				; go to the next command address

		move.w	blockaddr.w,a3				; load the block address register to a3
		move.l	d0,(a3)+				; save the block size too
		move.l	a2,(a3)+				; save the new sample address there
		move.w	a3,blockaddr.w				; store block address again
		bra.w	VGM_Play				; jump back to playback code
; --------------------------------------------------------------

VGM_SSF:
		tst.b	(a0)+					; check for stream 00
	;	bne.w	*					; if not, kill
		LittleEndianSucks.w	d0			; load the sucky little endian number

		add.w	d0,d0					;
		add.w	d0,d0					;
		add.w	d0,d0					; 8x value
		add.w	#blocks,d0				; add base address to d0

		move.w	d0,a1					; load it to a1
		movem.l	(a1),d5/a1				; load sample address and size

		addq.w	#1,a0					; skip the flags
		bra.w	VGM_Play				; jump back to playback code
; --------------------------------------------------------------

VGM_NullPCM:	dcb.b	$1000,$80				; fill with $80 (neutral)
		align	$1000
VGM:		incbin "sound.vgm"
		align	$80000
; --------------------------------------------------------------

	if test_vgm = 0
		incbin "delta/data.dat"
		align	$80000

Audio:		incbin "pcm/pcm.raw"
AudioEnd:
	else
Audio:
AudioEnd:
	endif
