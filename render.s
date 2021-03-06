		includeh MAC_TRAPS_68k.def

		SECTION RENDER
;--------------------------------------------------------------------------------------

RENDERER_INIT:
		; change d0,d1,d2,d3 + subs, a0
		lea		D_SCR(a5),a0	; demo screen letrehozasa, ezen renderelodnek a demo effektek...
	IFNE DEBUG_MODE
		moveq	#8,d0			; offset X (byte)	in debug mode
		moveq	#64,d1			; offset Y (line)
	ELSE								; normal modban kozepre rakjuk a screent
		move.w	M_SCR+__width(a5),d0	; teljes szelesseg
		subi.w	#DEMO_W,d0				; - DEMO szelesseg
		lsr.w	#4,d0						; /2
		move.w	M_SCR+__height(a5),d1	; teljes magassag
		subi.w	#DEMO_H,d1				; - DEMO magassag
		lsr.w	d1						; /2
	ENDC
		move.w	#DEMO_W,d2		; pixel width	64-gyel oszthatonak kell lennie!
		move.w	#DEMO_H,d3		; pixel height
		bsr.s	new_screen		; create screen result (a0)
		bsr.s	clear_screen	; clear screen (a0)
		bsr.s	generateRowStartAddrs	; generate row start address table
		rts


new_screen:
		;a0.l = screen structure pointer
		;d0.w = x offset (bytes)
		;d1.w = y offset
		;d2.w = Pixel Width		//64-gyel oszthatonak kell lennie
		;d3.w = Pixel Height

		; change: d1,d2,d4,d5

		;DEBUG

		move.l	M_SCR+__baseAddr(a5),d4		; main screen base address d4-be
		move.w	M_SCR+__rowBytes(a5),d5		; main screen row bytes d5 -be
		
		mulu	d5,d1						; beszorozzuk Y offsettel a row byte-ot
		add.l	d1,d4						; majd hozzaadjuk a base addr. 
		add.w	d0,d4						; vegul az X byte offset-et is, megvan a screen base addresse
		
		move.l	d4,__baseAddr(a0)			; taroljuk az uj screen strukturaba
		move.w	d5,__rowBytes(a0)
		andi.w	#$FFC0,d2					; kiszurjuk a 64-gyel nem oszthato szelesseget
		move.w	d2,__width(a0)				; taroljuk javitva
		move.w	d3,__height(a0)				; taroljuk a magassagot
		lsr.w	#3,d2
		move.w	d2,__BW(a0)					; byte width
		sub.w	d2,d5						; es kiszamoljuk hogy mennyit kell az utolso byte-hoz adni hogy a kovetkezo sorra jussunk
		move.w	d5,__BWA(a0)				; taroljuk

		rts

cls:
		lea		D_SCR(a5),a0

clear_screen:
		; input: a0.l = screen structure pointer
		; do not change any register
		movem.l	d0-d4/a1,-(sp)
		
		movea.l	__baseAddr(a0),a1
		move.w	__BW(a0),d0
		lsr.w	#2,d0			;long szamlalo
		subq.w	#1,d0
		move.w	__height(a0),d1
		subq.w	#1,d1			;Y szamlalo
		move.w	__BWA(a0),d4
		;lsr.w	#2,d4			;add at line end
		
		moveq	#0,d3
		subq.l	#1,d3
.loop1:		
		move.w	d0,d2
.loop0:
		move.l	d3,(a1)++
		dbra.s	d2,.loop0
		
		adda.w	d4,a1			;jump to next line
		dbra.s	d1,.loop1
		
		movem.l	(sp)+,d0-d4/a1
		rts



generateRowStartAddrs:
		; a0-ban a screen amihez kell
		; memoriafoglalas
		; szamolas,
		; linkeles
		movea.l	a0,a1				; free up a0, ugly a bit
		moveq	#0,d0
		move.w	__height(a1),d0
		move.w	d0,d1				; save height
		lsl.w	#2,d0				; sorok szama*4 memoria kell
		dc.w	_Newptr
		bne.s	.gen_quit			; ha nem ment, akkor kilepes...
		move.l	a0,__rowStartAddrs(a1)
		
		move.l	__baseAddr(a1),d0
		moveq	#0,d2
		move.w	__rowBytes(a1),d2
		
		subq.w	#1,d1

.loopgen:
		move.l	d0,(a0)+
		add.l	d2,d0
		dbra	d1,.loopgen

.gen_quit:
		rts


drawApple:
		lea		D_SCR(a5),a0
		movea.l	mem0_ptr+__appledata(a5),a1
		move.w	__rowBytes(a0),d0
		move.w	d0,d1
		mulu	#110,d1
		add.w	#25,d1					; nem lesz igy long aligned :(
		movea.l	__baseAddr(a0),a2
		adda.l	d1,a2					; a logo helye a video ram-ban
		
		moveq	#92,d1
		
.loopapple:
			move.l	(a1)+,(a2)
			move.l	(a1)+,4(a2)
			move.l	(a1)+,8(a2)
			adda.w	d0,a2
			dbra	d1,.loopapple
		rts
		
			
	IFNE DEBUG_MODE
		
plot:
		; d0.w - X
		; d1.w - Y
		movem.l	d0-d2/a0-a1,-(sp)
		
		tst.w	d0					; check if dot is out of screen area
		bmi.s	.skip
		cmpi.w	#DEMO_W,d0
		bge.s	.skip
		tst.w	d1
		bmi.s	.skip
		cmpi.w	#DEMO_H,d1
		bge.s	.skip
		
		lea		D_SCR(a5),a0		; screen structure base
		movea.l	__baseAddr(a0),a1	; screen base address
		move.w	__rowBytes(a0),d2
		mulu	d1,d2
		move.w	d0,d1
		lsr.w	#3,d1
		add.w	d1,d2
		adda.l	d2,a1				; a1 a manipulalando byte-ra mutat
		andi.w	#$7,d0
		move.b	bits(PC,d0),d0
		;eori.b	#$FF,d0
		;and.b	d0,(a1)
		eor.b	d0,(a1)
		;bchg.b	d0,(a1)

.skip:		
		movem.l	(sp)+,d0-d2/a0-a1
		rts

bits:	dc.b	%10000000,%01000000,%00100000,%00010000,%00001000,%00000100,%00000010,%00000001

drawresult:
	;	clr.l	d2
	;	move.w	M_SCR+__width(a5),d2
	;	lsr.w	#5,d2						; kepernyo szelessege longword-ben

		movea.l	M_SCR+__baseAddr(a5),a0
		addq	#4,a0
		movea.l	a0,a2						; kepernyo kezdete a memoriaban +8...
		move.w 	M_SCR+__rowBytes(a5),d5
	;	lsl.w	#1,d5
		
		clr.l	d1							; magassaga pixelben -1 mert szamlalo
		move.w	M_SCR+__height(a5),d1
		subq	#1,d1
		
		move.l	#255,d1
		
		move.l	d1,d3
		moveq	#47,d4						; oszlop szamlalo
		
		
	;	move.b	#$FF,(a0)
		
	;	move.l	#$00000010,d1
		
drloop:
		move.b	(a1)+,(a0)
		adda.w	d5,a0
		dbra.s	d1,drloop
		
		addq	#1,a2
		movea.l	a2,a0
		move.w	d3,d1
		
		dbra.s	d4,drloop
		rts

drawresult2:
		
		movea.l	M_SCR+__baseAddr(a5),a2
		move.w 	M_SCR+__rowBytes(a5),d5
		
		clr.l	d1							; magassaga pixelben -1 mert szamlalo
		move.w	M_SCR+__height(a5),d1
		subq	#1,d1
		
		move.l	d1,d3
		moveq	#7,d4						; oszlop szamlalo
		
		
	;	move.b	#$FF,(a0)
		
	;	move.l	#$00000010,d1
		
drloop3:

		movea.l	a1,a3
		movea.l	a2,a0						; kepernyo kezdete a memoriaban +8...
drloop2:
		move.b	(a3),(a0)
		addq	#8,a3
		adda.w	d5,a0
		dbra.s	d1,drloop2
		
		addq	#1,a1
		addq	#1,a2
		move.w	d3,d1
		
		dbra.s	d4,drloop3
		rts


		global drawresult,drawresult2,plot
		
	ENDC


		global RENDERER_INIT,new_screen
		global drawApple,cls

