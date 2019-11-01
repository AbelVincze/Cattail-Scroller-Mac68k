		includeh MAC_TRAPS_68k.def
		includeh LoMem.def
		
		

		; needed for code segment (header) to disable default long header
BEG:	dc.w	$0000
		dc.w	$0001


		entry								; execution starts here - mandatory for Fantasm 5

start:
		pea		QDglobals+grafSize-4(a5)	; grafglobals location under A5 world
		dc.w	_InitGraf					; init + copy grafglobals
		dc.w	_InitFonts					; standart Mac App initializations
		dc.w	_TEInit
		dc.w	_InitWindows
		dc.w	_InitMenus
		clr.l	-(a7)
		dc.w	_InitDialogs
		dc.w	_InitCursor
		dc.w	_HideCursor	
		
		bsr		PROGRESS_INIT				; initialize progressbar
		
		bsr		VIDEO_INIT					; Set screen colour depth, and get safe access to the VIDEO RAM
		bsr		SLOTVBL_INIT				; VBL irq start
		bsr		RENDERER_INIT				; initialize renderer, and create "screens"

		bsr		LOAD_RSRCS					; tomoritett resourceok betoltese/kicsomagolasa...
		bsr		PROGRESS_RESET				; create progressbar
		bsr		drawApple					; Apple logo kirakasa
		bsr		generateRotatedFont			; calculate 63 font rotations

		bsr		cls							; Clear Apple logo and the progressbar
	IFNE DEBUG_MODE		
		bsr		testDOut					; debug outputra only works on screen 1024+			
	ENDC
		bsr		DEMO_INIT
	
mainloop:
		bsr		WAIT_NEXT_VBL				; waiting for Vertical Blanking

		bsr		DEMO_LOOP					; DEMO effect	
		bsr.s	KEYCONTROL					; read keys
		bne.s	mainloop					; exit if SPACE is pressed
		
			
quit:	
		bsr		SLOTVBL_DEINIT					; Clear VBL interrupt
		bsr		VIDEO_DEINIT					; Give back the screen to the system
		
		move.w	#$FF,d0							; Flush events... nem kell semmit hatrahagyni
		dc.w	_FlushEvents
		;dc.w	_ShowCursor	
		dc.w	_ExitToShell					; kurzort visszaadja, de flush events kell
		;rts									; rts is lehetne, ExitToShell nem tudom miert jobb, ha jobb...


KEYCONTROL:										; some extra keyboard controls
		tst.w	$0184							; lastKey		
		beq.s	.nothing						; skip if nothing is pressed
		
		move.w	$0184,d0						; read lastKey
		andi.w	#$FF,d0							; we only need the lowbyte

		cmpi.w	#"1",d0							
		bne.s	.n0
		move.w	#1,std(a5)						; KEY "1" -> set scroll direction to LEFT
.n0:
		cmpi.w	#"2",d0
		bne.s	.n1
		move.w	#0,std(a5)	  	    			; KEY "2" -> disable scrolling
.n1:
		cmpi.w	#"3",d0
		bne.s	.n2
		move.w	#-1,std(a5)		 	 	 	; KEY "3" -> set scroll direction to RIGHT
.n2:
		cmpi.w	#"4",d0
		bne.s	.n3
		move.w	#1,sind(a5)						; KEY "4" -> set sin animation to normal 
.n3:
		cmpi.w	#"5",d0
		bne.s	.n4
		move.w	#0,sind(a5)						; KEY "5" -> disable sin animation 
.n4:
		cmpi.w	#"6",d0
		bne.s	.n5
		move.w	#-1,sind(a5)					; KEY "6" -> set sin animation to reverse 
.n5:
.nothing:

		cmpi.w	#" ",d0							; SPACE to exit
		rts
		
; --------------------------------------------------------------------------------------
; LOADER
; Decompress all resources, and assign pointers to the unpacked data
;
; Packed resources
; 
; Resources are packed with my own compression tool MPackerX
; available at https://github.com/AbelVincze/MPackerX-commandline
;
; RSRC    packed/unpacked	filename
; $80		 476	 2688	16x14_terminalfont_LR2.pkx
; $81		 164	 1116	apple_logo_96x93_LR2.pkx
; $82		1700	16896	64x48_funnyscrollfont.pkx
; $83		2560	 8192	rotTA_NR2.pkx
; $84		 564	 2048	rotTRS_NR8.pkx
; $85		3174	 8192	rotTR_.pkx
; $86		 322	 1024	distsc.pkx
; $87		1714	 2434	sin_sina_mul_dirs_NR2.pkx
;------------------------
;		   10674	42590 	we need $A65E bytes of memory to decompress all packed resources

			SECTION LOAD_RSRCS
			
rsrcc:		dc.w	$80							; the first resource to load, also act as counter...

LOAD_RSRCS:

			move.l	#$A65E,d0					; set size of needed memory
			dc.w	_Newptr						; allocate memory
			bne		load_quit					; exit on error
			move.l	a0,mem0_ptr(a5)				; store in main pointer
			move.l	a0,current_ptr(a5)			; also store as temporary pointer (will change after each decompressed rsrc
												; as we decompress all content to one memory block, but need to know where each
												; data is unpacked)
			move.l	#$5800,d0					; our unpack buffer which is reused later for other purposes
												; this is why the size
			dc.w	_Newptr						; allocate memory
			bne.s	load_quit2	 	 			; exit on error
			move.l	a0,BUFFER(a5)				; store in BUFFER pointer

.rsrcloop:										; iterating over all the resources in order
			subq.l	#4,a7
			move.l	#"DATA",-(SP)				; resource type DATA
			move.w	rsrcc(PC),-(SP)				; resource ID (will increment later)
			dc.w	_Get1Resource				; open resource
			movea.l	(SP)+,a1					; get the resulting handler
			move.l	a1,rsrc_handler(a5)			; store handler
			
			move.l	a1,d0						; checking successfull opening
			tst.l	d0
			beq.s	load_quit3					; exit on error
			
			move.l	(a1),a1						; get the pointer from the handler
			movea.l	current_ptr(a5),a2			; source a1, target a2, alternativ buffer a3 (in case of reordered rsrc*)		
			movea.l	BUFFER(a5),a3
			
				; We store the target address for each resource 
				move.w	rsrcc(PC),d0			; the current resource ID act now as index
				andi.w	#$7F,d0					; use only the lower 7 bits
				lea		mem0_ptr(a5),a0			; starting point is mem0_ptr, the others follows it in a5 world
				move.l	a2,(a0,d0.w*4)			; we store the current pointer to the right place
			
			bsr		UNPACK						; decompressing resource
			move.l	a0,current_ptr(a5)			; a0 - points to the first byte after the decompressed data in memory
			
			; no way of releasing resources? didn't find it...
			lea		rsrcc(PC),a1		
			addq	#1,(a1)						; incrementing the resource ID
			cmp.w	#$88,(a1)					; check if we reached the end?
			bne.s	.rsrcloop					; if not, just load the next one
			
			; finished loading/decopressing, now add missing pointers
			; data that was loaded along with another block of data in the same resources, needs their own pointers
			lea		mem0_ptr+__sin(a5),a1			;__sin was packed in the same resource as __sin_alt,__multable,__dirs
			movea.l	(a1)+,a0					; __sin, the stored pointer
			adda.w	#1024,a0					; increment pointed location
			move.l	a0,(a1)+					; __sin_alt
			adda.w	#1024,a0
			move.l	a0,(a1)+					; __multable
			adda.w	#260,a0	
			move.l	a0,(a1)						; __dirs

			; the same is true for __distsin, which was packed together with __distcos
			move.l	mem0_ptr+__distsin(a5),d0	; __distsin, the stored pointer
			add.w	#512,d0						; increment
			move.l	d0,mem0_ptr+__distcos(a5)	; store for __distcos	

		
			RTS____	"LOAD_RSRCS"				; We're done!
			
load_quit3:
			movea.l	BUFFER(a5),a0		  	  	; free up memory
			dc.w	_Disposeptr
load_quit2:
			movea.l	mem0_ptr(a5),a0				; free up memory
			dc.w	_Disposeptr
load_quit:
			RTS____ "LOAD_RSRCS_quit"	




PROGRESS_INIT:
		moveq	#0,d1
.progress_setvars:
		moveq	#0,d0
		move.w	d0,progress(a5)
		move.w	d0,progrvis(a5)
		move.w	d1,progract(a5)
		
		lea		PROGRESS_UPDATE(PC),a0	
		move.l	a0,progrFN(a5)
		RTS____ "PROGRESS_INIT"

PROGRESS_RESET:
		movea.l	D_SCR+__rowStartAddrs(a5),a0
		movea.l 280*4(a0),a0		;300.sor
		adda.w	#14,a0
		move.l	a0,progrbase(a5)
		move.w	D_SCR+__rowBytes(a5),d0		; nextline
		
		lea		progressbarleft(PC),a1		; source
		moveq	#3,d1
.loopreset:		
		move.w	(a1)+,(a0)
		move.w	6(a1)+,2(a0)
		adda.w	d0,a0
		dbra.s	d1,.loopreset
		
		moveq	#1,d1
		bra.s	.progress_setvars

PROGRESS_UPDATE:
		movem.l	d0-d4/a0-a1,-(sp)
		tst.w	VBLready(a5)
		beq.s	.skipupdate
		move.w	#0,VBLready(a5)
		
		move.w	progrvis(a5),d0				; lathato progress
		cmp.w	progress(a5),d0
		bgt.s	.skipupdate
		move.l	progrbase(a5),a0
		
		addq.w	#1,d0
		move.w	d0,progrvis(a5)				; save incremented value
		cmpi.w	#100,d0
		bne.s	.notfinished
		clr.w	progract(a5)
.notfinished:
		move.w	d0,d2
		andi.w	#7,d2	; right shift
		lsr.w	#3,d0	; byte index
		
		lea		progressbarright(PC),a1		; source
		move.w	D_SCR+__rowBytes(a5),d3		; nextline
		moveq	#3,d1
.loopupdate:		
		move.w	(a1)+,d4					; draw the bar
		lsr.w	d2,d4
		move.w	d4,2(a0,d0)	
		adda.w	d3,a0
		dbra.s	d1,.loopupdate
		
		
		
.skipupdate:
		movem.l	(sp)+,d0-d4/a0-a1
		RTS____ "PROGRESS_UPDATE"

;PROGRESS_CLEAR:		
;		rts

progressbarleft:
		dc.w	%1111111111111110
		dc.w	%1111111111111100
		dc.w	%1111111111111100
		dc.w	%1111111111111110
progressbarright:		
		dc.w	%0111111111111111
		dc.w	%0011111111111111
		dc.w	%0011111111111111
		dc.w	%0111111111111111





	IFNE DEBUG_MODE
;--------------------------------------------------------------------------------------
;	DebugOut - 6x14 font output to virtual window
;--------------------------------------------------------------------------------------

getdoutba:
	debug
		move.l	dout_by(a5),d0
		rts
	
initDebugOut:
		;dc.w	_DEBUGGER
		clr.l	d5
		move.w	M_SCR+__rowBytes(a5),d5		  ; pixel line add
		move.w	d5,d6						; save for mulu
		mulu	#14,d5						; char line add (*14)
		move.l	d5,dout_lineadd(a5)			;* store value
		
		move.l	M_SCR+__baseAddr(a5),d4		  ; screen base
		mulu	d3,d6						; y offsettel megszorozzuk
		lsl.l	#3,d6						; *8 hogy char magassag
		add.l	d2,d4						; + offset x char
		add.l	d6,d4	
		move.l	d4,dout_baseaddr(a5)		;* store base address

		move.w	d0,dout_w(a5)
		move.w	d1,dout_h(a5)
		move.w	#1,dout_bx(a5)
		clr.w	dout_by(a5)

		bsr.s	clsDOut
		
		RTS____ "initDebugOut"
		
clsDOut:
		movea.l	dout_baseaddr(a5),a0
		move.w	dout_w(a5),d0
		lsr.w	#2,d0					; x long cnt
		subq.w	#1,d0
		move.w	d0,d3					; save for repeated loop
		move.w	dout_h(a5),d1
		;lsl.w	#3,d1
		subq.w	#1,d1
		moveq	#-1,d2
		clr.l	d4
		move.w	M_SCR+__rowBytes(a5),d4	; calc line end add rowbyte-width
		sub.w	dout_w(a5),d4
clsDOut_loop0:
		move.l	d2,(a0)+
		dbra.s	d0,clsDOut_loop0
		adda.l	d4,a0
		move.w	d3,d0
		dbra.s	d1,clsDOut_loop0
		
		RTS____ "clsDOut"
		
scrollDOut:
		movea.l	dout_baseaddr(a5),a0	; write
		movea.l	a0,a1
		adda.l	dout_lineadd(a5),a1		; read
		
		move.w	dout_w(a5),d0
		lsr.w	#2,d0					; x long cnt
		subq.w	#1,d0
		move.w	d0,d3					; save for repeated loop

		move.w	dout_h(a5),d1			; y pixel counter
		;lsl.w	#3,d1
		sub.w	#13,d1
		
		
		clr.l	d4
		move.w	M_SCR+__rowBytes(a5),d4	; calc line end add rowbyte-width
		sub.w	dout_w(a5),d4
		
scrollDOut_loop0:
		move.l	(a1)+,(a0)+
		dbra.s	d0,scrollDOut_loop0
		adda.l	d4,a0
		adda.l	d4,a1
		move.w	d3,d0
		dbra.s	d1,scrollDOut_loop0
		
										; clean up last line
		moveq	#-1,d2					; write at the end
		moveq	#13,d1								
		bra.s	clsDOut_loop0			; finish as cls


		SECTION DOut_putChar
DOut_putChar:	 	 	 	 	 	 	; d0-ban az ascii kod amit ki akarunk irni
		and.w	#$ff,d0

		cmp.b	#$20,d0
		bge.s	.chk0				
										; 0-1f kodok (vezerles stb)
		cmp.b	#$d,d0
		beq.s	.LF								
										
										
		rts		
.chk0:	
		;DEBUG
		sub.w	#$20,d0

		movea.l	dout_baseaddr(a5),a0	; a charakter pozicioja a video memoriaban
		move.w	dout_by(a5),d1			; hanyadik sor?
		beq.s	.skip0
		move.l	dout_lineadd(a5),d2
	;	lsr.l	#3,d2	
		mulu	d1,d2					; szorozzuk meg
	;	lsl.l	#3,d2
		adda.l	d2,a0					; adjuk a cimhez
.skip0:
		move.w	dout_bx(a5),d2			; karakterpozicio soron belul
		;adda.w	d2,a0
			; calculate 6px wide char offset in byte, and shifts
			mulu	#6,d2
			move.w	d2,d3
			lsr.w	#3,d3
			adda.w	d3,a0				; megvan a memoriaoffset, hozza is adjuk
			andi.w	#7,d2				; ez meg a shift
		
		mulu	#28,d0					; d0-ban a character index
		movea.l mem0_ptr(a5),a1			; a font cime a memoriaban
		adda.l	d0,a1					; a karakter cime a memoriaban
		;move.w	M_SCR+__rowBytes(a5),d1	; novekmeny a kovetkezo sorhoz
		
		moveq	#13,d3					; szamlalo
.loop0:
		move.w	(a1)+,d0
		move.w	(a0),d1
		lsr.w	d2,d0
		or.w	charOR(PC,d2*2),d0	
		or.w	screenOR(PC,d2*2),d1
		and.w	d0,d1
		move.w	d1,(a0)
		
		move.w	M_SCR+__rowBytes(a5),d1	; novekmeny a kovetkezo sorhoz
		adda.w	d1,a0
				
		dbra.s	d3,.loop0
		
		
		; d2-ben meg a bx
		move.w	dout_bx(a5),d2
		addq.w	#1,d2
		cmp.w	dout_w(a5),d2
		blt.s	.skip1
.LF:	
		moveq	#1,d2
		move.w	dout_by(a5),d3
		addq.w	#1,d3
		move.w	d3,d1
		mulu	#14,d1
		cmp.w	dout_h(a5),d1
		blt.s	.skip2
		
		;scroll up here
		move.w	d2,-(sp)				;save d2 as we only need it...
		bsr	scrollDOut
		move.w	(sp)+,d2
		
		bra.s	.skip1					;skip y increment!
.skip2:
		move.w	d3,dout_by(a5)
.skip1:	
		move.w	d2,dout_bx(a5)
		
		RTS____ "DOut_putChar"
		
charOR:
		dc.w	%0000000000000000
		dc.w	%1000000000000000
		dc.w	%1100000000000000
		dc.w	%1110000000000000
		dc.w	%1111000000000000
		dc.w	%1111100000000000
		dc.w	%1111110000000000
		dc.w	%1111111000000000
		
screenOR:
		dc.w	%1111111100000000
		dc.w	%0111111110000000
		dc.w	%0011111111000000
		dc.w	%0001111111100000
		dc.w	%0000111111110000
		dc.w	%0000011111111000
		dc.w	%0000001111111100
		dc.w	%0000000111111110
			
testDOut:									; let's print something on the Debug output
		moveq	#384/8,d0		;	window char width 
		move.l	#768-8,d1		;	window char height
		moveq	#80,d2			;	window offset char X
		moveq	#0,d3			;	wondow offset char Y

		bsr		initDebugOut	; az ablak inicializalasa, ezutan tudunk bele irni

		move.l	dout_baseaddr(a5),-(sp)
		move.l	totalsize(a5),-(sp)
		move.l	chardb(a5),-(sp)		
		move.l	fontdb(a5),-(sp)
		move.l	current_ptr(a5),-(sp)		
		move.l	mem0_ptr+__dirs(a5),-(sp)
		move.l	mem0_ptr+__multable(a5),-(sp)
		move.l	mem0_ptr+__distcos(a5),-(sp)
		move.l	mem0_ptr+__distsin(a5),-(sp)
		move.l	mem0_ptr+__sin_alt(a5),-(sp)
		move.l	mem0_ptr+__sin(a5),-(sp)
		move.l	mem0_ptr+__rotTR(a5),-(sp)
		move.l	mem0_ptr+__rotTRS(a5),-(sp)
		move.l	mem0_ptr+__rotTA(a5),-(sp)
		move.l	mem0_ptr+__funnyfont(a5),-(sp)
		move.l	mem0_ptr+__appledata(a5),-(sp)
		move.l	mem0_ptr+__fontdata(a5),-(sp)
		move.l	BUFFER(a5),-(sp)
		
		move.w	M_SCR+__height(a5),-(sp)
		move.w	M_SCR+__height(a5),-(sp)
		move.w	M_SCR+__width(a5),-(sp)
		move.w	M_SCR+__width(a5),-(sp)
		move.w	M_SCR+__rowBytes(a5),-(sp)
		move.w	M_SCR+__rowBytes(a5),-(sp)
		move.l	M_SCR+__baseAddr(a5),-(sp)
		pea		text0(PC)
		bsr		PRINTF
		
		RTS____ "testDOut"
			
text0:
		dc.b	"Cattail Scroller DEBUG version by Macc",$d
		dc.b	"build #[build$], on [longyear$]/[month$]/[day$] at [time_with_secs$]",$d,$d
		dc.b	"screen baseAddr:  $%X",$d
		dc.b	"screen rowbytes:  $%x (%d bytes)",$d
		dc.b	"screen width:     $%x (%d px)",$d
		dc.b	"screen height:    $%x (%d px)",$d,$d

		dc.b	"BUFFER:           $%X",$d
		dc.b	"__fontdata Addr:  $%X",$d
		dc.b	"__appledata Addr: $%X",$d
		dc.b	"__funnyfont Addr: $%X",$d
		dc.b	"__rotTA:          $%X",$d
		dc.b	"__rotTRS:         $%X",$d
		dc.b	"__rotTR:          $%X",$d
		dc.b	"__sin:            $%X",$d
		dc.b	"__sin_alt:        $%X",$d
		dc.b	"__distsin:        $%X",$d
		dc.b	"__distcos:        $%X",$d
		dc.b	"__multable:       $%X",$d
		dc.b	"__dirs:           $%X",$d,$d
		dc.b	"current_ptr:      $%X",$d,$d
		dc.b	"__fontdb:         $%X",$d
		dc.b	"__chardb:         $%X",$d,$d
		dc.b	"rotfont size:     $%X",$d
		dc.b	"dbout_baseAddr:   $%X",$d
		dc.b	0
		align	2

		global DOut_putChar
		global getdoutba
		
		extern PRINTF
		
	ENDC
			



;----------------------------------------------------------


		; GLOBAL FUNCTIONS
		global BEG,PROGRESS_UPDATE	
				
		; EXTERNAL FUNCTIONS
		extern RENDERER_INIT
		extern UNPACK
		extern VIDEO_INIT,VIDEO_DEINIT
		extern SLOTVBL_INIT,SLOTVBL_DEINIT
		extern WAIT_VBL,WAIT_NEXT_VBL
		extern plot,drawApple,cls
		extern drawresult
		
		extern DEMO_INIT,DEMO_LOOP
		
		extern generateRotatedFont
		
		
		