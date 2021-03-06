		includeh MAC_TRAPS_68k.def
		includeh LoMem.def



;--------------------------------------------------------------------------------------

VIDEO_INIT:

;	 	 DEBUG
;	 	 move.l	M_SCR+__baseAddr(a5),d1
;	 	 move.l	M_SCR+__rowBytes(a5),d1
;	 	 move.l	M_SCR+__rect(a5),d1
;	 	 move.l	M_SCR+__height(a5),d1
;	 	 move.l	M_SCR+__width(a5),d1
;	 	 move.l	M_SCR+__BW(a5),d1
;	 	 move.l	M_SCR+__BWA(a5),d1
;	 	 move.l	M_SCR(a5),d0




		movea.l	(a5),a0									; QuickDraw Globals A0-ba, tarolni felesleges
		move.l	screenBits(a0),M_SCR+__baseAddr(a5)		; screen base addr
		move.l	screenBits+6(a0),M_SCR+__rect(a5)		; topleft
		move.l	screenBits+10(a0),M_SCR+__rect+4(a5)	; bottomright
		move.w	screenBits+6+6(a0),d0					; calculate the resolution
		sub.w	screenBits+6+2(a0),d0					; right-left
		move.w	d0,M_SCR+__width(a5)					; Width in pixels
		move.w	screenBits+6+4(a0),d0					; bottom-top
		sub.w	screenBits+6+0(a0),d0
		move.w	d0,M_SCR+__height(a5)		  			; Height in pixels

		subq.w	#$04,a7						; space for _GetMainDevice return value
		dc.w	_GetMainDevice				; _GetMainDevice
		movea.l	(a7)+,a0					; pointer
		move.l	a0,theMainDeviceHandle(a5)	; handle		
		movea.l	(a0),a0
		move.l	$16(a0),thePixelMapHandle(a5)
		
		movea.l thePixelMapHandle(a5),a0
		movea.l	(a0),a0
		move.w	$20(a0),d0
		move.w	d0,thePixelSize(a5)			; pixel depth
		
		cmp.w	#SCR_DEPTH,d0				; ellenorizzuk, hogy az aktualis szinmelyseg egyezik-e	  	  	  
		beq.s	videoskipchange				; ha igen nem kell valtoztatni
		move.w	#SCR_DEPTH,d1				
		move.w	#$0A14,d0					; 0x303C, 0x0A14	HasDepth
		bsr.s	video_palettedispatch		; van olyan opcio amilyen kell?
		beq.s	videoskipchange				; ha nincs akkor skip
		move.w	#SCR_DEPTH,d1				; ha van akkor allitsuk be
		move.w	#$0A13,d0					; 0x303C, 0x0A14	SetDepth
		bsr.s	video_palettedispatch
						
videoskipchange:
		movea.l thePixelMapHandle(a5),a0	; Meg kell egy fontos dolog: rowBytes
		movea.l	(a0),a0
		move.w	$4(a0),d0					; rowBytes
		andi.w	#$7fff,d0					; fix
		move.w	d0,M_SCR+__rowBytes(a5)		  ; save in globals
			
		bsr.s	video_directwindow			; make a direct window to protect our screen
		bra.s	video_cls					; do clear the video RAM
		
video_palettedispatch:
		subq.w	#$02,a7
		move.l 	theMainDeviceHandle(a5),-(a7)
		move.w	d1,-(a7)					; one bit depth needed...
		move.l	#$00010001,-(a7)			; whichFlags, flags
		dc.w	_PaletteDispatch			; 0xAAA2
		move.w	(a7)+,d0					; d0-ban az eredmeny
		rts
		
video_directwindow:
		subq.w	#$04,a7						; parameters for a new window
		clr.l	-(a7)
		pea		M_SCR+__rect(a5)
		clr.l	-(a7)
		move.b	#$01,-(a7)
		move.w	#$0002,-(a7)
		clr.l	-(a7)
		clr.b	-(a7)
		clr.l	-(a7)
		dc.w	_NewWindow
		movea.l	(a7)+,a0
		move.l	a0,directWindow(a5)			; save handle for our window
		rts	
		
VIDEO_DEINIT:
		move.l	directWindow(a5),-(a7)		; the handle for our window
		dc.w	_CloseWindow				; first we close our direct window
		move.w	thePixelSize(a5),d1			; check if the depth was changed
		cmp.w	#SCR_DEPTH,d1				; if yes change it back
		beq.s	videodeinit_end
		move.w	#$0A13,d0					; SetDepth 0x303C, 0x0A14
		bsr.s	video_palettedispatch
videodeinit_end:		
		rts


video_cls:
		clr.l	d2
		move.w	M_SCR+__width(a5),d2
		
		lsr.w	#5,d2
		subq	#2,d1

		movea.l	M_SCR+__baseAddr(a5),a0
		movea.l	a0,a1
		move.w 	M_SCR+__rowBytes(a5),d5
		adda.w	d5,a1
		move.w	d5,d6
		lsl.w	d5
		move.l	#$aaaaaaaa,d0
		move.l	#$55555555,d4
	;	clr.l	d0
		clr.l	d1
		move.w	M_SCR+__height(a5),d1
		lsr.w	#1,d1
		subq	#1,d1
		
		;move.w	#128,d1
vloop0:
		move.w	d2,d3
vloop1:	
		move.l	d0,(a0,d3.w*4)
		move.l	d4,(a1,d3.w*4)
		dbra.s	d3,vloop1

		adda.w	d5,a0
		adda.w	d5,a1
		dbra.s	d1,vloop0
		
		rts


SetEntries:		equ		$aa3f
 	 
colours:
		dc.w	$0000,$6c6c,$5e5e,$b5b5		; c64 light blue
		;dc.w	$0000,$cccc,$cccc,$cccc
		dc.w	$0000,$2525,$1818,$6969		; c64 dark blue
	
do_clut:
		; sys friendly style:
		move.w	#0,-(sp)				*start at zero
		move.w	#1,-(sp)				*255 entries
		pea		colours(pc)				*push colours
		;dc.w	_DEBUGGER
		dc.w	SetEntries				*KABLAM!
		rts


		; DAFB DAC LUT
;		lea		$F9800210,a0
;		move.l	#$03030300,-0x10(a0)
;		move.l	#$ffffffff,-0x0c(a0)
;		move.l	#$ffffffff,-0x08(a0)
;		move.l	#$ffffffff,-0x04(a0)
;		
;		lea		$F9800213,a0
;		move.b	#$00,(a0)	; eloszor elkuldunk, egy vak szint, sehova nem kerul
;		nop
;		move.b	#$00,(a0)
;		nop
;		move.b	#$00,(a0)
;		nop
;		
;		move.b	#$f0,(a0)	; 1. szin
;		nop
;		move.b	#$a0,(a0)
;		nop
;		move.b	#$00,(a0)
;		nop
;		
;		move.b	d0,(a0)	; 0. szin
;		nop
;		move.b	d0,(a0)
;		nop
;		move.b	d0,(a0)
;		nop
;		
		rts
		
		; ----------------------------------------------
		; SlotVBL
		; ----------------------------------------------


; Used lowmem vars (included with lowmem.def)	
;MainDevice		EQU		$08a4
;UTableBase		EQU		$011c
;VBL_RATE		EQU		1			; minden alkalommal vegrehajt. 2 = minden masodik...
		
SLOTVBL_INIT:
									;move.l	MainDevice,a2
		dc.w	$2478,MainDevice	; Fantasm nem ismeri az absolute word cimzest?
		move.l	(a2),a0				; (a0)-ban a MainDevice GDevice record
									; inline GetDCtlEntry( mainDeviceRefNum )
		move.w	(a0),d0				; get the refNum
		not.w	d0	  	  	  		; Change to unit number
		lsl.w	#2,d0				; times 4 bytes/entry
		dc.w	$2278,UTableBase	;movea.l	UTableBase,a1
		move.l	(a1,d0.w),d0		; return DCtlHandle
									; inline GetDCtlEntry end
		movea.l	d0,a0
		movea.l (a0),a0
		move.b	$28(a0),d0			; slot Number
		ext.w	d0	
		move.w	d0,slotnumber(a5)	; save slotnumber to later

		lea		VBLData(PC),a0		; VBL variables to a0
		move.l	a5,theA5(a0)		; save current A5 in VBLData
		moveq	#0,d1				; reset variables
		move.w	d1,theTicks(a0)
		move.l	d1,vsyncFlag(a0)
		move.w	d1,VBLsync(a0)
		move.w	d1,vblPhase(a0)
		moveq	#1,d1
		move.w	d1,qType(a0)
		move.w	d1,vblCount(a0)
		
		lea		VBLroutine(PC),a1
		move.l	a1,vblAddr(a0)
			
		dc.w	_slotVInstall		; a0 VBLTask, d0 slot number
		rts
		
SLOTVBL_DEINIT:
		lea		VBLTask(PC),a0		; a0 VBLTask, d0 slot number
		move.w	slotnumber(a5),d0
		dc.w	_SlotVRemove		; Remove Slot VBL task
		rts

VBLroutine:
		;movem.l	d0-d7/a0-a4,-(sp)
		move.l	a4,-(a7)
		lea		VBLData(PC),a4
		move.l	a5,savedA5(a4)		; save current A5
		movea.l theA5(a4),a5		; restore APP A5
		
		; Ide jon a VBL code, minden amit a Vertical Blanking alatt csinalni kell
		;moveq	#$f0,d0
		;move.b	d0,theColor(a5)
;	 	 tst.w	 progract(a5)
;	 	 bne.s	 .skippregress
;	 	 bsr		 PROGRESS_UPDATE
;.skippregress:		 
		
		move.w	#1,VBLready(a5)
		
		move.w	#1,vblCount(a4)		; reset counter to enable next irq
		movea.l savedA5(a4),a5		; restore original A5
		move.l	(a7)+,a4
		;movem.l	(sp)+,d0-d7/a0-a4
		rts

WAIT_NEXT_VBL:
		move.w	#0,VBLready(a5)
WAIT_VBL:
		tst.w	VBLready(a5)
		beq.s	WAIT_VBL
		move.w	#0,VBLready(a5)
		rts

		
		; GLOBAL FUNCTIONS
		global VIDEO_INIT,VIDEO_DEINIT
		global SLOTVBL_INIT,SLOTVBL_DEINIT
		global WAIT_VBL,WAIT_NEXT_VBL
		
		; EXTERNAL LABELS
		extern BEG,PROGRESS_UPDATE