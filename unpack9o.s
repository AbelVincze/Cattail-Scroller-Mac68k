		; ----------------------------------------------
		; Unpack9o
		; ----------------------------------------------
		
			SECTION UNPACK

UNPACK:
_unpack9o:
			; set up variables, and pointer to packed/unpacked data ------------------------
			lea		localsvars(PC),a0		; access local variables with a0
			
			; Unpack9o 68k ASM -------------------------------------------------------------
			; a0 - localsvars
			; a1 - packed data
			; a2 - output address
			; used; d0,d1,d2,d3,d4,d5,d6,d7/(a0,a1,a2),a3,a4
											; SETUP DATA

			move.w  (a1)+,d1				; data height	16bit
			move.w	d1,6(a0)				; store data height in H
			
			moveq	#0,d0					; clear d0...
			move.b	(a1)+,d0				; data width	8bit
				; ******************
				bne.s	unpackskip0			; fix 256 chr wide error (should move to word)
				move.w	#0x0100,d0
unpackskip0:	; ******************		
			move.w	d0,4(a0)				; store data width in DW+1 (DW is a word)
			mulu.w	d0,d1					; adathosszusag kiszamolasa a DW es H-bol
			move.w	d1,8(a0)				; final data length
			
			add.l	a2,d1					; add length to the Output address
			move.l	d1,(a0)					; set end address for finish comp
			
			moveq	#0,d5					; clear d5... BITBC!!!
			lea		CNTbv(PC),a3				; restore CNTbitdepths
			bsr.s	setupVNV				
			lea 	DISTbv(PC),a3				; restore DISTbitdepths
			bsr.s	setupVNV	
			; UNPACK data ------------------------------------------------------------------

											; unpack mainloop
											; isStream is true (>0) for starting
unpackloop:
			cmpa.l	(a0),a2					; compare data length ****** fixed to a2!!!
			bge		@finish					; -> finish
		
			tst.b	d3
			beq.s	@isrepeat				; no -> then repeat
			
			
@isstream:
			; STREAM BLOCK
			bsr.s	pullCNTbits				; d1-ben az eredmeny
@copyloop:
			move.b	(a1)+,(a2)+			; copy the stream;
			dbra	d1,@copyloop
			moveq	#0,d3					; nullazzuk a streamet
			bra.s	unpackloop
			
@isrepeat:	
			; REPEAT BLOCK 
			bsr.s	pullbit					; next?
			roxl.b	#1,d3					; set isStream true!
			
			bsr.s	pullDISTbits			; d1-ben az eredmeny pozicio
			movea.l	a2,a3
			suba.w	d1,a3					; a3-ban a masolas forrasa

			bsr.s	pullCNTbits				; d1-ben az eredmeny:	adathosszusag
			addq.w	#3,d1					; javitjuk	;mmchl

			bsr.s	pullbit					; is neg?
			bcs.s	@repeatnegloop
			
@repeatloop:
			move.b	(a3)+,(a2)+			; copy the repeated stream;
			dbra	d1,@repeatloop
			bra.s	unpackloop

@repeatnegloop:
			move.b	(a3)+,(a2)				; copy and negate the repeated stream;
			not.b	(a2)+
			dbra	d1,@repeatnegloop
			bra.s	unpackloop

			; finish unpacking, here we need to rearrange bytes...

	
			; HELPER FUNCTIONS -------------------------------------------------------------
setupVNV:
			moveq	#2,d0					; read 3 bits
			bsr.s 	pullnbits				; d1-ben az array hossza
			move.w	d1,d3					; d3- counter
			move.b	d1,(a3)+			
@loop2:
			moveq	#3,d0					; read 4 bits
			bsr.s	pullnbits
			move.b	d1,(a3)+				; store in the array
			dbra	d3,@loop2
__rts__:	rts
			
pullnbits:									; input  d0 = n, used d1
			; pull N control bits
			moveq	#0,d1
@loop3:
			bsr.s	pullbit
			addx.w	d1,d1
			dbra	d0,@loop3
			addq.w	#1,d0					; to clear w bits
			rts

pullbit:									; use global(!) d5/d4
			; pull 1 control bit
			tst.b	d5						; bit counter
			bne.s	@next
			moveq	#8,d5					; reset bitcounter
			move.b	(a1)+,d4				; if 0, get a new byte to the buffer
@next:
			subq.b	#1,d5					; decrement counter
			roxl.b	#1,d4						; shift buffer to get a bit out
			rts
			
pullCNTbits:
			; pull VNV 
			lea		CNTbv(PC),a4			; CNTbit data pointer in a4
			bra.s	pulldatabits			; continue
			
pullDISTbits:
			lea		DISTbv(PC),a4			; DISTbit data pointer in a4
			
pulldatabits:								; get the number with the selected format (CNT/DIST bits)
											; change: d0,d1/a4 (uses: d0,d1,d2,d6,d7/a4
			moveq	#0,d6					; clear for safety
			move.b	(a4)+,d6				; d6 length of bit table, a4 bit table
			moveq	#0,d2					; d2 index in the table vbit
			moveq	#0,d7		 			; fix = 0
			bra.s	@loopin					; enter point of the loop
@loop4:						
			bsr.s	pullbit					; is num here?
			bcs.s	@read					; -> yes, then read it and return...
			add.w	d0,d0					; rol.w	#1, d0, azaz x2
			add.w	exp(PC,d0),d7				; fix += Math.pow( 2, actbits ); read from exp table
			addq.w	#1,d2					; vbit++
@loopin:
			move.b	(a4,d2),d0				; d0 actbits			
			dbra	d6,@loop4				; kovetkezo bit;
@read:
			bsr.s	pullnbits				; d0-ban actbits, d1-ben eredmeny
			add.w	d7,d1					; add fix to the returned value
@finish:
			rts

		
		;	move.w	4(a0),unpackedDW
		;	move.w	6(a0),unpackedH
		;	move.w	8(a0),unpackedLength

		;	rts

exp:		dc.w	2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768			
			
localsvars:
ENDa:		dc.l	0	; saved 4 bytes by making this var first...
dw:			dc.w	0
h:			dc.w	0
l:			dc.w	0
CNTbv:		dc.b	0,0,0,0,0,0,0,0,0,0
DISTbv:		dc.b	0,0,0,0,0,0,0,0,0,0


			global UNPACK