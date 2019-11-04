			includeh MAC_TRAPS_68k.def

			; ----------------------------------------------
			; Unpackx
			; ----------------------------------------------
		
			SECTION UNPACK

UNPACK:
_unpackx:

			; set up variables, and pointer to packed/unpacked data ------------------------
			lea		locals(PC),a0			; access local variables with a0
	
			; UnpackX 68k ASM --------------------------------------------------------------
			; a0 - locals
			; a1 - packed data
			; a2 - output address
			; a3 - alt unpack buffer
			; used; d0,d1,d2,d3,d4,d5,d6,d7/a3,a4


											; SETUP DATA
			move.w  (a1)+,d1				; start reading the compressed data...
			move.w	d1,iH(a0)				; data height	16bit
			moveq	#0,d0					; clear d0...
			move.b	(a1)+,d0				; data width	8bit
			move.b	d0,iDW+1(a0)
			mulu.w	d0,d1					; uncompressed data lenght calculated from DW*H
			move.w	d1,iL(a0)				; final data length
			
			add.l	a2,d1					; add lenght to OUR address to
			move.l	d1,(a0)					; set end address for finish comp
			
			moveq	#0,d5					; clear d5... BITBC (control bit counter in buffer)

				moveq	#2,d0					; 3 bits to read
				bsr		pullnbits				; read control bits and
				move.b	d1,iFLAGS(a0)			; store in flags (USELOOKUP, NEGCHECK, DIR)

			btst	#0,d1					; reorder needed?
			beq.s		.skipreord
			
		;	dc.w	_DEBUGGER
			
			movea.l	a2,-(SP)				; save output addr
			movea.l	a3,-(SP)				; save alt unpack buffer addr
			movea.l	a3,a2

			;moveq	#0,d0
			move.w	l(PC),d0				;fix end addr. ugly like hell...
			add.l	a2,d0
			move.l	d0,(a0)
			moveq	#0,d0

.skipreord:
				btst	#2,d1					; USELOOKUP?
				beq.s	.skipli					; if we don't use LOOKUP table, just skip this part
				
				move.b	(a1)+,d1				; number of LUT entries
				move.l  a1,iLUT(a0)				; save LUT addr
				adda	d1,a1					; skip LUT entries (as we direct read them later)
				; restore LIbitdepths
				moveq	#2,d6					; 3 bits to read
				lea		LIbv(PC),a3
				bsr.s	setupVNV				; read and store LIbitdepths entries
.skipli:
			moveq	#3,d6					; 4 bits to read
			; restore CNTbitdepths
			lea		CNTbv(PC),a3
			bsr.s	setupVNV				; read and store CNTbitdepths entries

			; restore DISTbitdepths
			lea 	DISTbv(PC),a3
			bsr.s	setupVNV				; read and store DISTbitdepths entries
			
			; INIT unpack loop: isStream to true for starting
			;moveq	#1, d3					; isStream = 1	; don't need to set up, as it is not zero
											; setupVNV has left it -1.W
			

			;dc.w	_DEBUGGER
			
			; UNPACK data ------------------------------------------------------------------
											; unpack mainloop
unpackloop:
			cmpa.l	(a0),a2					; compare data length
			bge		unpackfinish			; -> finish
			
			tst.b	d3						; isStream?
			bne.s	.isstream							
			
.isrepeat:	
			; REPEAT BLOCK 
			bsr.s	pullbit					; next block?
			roxl.b	#1,d3					; set the read bit in isStream  
			
			bsr.s	pullDISTbits			; read distance, and store in d1
			movea.l	a2,a3					; copy write address to a3
			suba.w	d1,a3					; substract distance, and save as source of copy in a3

			bsr.s	pullCNTbits				; read counter, result in d1
			addq.w	#3,d1	 				; fix it

				btst	#1,iFLAGS(a0)		; NEGCHECK (do the compressed data contains NEGCHK bits?)
				beq.s	.repeatloop			; if no, just skip to the copy part
				
			bsr.s	pullbit					; read bit: NEG
			bcs.s	.repeatnegloop			; if NEG is set, we have an inverted block, skip there
			
.repeatloop:
			move.b	(a3)+,(a2)+				; copy the repeated stream
			dbra	d1,.repeatloop			; (read from the already decompressed data)
			bra.s	unpackloop

.repeatnegloop:
			move.b	(a3)+,(a2)				; copy and negate the repeated stream;
			not.b	(a2)+
			dbra	d1,.repeatnegloop
			bra.s	unpackloop

			
.isstream:
			; STREAM BLOCK
			bsr.s		pullCNTbits			; read counter, result in d1
				btst	#2,iFLAGS(a0)			; USELOOKUP?
				bne.s	.lcopy			
.copyloop:
			move.b	(a1)+,(a2)+				; copy the stream;
			dbra	d1,.copyloop
			bra.s	.copyend				; d3 nulla, tehat nem stream a kovetkezo
.lcopy:
				move.w	d1,d3					; LOOKUP table is used, store counter in d3
				
				;dc.w _DEBUGGER
				
				movea.l	Lut(PC),a3				; get LOOKUP table address, and store in a3
.lcopyloop:
				bsr.s	pullLIbits				; read LOOKUP table index
				move.b	(a3,d1),(a2)+			; read byte from LOOKUP table and 
				dbra	d3,.lcopyloop			; repeat until counter=-1
.copyend:				
				moveq	#0,d3					; set next block as repeat!
				bra.s	unpackloop				
	
			; HELPER FUNCTIONS -------------------------------------------------------------
setupVNV:									; input d6 = bitlength of the entries
											; a3 = address of the bitdepths array
			moveq	#2,d0					; setup bitdepths table 
			bsr.s 	pullnbits				; 3 bits to read, result in d1 (number of bitdepths entries)
			move.w	d1,d3					; save as counter for the read loop
			move.b	d1,(a3)+				; store also in the first entry of our array
.loop0:
			move.w	d6,d0					; bits to read
			bsr.s	pullnbits				; get that number of bits
			move.b	d1,(a3)+				; store in the array
			dbra	d3,.loop0
__rts__:	rts
			
pullbit:									; used global(!) d5/d4 (Bitcounter, buffer)
											; pull 1 control bit
			tst.b	d5						; bit counter
			bne.s	.next
			moveq	#8,d5					; reset bitcounter
			move.b	(a1)+,d4				; if 0, get a new byte to the buffer
.next:
			subq.b	#1,d5					; decrement counter
			roxl.b	d4						; read a bit from the buffer
			
			rts						 	 	  	; result in X/C

pullDISTbits:
			lea		DISTbv(PC),a4			; address of the DISTbitdepths array in a4
			bra.s	pulldatabits			; continue
			
pullCNTbits:
			lea		CNTbv(PC),a4			; address of the CNTbitdepths array in a4
			bra.s	pulldatabits			; continue

pullnbits:									; inputd0 = n, used d6, out d1
											; pull N number of control bits
			moveq	#0,d1					; clear result reg
.loop1:
			bsr.s	pullbit					; pull a bit from buffer
			addx.w	d1,d1					; push it to the result
			dbra	d0,.loop1
			addq.w	#1,d0					; to clear w bits
			
			rts

			
pullLIbits:
				lea		LIbv(PC),a4			; address of the LIbitdepths array in a4	
pulldatabits:								; get the number with the selected format (CNT/DIST bits)
											; change: d0,d1/a4 (uses: d0,d1,d2,d6,d7/a4
			moveq	#0,d6					; clear for safety
			move.b	(a4)+,d6				; d6 length of bitdepths array, a4 bitdepths array (first entry is the length)
			moveq	#0,d2					; d2 index in the table vbit
			moveq	#0,d7		 			; fix = 0
			bra.s	.loopin					; enter point of the loop
.loop2:						
			bsr.s	pullbit					; is num here?
			bcs.s	.read					; -> yes, then read it and return...
			add.w	d0,d0					; rol.w	#1, d0, azaz x2
			add.w	exp(PC,d0),d7			; fix += Math.pow( 2, actbits ); read from exp table
			addq.w	#1,d2					; vbit++
.loopin:
			move.b	(a4,d2),d0				; d0 actbits			
			dbra	d6,.loop2				; next bit
.read:
			bsr.s	pullnbits				; d0-ban actbits, d1-ben eredmeny
			add.w	d7,d1
			
			rts
			
unpackfinish:
			btst	#0,iFLAGS(a0)			; USELOOKUP?
			beq.s	.theend
		
			movea.l	(sp)+,a3				; restore original unpack buffer address
			movea.l	(sp)+,a2				; restore original output address
			movea.l	a2,a0					; visszateresi ertekhezd
			;rts
			
			move.w	dw(PC),d1				; max szelesseg
			move.w	d1,d3					
			subq	#1,d1					; szamlalokent...
.loopord1:
			move.w	h(PC),d0				; max magassag
			subq	#1,d0
			moveq	#0,d2
.loopord0:			
			move.b	(a3)+,(a2,d2)				; byte masolasa
			add.w	d3,d2
			
			dbra	d0,.loopord0
			
			addq	#1,a2
			dbra	d1,.loopord1
			
			moveq	#0,d0
			move.w	l(PC),d0
			adda.l	d0,a0
			
			rts
			
.theend:	
			move.w	l(PC),d0
			movea.l	a2,a0
			
			rts						
			
			; table for quick fix
exp:		dc.w	2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768			
	

	
locals:
ENDa:		dc.l	0	; saved 4 bytes by making this var first...
Lut:		dc.l	0
dw:			dc.w	0
h:			dc.w	0
l:			dc.w	0
CNTbv:		dc.b	0						; order is important
CNTbits:	dc.b	0,0,0,0,0,0,0,0
DISTbv:		dc.b	0
DISTbits:	dc.b	0,0,0,0,0,0,0,0
LIbv:		dc.b	0
LIbits:		dc.b	0,0,0,0,0,0,0,0
Flags:		dc.b	0	; 0: DIR	, 1: NEGCHECK	, 2: USELOOKUP

iENDA		equ		0	;ENDa	-locals
iLUT		equ		4	;Lut		-locals
iDW			equ		8	;dw		-locals
iH			equ		10	;h		-locals
iL			equ		12	;l		-locals
iCNTBV		equ		14	;CNTbv	-locals
iCNTBITS	equ		15	;CNTbits	-locals
iDISTBV		equ		23	;DISTbv	-locals
iDISTBITS	equ		24	;DISTbits-locals
iLIBV		equ		32	;LIbv	-locals
iLIBITS		equ		33	;LIbits	-locals
iFLAGS		equ		41	;Flags	-locals

			global UNPACK

;	This compression is optimized for 1 bit graphics data, and small data sizes <64K
;
;	Unpacking mechanism 9o
;
;	Source data (compressed) is read by bytes in linear order forward -> 0, 1, 2, 3,... n-1, n.
;	Target data (decompressed) is also reproduced in linear order forward. The written data is
;	composed of BLOCKs, that can be either STREAMs or REPEATs. STREAMs are series of uncompressed
;	bytes copied from the Source data, and REPEATs are copies of the previously written data,
;	optionally inverted (NEG).
;
;	3 types of informations are stored in the compressed data (source data):
;	- setup data:		byte and bit informations how to handle compressed data.
;	- data bytes:		original content to be copied.
;	- control bits:		an array of bit, containing unpacking flow, they describes the BLOCKs
;
;	While data bytes are unchanged part of the original content, control bits holds informations
;	how to read/write data. These control bits (one or more) are the following:
;	- STREAM FOLLOWS	1 bit:		1: the next BLOCK is a STREAM, 0: next BLOCK is a REPEAT
;	- NEG REPEAT		1 bit:		1: the repeated bytes are inverted, 0: normal repeat
;	- CNTbits			1-x bits:	Counter value stored with variable bitlength*
;	- DISTbits			1-x bits:	Offset values stored with variable bitlenght*
;	- n BIT VALUE		n bits:		binary value
;
;	To allow linear read of source data bytes while variable bitlength data needs to be inserted
;	in the data flow, the control bits are always read by bytes, 8 at a time, and cached. When
;	the cache runs out of bits, a new byte is read into it.
;
;	* Some words about the variable bitlength values (VBV)
;	A VBV type is composed from 0 or more SELECTOR bits, and 1 or more VALUE bits (X)
;	Here are some examples:
;
;	(A)												(B)
;	SELECTOR/VALUEBITS	VALUE	FINAL VALUE			SELECTOR/VALUEBITS	VALUE	FINAL VALUE
;	1XX			3 bits	0-3		0-3					1XXX		4 bits	0-7		0-7
;	01XXX		5 bits	0-7		4-11				01XXXXXX	8 bits	0-63	8-71
;	001XXXX		7 bits	0-15	12-27				00XXXXXXXX 10 bits	0-255	72-327
;	000XXXXX	8 bits	0-31	28-59
;
;	(C)
;	SELECTOR/VALUEBITS	VALUE	FINAL VALUE
;	XXXX		4 bits	0-15		0-15
;
;	The configuration of the VBV can varie depending on the compressed data, so it is stored in
;	the setup data, by the following way:
;	- BV	3bit: the number of different bit length variation-1
;	- BITS	4 bits each: an array of the bit lengths used-1 
;
;	The compressed data structure:
;	(compressed data are read as data bytes (DB), or control bits (Cb)
;
;	Setup data:
;	2 DB:	Uncompressed Height
;	1 DB:	Uncompressed Byte width	(total uncompressed bytes = Height x Byte width)
;	3 Cb:	3 bit:	CNTBV
;	x4 Cb:	4 bit x (CNTBV+1)	-> CNTVBITS array
;	3 Cb:	3 bit:	DISTBV
;	x4 Cb:	4 bit x (DISTBV+1)	-> DISTVBITS array
;
;	Compressed data:
;	Always start with STREAM, and STREAM is always followed by a REPEAT
;
;	STREAM:
;	VBV Cb:	CNTbits, length of the STREAM bytes -1
;	x DB:	STREAM bytes x = CNTbits+1
;
;	REPEAT:
;	1 Cb:	STREAM FOLLOWS, the next block is a stream if set (1)
;	VBV Cb:	DISTbits, offset of the repeat source: source = destination-DISTbits
;	VBV Cb: CNTbits, number of the repeated bytes -4
;	1 Cb:	NEG: the repeated bytes are inverted if set
;
;	The decompression:
;	After all setup bytes/bites are read, and processed, we start with a STREAM.
;	STREAM data is read, then copied to the target.
;	then a REPEAT data is read processed: target is written by copying from its previously
;	written location.
;	then continue with a STREAM or another REPEAT depending on the STREAM FOLLOWS bit.
;	before start writing a BLOCK, check if the total uncompressed bytes are reached or not.
;
;	That's all.
;
;	The code above does all of these in 188 bytes (79 instr.) of 68k assembly (relocatable)...
;	(UNPACK9O byte count without local variables, and exp table)
;
;	A small check loop is included, if the decompression was successfull, the code ends with okloop:
;	Compression tool used: http://iparigrafika.hu/hoh_proto/serialize/serialize_c.html




