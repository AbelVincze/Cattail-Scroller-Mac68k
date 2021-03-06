; =========================================================================================================================
; This is a homemade implementation of the famous printf function 2018.06.04
; ALL REGISTERS SAVED! (uses d0-d3/a0-a3)
; ALWAYS PROVIDE THE RIGHT AMOUNT AND SIZE OF PARAMETERS, OR CODE WILL FAIL AFTER RETURN
; TO BE SAFE, ALWAYS PRINT USER INPUTS AS EMBEDDED STRING

				SECTION	PRINTF
	IFNE DEBUG_MODE	
										;----------------------------------------------------------------------------------
PRINTF:			
				suba.w	#$128,sp				; make additional space for sendChar!
				movem.l	d0-d3/a0-a3,-(sp)	; save regs
				move.l	sp,a3				; save sp in a3 to load regs at the end
				adda.w	#$148,sp			; go back to original point, to access return address and parameters
										; a $20-nyi frissen mentett, + $16 hely mentett regisztereknek.
				movea.l	(sp)+,a2		; save return address
				movea.l	(sp)+,a0		; the text address
										;----------------------------------------------------------------------------------

	;debug
.loop0:			move.b	(a0)+,d0		; get the current character from memory
				bne.s	.skipend		; if not zero, let's process it
	;debug			
				move.l	a2,-(sp)		  	  ; our return address
				movem.l (a3)+,d0-d3/a0-a2	; restore regs
				movea.l	(a3),a3				; restore last reg...
				RTS____ "PRINTF"	
				
.skipend:		cmp.b	#"%",d0			; "%" escape?
				bne.s	.skip0			; no, just print
				move.b	(a0)+,d0		; get next character
				cmp.b	#"%",d0			; "%%" ? if, yes just output this "%" this time
				bne.s	.spec4
		
.skip0:			bsr		.sendChar		; output the char
				bra.s	.loop0
										;----------------------------------------------------------------------------------
.spec4:			cmp.b	 #"b",d0		; "%b" - HEX BYTE
				bne.s	.spec0

				moveq	#1,d2			; loop szamlalo, 1+1 szamjegy
				bra.s	.spec0in		; beleugrunk a hex kiirokodba
										;----------------------------------------------------------------------------------
.spec0:			cmp.b	#"x",d0			; "%x" - HEX WORD
				bne.s	.spec1

				moveq	#3,d2			; loop szamlalo, 3+1 szamjegy
.spec0in:		move.w	(sp)+,d1		; d1-ben a parameter (word)
				swap	d1
				bra.s	.loop2			; beleugrung a hex kiirokodba
										;----------------------------------------------------------------------------------
.spec1:			cmp.b	#"X",d0			; "%X" - HEX LONGWORD
				bne.s	.spec2

				move.l	(sp)+,d1		; d1-ben a parameter (long)
				moveq	#7,d2			; loop szamlalo, 7+1 szamjegy
.loop2:									; hex kiirokod (lehet byte-hoz, word-hoz is hasznalni)
				rol.l	#4,d1			; hatraleptetjuk az utolso szamjegyet
.loop2in:		move.w	d1,d0			; d0 mar nem kell, abban lesz a masolat
				and.w	#$0f,d0			; csak a low 4 bit
				add.w	#$30,d0			; hogy szamjegy legyen
				cmp.w	#$3a,d0			; ha 9-esnel nagyobb
				blt.s	.skip2
				addq.w	#$7,d0			; akkor meg egy kis hozzaadassal jon az A-B-C-D-E-F
.skip2:
				bsr.s	.sendChar		; output the char
				dbra	d2,.loop2
				bra.s	.loop0			; back to main loop
										;----------------------------------------------------------------------------------
.spec2:			cmp.b	#"d",d0			; "%d" - DECIMAL word (leading zeros removed)
				bne.s	.spec3

				move.w	(sp)+,d0		; konvertalando szam a d0-ban word hosszusagu, nagyobb ugysem kell, ha igen, atirom...
				clr.w   d1				; +++++++++++++++ start of Ludis Langens ludis@netcom.com code
				clr.w   d2				; https://mikro.naprvyraz.sk/docs/Coding/Atari/BIN2DEC.TXT
				clr.w   d3				; atalakitjuk BCD szamma, mivel word, 3 byte eleg a szamjegyek tarolasara (6 szamjegy)
				neg.w   d0      		; Check for zero
				beq.s   .dskip2
				neg.w   d0      		; Restore value and set X
.dskip0: 		  	addx.w  d0,d0   		; Left align value
        		bcc.s   .dskip0
.dskip1:  	  	abcd.b  d1,d1   		; Double and add X
				abcd.b  d2,d2
				abcd.b  d3,d3
				add.w   d0,d0   		; Next bit to X
				bne.S   .dskip1			; +++++++++++++++ end of Ludis Langens ludis@netcom.com code
.dskip2:		swap	d3				; az eredmenyt osszeallitjuk BCD szamma 1 regiszterben
				move.w	d2,d3
				lsl.w	#8,d3
				or.w	d1,d3
				moveq	#6,d2			; 8 szamjegyunk van (7), abbol 1 0 legalabb maradjon ha teljesen 0.
.loop3:			rol.l	#4,d3			; elso szamjegy hatra
				move.b	d3,d1
				and.b	#7,d1			; 0 az a szamjegy?
				dbne	d2,.loop3		; ha igen skippeljuk, es nezzuk a kovetkezot
				addq.w	#1,d2			; javitjuk a szamlalot, mert kiirasnal 1-gyel tobb kell
				move.l	d3,d1			; betesszuk a megfelelo regiszterbe
				bra.s	.loop2in		; es kiirjuk, mint egy HEX szamot, hiszen BCD
										;----------------------------------------------------------------------------------
.spec3:			cmp.b	#"s",d0			; "%s" - EMBED STRING (not parsed!)
				bne		.loop0
				
				movea.l	(sp)+,a1		; stack-rol a mutato a beilesztendo string-re
.loop4:			move.b	(a1)+,d0		; karakter beolvasasa
				beq		.loop0			; ha nulla, akkor folytassuk az alap stringunket
				bsr.s	.sendChar		; output the char
				bra.s	.loop4			; kovetkezo karakter
.end:
; =========================================================================================================================
										; a stack-en egy felesleges hely lenne,
										; de a printf elejen csinalunk tobb helyet +$16 (6 regiszternek, 1 nek van hely)
										; szabadon hasznalhatjuk:
										; d0-d3/a0-a2
										; ha lesznek szubrutinjai a sendchar-nak, akkor annak is kell helyet hagyni!!!
.sendChar:		
				movem.l	d1-d3/a0-a2,-(sp)
				;move.b	d0,$FFE001
	IFNE DEBUG_MODE
				bsr		DOut_putChar
	ENDC			
				movem.l	(sp)+,d1-d3/a0-a2
				RTS____ ".sendChar"
										; Ennek a funkcionak kell kiirnia a karaktert, leptetni a kurzort, scrollozni
										; ha a kepernyo aljara ertunk
										; egyeb funkciok, amit tudni kell (azaz specialis karakterek):
										; - $0		EOT - Itt ezzel nem lesz dolgunk, mivel a hivo kod elintezi
										; 	- $1		setpos NINCS lehetoseg parameter olvasasra! (max a printf csinalhatna ilyet)
										; - $2		HOME (top)	<- first line, first char
										; - $3		CLS
										; - $4		BOTTOM		<- last line, first char
										; - $a		CR \n	<- first char (no line feed)
										; - $d		LF \r	<- next line, first char (scroll if needed)
										; - $e		LC - (UC/LC toggle helyett)
										; - $f		UC
										; -	$12		invert on - (invert on/off helyett)
										; - $13		invert off
										; - $14		DEL
										; - $15		DOWN
										; - $16		UP
										; - $17		LEFT
										; - $18		RIGHT
										; - $1d		disable auto scroll?
										; - $1e		scroll up
										; 	- $1f		partial scroll up NINCS lehetoseg parameter olvasasra!


; =========================================================================================================================

		; GLOBAL FUNCTIONS
		global PRINTF
	ENDC
		
		dc.w	0
		
		; EXTERNAL FUNTIONS
	IFNE DEBUG_MODE
		extern DOut_putChar
	ENDC