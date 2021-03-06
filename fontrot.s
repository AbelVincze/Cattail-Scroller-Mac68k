		includeh MAC_TRAPS_68k.def


		SECTION FONTROT
		
;--------------------------------------------------------------------------------------
;	Generate rotated fonts
;--------------------------------------------------------------------------------------

;fontdb:			globoff.l	1
;chardb:			globoff.l	1

generateRotatedFont:

		; memoria foglalas
		; 63 szog * 64*8 Byte char data * 43 character
		; 512 byte/char
		; 22016 byte/font
		; $152A00 bytes total
		; de compresszalunk, mert kis erofeszites, es render optimalizacio miatt, amugy is meg kell neznunk
		; melyik char-nak mekkora a magassaga, hogy kevesebbet kelljen eloben szamolgatni
		
		move.l	#$5898,d0				; foglalunk eloszor egy kis memoriat...
		dc.w	_Newptr
		bne		.gen_quit				; ha nem ment, akkor kilepes...
		move.l	a0,fontdb(a5)			; ez lesz a fontdb cime
		adda.w	#504,a0
		move.l	a0,chardb(a5)			; kicsit odebb a chardb

		move.w	#4,progress(a5)			; base progress
		; iteracio a szogeken 0-62 (ossz 63 szog, ebbol egy 0, azt helyettesiteni kellene masolorutinnal
		moveq	#$0,d0					; **** d0 - szog szamlalo		  
		move.l	d0,totalsize(a5)		; itt van 0-ank, itt toroljuk vele a total size-ot...

.loopDIR:	; --------------------------------------------------------
			; ROT kiszamitasa (+ elforgatas) legyen ez d7-ben (word)
			movea.l	mem0_ptr+__dirs(a5),a4
			move.w	(a4,d0.w*2),d1			; **** d1 -ben maga a szog!!! (ROT)
			
			; iteracio a karakterpixeleken
			
			; Y iteracio
			;debug			
			moveq	#$0,d2				; **** d2 - Y szamlalo
			
			movea.l	BUFFER(a5),a0		; buffer cime, ide dolgozunk	
			move.w	#$15FF,d3			; de eloszor torlunk $1600 long-ot
.loopCLR:	
			move.l	d2,(a0,d3.w*4)		; vegig 0-zzuk, az Y szamlalo pont kapora jon! 
			dbra	d3,.loopCLR

.loopY:			; --------------------------------------------------------
				move.w	d2,d3
				lsl.w	#6,d3			; d3 = pixelcount (*64 Y*64, sor eleje)
				
				; X iteracio
				moveq	#$0,d4			; **** d4 - X szamlalo
						
.loopX:				; --------------------------------------------------------
					move.w	d3,d5
					add.w	d4,d5		; **** d5 - pixel... kell-e meg?


					movea.l	mem0_ptr+__rotTR(a5),a3		; pontokhoz tartozo sugar meret tablazat
					move.w	(a3,d5.w*2),d7				; d7 = R
					beq		.skippixel
					movea.l	mem0_ptr+__rotTA(a5),a3		; pontokhoz tartozo elforgatasi szog tablazat
					move.w	(a3,d5.w*2),d6				; d6 = A+ROT
					add.w	d1,d6
					
					
					movea.l	mem0_ptr+__rotTRS(a5),a3	; szinusztabla amivel az uj koordinatakat kapjuk meg	
					move.w	d6,d5						; save d6
														; a +$800 zal cosinus lesz a sinuszbol.
					add.w	#$800,d5					; kell egy kis matek, mivel a leheto legpontosabb
					andi.w	#$1FFF,d5					; ertekek elerese erdekeben a tarol szamok nagyon nagyok.
					lsr.w	#2,d5
					move.b	(a3,d5),d5					; d5 -ben lesz sx
					ext.w	d5							; ezzel is matekozunk
					lsr.w	#1,d7	;*** pozitiv szam negativval szorzas para miatt, ezt a szamot elobb felezzuk.
					muls	d7,d5						; megszorozzuk a sugarral
					swap	d5							; es kerekitun
					asr.w	#1,d5	;***
					addi.w	#32,d5						; sx kiszamolva
				;	bmi.s	.skippixel					; ha kisebb 0-nal vagy nagyobb 64-nel akkor skip...
				;	cmpi.w	#64,d5						; de ez nem tortenhet meg, mert mar az input is szurve van!
				;	bge.s	.skippixel

					andi.w	#$1FFF,d6					
					lsr.w	#2,d6
					move.b	(a3,d6),d6					; d6 -ban lesz sy
					ext.w	d6
					muls	d7,d6						; megszorozzuk a sugarral
					swap	d6
					asr.w	#1,d6	;***
					addi.w	#24,d6						; sy
					bmi.s	.skippixel					; Y iranyban viszont ki tudunk logni mert a forras magassaga kisebb 64 pixelnel
					cmpi.w	#48,d6
					bge.s	.skippixel
					lsl.w	#3,d6						; *8 hogy megkapjuk indexnek a fontmemoriaba
					
					movem.l	d0-d4,-(sp)					; most mar kellenek uj regiszterek, ezert mentunk

					; itt egyelore d2 az Y, d4 a cel X szamlalo, ezek meg kellelek
					move.w	d4,d3						; celpixel elerese
					move.l	d4,d1
					lsr.w	#3,d4
					;lsl.w	#3,d2						; Y-t nem adunk hozza, az a sor elejen megvolt
					;add.w	d2,d4						; d4 = index a cel karakter byte-jara a sorban
					andi.w	#7,d3
					move.b	bitn(PC,d3),d3				; d3 = pixel szama bset-hez
					
					move.w	d5,d7						; forras pixel elerese
					lsr.w	#3,d7						; d4 hamarosan felszabadul!
					add.w	d7,d6						; d6 = index a forras karakter byte-jara
					andi.w	#$7,d5						; d5 = pixel index a forrasban
					move.b	bitn(PC,d5),d5				; d5 = pixel szama btst-hez
		
					; iteracio a karaktereken
					
					movea.l mem0_ptr+__funnyfont(a5),a2	; a2-ben a forrasfont
					movea.l	a0,a3						; a3-ban a celfont bazis...
					
					moveq	#43,d0						; karakter szamlalo
		
.loopCH:				; --------------------------------------------------------
						; kell meg:
						; forras char cime, azt indexeljuk
						
						btst.b	d5,(a2,d6.w)			; megnezzuk, hogy a forrasban az adott pixel ott van-e
						bne.s	.skipwrite				; negativ kepen dolgozunk, tehat a 0 a jo
						bset.b	d3,(a3,d4.w)			; ha igen, a celpozicioban bekapcsoljuk a pixelt
						
.skipwrite:
						adda.l	#$180,a2				; forras char cim novelese (48px magasak)
						adda.l	#$200,a3				; cel char cim  novelese (64px magasak)
						
						dbra.s	d0,.loopCH				; az osszes karakteren vegigmegyunk, ezzel a pixel koordinataval...
						; --------------------------------------------------------
			
					movem.l	(sp)+,d0-d4
					
.skippixel:
					addq.w	#1,d4
					cmpi.w	#64,d4
					blt		.loopX
					; --------------------------------------------------------
				bsr		PROGRESS_UPDATE
				
					
				addq.l	#8,a0		; soronkent novelni a cel fontban a cimet (a0 += 8)
				
				addq.w	#1,d2		; noveljuk az Y poziciot
				cmpi.w	#64,d2
				blt		.loopY
				; --------------------------------------------------------


			; --------------------------------------------------------
			; itt most megvagyunk egy font forgatassal
			; ki kell szamolni mekkora hely kell neki,
			; memoriat foglalni a tarolashoz
			; atmasolni
			; font_db-t kitolteni
			; char_db-t kitolteni
			
			movem.l	d0-d4,-(sp)		; nem kell mindent menteni, itt mar keves szukseges valtozo van
			bsr.s	compressfont	
			movem.l	(sp)+,d0-d4
			; --------------------------------------------------------
		
	IFNE DEBUG_MODE	
		movea.l	fontdb(a5),a1		
		bsr	drawresult2				; megnezzuk mi keszul, de csak debug modban...
	ENDC
			
			addq.w	#4,progress(a5)
		;	bsr		PROGRESS_UPDATE
				
			addq.w	#1,d0
			cmpi.w	#63,d0
			blt		.loopDIR
			; --------------------------------------------------------
		
.gen_quit:
		rts

bitn:	dc.b	7,6,5,4,3,2,1,0


		
	
;--------------------------------------------------------------------------------------
		
compressfont:
		; input elforgatas 0-63 d0-ban
		; output a1-ben a lefoglalt memoria cime
		; d1-ben a terulet merete
		movea.l	BUFFER(a5),a1		; **** a1-ban az elforgatott karakterek, egyelore
		
		movea.l	fontdb(a5),a3
		move.w	d0,d1				; elforgatas (0-63)
		move.w	d0,d7				; mentjuk az elforgatast, kicsit randa...
		lsl.w	#3,d1				; *8 a fontdb entry index
		adda.w	d1,a3				; **** a3-ban a fontdb entry-re mutatunk
		movea.l	chardb(a5),a4
		mulu	#CHDBNTRYSZ,d0		; *352 a chardb entryhez (ami ujabb 44 elemet tartalmaz)
		adda.w	d0,a4				; **** a4-ben a chardb entry-re mutatunk (chardata)
		; iteracio a karaktereken
		moveq	#0,d0				; CH szamlalo
		moveq	#0,d5				; total Height szamlalo
		
.loopCH2:
			; iteracio fentrol lefele a karakteren
			;moveq	#0,d1			; Y szamlalo
			
			moveq	#0,d2			; Yoffs szamlalo
			moveq	#63,d3			; Yend  szamlalo
			
.loopFirst:	
				move.l	(a1,d2.w*8),d4
				or.l	4(a1,d2.w*8),d4
				bne.s	.firstFound		
				addq.w	#1,d2
				;cmpi.w	#64,d1		; nem lesz infinite loop ugye???
				bne.s	.loopFirst
				; itt kellene reagalni arra, hogy jajj nincsen data, de olyan nem tortenhet...
.firstFound:
			; d2-ben most az Yoffs *****
			
			; iteracio lerol fel			
.loopLast:			
				move.l	(a1,d3.w*8),d4
				or.l	4(a1,d3.w*8),d4
				bne.s	.lastFound		
				subq.w	#1,d3
				;cmpi.w	#64,d1		; nem lesz infinite loop ugye???
				bne.s	.loopLast
				
.lastFound:
			; d3-ban az utolso sor
			lea		__Yoffset(a4,d0.w*8),a0	
			move.b	d2,(a0)+			; save Y offset
			move.b	d3,(a0)+			; save Y end
			move.w	d3,d1
			addq.w	#1,d1				; a magassag 1-gyel nagyobb, de nekunk igy jobb lesz, ha kevesebb
			sub.w	d2,d1				; d1-ben a magassag
			move.b	d1,(a0)+			; save Height
			add.w	d1,d5				; total magassag szamlalo
			
			adda.w	#$200,a1			; lepjunk a kovetkezo karakterre
			
			addq.w	#1,d0
			cmpi.w	#44,d0
			bne.s	.loopCH2
			
		; memoriafoglalas
		lsl.w	#3,d5					; total magassag*8 = a kello memoriaterulet
		;lea		totalsize(a5),a0
		add.l	d5,totalsize(a5)		 	; rogzitsuk a teljes memoriaigenyt
		move.l	d5,d0					; foglalunk eloszor egy kis memoriat...
		dc.w	_Newptr
		bne.s	.conv_quit				; ha nem ment, akkor kilepes...
			
		; az eredmeny rogzitese a db-ben
		; fontdb -be mentjuk a chardb cimet, es a font cimet
		move.l	a0,__fontBitmapAddr(a3) ; a3 a fontdb entry, 4-es offsetre megy a font cime
		move.l	a0,-(sp)				; **** a0 az uj font memoria, ide masolunk
		move.l	a4,__chardata(a3)		; a4 a chardata-ra mutat, ami a masolashoz kell!!!
		
		; masolas
		
		; CH iteracio
		moveq	#0,d1					; CH szamlalo
		movea.l	BUFFER(a5),a2			; innen olvasunk
		
.loopCH3:			
			moveq	#0,d2				; Y offset lesz
			moveq	#0,d3				; H lesz
			move.b	__Yoffset(a4),d2	; Y offset a chardb-bol
			move.b	__H(a4),d3	  	    ; H a chardb-bol
										; a0- a memoriafoglalas eredmenye
			move.l	a0,__charBitmapAddr(a4)	; most eltaroljuk a character kezdocimet a chardb-be
			
			subq.w	#1,d3				; dbra indexet csinalunk belole...
										; de mi lenne ha mar a tabazatban is kevesebb lenne 1-gyel?
			
				
.loopYC:
				move.l	(a2,d2.w*8),(a0)+			
				move.l	4(a2,d2.w*8),(a0)+			
			
				addq.w	#1,d2
				dbra	d3,.loopYC
			
			
			adda.w	#$200,a2
			addq.w	#1,d1				; kovetkezo karakter
			addq.w	#8,a4				; lepunk a kovetkezo chardb entry-re
		
			cmpi.w	#44,d1
			bne.s	.loopCH3
		
		; megvan a masolas
		move.l	(sp)+,a1				; a1-ben vegezzuk a tomoritett font cimevel
		rts
		
.conv_quit:
	debug
		rts
				
		
		
		global generateRotatedFont

		extern drawresult2,PROGRESS_UPDATE