		SECTION DEMO
;--------------------------------------------------------------------------------------

CY:			EQU		171
ST:			EQU		4
M:			EQU		180*ST

sinoffs:	dc.w	$1C0


DEMO_INIT:
	
		bsr		FRAMEBUFFER_INIT	; Initialize framebuffers
		bsr		SCROLLER1_INIT		; Initialize the character screller

		move.w	#-1,sind(a5)		; set default sinus anim direction

		RTS____	"DEMO_INIT"
		
DEMO_LOOP:
	IFNE DEBUG_MODE
;		bsr.s	plotCoords			; elozo torlese
	ENDC

		bsr		RENDER_CHARS		; Render buffer contents on the screen	
		eori.w	#1,frame(a5)		; switch active framebuffer

		bsr.s	GENERATE_COORDS		; Generate new set of coordinates
		bsr		SCROLLER1_STEP		; character scroller
		bsr		SCROLLER1_GENCHL	; generate a list of 13 chars with coords/dirs
		bsr		MAKE_RENDERLIST		; fill buffers with shifted character bitmap data

		lea		sinoffs(PC),a0		; advance in sin movement
		move.w	sind(a5),d0			; with direction
		add.w	d0,(a0)				; write back changed value


	IFNE DEBUG_MODE
;		bsr.s	plotCoords			; uj kirajzolasa
	ENDC

		RTS____	"DEMO_LOOP"

	IFNE DEBUG_MODE
plotCoords:
		lea		D_buff(a5),a4		; a4 D_buff base
		moveq	#0,d2				; **** mi szamlalo (0-129)
.loopplot:
			move.w	(a4)+,d0		; dir nem kell
			move.w	(a4)+,d0		; x
			move.w	(a4)+,d1		; y
			addq.w	#2,a4			; skippelunk, hogy a kov rekord 8 aligned legyen
				asr.w	#4,d0
				asr.w	#4,d1
				sub.w	#32,d0
				bsr		plot
			addq.w	#1,d2
			cmpi.w	#130,d2
			bne.s	.loopplot
			
		RTS____	"plotCoords"
		
		extern plot,PRINTF

	ENDC
		
		; 130 koordinata kiszamolasa, ezeken fog lepkedni a font
		
GENERATE_COORDS:
		move.l	a6,-(sp)					; save extra reg
		
		move.w	sinoffs(PC),d1				; d1 sinoffs
		movea.l	mem0_ptr+__sin_alt(a5),a2	; a2 sinalt
		movea.l	mem0_ptr+__sin(a5),a3		; a3 sin
		move.w	d1,d2						; save sinoffs
		add.w	#64,d2
		andi.w	#$1FF,d2
		move.w	(a2,d2.w*2),d2				; d2 dir!!! (starting dir)
		asr.w	#1,d2						; dirt felezzuk...
		
		movea.l	mem0_ptr+__distcos(a5),a0	; a0 distcos
		movea.l	mem0_ptr+__distsin(a5),a1	; a1 distsin
		
		lea		D_buff(a5),a4				; a3 D_buff base
		movea.l	mem0_ptr+__multable(a5),a6  ; a6 mul
		
		; mi loop
		moveq	#0,d0		; **** mi szamlalo (0-129)
		moveq	#0,d6		; X
		move.w	#CY*16,d7	; Y
		move.w	d1,d3		; Y-hoz adjunk hozza meg egy offset-et is:
		addi.w	#480,d3
		andi.w	#$1ff,d3
		move.w	(a3,d3.w*2),d3
		asr.w	#4,d3
		sub.w	d3,d7		
		move.l	d0,dai(a5)	; dai reset
		
.loopmi:	
			move.w	d2,d3
			asr.w	#7,d3
			add.w	#$100,d3		

			; dx, dy
			move.b	(a0,d3.w),d4			; d4 = dx
			ext.w	d4
			move.b	(a1,d3.w),d5			; d5 = dy
			ext.w	d5

			move.w	d2,d3
			asr.w	#2,d3
			move.w	d3,(a4)+		; taroljuk a dir-t
			move.w	d6,(a4)+		; x-et
			move.w	d7,(a4)+		; y-t
			addq.w	#2,a4			; paddoljuk, hogy a kov rekord 8 aligned legyen
		
			; dirAdd
			move.l	dai(a5),d3
			lsr.l	#8,d3
			add.w	d1,d3			; sinoffs + dai>>8
			andi.w	#$1FF,d3
			move.w	(a3,d3.w*2),d3	; d3 = sin(d3)
			muls	(a6,d0.w*2),d3	; * mul[mi]
			swap	d3				; result>>17
			asr.w	#2,d3
			
			add.w	d3,d2			; dir += dirAdd
			add.w	d4,d6			; x += dx
			;sub.w	d4,d6			; x += dx
			sub.w	d5,d7			; y -= dy
			
			addi.l	#M,dai(a5)
			addq.w	#1,d0
			cmpi.w	#130,d0
			bne.s	.loopmi

.skipend:		
		move.l	(sp)+,a6			

		RTS____	"GENERATE_COORDS"
		
; text scroller (only subpos/and text char's)		
		

__charcode:	EQU		0				; charlist egy elemenek valtozoi
__chardir:	EQU		2
__charx:	EQU		4
__chary:	EQU		6


SCROLLER1_GENCHL:					; scroller 1 generate charList:
	;debug
		lea		charlist(a5),a0		; a0 charList tomb
		lea		sc1txt(PC),a1		; a1 text
		lea		D_buff(a5),a2		; D_buff - itt taroltuk a koordinatakat ugye...

		moveq	#12,d0				; chr szamlalo
		moveq	#0,d3				; chr buffer torles - hogy wordkent tarolhassuk a byte-ot
		move.w	stc(a5),d1			; stc character position in the text
		move.w	stsc(a5),d2			; stsc scroll sub pos

.sgloop:

		move.b	(a1,d1.w),d3
		move.w	d3,(a0)+			; char - taroljuk a charlist tombben
		
		move.w	(a2,d2.w*8),d4
		move.w	d4,(a0)+			; dir
		
		move.w	2(a2,d2.w*8),d4
		asr.w	#4,d4
		move.w	d4,(a0)+			; X
		
		move.w	4(a2,d2.w*8),d4
		asr.w	#4,d4
		move.w	d4,(a0)+			; Y
		
		addq.w	#1,d1
		add.w	#10,d2
		
		dbra.s	d0,.sgloop
		
		RTS____	"SCROLLER1_GENCHL"

SCROLLER1_STEP:						; scroller 1 szamlaloinak leptetese
		move.w	std(a5),d0			; d0 = std
		move.w	stsc(a5),d1			; d1 = stsc
		sub.w	d0,d1					; noveljuk/csokkentjuk a scroll poziciot
		bpl.s	.ssskipneg
		
		add.w	#10,d1				; ha 0-nal kisebb, akkor elozo karakter
		add.w	d0,stc(a5)
		cmpi.w	#sc1len,stc(a5)
		blt.s	.ssend
		move.w	#-1,d0
		bra.s	.ssend	
.ssskipneg:
		cmpi.w	#10,d1
		blt.s	.ssend
		sub.w	#10,d1				; ha 10-nel nagyobb, akkor kov kararkter
		add.w	d0,stc(a5)
		bne.s	.ssend
		move.w	#1,d0
		bra.s	.ssend		

SCROLLER1_INIT:						; scroller 1 szamlaloinak inicializalasa
		moveq	#0,d1
		moveq	#1,d0
		move.w	d1,stc(a5)
.ssend:								; ez egyben az elozo rutin vege is!	
		move.w	d1,stsc(a5)
		move.w	d0,std(a5)
		
		RTS____	"SCROLLER1_STEP"
		
sc1len: EQU		281-24				; a szoveg hossza	

sc1txt:		;          1         2         3         4         5         6
				;0123456789012345678901234567890123456789012345678901234567890123
		dc.b	"                     hello 2019 and welcome here @ tryagain with"
		dc.b	" my funny cat-tail scroller... the text is just for checking how"
		dc.b	" the fuck it looks like. ok we saw it, now restart but first the"
		dc.b	" whole charset: @abcdefghijklmnopqrstuvwxyz0123456789",$27,$22,".,!"
		dc.b	"?*                        "
		align	2

charcodes:
		; ascii 32-127 to chrcode 0-43	
		dc.b	-1,41,39,-1,-1,-1,-1,40,-1,-1,43,-1,38,-1,37,-1
		dc.b	27,28,29,30,31,32,33,34,35,36,-1,-1,-1,-1,-1,42
		dc.b	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
		dc.b	16,17,18,19,20,21,22,23,24,25,26,-1,-1,-1,-1,-1
		dc.b	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
		dc.b	16,17,18,19,20,21,22,23,24,25,26,-1,-1,-1,-1,-1

;----------------------------------------------------------------

FRAMEBUFFER_INIT:
		moveq	#0,d0
		move.w	d0,frame(a5)			; init frame, start with frame A
		move.l	BUFFER(a5),a0
		movea.l	a0,a1					; framebuffer pointerek beallitasa
		adda.w	#__framebuffers,a1		; FRAME_A ptr
		move.l	a1,(a0)
		move.l	a1,8(a0)
		bsr.s	.clear_fb		 		; toroljuk FRAME_A valtozoit
		adda.w	#FBNTRYSZ,a1			; FRAME_B ptr
		move.l	a1,4(a0)
										; toroljuk FRAME_B valtozoit
.clear_fb:								; a1 mutat egy FRAMEBUFFER-re
		move.l	d0,__itemcount(a1)					;
		move.l	d0,__firstitem(a1)

		RTS____	"FRAMEBUFFER_INIT"
		
MAKE_RENDERLIST:
		; renderlista + bufferek shiftelt charbitmappal keszen a renderelesre
		; sources:
		; 	fontdb/chardb
		; 	chardata
		; 	charlist
		; target:
		;	framebuffer
		;	renderlist
		
		; set up frame buffer access
		; a4 legyen a renderlist elso elemenek a cime
		move.w	frame(a5),d0				; frame mint index
		movea.l	BUFFER(a5),a4
		movea.l	(a4,d0.w*4),a4				; kiolvassuk a bufferbol a megfelelo cimet
		move.l	a4,FB(a5)
		addq.w	#__renderlist,a4
		move.l	a4,firstitem(a5)			; taroljuk, ez a default first item	
		
		lea		charlist(a5),a0				; a0 charList tomb
		movea.l	fontdb(a5),a1				; a1 fontdb tomb
				
		; iteracio a charlist elemein		
		moveq	#0,d0						; char index in charlist
		moveq	#0,d5						; 8 bites olvasahoz
		moveq	#0,d6						; 8 bites olvasahoz
		move.w	d0,itemcount(a5)			; 0-zzuk az itemcount-ot
		
.loopcharlist:
			; vedett: d0, a1

			move.w	(a0,d0.w*8),d1			; a karakterunk
			lea		charcodes-32(PC),a2		; NINCS hibakezeles!
			move.b	(a2,d1),d1				; a charcode-unk (ami index a fontban a char-ra)
			cmpi.w	#44,d1					; ha nem renderelheto, ugras a kovetkezore
			bge		.nextchar
			
			; ervenyes charcode-unk van d1-ben
			; allapitsuk meg, hogy a kepernyore esik-e a renderelt char?
			move.w	__Xpos(a0,d0.w*8),d2		; az X pozicionk
			cmpi.w	#1,d2					; lathato kepen a char?
			blt		.nextchar				; balra kint
			cmpi.w	#DEMO_W+62,d2
			bge		.nextchar				; jobbra kint
			
			; Y pozicional mar bonyolultabb, bele kell szamolnunk a charbitmap meretet
			move.w	__Ypos(a0,d0.w*8),d3	; az Y pozicionk
			move.w	__dir(a0,d0.w*8),d4		; dir, ez csak a megfelelo charbitmap-hoz kell
			asr.w	#8,d4
			add.w	#31,d4					; 0-63-ig a dir!!!
	;debug
			movea.l	(a1,d4.w*8),a2			; fontdb-bol a chardb entry, ehhez kell meg a char index
			lea		(a2,d1.w*8),a2			; a2 most a chardata entry-re mutat a megfelelo
											; elforgatasban, es a megfelelo char
			; fix Y coord		
			sub.w	#32,d3					; kivonunk 32-t, igy a sprite tetejere mutat
			move.w	d3,d4
			move.b	__Yoffset(a2),d5		; 8 bitet olvasunk, d5 szep tiszta!!! maradjon is az
			add.w	d5,d4					; d4 megvan az Y offszettel mindennel
			
			cmpi.w	#DEMO_H,d4				; lathato kepen a char?
			bge		.nextchar				; lent kint
			
			move.b	__Yend(a2),d6
			add.w	d6,d3
			tst.w	d3
			blt		.nextchar				; fent kint

			; a karakerunk lathato helyen van
			; tudjuk milyen magas, hol kezdodik stb, akkor most szepen masoljuk be a bufferbe
			
			; most vagjuk le azokat a sorokat amik fent v. lent kilognap
			moveq	#0,d7					; bitmapYoffset, alapbol 0 ugye
			move.b	__H(a2),d6				; H a font magassaga most mar kell
			
			cmpi.w	#DEMO_H,d3				; lent tullog?
			blt.s	.dontcutbottom
			subi.w	#DEMO_H-1,d3
			sub.w	d3,d6					; levagunk a magassagbol
			bra.s	.dontcuttop
			
.dontcutbottom:
			; d4 -ben a cel Y koordinata, ez kell, tarolni fogjuk
			tst.w	d4						; 
			bge.s	.dontcuttop
			
			add.w	d4,d6					; H -bol kivonjuk a amennyit levag a teteje
			sub.w	d4,d7					; bitmapYoffset-hez hozzaadjuk
			moveq	#0,d4					; es a top pozicio o legyen
			
.dontcuttop:
			; lavagtuk amit kellett
			; fixaltuk a magassagot (H)
			; fixaltuk az Yoffset-et
			; most akkor johet a renderlist entry letrehozasa!!!!


			; prev/next nem kell azt a kod letrehozza
			; destination, video RAM cim, ezt ki kell szamolnunk

			; renderType, ez meg ki kell talalni
			; maga a buffer (3 long/line, max 64 line - 768 bytes)

			; d0 karakterszamlalo
			; d2 x pozicio, a shifteleshez, es a destination meghatarozasahoz
			; d4 - renderY, mar megvan
			; d6 - H, mar megvan
			; d7 offset sor a charbitmapdata-hoz
			; a4 a renderlist aktualis elemere mutat!
			sub.w	#64,d2
			move.w	d2,d3	; save d2 for later
			
			movea.l	D_SCR+__rowStartAddrs(a5),a3	; a3-ban a tabalzat cime
			movea.l (a3,d4.w*4),a3					; a3-ban a sorunk VIDEO RAM cime, d4 nem lehet ugye negativ!

			move.w	d3,d1
			asr.w	#3,d1		; byte address
			andi.w	#$FFFE,d1	; word address
			adda.w	d1,a3		; a3-ban a destination


			; rendertype
			moveq	#0,d1							; render Type default		  
			tst.w	d2					; X koordinatank >= 0?
			bpl.s	.positive
			; x negative, itt kell korrigalni a type-megjelolessel
			asr.w	#4,d2		; osztjuk 16-tal, megkapjuk a 
			moveq	#0,d1
			sub.w	d2,d1		; megkaptuk a word offsetet- buffer olvasashoz, ami lehet 0,1,2,3,4
								; a normal Type lesz az 5, a vegre jut a 6,7,8,9,10

			move.w	d1,d2
			lsl.w	d2
			adda.w	d2,a3		; fix destination address
			bra.s	.setRenderType
.positive:
			cmpi.w	#DEMO_W-64,d2
			blt.s	.setRenderType
			
			sub.w	#DEMO_W-64-16,d2
			asr.w	#4,d2
			add.w	#4,d2
			move.w	d2,d1

.setRenderType:
			move.b	d1,__renderType(a4)

			moveq	#0,d1
			moveq	#0,d5
			move.l	d1,__previtem(a4)	; prev itemet 0 mert majd kesobb allitodik
			move.l	d1,__nextitem(a4)	; next itemet sinten
			move.l	a3,__destination(a4)	;d2 fuggo, abbol szamoljuk ki... szamoljuk!!!
			move.w	d4,__renderY(a4)
			move.b	d6,__renderH(a4)
		;	move.b	#0,__renderType(a4)
			
			; charbitmapdata
			movea.l	__charBitmapAddr(a2),a3	; a2 ezutan mar nem kell...
			subq.w	#1,d6					; sorszamlalo lesz a magassagbol
			; kell meg az X eltolas, utana johet a JT-bol a masolo shiftelo rutin
			
			;(a3,d7.w*8) d7 egyben szamlalo is es kezdo offset Y az olvasashoz
			;(d2),d3,d4,d6 hasznalhato
			
			; masolas + shifteles, hogy a bufferbol mar csak a kepernyore kelljen eor-olni
		;	move.w	d2,d3				; itt most vizszintes pozicio alapjan ugrunk
			andi.w	#$F,d3				; bitshift optimalizacio
			move.w	JT0(PC,d3.w*2),d3	;	
			jmp		JT0(PC,d3.w)
			; will continue execution just below loopcopyexit
			
loopcopyexit:


	
			moveq	#0,d6			
			; renderlist order meghatarozasa most elemenkent, ahogy jonnek
			tst.w	itemcount(a5)
			beq.s	.skipsort				
			bsr		sortrenderlist			; o szepen beallitja a prev-et es next-et!
			
.skipsort:			
			adda.w	#RLNTRYSZ,a4			; kovetkezo renderlist item			
			addq.w	#1,itemcount(a5)
			
.nextchar:
			addq.w	#1,d0					; kovetkezo karakter johet
			cmpi.w	#13,d0					; ha van
			bne	.loopcharlist
			;move.l	d6,__nextitem(a3)		; ha az utolso elem volt, akkor a next item 0

	IFNE DEBUG_MODE	
	;	movea.l	BUFFER(a5),a1		
	;	bsr	drawresult2				; megnezzuk mi keszul, de csak debug modban...
	ENDC
	
		moveq	#0,d0					; felso bitek miatt
		move.l	firstitem(a5),d1		; a valtozoinkat bementjuk FRAMEBUBBER-be
		move.w	itemcount(a5),d0
		tst.w	d0
		bne.s	.haveitems
		move.l	d0,d1					; ha nincsen elemunk a listaban, nullazzuk a firstitem-et
.haveitems:
		movea.l	FB(a5),a4
		move.wl	d0,__itemcount(a4)
		move.l	d1,__firstitem(a4)
		

		RTS____	"MAKE_RENDERLIST"

JT0:
		dc.w	chcopyshift_0-JT0
		dc.w	chcopyshift_1-JT0
		dc.w	chcopyshift_2-JT0
		dc.w	chcopyshift_3-JT0
		dc.w	chcopyshift_4-JT0
		dc.w	chcopyshift_5-JT0
		dc.w	chcopyshift_6-JT0
		dc.w	chcopyshift_7-JT0
		dc.w	chcopyshift_8-JT0
		dc.w	chcopyshift_9-JT0
		dc.w	chcopyshift_A-JT0
		dc.w	chcopyshift_B-JT0
		dc.w	chcopyshift_C-JT0
		dc.w	chcopyshift_D-JT0
		dc.w	chcopyshift_E-JT0
		dc.w	chcopyshift_F-JT0


;--------------------------------------------------------
RENDER_CHARS:
		
		; feladat, torolni a regieket, rajzolni az ujakat
		; 
		; new_buffer
		; old_buffer
		move.w	frame(a5),d0			; frame mint index
		movea.l	BUFFER(a5),a0			; BUFFER location
		movea.l	4(a0,d0.w*4),a1			; OLD FRAME
		movea.l	(a0,d0.w*4),a0				; NEW FRAME
		
		move.l	__firstitem(a0),a0		; a0 - ban az uj renderlist elso elemere pointer
		move.l	__firstitem(a1),a1		; a1 - ben a regi renderlist elso elemere pointer
		
		
.looprender:
		
		move.l	a0,d0
		tst.l	d0						; pointer to renderlist item NEW
		bne.s	.skipcheck
		move.l	a1,d0
		tst.l	d0						; no NEW items, check OLD
		beq.s	.finished				; no OLD either
		;bra.s	.renderOLD				; render OLD then		
.renderOLD:

		movea.l	a1,a4
		bsr.s	quickrender
		move.l	__nextitem(a1),a1		; point to the next item
		bra.s	.looprender
		
.skipcheck:								; we have new items...
		move.l	a1,d0
		tst.l	d0						; pointer to renderlist item OLD
		beq.s	.renderNEW				; no items

		move.w	__renderY(a0),d0		; __renderY NEW
		cmp.w	__renderY(a1),d0		; compare with __renderY OLD
		bgt.s	.renderOLD				; ha az uj nagyobb, akkor rendereljuk a regit
										; amugy az ujat fogjuk renderelni		
.renderNEW:
		
		movea.l	a0,a4
		bsr		quickrender
		move.l	__nextitem(a0),a0		; point to the next item
		bra.s	.looprender
		


.finished:
		RTS____ "RENDER_CHARS"


quickrender:
			;quick render ez innen kikerul messzire
			; a4-ben a renderlistitem cime
			moveq	#0,d6
			moveq	#0,d7			
			movea.l	__destination(a4),a3
			move.b	__renderH(a4),d6
			move.b	__renderType(a4),d7
			move.w	D_SCR+__rowBytes(a5),d2			
			subq.w	#1,d6
			andi.w	#$1FF,d6
			moveq	#0,d1

			move.w	JT1(PC,d7.w*2),d7	;	
			jmp		JT1(PC,d7.w)



			
.quickrenderloop:
				move.l __charbuff(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.l __charbuff+4(a4,d1.w*4),d3
				eor.l	d3,4(a3)
				move.w __charbuff+8(a4,d1.w*4),d3
				eor.w	d3,8(a3)				
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,.quickrenderloop


			RTS____	"quickrender"		


JT1:
		dc.w	chrender_0-JT1
		dc.w	chrender_1-JT1
		dc.w	chrender_2-JT1
		dc.w	chrender_3-JT1
		dc.w	chrender_4-JT1
		dc.w	chrender_5-JT1
		dc.w	chrender_6-JT1
		dc.w	chrender_7-JT1
		dc.w	chrender_8-JT1

chrender_0:
				move.l	__charbuff(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.l	__charbuff+4(a4,d1.w*4),d3
				eor.l	d3,4(a3)
				move.w	__charbuff+8(a4,d1.w*4),d3
				eor.w	d3,8(a3)				
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_0
				rts
chrender_1:
				move.l	__charbuff+2(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.l	__charbuff+6(a4,d1.w*4),d3
				eor.l	d3,4(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_1
				rts
chrender_2:
				move.l	__charbuff+4(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.w	__charbuff+8(a4,d1.w*4),d3
				eor.w	d3,4(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_2
				rts
chrender_3:
				move.l	__charbuff+6(a4,d1.w*4),d3
				eor.l	d3,(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_3
				rts
chrender_4:
				move.w	__charbuff+8(a4,d1.w*4),d3
				eor.w	d3,(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_4
				rts
chrender_5:
				move.l	__charbuff(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.l	__charbuff+4(a4,d1.w*4),d3
				eor.l	d3,4(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_5
				rts
chrender_6:
				move.l	__charbuff(a4,d1.w*4),d3
				eor.l	d3,(a3)
				move.w	__charbuff+4(a4,d1.w*4),d3
				eor.w	d3,4(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_6
				rts
chrender_7:
				move.l	__charbuff(a4,d1.w*4),d3
				eor.l	d3,(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_7
				rts
chrender_8:
				move.w	__charbuff(a4,d1.w*4),d3
				eor.w	d3,(a3)
				adda.w	d2,a3
				addq.w	#3,d1
				dbra.s	d6,chrender_8
				rts

;------------------------------------------------------------------------
sortrenderlist:
		; kell a render list
		; a0-ban a korabbi elemek lesznek
		; a4-ben az aktualis!!!
		; kell hogy hany elozo eleme van (ha 0, ide sem jovunk)
		; a tobbit elvegzi a program
		movem.l	d0/a0-a1,-(sp)	;d1-et nem mentjuk...

	;debug		
		;move.w	itemcount(a5),d0	
		; az aktualis (utolso) elemet akarjuk renderY alapjan sorbarendezni, az elozoeket vizsgalva
	;debug		
		; a4 most az uj elem a renderlist-ben (az utolso)
		move.w	__renderY(a4),d1	; d1-ban az Y pozicionk
		movea.l	firstitem(a5),a0	; a0-ban a renderlist elso eleme

.loopsort:
		cmp.w	__renderY(a0),d1				; osszeh annak az Y poziciojaval
		bgt.s	.checknext			; ha lejebb vagyunk, nezzuk a kovetkezot
	;debug		
		; betesszuk ez ele az ujat, es vegeztunk is!		
		move.l	__previtem(a0),d0
		tst.l	d0
		bne.s	.notthefirst
		movea.l	a4,firstitem(a5)	; ha elso volt, akkor mi leszunk az elsok!!!
		bra.s	.thefirst
.notthefirst:
		move.l	d0,a1
		move.l	a4,__nextitem(a1)	; es az elozo elozo next-je is mi lennenk...
.thefirst:
		move.l	d0,__previtem(a4)	; atvesszuk a prev-et
		move.l	a4,__previtem(a0)	; mi leszunk az o elozo prev-je
		move.l	a0,__nextitem(a4)	; es o lesz a mi next-unk
		bra.s	.sortend
				
.checknext:
		movea.l	__nextitem(a0),d0
		tst.l	d0
		beq.s	.nomore

		movea.l	d0,a0				; ha van kovetkezo, akkor mutassunk ra!
		bra.s	.loopsort
		
.nomore:
		; akkor az utolso utan tesszuk
		move.l	d0,__nextitem(a4)	; utolsokent 0-azzuk a next-et, d0 itt most 0 ugye...
		move.l	a0,__previtem(a4)	; o lesz a prev-unk
		move.l	a4,__nextitem(a0)	; es mi leszunk az o next-je
		
.sortend:		
		movem.l	(sp)+,d0/a0-a1	
		
		RTS____	"sortrenderlist"

;----------------------------------------------------------
		;		offsetx 0/1 bytes,
		;		result buff width in words,
		;		shift dir -1/0/1 left/noshift/right
		;		shift
SHIFTTYPE:		; 16 long hosszusagu tablazat (64 bytes)		
		dc.b	0,4,0,0			; 0
		dc.b	0,4,1,1			; 1
		dc.b	0,6,1,2			; 2
		dc.b	0,6,1,3			; 3
		dc.b	0,6,1,4			; 4
		dc.b	1,6,-1,3		; 5
		dc.b	1,6,-1,2		; 6
		dc.b	1,6,-1,1		; 7
		dc.b	1,6,0,0			; 8
		dc.b	1,6,1,1			; 9
		dc.b	1,6,1,2			; A
		dc.b	1,6,1,3			; B
		dc.b	2,6,-1,4		; C
		dc.b	2,6,-1,3		; D
		dc.b	2,6,-1,2		; E
		dc.b	2,4,-1,1		; F


; JUMPTABLE EXAMPLE
;
;JUMPTEST:
;	 debug
;	 	 moveq	 #0,d0
;
;.loopjt:		
;	 	 move.w	JT(PC,d0.w*2),d1	 ;	 
;	 	 jmp		 JT(PC,d1.w)
;
;JT:
;	 	 dc.w	 jt0-JT,jt1-JT,jt2-JT,jt3-JT,jt4-JT


chcopyshift_0:

.loopcopy0:
				move.l	(a3,d7.w*8),__charbuff(a4,d1.w*4)
				move.l	4(a3,d7.w*8),__charbuff+4(a4,d1.w*4)
				move.w	d5,__charbuff+8(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy0

				bra		loopcopyexit
				;IFNE DEBUG_MODE dc.b "@loopcopy0",0 ENDC

chcopyshift_1:

.loopcopy1:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3		; 1 pixel eltolasnal meg nem erdekel a tulcsordulas, mert ott nincs pixel
				roxr.l	d4
				
				move.l	d3,__charbuff(a4,d1.w*4)
				move.l	d4,__charbuff+4(a4,d1.w*4)
				
				move.w	d5,__charbuff+8(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy1

				bra		loopcopyexit

chcopyshift_2:

.loopcopy2:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3		
				roxr.l	d4
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5		; 2 pixel shift felett d5-ben lesz a mi kis tulcsordulonk
				
				move.l	d3,__charbuff(a4,d1.w*4)
				move.l	d4,__charbuff+4(a4,d1.w*4)
				
				move.w	d5,__charbuff+8(a4,d1.w*4)
				
				moveq	#0,d5
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy2

				bra		loopcopyexit

chcopyshift_3:

.loopcopy3:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3		
				roxr.l	d4
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5		; 2 pixel shift felett d5-ben lesz a mi kis tulcsordulonk
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5
								
				move.l	d3,__charbuff(a4,d1.w*4)
				move.l	d4,__charbuff+4(a4,d1.w*4)
				
				move.w	d5,__charbuff+8(a4,d1.w*4)
				
				moveq	#0,d5
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy3

				bra		loopcopyexit

chcopyshift_4:

.loopcopy4:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3		
				roxr.l	d4
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5		; 2 pixel shift felett d5-ben lesz a mi kis tulcsordulonk
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5
				lsr.l	d3
				roxr.l	d4
				roxr.w	d5
								
				move.l	d3,__charbuff(a4,d1.w*4)
				move.l	d4,__charbuff+4(a4,d1.w*4)
				
				move.w	d5,__charbuff+8(a4,d1.w*4)
				
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopy4

				bra		loopcopyexit

chcopyshift_5:
				
.loopcopy5:
				move.b	d5,__charbuff+9(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4
				roxl.l	d3	
				lsl.l	d4
				roxl.l	d3	
				roxl.b	d5	
				lsl.l	d4
				roxl.l	d3	
				roxl.b	d5	
				
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopy5
				bra		loopcopyexit

chcopyshift_6:
				
.loopcopy6:
				move.b	d5,__charbuff+9(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4
				roxl.l	d3	
				lsl.l	d4
				roxl.l	d3	
				roxl.b	d5	
				
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopy6
				bra		loopcopyexit

chcopyshift_7:
				
.loopcopy7:
				move.b	d5,__charbuff+9(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4
				roxl.l	d3	
				
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy7
				bra		loopcopyexit

chcopyshift_8:
				
.loopcopy8:
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	(a3,d7.w*8),__charbuff+1(a4,d1.w*4)
				move.l	4(a3,d7.w*8),__charbuff+5(a4,d1.w*4)
				move.b	d5,__charbuff+9(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy8
				bra		loopcopyexit

chcopyshift_9:
				
.loopcopy9:
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3
				roxr.l	d4	
				
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				move.b	d5,__charbuff+9(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopy9
				bra		loopcopyexit

chcopyshift_A:
				
.loopcopyA:
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3
				roxr.l	d4
				lsr.l	d3
				roxr.l	d4
				roxr.b	d5	
				
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				move.b	d5,__charbuff+9(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopyA
				bra		loopcopyexit

chcopyshift_B:
				
.loopcopyB:
				move.b	d5,__charbuff(a4,d1.w*4)
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsr.l	d3
				roxr.l	d4
				lsr.l	d3
				roxr.l	d4
				roxr.b	d5	
				lsr.l	d3
				roxr.l	d4
				roxr.b	d5	
				
				move.l	d3,__charbuff+1(a4,d1.w*4)
				move.l	d4,__charbuff+5(a4,d1.w*4)
				move.b	d5,__charbuff+9(a4,d1.w*4)
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopyB
				bra		loopcopyexit

chcopyshift_C:

.loopcopyC:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4		; 1 pixel eltolasnal meg nem erdekel a tulcsordulas, mert ott nincs pixel
				roxl.l	d3
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				
				move.w	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+2(a4,d1.w*4)
				move.l	d4,__charbuff+6(a4,d1.w*4)
				
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopyC
				bra		loopcopyexit

chcopyshift_D:

.loopcopyD:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4		; 1 pixel eltolasnal meg nem erdekel a tulcsordulas, mert ott nincs pixel
				roxl.l	d3
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				
				move.w	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+2(a4,d1.w*4)
				move.l	d4,__charbuff+6(a4,d1.w*4)
				
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopyD
				bra		loopcopyexit

chcopyshift_E:

.loopcopyE:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4		; 1 pixel eltolasnal meg nem erdekel a tulcsordulas, mert ott nincs pixel
				roxl.l	d3
				lsl.l	d4
				roxl.l	d3
				roxl.w	d5
				
				move.w	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+2(a4,d1.w*4)
				move.l	d4,__charbuff+6(a4,d1.w*4)
				
				addq.w	#3,d1
				addq.w	#1,d7
				moveq	#0,d5
				dbra.s	d6,.loopcopyE
				bra		loopcopyexit

chcopyshift_F:

.loopcopyF:
				move.l	(a3,d7.w*8),d3
				move.l	4(a3,d7.w*8),d4
				
				lsl.l	d4		; 1 pixel eltolasnal meg nem erdekel a tulcsordulas, mert ott nincs pixel
				roxl.l	d3
				
				move.w	d5,__charbuff(a4,d1.w*4)
				move.l	d3,__charbuff+2(a4,d1.w*4)
				move.l	d4,__charbuff+6(a4,d1.w*4)
				
				addq.w	#3,d1
				addq.w	#1,d7
				dbra.s	d6,.loopcopyF
				bra		loopcopyexit





		global DEMO_INIT,DEMO_LOOP
		extern drawresult2
		
		extern getdoutba
>>>>>>> d22939fa28442acfe0a9425a3d30e3f5e7d51674
