
;http://amiga-dev.wikidot.com/information:hardware
;https://retro-commodore.eu/files/downloads/amigamanuals-xiik.net//eBooks/Amiga%20Assembly%20Language%20Programming%20-%20eBook-ENG.pdf
;https://retro-commodore.eu/files/downloads/amigamanuals-xiik.net//eBooks/Amiga%20Machine%20Language%20-%20eBook-ENG.pdf
;https://retro-commodore.eu/files/downloads/amigamanuals-xiik.net//Other/Amiga%20Hardware%20Reference%20(within%20all%20pictures)%20-%20Manual-ENG.pdf
;https://amigamuseum.emu-france.info/Fichiers/mags/La%20Bible%20Amiga%20500.pdf
;	ORG $20000
;	LOAD $20000

      SECTION MYSEC,CODE
      JUMPPTR Start

       INCLUDE "BlitterRegList"
       INCLUDE "P61data.i"

DMACONR =$dff002 
VPOSR =$dff004
VHPOSR  =$dff006

INTENAR =$dff01c

COP1LCH =$dff080

INTENA  =$dff09a  
INTREQ  =$dff09c  
DMACON  =$dff096

;;--------------------------------- SCREEN BUFFER DIMENSIONS
w       =320+32       ;SCREEN WIDTH + 32  extend screen buffer by the width of one character to prevent blobbing/clipping
h      =256          ;screen height'
bplsize = (h*w)/8    ;bitplane area size in bytes (h*w)/8 
ScrBpl	 = w/8        ;bitplane width in bytes w//8


;      320*256=$14000=81920 pixels(bits) per screen
;      320*256=$14000/8 = $2800 = 10240  zise of a bitplane in bytes (divide by 2 for words)


;   ---------- neil   ========
neilw  = 288                      ; width is 205 but all graphics are to be rounded to the higher multiple of 16. 
                                   ; 205/16=12.8 so width is 13*16=208  . neil is 13 words wide
neilh  = 71
neilbitplanes = 5
neilmargin = (320-neilw)/2       ;gets the size of the margins around the neil for the window display size
neilwbpbpl   =  neilw/8            ; gets the neil width in bytes . This is the number of horizontal bytes per bitplane
neilbwid     =  neilwbpbpl*neilbitplanes       ; neil byte width for all bitplanes .  
neilmodulo   =  neilbwid-neilwbpbpl ;modulo in bytes


twirlogow  = 288                      
twirlogoh  = 71
twirlogobitplanes = 3
twirlogomargin = (320-twirlogow)/2       
twirlogowbpbpl   =  twirlogow/8            
twirlogobwid     =  twirlogowbpbpl*twirlogobitplanes         
twirlogomodulo   =  twirlogobwid-twirlogowbpbpl 

; ------------ fonts --------
fontw         =  288    
fonth         =  100    
fontbpls      =  3
fontbpl       =  fontw/8

plotY =  140     ; y position to where we want the font character to appear
plotx = w-32    ; character to left ot screen
Start:

OSoff:
       movem.l d1-a6,-(sp)                ; we only save d1 toa6 because we need to set d0 to 0 when exiting anyway.... and a7 is the same as SP anyway
       move.l 4.w,a6                      ;execbase

       clr.l  d0
       move.l #gfxname,a1
       jsr    -408(a6)                    ;oldpenlibrary()
       move.l d0,a1
       move.l 38(a1),d4                   ;original copper pointer in d4
       
       jsr   -414(A6)                    ;close library

       
       move   #$4c-6,d7                   ;start copper bar y position say 6 pixels above the neil
       moveq   #1,d6                      ;y add
       move.w   INTENAR,d5                ;save interupt status from read only version of interupt : INTENAR https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0036.html
       move.w   DMACONR,d3                ; read current DMA (002 read only) and put into D3


       ;wait for end of frame (recommendation is to wait for eof/vblank to set DMA bellow )                                  
       move.w #138,d0                     ; move 138 (end of frame position) into d0
       bsr    WaitRaster                  ; branch sub routinewait for raster to move to that position (for code closer than with  jsr and could even be bsr.b)

       move.w  #$7fff,INTENA               ;disable interupts https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0036.html
                                          ;set all bits and bit 15 to clear : it clears all
                                          ;!!! SYSTEM is not available for library calls
       move.w   #$7fff,INTREQ               ; turn off iterupt requests as well (not required but good practce_
       move.w   #$7fff,INTREQ               ; twice (a4000 compatibility)
       move.w   #$7fff,DMACON             ;disable all DMA channels https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node002F.html
       move.w   #$87e0,DMACON             ;enable selected DMA bits copper,blitter,sprite...

	bsr Init
       move.l #Copper,$dff080             ;set own copperlist first location (after disaling iterupts) https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0028.html
       
       ;;   MUSIC ROUTINE ---  Call P61_Init  ---
	movem.l d0-a6,-(sp)
	lea Module1,a0
	sub.l a1,a1
	sub.l a2,a2
	moveq #0,d0                        ;0:auto 1:pal 2:ntsc
	jsr P61_Init
	movem.l (sp)+,d0-a6
       ;;   MUSIC ROUTINE ---  Call P61_Init  ---
       
       bsr Main
************* EXIT **************
OSon:
       ;;   MUSIC ROUTINE ---  END P61  ---
       movem.l d0-a6,-(sp)
       jsr P61_End
       movem.l (sp)+,d0-a6
       ;;   MUSIC ROUTINE ---  END P61  ---
       
       move.w #$7fff,DMACON            ;clear DMA just in case we changed it 
       or.w   #$8200,d3                    ; restore DMA to initial state
	move.w d3,DMACON
      ; move.w #$000f,DMACON               ;make sure sound DMA is OFF

       move.l d4,COP1LCH                 ; restore copper
       ;bset  #15,d5                    ; set all bits in d5 to 1 old , slower way, use or #8000,d5
       ;or    #8000,d5                  ; d5 stores interupt. set bit 15 to 1 so it enables     
       or     #$c000,d5                  ; aslo set bit 14 to make sure master interupt is also set
       MOVE   d5,INTENA                  ; restore interupts 
       movem.l (sp)+,d1-a6
	moveq #0,d0                       ; if d0 is not 0 it will display an error message upon returning so we need to clear it 
       rts                               ;return ot OS


**************************************************************************************
**********************************  INIT   *******************************************
**************************************************************************************
Init:
      
       movem.l d0-a6,-(sp)                ;save registers to stack (PUSH)


******************************************************
neilPtrs:
;--- pointers for neil bitplane           ; fill  dff000 BPL1PTH / dff00e2 BPL1PTL ... 004/006 008/00a 
                                          ; with bitplane data from neil.
       lea    neil,a0                     ;a0 points to first bitplane of the neil data  !!! lea reads a long word. data nneds to be swapped then    
       lea    neilBplPt,a1                 ;a1 points to the bitplane pointers
       
       move   #neilbitplanes-1,d0                     ;d0 is the loop... will repeat 3 times
.bpll  
       move.l a0,d1                       ;move first longword of neil (a0_ into d1 ...say it's ffff d5f0" 
                                          ;!!! REMEMBER "dc.w $e0,0"  is set in memory as 00E0 0000 !!  
       swap   d1                          ;swap low word to high word d1 / D1 is now d5f0 ffff                                                      
       move.w d1,2(a1)                    ;move high word of d1 ffff ... the data from d1 into CopBplP with an offset of 2
       swap   d1                          ;swap d1 for low word now  ffff d5f0
       move.w d1,6(a1)                    ;do the same with the low word but with an offset of 6 "d5f0"

       addq   #8,a1                       ;skip 8 bytes to the next bitplane poiter so from dff00E0 to dff00E4
                                          ; lea neilBpl(a0),a0 does the same thing and is faster 
       lea neilwbpbpl(a0),a0              ; load a0 with offset of neilbpbpl
       dbf    d0,.bpll                    ;loop 3 times


SpritePtr:
;--- pointers for do sprite
	lea SprBplPt,a1
	lea Spr,a0
	move.l a0,d1
	swap d1
	move.w d1,2(a1)
	swap d1
	move.w d1,6(a1)


	lea NullSpr,a0
	move.l a0,d1
	moveq #7-1,d0
.sprpl:
	addq.w #8,a1
	swap d1
	move.w d1,2(a1)
	swap d1
	move.w d1,6(a1)
	DBF d0,.sprpl

FontPallFill:
*********** copy the font colors to dummy pallet
       lea FontE-8*2,a0            ; point to the pallet at end of the font file minus 8 long words
	lea FontPalP+2,a1           ; 2 skips the first word to point to the color
	moveq #8-1,d0
.coll:	move.w (a0)+,(a1)+
	addq.w #2,a1
	DBF d0,.coll
******************

	movem.l (sp)+,d0-a6
       rts
************** END OF INIT **************




**************************************************************************************
************************************** MAIN LOOP *************************************
**************************************************************************************
Main:
       movem.l d0-a6,-(sp)
Mainloop:
      	move.w #$02a,d0		; begining of screen in d0
	bsr.w WaitRaster            ; and go wait for that
      
;------ frame loop start------------
       bsr    BounceScroller
       add.b  #1,Spr+1                    ;move sprite
       add d6,d7                          ;increase y position   

;------copper bar movements---------
Checks:                                          
       cmp.b  #$4c+neilh-3,d7               ;bottom check 1 pixel bellow the neil
       blo.b  ok1
       neg    d6   
ok1:   cmp.b  #$4c-6,d7                     ; top check
       bhi    ok2
       neg    d6                          ;change direction
ok2:

************ create copperbar **************
       move.l #waitras1,a0
       move   d7,d0                              ; put raster position ,d7 into d0 so we can manipulate
       moveq  #6-1,d1                            ; number of times to loop to create copper bar
.l:                                              ;local variable
       move.b d0,(a0)                            ; put d0 inside memory address of a0 -> waitrast      
       add.w  #1,d0                              ; increase vertical position counter by 1
       add.w  #8,a0                              ; jump to next copperlist entry (8 bytes)
       dbf    d1,.l                              ; decrease d1 and loop back till it's 0 / false
       
;------ SCROLL -----------------
       bsr Scrollit

*********** CHARACTER CHECKS
       moveq #32,d2                              ; length of a character for wrapping 
	move.b LastChar(PC),d0                    ; check latest character plotted
 
       cmp.b #"I",d0                             ; is it an I ?
       bne.s .noi
       moveq #16,d2                              ; modify d2 to 16 pixel wide to wrap soonner
.noi:
       cmp.b #"!",d0                             ; is it an ! ?
       bne.s .noi2
       moveq #16,d2 
.noi2:
       cmp.b #"'",d0                             ; is it an ' ?
       bne.s .noi3
       moveq #16,d2 
.noi3:
       cmp.b #",",d0                             ; is it an , ?
       bne.s .noi4
       moveq #16,d2
.noi4: 
       cmp.b #".",d0                             ; is it an . ?
       bne.s .noi5
       moveq #16,d2
.noi5:     
       move.w ScrollCtr(PC),d0                   ; create a counter for the scroller
       addq.w #4,d0                              ; add pixels to character to wrap (also needs to be modified in BLTCON0 command in Scrollit)
       cmp.w  d2,d0                              ; check if character is full 32 pixel length
       blo.s  .nowrap

       move.l ScrollPtr(PC),a0                   ; check if we need to plot a character at all
       cmp.l  #ScrollTextWrap,a0                 ; has it reach the end label ?
       blo.s  .noplot                            ; yes then go to noplot
       lea    ScrollText(PC),a0                  ; same as move.l #ScrollText,a0
 .noplot:      
       bsr PlotChar
       addq.w #1,a0                       ; move to next entry in ScrollPtr
       move.l a0,ScrollPtr
       clr.w d0                             ; reset d0 for Scroll Counter
.nowrap:
       move.w d0,ScrollCtr

;------ frame loop end------------

       
       btst   #6,$bfe001
       bne    Mainloop
 
       movem.l (sp)+,d0-a6
       rts
**************MAIN LOOP END ***************


**************************************************************************************
********************************************** ROUTINES ******************************
**************************************************************************************
row = 288*3*20/8
col     =4
charperow = 9
PlotChar:                                        ; takes a0 = Scrollptr 
	movem.l d0-a6,-(sp)
	lea $dff000,a6
	bsr Blitwait



       ;****ASCII MAPPING
       ;bitmap of fonts is
       ; ABCDEFGHI
       ; JKLMNOPQR
       ; STUVWXYZ0
       ; 123456789
       ; .,!?-/' 

       moveq #0,d0                               ; clear d0 (upper and lower half)
       move.b (a0)+,d0                           ; put a0 (Scrollptr) into a register for ASCII value ... say it's a Z (ascii 90 / $5A)
       move.b d0,LastChar                        ; store last ploted character to check for I and other small characters
       
       sub.w #32,d0                              ; (D becomes ascii 58 / $3A) . this is bcs the lowest ascii character we use is space(32) so everything is indexed based on that
       lea FontTBl(PC),a1                        ;put address of table into A1  
       move.b (a1,d0.w),d0                       ; take a1/FontTBl and index it by d0.w/58 ... it points to 25/$19 
       divu   #9,d0                              ; divide by the charperow . that way the result is the row and the remainder (upper word) is the position in that row
                                                 ; now higher word is number of character on the row, while lower byte is the row 00070002
       move.l d0,d1
       swap d1                                   ; now d1 has the remainder is the column 00020007

       mulu #row,d0                              ;gets up the bitmap line that the row starts on "10e0"
	mulu #col,d1                              ;get offset for the row in pixels "1c"

	add.l d1,d0			              ;add both to get the starting offset of bitmap letter
	add.l #Font,d0                            ;add start of the Font address to d0 . d0 is now the start of BLTAPTH

	move.l #$09f00000,BLTCON0(a6)
	move.l #$ffffffff,BLTAFWM(a6)
	move.l d0,BLTAPTH(a6)
	move.l #Screen+ScrBpl*3*plotY+(plotx/8),BLTDPTH(a6)
	move.w #fontbpl-col,BLTAMOD(a6)         ; -4 is because we're blitting  words (32 pixel wide font)
	move.w #ScrBpl-col,BLTDMOD(a6)              

	move.w #20*3*64+2,BLTSIZE(a6)      ; characters are 20 high, 3 bitplanes, width of window is 2 words (32 pixels)
	movem.l (sp)+,d0-a6
	rts



Scrollit:
; scroll
;---   Blitter stuff        http://coppershade.org/articles/AMIGA/Agnus/Programming_the_Blitter/
;---  clear part of screen


bltoffs =plotY*(ScrBpl*3)          ;  size in byte of the offset to the screen buffer (ploty=140 )
 
                                          ;( 30 times the width of each screen line (in bytes so /8) + x value in byttes
blth   =20                               ; height of box in pixels
bltw   =w/16                            ; width in words (a width of 224 pixels / 16 )
bltskip =0                                ;modulo of the blitter in bytes (space between the end of blitted line and begining of next line)    
brcorner=(blth)*(ScrBpl*3)-2            ;Bottom right corner for Desc mode in DMACON0 (in bytes)
                                          ; height of box-1 in words times the width of screen  
       movem.l d0-a6,-(sp) 
       lea    $dff000,a6                  ; effective address to hardware 
 
       bsr    Blitwait                    ;You must wait for the Blitter, because a previous operation may be run by the operating system 
                                          ; or even by a chunk of your own code executed previously.
                                          ; This is done by reading a bit in DMACONR. To be compatible with Amiga 1000s having the first 
                                          ;revision of the Agnus chip, you must read DMACONR once before relying on the value of this bit.

       move.l #$49f00002,BLTCON0(a6)      ;BLTCON0 + BITCON1 Blitter control register 0 "anoo b000" a is Asource shift. n is source mask
                                          ; enable A source and D destination.
       move.l #$ffffffff,BLTAFWM(a6)
       move.l #Screen+bltoffs+brcorner,BLTAPTH(a6) 
       move.l #Screen+bltoffs+brcorner,BLTDPTH(a6)     ;BLTDPTH+BLTDPTHL bliter destination pointer : stating address of the data to be processed .
                                                        ;       screen (320*256/8) would be the top left of the screen buffer so we add the offset
       
       move.w #bltskip,BLTAMOD(a6)
       move.w #bltskip,BLTDMOD(a6)           ;BLTDMOD bliter modulo
       move.w #blth*3*64+bltw,BLTSIZE(a6)    ;BLTSIZE . should be height of your area * 64  +  width in words http://amiga-dev.wikidot.com/hardware:bltsize
                                          ; blth is just a wrord value. we must shift it to the left by 6 bits to be applied t the to 10 highest bits 
                                          ;which is tthe same as multipy by 64 (bltsize is 10high h bit and 6 low h bits)  hhhhhhhhhh.vvvvvv 
       movem.l (sp)+,d0-a6
       rts

     
************** WAIT SR***************
Blitwait:
	tst $dff002			       ; for compatibility
.waitblit:                                ;WAIT FOR BLITTER TO FINISH IT'S LAST BLIT 
      	btst #6,$dff002                 ; BIT 14=BBUSY	Blitter busy status bit
	bne.s .waitblit
       rts

WaitRaster:		                     ;wait for rasterline d0.w. Modifies d0-d2/a0. if d0=138 (00000000..00000000.000000000.10001010)
	move.l #$1ff00,d2              ; put upper bit mask into d2       00000000.000000001.11111111.00000000
	lsl.l #8,d0                        ; shift d0 8 bits to the left      00000000.000000000.10001010.00000000
	and.l d2,d0                        ; and d2 with d0  so d0 =          00000000.000000000.10001010.00000000
                                          ; this mask makes sure no residual data is in d0 ??
	lea VPOSR,a0                       ; put address of VPOSR into A0 long word so it puts VPOSR & VHPOSR (stored as v.vvvvvvvv.hhhhhhhh)
.wr:	move.l (a0),d1                     ; move value at a0 into d1
	and.l d2,d1                        ; and d1 with d2  to mask it 
	cmp.l d1,d0                        ; compare d1 with d0
	bne.s .wr                          ; loop till d1 =138
	rts







*************** BOUNCE ***********
BounceScroller:
       movem.l D0-A6,-(SP)
       
       lea    Screen,a0
       move.w BounceY(PC),d0
	move.w BounceYaccel(PC),d1
	add.w d1,BounceYspeed
       add.w  BounceYspeed(PC),d0            ;add instruction sets condition flags so we can check if it's below zero or above
       bpl.s  .nobounce                   ; if zero or above don't bounce
	move.w #11,BounceYspeed
	clr.w d0
.nobounce:
       move.w d0,BounceY
       mulu   #3*ScrBpl,d0
       add.l  d0,a0

;font pointers 
       lea    ScrBplPt,a1
      	moveq #fontbpls-1,d0
.bpll2:
	move.l a0,d1
	swap d1
	move.w d1,2(a1)		;hi word
	swap d1
	move.w d1,6(a1)		;lo word

	addq #8,a1		       ;point to next bpl to poke in copper
	lea ScrBpl(a0),a0           
	dbf d0,.bpll2
;      move.l a0,d1
;      swap d1
;      move.w d1,2(a1)
;      swap d1
;      move.w d1,6(a1)

       movem.l (SP)+,d0-a6
       rts






**************************************************************************************
*******************************    DATA   ********************************************
**************************************************************************************

********** PLAYROUTINE CODE **********
;Note: if this is put in its own section (or compiled as separate binary), then
;jsr <addr>+P61_InitOffset,P61_MusicOffset,P61_EndOffset,P61_SetPositionOffset
;to call the routines.
       EVEN
Playrtn:
	include "P6110/P6110-Play.i"


FontTBl:                                  ;font lookup table . start font at SPACE character (value $32) so this orders them sequentially in ASCII
       dc.b 43,38                         ; 43rd character is space, 38 is !  / special characters are after 9 .,!?-/'SPACE
	blk.b 5,0
	dc.b 42                            ; '
	blk.b 4,0
	dc.b 37,40,36,41                   ;  ,-./
	dc.b 26,27,28,29,30,31,32,33,34,35 ; 0,1,2,3,4,5,6,7,8,9
	blk.b 5,0
	dc.b 39,0                          ; ?
	dc.b 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21   ;alphabet characters
	dc.b 22,23,24,25                                               ;alphabet characters
	EVEN
       
ScrollPtr:
       dc.l   ScrollText
ScrollText:
       dc.b "THIS IS DAVE'S HOUSEKEEPING SEGMENT !!! EVERY SECOND MONDAY OF EACH THURSDAY !!! "
       blk.b w/32,' '       
ScrollTextWrap:

LastChar:
       dc.b 0
       EVEN
ScrollCtr:
	dc.w 0
BounceY:
       dc.w 48
BounceYspeed:
       dc.w 0
BounceYaccel:
       dc.w -1

gfxname:
       dc.b   "graphics.library",0           ;ascii 0 notation
 
 
	SECTION TutData,DATA_C
 Spr:
	dc.w $9840,$A800	;Vstart.b/Hstart/2.b , Vstop.b (Vstart.b+number of lines),%A0000SEH
	dc.w %1111110110000011,%0000000000000000
	dc.w %1111110110000011,%0000000000000000
	dc.w %0011000011011011,%0000000000000000
	dc.w %0011000011111110,%0000000000000000
	dc.w %0011000001100110,%0000000000000000
	dc.w %0011000000100100,%0000000000000000
	dc.w %0011000000000000,%0000000000000000
	dc.w %0000000000000000,%0000000000000000
	dc.w %0110001111100000,%0000000000000000
	dc.w %0110001111111000,%0000000000000000
	dc.w %0110001100011100,%0000000000000000
	dc.w %0110001111111100,%0000000000000000
	dc.w %0110001101111000,%0000000000000000
	dc.w %0110001100011100,%0000000000000000
	dc.w %0110001100001110,%0000000000000000

	dc.w 0,0

NullSpr:
	dc.w $2a20,$2b00
	dc.w 0,0
	dc.w 0,0

       ;SECTION tut,DATA_C                    ; create section to put copperlist in chipmem
Copper:
       dc.w $1fc,0                             ;set slow fetch mode for AGA compatibility
       dc.w $100,$0200                         ;bitplane control register https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0022.html

                                               ;typical: 320 pixel WIDE
                                               ;diw: 0x81 <-> 0xc1 (0x1c1) DIW = display window position.
                                               ;ddf: 0x38 <-> D0(D7) DMA position (note that DDFSTRT is not restricted to divisible by 8 values, other values do work but can cause bitplane delays to work strangely)
                                               ;DFSTRT=$38 -> DIWSTRT $81. For each 16px you add/subtract from DIWSTRT, add/subtract 8 from DDFSTRT
                                                                                        
       dc.w $8e,$4c81                          ;DIWSTRT display window start https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node002E.html
       dc.w $90,$2cc1                          ;DIWSTOP display window stop DIWSTRT+value 4c + 2a = 76
       dc.w $92,$38+neilmargin/2               ;DDFSTRT display bitplane DMA fetch start https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node002C.html
       dc.w $94,$d0-neilmargin/2               ;DDFSTOP display bitplane dataf fetch stop 
                                               ;(38 and d0 are standard values for a non overscann screen)
                                               ;these tells the DMA chip when to start and stop fetching data from the bitplanes
                                               ;for each scanlnes.
                                               ; each H value = 2 pixels so from 3800 , 3801 is 2 pixels after
                                               ;as you shrink the window size you substact half from DDF values


       dc.w $108,neilmodulo                    ; bitplane modulos https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0021.html
	dc.w $10a,neilmodulo                    ;108 BPL1MOD 10a BPL2OD
	dc.w $102,0                             ; BPLCON1 / Bit Plane Control Register (horizontal, scroll counter)

       dc.w $100,$5200 


SprBplPt:
	dc.w $120,0
	dc.w $122,0
	dc.w $124,0
	dc.w $126,0
	dc.w $128,0
	dc.w $12a,0
	dc.w $12c,0
	dc.w $12e,0
	dc.w $130,0
	dc.w $132,0
	dc.w $134,0
	dc.w $136,0
	dc.w $138,0
	dc.w $13a,0
	dc.w $13c,0
	dc.w $13e,0

neilBplPt:                          ; bitplane pointers
       dc.w $e0,0                  ;BPL1PTH and BPL2PTL BITPLANE POINTER high 3 bits and L 15bits
       dc.w $e2,0                  ;https://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0024.html
       dc.w $e4,0                  ;bitplane pointer 2
       dc.w $e6,0
       dc.w $e8,0                  ;bitplane 3
       dc.w $eA,0
       dc.w $ec,0                  ;bitplane 4
       dc.w $ee,0
       dc.w $f0,0                  ;bitplane 5
       dc.w $f2,0
       
       dc.w $180,$bbe
       dc.w $4527,$fffe                    ;wait for top (2b) of screen and left (07)
       dc.w $180,$fff
       dc.w $4537,$fffe


       dc.w $180,$eee
       dc.w $4707,$fffe

neilPal:
 	dc.w $0180,$0301,$0182,$0fff,$0184,$0fdb,$0186,$0fb9
       dc.w $0188,$0e97,$018a,$0b86,$018c,$0954,$018e,$0433
       dc.w $0190,$0421,$0192,$0211,$0194,$09ae,$0196,$0bcf
       dc.w $0198,$0576,$019a,$0244,$019c,$0544,$019e,$045c
       dc.w $01a0,$0f00,$01a2,$0f11,$01a4,$0111,$01a6,$0fff
       dc.w $01a8,$0333,$01aa,$0444,$01ac,$0555,$01ae,$0666
       dc.w $01b0,$0777,$01b2,$0888,$01b4,$0999,$01b6,$0aaa
       dc.w $01b8,$0ccc,$01ba,$0ddd,$01bc,$0eee,$01be,$0fff


       dc.w $100,$5200                           ;set the number of bitplanes in PBLCON0 to 3                  


waitras1:
       dc.w   $8007,$fffe                    ;top bar
       dc.w   $180,$469
waitras2:
       dc.w   $8107,$fffe
       dc.w   $180,$69c
waitras3:
       dc.w   $8207,$fffe
       dc.w   $180,$fff
waitras4:
       dc.w   $8307,$fffe
       dc.w   $180,$69c
waitras5:
       dc.w   $8407,$fffe
       dc.w   $180,$469
waitras6:
       dc.w   $8507,$fffe
       dc.w   $180,$301

      
       dc.w   $9607,$fffe
       dc.w   $180,$FFF
       dc.w   $100,$0200
       dc.w   $97df,$fffe          ; points to end of line instead of next line 
                                   ;this is to allow time to set the font pallet during Hblank


ScrBplPt:                          ;bitplane pointers for after the neil
       dc.w   $e0,0                 ;BPL1PTH and BPL1PTL
       dc.w   $e2,0
       dc.w   $e4,0                 ;BPL2PTH and BPL2PTL
       dc.w   $e6,0
       dc.w   $e8,0                 ;BPL3PTH and BPL3PTL
       dc.w   $ea,0
	dc.w   $108,ScrBpl*3-320/8               ;BPL1MOD 
	dc.w   $10a,ScrBpl*3-320/8                 ;BPL2MOD 
	dc.w   $92,$38              ;DDFSTRT Display bit plane data fetch start,horiz pos
	dc.w   $94,$d0              ;DDFSTOP
	dc.w   $100,$3200           ;BPLCON0 as an example, this could be dynami
                                   ; dc.w $100,fontbpl*$1000+$200

	Dc.w   $180,$53e
       dc.w   $a707,$fffe
 	Dc.w   $180,$fff
       dc.w   $a907,$fffe    
       
       
       dc.w $0180,$C6A,$0182,$0e8d,$0184,$0a49,$0186,$0c5b
	dc.w $0188,$0e8d,$018a,$0ead,$018c,$0ede,$018e,$0fff
 
       dc.w   $FF07,$fffe
       
FontPalP:  ; DUMMY Pallet!! will be filled by FontPallFill
	dc.w $0180,$000,$0182,$0e9e,$0184,$0837,$0186,$0c5b
	dc.w $0188,$0e8d,$018a,$0ead,$018c,$0ede,$018e,$0fff

       dc.w $0180,$301

       dc.w   $ffff,$fffe                    ; end mark of copper (WAIT for impossible location
CopperE:

Font:
       INCBIN "fonts/FastCarFont.284x100x3"
FontE:

neil:  INCBIN "TWIRFACESLOGO283x71x5"
neilE:
       dcb.b neilbwid*6,0                     ; this is to prevent crap from showing up at the bottom of the neil
                                              ; dcb declare block
twirlogo:  INCBIN "TWIRLOGO-283x71x3"
twirlogoE:
       dcb.b twirlogobwid*6,0  

Module1:
	incbin "P61.cli_loop_315.911D.mod"		;usecode $c00b43b

	SECTION TutBSS,BSS_C
Screen:       
       ds.b  bplsize*fontbpls                 ; ds declare space for font area (3 bitplanes)

       END


BLITTER SOURCES f0r BLTCON0 value "anoo b000"

"n" value is from
Bit	Channel
	ABCD ->D
       1001   9
       1101   B
       1111   F

respective "oo" value
              9      B      
0	000	0      0 
1	001	0      0 
2	010	0      1
3	011	0      1       
4	100	1      1
5	101	1      1 
6	110	1      1 
7	111	1      1 

%11110000	=$f0   fc