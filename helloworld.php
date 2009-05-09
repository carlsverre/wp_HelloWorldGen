<?php
$snippets = array (
array(
'title'=>'1C-Enterprise',
'code'=>'
// Hello World in 1C:Enterprise built-in script language

Message("Hello, World!");
',
),
array(
'title'=>'4Test',
'code'=>'
// Hello World in 4Test

testcase printHelloWorld()
    print("Hello World!")
',
),
array(
'title'=>'ABAP4',
'code'=>'
REPORT ZHB00001.
*Hello world in ABAP/4 *
WRITE: "Hello world".
',
),
array(
'title'=>'Action!',
'code'=>'
; Hello world in Action! programming language for the Atari 8-Bit computers

PROC Main()
 PrintE("Hello World!")
RETURN
',
),
array(
'title'=>'Actionscript-Flash5',
'code'=>'
// Hello World in Actionscript (up to Flash 5, IDE only)

trace ("Hello World");
',
),
array(
'title'=>'ActionScript-Flash8',
'code'=>'
// Hello World in ActionScript 2.0 (Flash 8)
class HelloWorld
{
    private var helloWorldField:TextField;
 
    public function HelloWorld( mc:MovieClip )
    {
        mc.helloWorldField = mc.createTextField("helloWorldField", mc.getNextHighestDepth(), 0, 0, 100, 100);
        mc.helloWorldField.autoSize = "left";
	mc.helloWorldField.html = true;
        mc.helloWorldField.htmlText = "<font size="20" color="#0000FF">Hello World!</font>";
    }
}

// on a frame
import HelloWorld;
var hw:HelloWorld = new HelloWorld( this );
',
),
array(
'title'=>'Actionscript-FlashMX',
'code'=>'
// Hello World in Actionscript (Flash MX onwards) 

_root.createTextField("mytext",1,100,100,300,100);
mytext.multiline = true;
mytext.wordWrap = true;
mytext.border = false;

myformat = new TextFormat();
myformat.color = 0xff0000;
myformat.bullet = false;
myformat.underline = true;

mytext.text = "Hello World!";
mytext.setTextFormat(myformat);
',
),
array(
'title'=>'Ada',
'code'=>'
-- Hello World in Ada

with Text_IO;
procedure Hello_World is

begin
  Text_IO.Put_Line("Hello World!");
end Hello_World;
',
),
array(
'title'=>'Algol-60',
'code'=>'
"BEGIN"
   "COMMENT" Hello World in Algol 60;
    OUTPUT(4,"(""("Hello World!")",/")")
"END"
',
),
array(
'title'=>'Algol-68',
'code'=>'
( # Hello World in Algol 68 # print(("Hello World!",newline)))
',
),
array(
'title'=>'Alpha-Five-Xbasic',
'code'=>'
" Hello World in Alpha Five Xbasic

ui_msg_box("The "Hello World" Collection", "Hello World", UI_ATTENTION_SYMBOL)
',
),
array(
'title'=>'amharic',
'code'=>'
Hello World in amharic (in JavaScript)

 
<%@ language="javascript" %>
<html><body>
<%
Response.Write("Hello World!");
%>
</body></html>
',
),
array(
'title'=>'Amiga-E',
'code'=>'
-> Hello World in Amiga-E

PROC main() IS WriteF("Hello World\n")
',
),
array(
'title'=>'APC',
'code'=>'
// Hello World in the APC language for probes

probe program
{
    on_entry log ("Hello, world!\n");
}
',
),
array(
'title'=>'APL',
'code'=>'
Hello World for APL. "[]" and "<-" are a single character in APL.
Comment character is Alt-comma.

[]<-"Hello World!"
',
),
array(
'title'=>'AppleScript',
'code'=>'
-- "Hello World" in AppleScript:

display dialog "Hello World"
',
),
array(
'title'=>'Argh!',
'code'=>'
Hello World in Argh!. No comment character exists.

j       world
lppppppPPPPPPsrfj
 hello,      *  j
              qPh
',
),
array(
'title'=>'ASP-JavaScript',
'code'=>'
Hello World for Microsoft ASP (in JavaScript)

<%@ language="javascript" %>
<html><body>
<%
Response.Write("Hello World!");
%>
</body></html>
',
),
array(
'title'=>'ASP-VBE',
'code'=>'
<!-- Hello World in ASP-VBE (Visual Basic Script Encided) -->
<html>
<script language="VBScript.Encode">#@~^HQAAAA==@#@&HdTAK6PrCsVKP;    WMV[Zr@#@&HwcAAA;==^#~@</script>
</html>
',
),
array(
'title'=>'ASP-VBS',
'code'=>'
Hello World for Microsoft ASP (in VBScript)

<%@ language="vbscript" %>
<html><body>
<%
Response.write "Hello World!"
%>
</body></html>
',
),
array(
'title'=>'Assembler-6502-AppleII',
'code'=>'
**********************************
*                                *
*      HELLO WORLD FOR 6502      *
*    APPLE ][, MERLIN ASSEMBLER  *
*                                *
**********************************

STROUT 	EQU	$DB3A ;OUTPUTS AY-POINTED NULL TERMINATED STRING
	LDY	#>HELLO
	LDA	#<HELLO
	JMP	STROUT

HELLO	ASC	"HELLO WORLD!",00
',
),
array(
'title'=>'Assembler-6502-C64',
'code'=>'
; Hello World for 6502 Assembler (C64)

ldy #0
beq in
loop:
jsr $ffd2
iny
in:
lda hello,y
bne loop
rts
hello: .tx "Hello World!"
       .by 13,10,0
',
),
array(
'title'=>'Assembler-68000-Amiga',
'code'=>'
; Hello World in 68000 Assembler for dos.library (Amiga)

        move.l  #DOS
        move.l  4.w,a6
        jsr     -$0198(a6)      ;OldOpenLibrary
        move.l  d0,a6
        beq.s   .Out
        move.l  #HelloWorld,d1

A)      moveq   #13,d2
        jsr     -$03AE(a6)      ;WriteChars

B)      jsr     -$03B4          ;PutStr

        move.l  a6,a1
        move.l  4.w,a6
        jsr     -$019E(a6)      ;CloseLibrary
.Out    rts

DOS          dc.b    "dos.library",0
HelloWorld   dc.b    "Hello World!",$A,0
',
),
array(
'title'=>'Assembler-68000-AtariST',
'code'=>'
; Hello World in 68000 Assembler (Atari ST)

     move.l #helloworld,-(A7)
     move   #9,-(A7)
     trap   #1
     addq.l #6,A7
     move   #0,-(A7)
     trap   #1
helloworld:
     dc.b "Hello World!",$0d,$0a,0
',
),
array(
'title'=>'Assembler-68008',
'code'=>'
; Hello World in 68008 Assembler (Sinclar QL)

     move.l #0,a0
     lea.l  mess,a1
     move.w $d0,a2
     jsr    (a2)
     rts
mess dc.w   12
     dc.b   "Hello World!",10
     end
',
),
array(
'title'=>'Assembler-ARM',
'code'=>'
; Hello World in ARM code assembler, with RISC OS software interrupt

SWI "OS_WriteS"
EQUS "Hello World!"
EQUB 0
ALIGN
MOV PC,R14
',
),
array(
'title'=>'Assembler-Darwin-PPC',
'code'=>'
; Hello World in Assembler for the Darwin Power-PC

.data
.cstring
.align 2
msg:
.asciz "Hello world!\n"
len = . - msg
.text
.align 2
.globl _start
_start:
li r0,4
li r3,1
lis r4,ha16(msg)
ori r4,r4,lo16(msg)
li r5,len
sc
li r0,1
li r3,0
sc
',
),
array(
'title'=>'Assembler-DG-Nova',
'code'=>'
                        .TITL HELLO
02                      ; "HELLO, WORLD" FOR NOVA RUNNING RDOS
03                      ; USES PCHAR SYSTEM CALL
04                      .NREL
05                      .ENT START
06
07              START:
08 00000"022424 DOCHAR: LDA 0,@PMSG     ; LOAD AC0 WITH NEXT CHARACTER,
09 00001"101015         MOV# 0,0,SNR    ; TEST AC0;
10 00002"000412          JMP DONE ; SKIPPED IF NONZERO
11 00003"006017         .SYSTM
12 00004"010000         .PCHAR          ; PRINT FIRST
13 00005"000413          JMP ER ; SKIPPED IF OK
14 00006"101300         MOVS 0,0        ; SWAP BYTES
15 00007"006017         .SYSTM
16 00010"010000         .PCHAR          ; PRINT SECOND
17 00011"000407          JMP ER ; SKIPPED IF OK
18 00012"010412         ISZ PMSG        ; POINT TO NEXT WORD
19 00013"000765         JMP DOCHAR      ; GO AROUND AGAIN
20
21 00014"006017 DONE:   .SYSTM          ; NORMAL EXIT
22 00015"004400         .RTN
23 00016"000402          JMP ER
24 00017"063077         HALT
25 00020"006017 ER:     .SYSTM          ; ERROR EXIT
26 00021"006400         .ERTN
27 00022"063077          HALT
28 00023"063077         HALT
29
30 00024"000025"PMSG:   .+1     ; ADDRESS OF FIRST WORD OF TEXT
31                      ; NOTE BYTES ARE PACKED RIGHT-TO-LEFT BY DEFAULT
32 00025"042510         .TXT /HELLO, WORLD!<15><12>/ ; THAT"S CR LF
33       046114
34       026117
35       053440
36       051117
37       042114
38       006441
39       000012
40 00035"000000         0 ; FLAG WORD TO END STRING
41
42                      .END START
',
),
array(
'title'=>'Assembler-HLA',
'code'=>'
; Hello World for Intel compatible High Level Assembler

program HELLO;
       #include( "stdlib.hhf" );
begin HELLO;
       stdout.put("Hello World",nl);
end HELLO;
',
),
array(
'title'=>'Assembler-IBM-370',
'code'=>'
ITLE "Hello World for IBM Assembler/370 (VM/CMS)"
HELLO    START
BALR  12,0
USING *,12
*
WRTERM "Hello World!"
*
SR    15,15
BR    14
*
END   HELLO
',
),
array(
'title'=>'Assembler-Intel',
'code'=>'
; Hello World for Intel Assembler (MSDOS)

mov ax,cs
mov ds,ax
mov ah,9
mov dx, offset Hello
int 21h
xor ax,ax
int 21h

Hello:
  db "Hello World!",13,10,"$"
',
),
array(
'title'=>'Assembler-Itanium',
'code'=>'
/* Hello world for IA64 (Itanium) Assembly */

.HW:
        stringz "Hello World"
        .text
        .align 16
        .global main#
        .proc main#
main:
        .prologue 14, 32
        .save ar.pfs, r33
        alloc r33 = ar.pfs, 0, 4, 1, 0
        .vframe r34
        mov r34 = r12
        adds r12 = -16, r12
        mov r35 = r1
        .save rp, r32
        mov r32 = b0
        .body
        addl r14 = @ltoffx(.HW), r1
        ;;
        ld8.mov r14 = [r14], .HW
        ;;
        st8 [r34] = r14
        ld8 r36 = [r34]
        br.call.sptk.many b0 = puts#
        mov r1 = r35
        ;;
        mov ar.pfs = r33
        mov b0 = r32
        .restore sp
        mov r12 = r34
        br.ret.sptk.many b0
',
),
array(
'title'=>'Assembler-Linux',
'code'=>'
	;; Hello World for the nasm Assembler (Linux)
	
	SECTION .data

	msg	db	"Hello, world!",0xa ; 
	len	equ     $ - msg

	SECTION .text
	global main

main:
        mov     eax,4		; write system call
        mov     ebx,1           ; file (stdou)
        mov     ecx,msg         ; string
        mov     edx,len         ; strlen
	int     0x80		; call kernel

	mov	eax,1		; exit system call
        mov     ebx,0      
        int     0x80		; call kernel
',
),
array(
'title'=>'Assembler-MIPS',
'code'=>'
## Hello Word in Assemlber for the MIPS Architecture

.globl main

main:   jal hwbody              #call Hello Word Procedure
       trap 10                 #exit

hwbody: addi $30, $30,-4        #we need to preserve
       sw $4, 0($30)           #existing values in register 4

       addi $4,$0,72           # H
       trap 101
       addi $4,$0,101          # e
       trap 101
       addi $4,$0,108          # l
       trap 101
       trap 101                # l
       addi $4,$0,111          # o
       trap 101
       addi $4,$0,32           # <space>
       trap 101
       addi $4,$0,87           # W
       trap 101
       addi $4,$0,111          # o
       trap 101
       addi $4,$0,114          # r
       trap 101
       addi $4,$0,108          # l
       trap 101
       addi $4,$0,100          # d
       trap 101
       addi $4,$0,33           # !
       trap 101
       addi $4,$0,10           # \n
       trap 101

done:   lw $4, 0($30)           #restore values
       addi $30, $30, 4        #in register 4
       jr $31                  #return to the main
',
),
array(
'title'=>'Assembler-MMIX',
'code'=>'
*	Hello World in Assembler 
*	for the MMIX Computer 

       LOC   #100
Main   GETA  $255,String
       TRAP  0,Fputs,StdOut
       TRAP  0,Halt,0
String BYTE  "Hello, world!",#a,0
',
),
array(
'title'=>'Assembler-PA-RISC',
'code'=>'
// Hello World written in PA-RISC 2.0 assembly code

    .LEVEL  2.0N

    .SPACE  $TEXT$,SORT=8
    .SUBSPA $CODE$,QUAD=0,ALIGN=4,ACCESS=0x2c,CODE_ONLY,SORT=24
main
    .PROC
    .CALLINFO CALLER,FRAME=16,SAVE_RP,ORDERING_AWARE
        .ENTRY
        STW     %r2,-20(%r30)   ;offset 0x0
        LDO     64(%r30),%r30   ;offset 0x4
        ADDIL   LR"M$3-$global$,%r27,%r1        ;offset 0x8
        LDO     RR"M$3-$global$(%r1),%r1        ;offset 0xc
        STW     %r1,-56(%r30)   ;offset 0x10
        ADDIL   LR"M$3-$global$+16,%r27,%r1     ;offset 0x14
        LDO     RR"M$3-$global$+16(%r1),%r26    ;offset 0x18
        LDW     -56(%r30),%r25  ;offset 0x1c
        LDIL    L"printf,%r31   ;offset 0x20
        .CALL   ARGW0=GR,ARGW1=GR,RTNVAL=GR     ;in=25,26;out=28;
        BE,L    R"printf(%sr4,%r31),%r31        ;offset 0x24
        COPY    %r31,%r2        ;offset 0x28
        LDW     -84(%r30),%r2   ;offset 0x2c
        BVE     (%r2)   ;offset 0x30
        .EXIT
        LDO     -64(%r30),%r30  ;offset 0x34
    .PROCEND    ;


    .SPACE  $TEXT$
    .SUBSPA $CODE$
    .SPACE  $PRIVATE$,SORT=16
    .SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=0x1f,SORT=16
M$3
    .ALIGN  8
    .STRINGZ    "Hello World"
    .BLOCK  4
    .STRINGZ    "%s\n"
    .IMPORT $global$,DATA
    .SPACE  $TEXT$
    .SUBSPA $CODE$
    .EXPORT main,ENTRY,PRIV_LEV=3,LONG_RETURN
    .IMPORT printf,CODE
    .END
',
),
array(
'title'=>'Assembler-PDP11',
'code'=>'
;       Hello World in Assembler for the DEC PDP-11 with the
;	RSX-11M-PLUS operating system
;
        .title Hello
        .ident /V0001A/
        .mcall qiow$s, exit$s
        .psect $code,ro,i
start:  qiow$s #5,#5,,,,<#str, #len, #40>
        exit$s
        .psect $data,ro,d
str:    .ascii / Hello World!/
        len=.-str
        .end start
',
),
array(
'title'=>'Assembler-PDP8',
'code'=>'
/ Hello World in Assembler for the DEC PDP-8
*200
hello,    cla cll
        tls            / tls to set printer flag.
        tad charac    / set up index register
        dca ir1        / for getting characters.
        tad m6        / set up counter for
        dca count    / typing characters.
next,    tad i ir1    / get a character.
        jms type    / type it.
        isz count    / done yet?
        jmp next    / no: type another.
        hlt

type,    0            / type subroutine
        tsf
        jmp .-1
        tls
        cla
        jmp i type
charac,    .            / used as initial value of ir1
        310 / H
        305 / E
        314 / L
        314 / L
        317 / O
        254 / ,
        240 /
        327 / W
        317 / O
        322 / R
        314 / L
        304 / D
        241 / !
m6,        -15
count,    0
ir1 = 10
$
',
),
array(
'title'=>'Assembler-VP',
'code'=>'
; Hello World in VP Assembler for intent (Amiga Anywhere)

.include "tao"

tool "home/hello",VP,TF_MAIN,8192,0
	ent (-:-)
		qcall lib/print,(hello_world.p : i~)
		ret ()
	entend

	data

hello_world:
	dc.b "Hello World!",ASCII_LF,0

toolend
',
),
array(
'title'=>'Assembler-Win32',
'code'=>'
; Hello world in Assembler for the Win32 architecture

TITLE Hello world in win32. Tasm

VERSION T310
Model use32 Flat,StdCall

start_code segment byte public "code" use32
begin:
 Call MessageBox, 0, offset sHallo, offset caption, 0
 Call ExitProcess, 0
start_code Ends

start_data segment byte public "data" use32

sHallo  db "Hello world",0
caption	db "Hi",0

start_data Ends
End begin
',
),
array(
'title'=>'Assembler-z390',
'code'=>'
; Hello World for z390 IBM compatible mainframe assembler

HELLO CSECT
     USING *,15
     WTO "Hello World"
     BR 14
     END
',
),
array(
'title'=>'Assembler-Z80-Console',
'code'=>'
; This is a "Hello World" program for Z80 and TMS9918 / TMS9928 / TMS9929 /
; V9938 or V9958 VDP.
; That means that this should work on SVI, MSX, Colecovision, Memotech,
; and many other Z80 based home computers or game consoles.
;
; Because we don"t know what system is used, we don"t know where RAM
; is, so we can"t use stack in this program.
;
; This version of Hello World was written by Timo "NYYRIKKI" Soilamaa
; 17.10.2001
;
;----------------------------------------------------------------------
; Configure this part:

DATAP: EQU #98 ; VDP Data port #98 works on all MSX models
; (TMS9918/TMS9929/V9938 or V9958)
; #80 works on SVI 
; (for other platforms you have to figure this out by your self)

CMDP: EQU #99 ; VDP Command port #99 works on all MSX models
; (TMS9918/TMS9929/V9938 or V9958)
; #81 works on SVI
; (for other platforms you have to figure this out by your self)
;-----------------------------------------------------------------------
; Program starts here:

ORG 0 ; Z80 starts always from here when power is turned on
DI ; We don"t know, how interrupts works in this system, so we disable them.

; Let"s set VDP write address to #0000
XOR A
OUT (CMDP),A
LD A,#40
OUT (CMDP),A

; Now let"s clear first 16Kb of VDP memory
LD B,0
LD HL,#3FFF
LD C,DATAP
CLEAR:
OUT (C),B
DEC HL
LD A,H
OR L
NOP ; Let"s wait 8 clock cycles just in case VDP is not quick enough.
NOP
JR NZ,CLEAR

; Now it is time to set up VDP registers:
;----------------------------------------
; Register 0 to #0
;
; Set mode selection bit M3 (maybe also M4 & M5) to zero and 
; disable external video & horizontal interrupt
LD C,CMDP
LD E,#80

OUT (C),A
OUT (C),E
;---------------------------------------- 
; Register 1 to #50
;
; Select 40 column mode, enable screen and disable vertical interrupt

LD A,#50
INC E
OUT (C),A
OUT (C),E
;---------------------------------------- 
; Register 2 to #0
;
; Set pattern name table to #0000

XOR A
INC E
OUT (C),A
OUT (C),E
;---------------------------------------- 
; Register 3 is ignored as 40 column mode does not need color table
;
INC E
;---------------------------------------- 
; Register 4 to #1
; Set pattern generator table to #800

INC A
INC E

OUT (C),A
OUT (C),E
;---------------------------------------- 
; Registers 5 (Sprite attribute) & 6 (Sprite pattern) are ignored 
; as 40 column mode does not have sprites

INC E
INC E
;---------------------------------------- 
; Register 7 to #F0
; Set colors to white on black

LD A,#F0
INC E
OUT (C),A
OUT (C),E
;----------------------------------------

; Let"s set VDP write address to #808 so, that we can write
; character set to memory
; (No need to write SPACE it is clear char already)
LD A,8
OUT (C),A
LD A,#48
OUT (C),A

; Let"s copy character set
LD HL,CHARS
LD B, CHARS_END-CHARS
COPYCHARS:
LD A,(HL)
OUT (DATAP),A
INC HL
NOP ; Let"s wait 8 clock cycles just in case VDP is not quick enough.
NOP
DJNZ COPYCHARS

; Let"s set write address to start of name table
XOR A
OUT (C),A
LD A,#40
OUT (C),A

; Let"s put characters to screen
LD HL,ORDER
LD B,ORDER_END-ORDER
COPYORDER:
LD A,(HL)
OUT (DATAP),A
INC HL

JR OVERNMI
NOP
NOP

; Here is address #66, that is entry for NMI
RETN ;Return from NMI

OVERNMI:
DJNZ COPYORDER

; The end
HALT

; Character set:
; --------------
ORDER:
DEFB 1,2,3,3,4,0,5,4,6,3,7
ORDER_END:

CHARS:

; H
DEFB %10001000
DEFB %10001000
DEFB %10001000
DEFB %11111000
DEFB %10001000
DEFB %10001000
DEFB %10001000
DEFB %00000000
; e
DEFB %00000000
DEFB %00000000
DEFB %01110000
DEFB %10001000
DEFB %11111000
DEFB %10000000
DEFB %01110000
DEFB %00000000
; l
DEFB %01100000
DEFB %00100000
DEFB %00100000
DEFB %00100000
DEFB %00100000
DEFB %00100000
DEFB %01110000
DEFB %00000000
; o
DEFB %00000000
DEFB %00000000
DEFB %01110000
DEFB %10001000
DEFB %10001000
DEFB %10001000
DEFB %01110000
DEFB %00000000
; W
DEFB %10001000
DEFB %10001000
DEFB %10001000
DEFB %10101000
DEFB %10101000
DEFB %11011000
DEFB %10001000
DEFB %00000000

; r
DEFB %00000000
DEFB %00000000
DEFB %10110000
DEFB %11001000
DEFB %10000000
DEFB %10000000
DEFB %10000000
DEFB %00000000
; d
DEFB %00001000
DEFB %00001000
DEFB %01101000
DEFB %10011000
DEFB %10001000
DEFB %10011000
DEFB %01101000
DEFB %00000000
chars_end:
',
),
array(
'title'=>'Assembler-ZX81',
'code'=>'
; Hello World in Assembler for the ZX81 (Zilog Z80)

          CALL SPRINT
          DEFM HELLO WORLD.
          DEFB FF
          RET
SPRINT    POP HL
          LD A,(HL)
          INC HL
          PUSH HL
          CP FF
          RET Z
          CALL PRINT
          JR SPRINT
',
),
array(
'title'=>'Asterisk',
'code'=>'
;; Hello World application for an Asterisk dial plan. Asterisk is a
;; GNU GPL telephony server. More details at http://www.asterisk.org

;; Displays Hello World at the Asterisk console if in verbose mode
exten => _X.,1,NoOp(Hello World)
',
),
array(
'title'=>'AutoHotkey',
'code'=>'
; Hello World in AutoHotkey

Msgbox Hello, World!
',
),
array(
'title'=>'AutoIT3',
'code'=>'
;Hello, World for AutoIT3  http://www.autoitscript.com

msgbox(0,"Hello World",0)
',
),
array(
'title'=>'AviSynth',
'code'=>'
# Hello World for AviSynth video editor
BlankClip()
Subtitle("Hello, world!")
',
),
array(
'title'=>'awk',
'code'=>'
# Hello World in awk
BEGIN {
  print "Hello World!"
  exit
}
',
),
array(
'title'=>'Axel',
'code'=>'
Hello World in AXEL (lip-synched speech)

... too large for this page, can be found here:
http://medieskolan.avc.edu.stockholm.se/axel/index.htm
',
),
array(
'title'=>'B',
'code'=>'
/* Hello World in B */

main() {
  extern a, b, c;
  putchar (a); putchar (b); putchar (c); putchar ("!*n");
}

a "hell" ;
b "o, w" ;
c "orld" ;
',
),
array(
'title'=>'BAL',
'code'=>'
Hello World in IBM mainframe Basic Assembler Language (BAL)

HELLO    CSECT
         STM   R14,R12,12(R13)
         LR    R12,R15
         USING HELLO,R12
         LA    R10,SAVEAREA
         ST    R13,4(R10)
         ST    R10,8(R13)
         LR    R13,R10
*
         WTO   "HELLO WORLD",ROUTCDE=1
*
         L     R13,4(R13)
         LM    R14,R12,12(R13)
         SR    R15,R15
         BCR   B"1111",R14
*
SAVEAREA DS    18F
         LTORG
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         END   HELLO
',
),
array(
'title'=>'BASIC',
'code'=>'
10 REM Hello World in BASIC
20 PRINT "Hello World!"
',
),
array(
'title'=>'bc',
'code'=>'
#!/usr/bin/bc -q
# Hello World for the Unix "bc" calculator

print "Hello World!\n"
',
),
array(
'title'=>'BCPL',
'code'=>'
// Hello world in BCLP
GET "libhdr"

LET start() = VALOF
$( writes("Hello world*N")
   RESULTIS 0
$)
',
),
array(
'title'=>'Befunge',
'code'=>'
v Hello World in Befunge

>"dlroW olleH",,,,,,,,,,,@
',
),
array(
'title'=>'Beta',
'code'=>'
{ *** Hello World in BETA ***}
(#
  do
     "Hello World!"->putLine
#)
',
),
array(
'title'=>'BIT',
'code'=>'
Hello World in BIT.
No comments possible.

LINENUMBERZEROCODEPRINTZEROGOTOONELINENUMBERONECODEPRINTONEGOTOONEZEROLINENUMBE
RONEZEROCODEPRINTZEROGOTOONEONELINENUMBERONEONECODEPRINTZEROGOTOONEZEROZEROLINE
NUMBERONEZEROZEROCODEPRINTONEGOTOONEZEROONELINENUMBERONEZEROONECODEPRINTZEROGOT
OONEONEZEROLINENUMBERONEONEZEROCODEPRINTZEROGOTOONEONEONELINENUMBERONEONEONECOD
EPRINTZEROGOTOONEZEROZEROZEROLINENUMBERONEZEROZEROZEROCODEPRINTZEROGOTOONEZEROZ
EROONELINENUMBERONEZEROZEROONECODEPRINTONEGOTOONEZEROONEZEROLINENUMBERONEZEROON
EZEROCODEPRINTONEGOTOONEZEROONEONELINENUMBERONEZEROONEONECODEPRINTZEROGOTOONEON
EZEROZEROLINENUMBERONEONEZEROZEROCODEPRINTZEROGOTOONEONEZEROONELINENUMBERONEONE
ZEROONECODEPRINTONEGOTOONEONEONEZEROLINENUMBERONEONEONEZEROCODEPRINTZEROGOTOONE
ONEONEONELINENUMBERONEONEONEONECODEPRINTONEGOTOONEZEROZEROZEROZEROLINENUMBERONE
ZEROZEROZEROZEROCODEPRINTZEROGOTOONEZEROZEROZEROONELINENUMBERONEZEROZEROZEROONE
CODEPRINTONEGOTOONEZEROZEROONEZEROLINENUMBERONEZEROZEROONEZEROCODEPRINTONEGOTOO
NEZEROZEROONEONELINENUMBERONEZEROZEROONEONECODEPRINTZEROGOTOONEZEROONEZEROZEROL
INENUMBERONEZEROONEZEROZEROCODEPRINTONEGOTOONEZEROONEZEROONELINENUMBERONEZEROON
EZEROONECODEPRINTONEGOTOONEZEROONEONEZEROLINENUMBERONEZEROONEONEZEROCODEPRINTZE
ROGOTOONEZEROONEONEONELINENUMBERONEZEROONEONEONECODEPRINTZEROGOTOONEONEZEROZERO
ZEROLINENUMBERONEONEZEROZEROZEROCODEPRINTZEROGOTOONEONEZEROZEROONELINENUMBERONE
ONEZEROZEROONECODEPRINTONEGOTOONEONEZEROONEZEROLINENUMBERONEONEZEROONEZEROCODEP
RINTONEGOTOONEONEZEROONEONELINENUMBERONEONEZEROONEONECODEPRINTZEROGOTOONEONEONE
ZEROZEROLINENUMBERONEONEONEZEROZEROCODEPRINTONEGOTOONEONEONEZEROONELINENUMBERON
EONEONEZEROONECODEPRINTONEGOTOONEONEONEONEZEROLINENUMBERONEONEONEONEZEROCODEPRI
NTZEROGOTOONEONEONEONEONELINENUMBERONEONEONEONEONECODEPRINTZEROGOTOONEZEROZEROZ
EROZEROZEROLINENUMBERONEZEROZEROZEROZEROZEROCODEPRINTZEROGOTOONEZEROZEROZEROZER
OONELINENUMBERONEZEROZEROZEROZEROONECODEPRINTONEGOTOONEZEROZEROZEROONEZEROLINEN
UMBERONEZEROZEROZEROONEZEROCODEPRINTONEGOTOONEZEROZEROZEROONEONELINENUMBERONEZE
ROZEROZEROONEONECODEPRINTZEROGOTOONEZEROZEROONEZEROZEROLINENUMBERONEZEROZEROONE
ZEROZEROCODEPRINTONEGOTOONEZEROZEROONEZEROONELINENUMBERONEZEROZEROONEZEROONECOD
EPRINTONEGOTOONEZEROZEROONEONEZEROLINENUMBERONEZEROZEROONEONEZEROCODEPRINTONEGO
TOONEZEROZEROONEONEONELINENUMBERONEZEROZEROONEONEONECODEPRINTONEGOTOONEZEROONEZ
EROZEROZEROLINENUMBERONEZEROONEZEROZEROZEROCODEPRINTZEROGOTOONEZEROONEZEROZEROO
NELINENUMBERONEZEROONEZEROZEROONECODEPRINTZEROGOTOONEZEROONEZEROONEZEROLINENUMB
ERONEZEROONEZEROONEZEROCODEPRINTONEGOTOONEZEROONEZEROONEONELINENUMBERONEZEROONE
ZEROONEONECODEPRINTZEROGOTOONEZEROONEONEZEROZEROLINENUMBERONEZEROONEONEZEROZERO
CODEPRINTZEROGOTOONEZEROONEONEZEROONELINENUMBERONEZEROONEONEZEROONECODEPRINTZER
OGOTOONEZEROONEONEONEZEROLINENUMBERONEZEROONEONEONEZEROCODEPRINTZEROGOTOONEZERO
ONEONEONEONELINENUMBERONEZEROONEONEONEONECODEPRINTZEROGOTOONEONEZEROZEROZEROZER
OLINENUMBERONEONEZEROZEROZEROZEROCODEPRINTZEROGOTOONEONEZEROZEROZEROONELINENUMB
ERONEONEZEROZEROZEROONECODEPRINTONEGOTOONEONEZEROZEROONEZEROLINENUMBERONEONEZER
OZEROONEZEROCODEPRINTONEGOTOONEONEZEROZEROONEONELINENUMBERONEONEZEROZEROONEONEC
ODEPRINTONEGOTOONEONEZEROONEZEROZEROLINENUMBERONEONEZEROONEZEROZEROCODEPRINTZER
OGOTOONEONEZEROONEZEROONELINENUMBERONEONEZEROONEZEROONECODEPRINTONEGOTOONEONEZE
ROONEONEZEROLINENUMBERONEONEZEROONEONEZEROCODEPRINTONEGOTOONEONEZEROONEONEONELI
NENUMBERONEONEZEROONEONEONECODEPRINTONEGOTOONEONEONEZEROZEROZEROLINENUMBERONEON
EONEZEROZEROZEROCODEPRINTZEROGOTOONEONEONEZEROZEROONELINENUMBERONEONEONEZEROZER
OONECODEPRINTONEGOTOONEONEONEZEROONEZEROLINENUMBERONEONEONEZEROONEZEROCODEPRINT
ONEGOTOONEONEONEZEROONEONELINENUMBERONEONEONEZEROONEONECODEPRINTZEROGOTOONEONEO
NEONEZEROZEROLINENUMBERONEONEONEONEZEROZEROCODEPRINTONEGOTOONEONEONEONEZEROONEL
INENUMBERONEONEONEONEZEROONECODEPRINTONEGOTOONEONEONEONEONEZEROLINENUMBERONEONE
ONEONEONEZEROCODEPRINTONEGOTOONEONEONEONEONEONELINENUMBERONEONEONEONEONEONECODE
PRINTONEGOTOONEZEROZEROZEROZEROZEROZEROLINENUMBERONEZEROZEROZEROZEROZEROZEROCOD
EPRINTZEROGOTOONEZEROZEROZEROZEROZEROONELINENUMBERONEZEROZEROZEROZEROZEROONECOD
EPRINTONEGOTOONEZEROZEROZEROZEROONEZEROLINENUMBERONEZEROZEROZEROZEROONEZEROCODE
PRINTONEGOTOONEZEROZEROZEROZEROONEONELINENUMBERONEZEROZEROZEROZEROONEONECODEPRI
NTONEGOTOONEZEROZEROZEROONEZEROZEROLINENUMBERONEZEROZEROZEROONEZEROZEROCODEPRIN
TZEROGOTOONEZEROZEROZEROONEZEROONELINENUMBERONEZEROZEROZEROONEZEROONECODEPRINTZ
EROGOTOONEZEROZEROZEROONEONEZEROLINENUMBERONEZEROZEROZEROONEONEZEROCODEPRINTONE
GOTOONEZEROZEROZEROONEONEONELINENUMBERONEZEROZEROZEROONEONEONECODEPRINTZEROGOTO
ONEZEROZEROONEZEROZEROZEROLINENUMBERONEZEROZEROONEZEROZEROZEROCODEPRINTZEROGOTO
ONEZEROZEROONEZEROZEROONELINENUMBERONEZEROZEROONEZEROZEROONECODEPRINTONEGOTOONE
ZEROZEROONEZEROONEZEROLINENUMBERONEZEROZEROONEZEROONEZEROCODEPRINTONEGOTOONEZER
OZEROONEZEROONEONELINENUMBERONEZEROZEROONEZEROONEONECODEPRINTZEROGOTOONEZEROZER
OONEONEZEROZEROLINENUMBERONEZEROZEROONEONEZEROZEROCODEPRINTONEGOTOONEZEROZEROON
EONEZEROONELINENUMBERONEZEROZEROONEONEZEROONECODEPRINTONEGOTOONEZEROZEROONEONEO
NEZEROLINENUMBERONEZEROZEROONEONEONEZEROCODEPRINTZEROGOTOONEZEROZEROONEONEONEON
ELINENUMBERONEZEROZEROONEONEONEONECODEPRINTZEROGOTOONEZEROONEZEROZEROZEROZEROLI
NENUMBERONEZEROONEZEROZEROZEROZEROCODEPRINTZEROGOTOONEZEROONEZEROZEROZEROONELIN
ENUMBERONEZEROONEZEROZEROZEROONECODEPRINTONEGOTOONEZEROONEZEROZEROONEZEROLINENU
MBERONEZEROONEZEROZEROONEZEROCODEPRINTONEGOTOONEZEROONEZEROZEROONEONELINENUMBER
ONEZEROONEZEROZEROONEONECODEPRINTZEROGOTOONEZEROONEZEROONEZEROZEROLINENUMBERONE
ZEROONEZEROONEZEROZEROCODEPRINTZEROGOTOONEZEROONEZEROONEZEROONELINENUMBERONEZER
OONEZEROONEZEROONECODEPRINTONEGOTOONEZEROONEZEROONEONEZEROLINENUMBERONEZEROONEZ
EROONEONEZEROCODEPRINTZEROGOTOONEZEROONEZEROONEONEONELINENUMBERONEZEROONEZEROON
EONEONECODEPRINTZEROGOTOONEZEROONEONEZEROZEROZEROLINENUMBERONEZEROONEONEZEROZER
OZEROCODEPRINTZEROGOTOONEZEROONEONEZEROZEROONELINENUMBERONEZEROONEONEZEROZEROON
ECODEPRINTZEROGOTOONEZEROONEONEZEROONEZEROLINENUMBERONEZEROONEONEZEROONEZEROCOD
EPRINTONEGOTOONEZEROONEONEZEROONEONELINENUMBERONEZEROONEONEZEROONEONECODEPRINTZ
EROGOTOONEZEROONEONEONEZEROZEROLINENUMBERONEZEROONEONEONEZEROZEROCODEPRINTZEROG
OTOONEZEROONEONEONEZEROONELINENUMBERONEZEROONEONEONEZEROONECODEPRINTZEROGOTOONE
ZEROONEONEONEONEZEROLINENUMBERONEZEROONEONEONEONEZEROCODEPRINTZEROGOTOONEZEROON
EONEONEONEONELINENUMBERONEZEROONEONEONEONEONECODEPRINTONE
',
),
array(
'title'=>'BlitzPlus',
'code'=>'
; Hello World in Blitz Plus (graphical mode)

Graphics 800,600,0,1
Text 790, 600, "Hello World"
WaitKey
',
),
array(
'title'=>'Boo',
'code'=>'
# Hello World in Boo
print "Hello World"
',
),
array(
'title'=>'BrainFuck',
'code'=>'
Hello World in BrainF***. No comment character exists.

++++++++++[>+++++++>++++++++++>+++<<<-]>++.>+.+++++++
..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.
',
),
array(
'title'=>'BS2000',
'code'=>'
/BEGIN-PROCEDURE LOGGING=N
/REMARK "HELLO WORLD" IN BS2000 (SDF)
/ASSIGN-SYSDTA TO-FILE=*SYSCMD
/WRITE-TEXT "HELLO WORLD!"
/SET-JOB-STEP
/ASSIGN-SYSDTA TO-FILE=*PRIMARY
/END-PROCEDURE
',
),
array(
'title'=>'Byte Syze',
'code'=>'
c7 3c 2a 3c 2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c 3c
28 5c 2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c 3c 28 5c
2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c 3c 28 5c 2a 2b
2a 5c 3c 28 5c 2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c
3c 28 5c 2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c 3c 28
5c 2a 2b 2a 5c 3c 28 5c 2a 2b 2a 5c 3c 28 5c 2a
2b 2a 00 00 01 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 64 48 65 6c 6c 6f 2c 20 57
6f 72 6c 64 21 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
',
),
array(
'title'=>'C#',
'code'=>'
// Hello World in Microsoft C# ("C-Sharp").

using System;

class HelloWorld
{
    public static int Main(String[] args)
    {
        Console.WriteLine("Hello, World!");
        return 0;
    }
}
',
),
array(
'title'=>'C++',
'code'=>'
// Hello World in C++ (pre-ISO)

#include <iostream.h>

main()
{
    cout << "Hello World!" << endl;
    return 0;
}
',
),
array(
'title'=>'C++-CLI',
'code'=>'
// Hello World in C++/CLI for .NET

using namespace System;

void main()
{
    Console::WriteLine("Hello World");
}
',
),
array(
'title'=>'C++-Epoc',
'code'=>'
// Hello World in C++, Epoc style (for Symbian OS)

#include <eikapp.h>
#include <eikdoc.h>
#include <eikappui.h>

class CHelloWorldAppUi;
class CEikApplication;
class CHelloWorldAppView;

class CHelloWorldApplication : public CEikApplication
    {
        public:
            TUid AppDllUid() const;
        protected:
            CApaDocument* CreateDocumentL();
    };

class CHelloWorldDocument : public CEikDocument
    {
        public:
            static CHelloWorldDocument* NewL(CEikApplication& aApp);
            static CHelloWorldDocument* NewLC(CEikApplication& aApp);
            ~CHelloWorldDocument(){};
        public:
            CEikAppUi* CreateAppUiL();
        private:
            void ConstructL() {};
            CHelloWorldDocument(CEikApplication& aApp){};
    };

class CHelloWorldAppUi : public CEikAppUi
    {
        public:
                void ConstructL();
                CHelloWorldAppUi(){};
                ~CHelloWorldAppUi(){};
    };

static const TUid KUidHelloWorldApp = {0x10005B91};

GLDEF_C TInt E32Dll(TDllReason )
    {
    return KErrNone;
    }

EXPORT_C CApaApplication* NewApplication() 
    {
    return (new CHelloWorldApplication);
    }

CApaDocument* CHelloWorldApplication::CreateDocumentL()
    {  
    CApaDocument* document = CHelloWorldDocument::NewL(*this);
    return document;
    }

TUid CHelloWorldApplication::AppDllUid() const
    {
    return KUidHelloWorldApp;
    }
    
CHelloWorldDocument* CHelloWorldDocument::NewL(CEikApplication& aApp)
    {
    CHelloWorldDocument* self = NewLC(aApp);
    CleanupStack::Pop(self);
    return self;
    }

CHelloWorldDocument* CHelloWorldDocument::NewLC(CEikApplication& aApp)
    {
    CHelloWorldDocument* self = new (ELeave) CHelloWorldDocument(aApp);
    CleanupStack::PushL(self);
    self->ConstructL();
    return self;
    }

CEikAppUi* CHelloWorldDocument::CreateAppUiL()
    {
    CEikAppUi* appUi = new (ELeave) CHelloWorldAppUi;
    return appUi;
    }

void CHelloWorldAppUi::ConstructL()
    {
    BaseConstructL();

    _LIT(message,"Hello!");
    CAknInformationNote* informationNote = new (ELeave) CAknInformationNote;
    informationNote->ExecuteLD(message);
    }
',
),
array(
'title'=>'C++-FLTK',
'code'=>'
// Hello World in C++-FLTK

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

int main(int argc, char **argv) {
   Fl_Window *ventana = new Fl_Window(300,180);
   ventana->begin();
   Fl_Box *box = new Fl_Box(20,40,260,100,"Hello World!");
   box->labelsize(50);
   ventana->end();
   ventana->show(argc, argv);
   return Fl::run();
}
',
),
array(
'title'=>'C++-gtkmm',
'code'=>'
// Hello World in C++ for the Gtk+ toolkit

#include <gtkmm/main.h>
#include <gtkmm/button.h>
#include <gtkmm/window.h>
#include <iostream>

void button_clicked()
{
	std::cout << "Hello, World !" << std::endl;
}

int main (int argc, char *argv[])
{
	Gtk::Main kit(argc, argv);
	Gtk::Window hello_window;
	Gtk::Button hello_button("Hello World");
	
	hello_window.set_border_width(10);
	hello_window.add(hello_button);
	hello_button.signal_clicked().connect(sigc::ptr_fun(&button;_clicked));
	hello_button.show();
	
	Gtk::Main::run(hello_window);
	return 0;
}
',
),
array(
'title'=>'C++-MFC',
'code'=>'
// Hello World in C++ for Microsoft Foundation Classes
// (Microsoft Visual C++).

#include <afxwin.h>

class CHello : public CFrameWnd
{
public:
    CHello()
    {
        Create(NULL,_T("Hello World!"),WS_OVERLAPPEDWINDOW,rectDefault);
    }
};

class CHelloApp : public CWinApp
{
public:
    virtual BOOL InitInstance();
};

BOOL CHelloApp::InitInstance()
{
    m_pMainWnd = new CHello();
    m_pMainWnd->ShowWindow(m_nCmdShow);
    m_pMainWnd->UpdateWindow();
    return TRUE;
}

CHelloApp theApp;
',
),
array(
'title'=>'C++-Qt',
'code'=>'
// Hello World in C++ for the Qt framework

#include <qapplication.h>
#include <qlabel.h>

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  QLabel l("Hello World!", 0);
  l.setCaption("Test");
  l.setAlignment(Qt::AlignCenter);
  l.resize(300, 200);
  a.setMainWidget(&l;);
  l.show();
  return(a.exec());
}
',
),
array(
'title'=>'C-AL',
'code'=>'
OBJECT Codeunit 50000 HelloWorld
{
  PROPERTIES
  {
    OnRun=BEGIN
            //Hello World in C/AL (Microsoft Business Solutions-Navision)
            MESSAGE(Txt001);
          END;
  }
  CODE
  {
    VAR
      Txt001@1000000000 : TextConst "ENU=Hello World";
    BEGIN
    END.
  }
}
',
),
array(
'title'=>'C-AmigaAnywhere',
'code'=>'
/* Hello World in C for Amiga Anywhere 2 (AA2) */

#include <aa.h>

int aaMain(int argc, char **argv)
{
   aaOpenDisplay(200, 200, 16, "Hello World", FAA_DISPLAY_WINDOW);
   aaDrawString(AA_DISPLAY_PIXMAP, "Hello, world!", 20, 20, AA_DEFAULT_FONT, 0xffff00, 0, FAA_FONT_INK, -1);
   aaUpdate();
   aaWaitInput();
   return 0;
}
',
),
array(
'title'=>'C-ANSI',
'code'=>'
/* Hello World in C, Ansi-style */

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
  puts("Hello World!");
  return EXIT_SUCCESS;
}
',
),
array(
'title'=>'C-Curses',
'code'=>'
/* Hello World in C for Curses */

#include <curses.h>
main()
{
  initscr();
  addstr("Hello World!\n");
  refresh();
  endwin();
  return 0;
}
',
),
array(
'title'=>'C-GEM',
'code'=>'
/* Hello World for C with GEM */

#include <aes.h>
main()
{
  appl_init();
  form_alert(1,"[0][Hello World!][Ok]");
  appl_exit();
  return 0;
}
',
),
array(
'title'=>'C-Intuition',
'code'=>'
/* Hello World in C for Intution (Amiga GUI) */

#include <intuition/intuition.h>

struct IntuitionBase *IntuitionBase = NULL;

struct IntuiText hello_text = {-1,-1,JAM1,0,0,NULL,"Hello World!",NULL };
struct IntuiText ok_text    = {-1,-1,JAM1,0,0,NULL,"Ok",NULL };

void main(void)
{
   IntuitionBase = (struct IntuitionBase *)
                   OpenLibrary("intuition.library", 0);
   AutoRequest(NULL, &hello;_text, NULL, &ok;_text, NULL, NULL, 100, 50);
   CloseLibrary(IntuitionBase);
}
',
),
array(
'title'=>'C-K+R',
'code'=>'
/* Hello World in C, K&R-style; */

main()
{
  puts("Hello World!");
  return 0;
}
',
),
array(
'title'=>'C-Objective',
'code'=>'
/* Hello World in Objective-C.
** Since the standard implementation is identical to K&R; C,
** a version that says hello to a set of people passed on
** the command line is shown here.
*/

#include <stdio.h>
#include <objpak.h>
int main(int argc,char **argv)
{
    id set = [Set new];
    argv++;while (--argc) [set add:[String str:*argv++]];
    [set do:{ :each | printf("hello, %s!\n",[each str]); }];
    return 0;
}
',
),
array(
'title'=>'C-OpenGL',
'code'=>'
/* "Hello World" in C using OGL - Open Graphics Library */

#include <GL/glut.h>
#define font GLUT_BITMAP_HELVETICA_18
#define tx "Hello World!"

void text(void)
{
 char *p, tex[] = tx;
 p = tex;
 glColor3d(1.0, 1.0, 0.0);
 glRasterPos2d(-.5, 0.);
 while(*p) glutBitmapCharacter(font, *p++);
}

void display()
{
 glClear(GL_COLOR_BUFFER_BIT);
 text();
 glFlush();
}

void reshape(int width, int height)
{
 glViewport(0, 0, width, height);
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 glOrtho(-1, 1, -1, 1, -1, 1);
 glMatrixMode(GL_MODELVIEW);
 display();
}

int main(int argc, char **argv)
{
 glutInit(&argc;, argv);
 glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
 glutInitWindowPosition(50, 50);
 glutInitWindowSize(500, 500);
 glutCreateWindow("Hello World OpenGL");
 glClearColor(0,0,0,0);
 glutDisplayFunc(display);
 glutReshapeFunc(reshape);
 glutMainLoop();
 return 0;
}
',
),
array(
'title'=>'C-PresManager',
'code'=>'
/* Hello World for C with PresentationManager / OS/2 2.11  */

#define INCL_WIN

#include <os2.h>

int main( void )
{
   HMQ   hmq;

   hmq = WinCreateMsgQueue( 0, 0 );

   WinMessageBox( HWND_DESKTOP, HWND_DESKTOP, (PSZ)"Hello World!",
      (PSZ)"", 0, MB_OK );

   WinDestroyMsgQueue( hmq );

   return 0;
}
',
),
array(
'title'=>'C-Windows',
'code'=>'
/* Hello world in C for MS-Windows */

#include <windows.h>

int PASCAL WinMain(HINSTANCE hInstance,
  HINSTANCE hPrevInstance, LPSTR CmdLine, int Show)
{
  MessageBox(GetActiveWindow(), "Hello World!", "Hello Windows World", MB_OK);
  return 0;
}
',
),
array(
'title'=>'C-X11-Athena',
'code'=>'
/* Hello World in C with X11 using Athena widgets */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>

main(int argc,char **argv)
{
  XtAppContext app_context;
  Widget toplevel,hello;

  toplevel = XtVaAppInitialize(&app;_context,"XHello",NULL,0,
    &argc;,argv,NULL,NULL);
  hello = XtVaCreateManagedWidget("Hello World!",labelWidgetClass,
    toplevel,(void*)0);

  XtRealizeWidget(toplevel);

  XtAppMainLoop(app_context);
  return 0;
}
',
),
array(
'title'=>'CAML-Light',
'code'=>'
(* Hello World in CAML Light *)

let hello =
   print_string "Hello World!";
;;
',
),
array(
'title'=>'CDuce',
'code'=>'
(* Hello World in CDuce *)

print "Hello World!\n";;
',
),
array(
'title'=>'Centura',
'code'=>'
! Hello World in Centura

Function: HelloWorld
Description:
Returns
Parameters
Static Variables
Local variables
Actions
 Call SalMessageBox( "Hello World","Message",MB_Ok)
',
),
array(
'title'=>'Chef',
'code'=>'
Hello World Souffle.

This recipe prints the immortal words "Hello world!", in a basically brute force
way. It also makes a lot of food for one person.

Ingredients.
72 g haricot beans
101 eggs
108 g lard
111 cups oil
32 zucchinis
119 ml water
114 g red salmon
100 g dijon mustard
33 potatoes

Method.
Put potatoes into the mixing bowl. Put dijon mustard into the mixing bowl. Put
lard into the mixing bowl. Put red salmon into the mixing bowl. Put oil into
the mixing bowl. Put water into the mixing bowl. Put zucchinis into the mixing
bowl. Put oil into the mixing bowl. Put lard into the mixing bowl. Put lard
into the mixing bowl. Put eggs into the mixing bowl. Put haricot beans into
the mixing bowl. Liquefy contents of the mixing bowl. Pour contents of the
mixing bowl into the baking dish.

Serves 1.
',
),
array(
'title'=>'Clarion',
'code'=>'
!Hello World in Clarion 
 
  PROGRAM
 
 MAP
 END
 
 CODE
 
 MESSAGE("Hello World!")
 
 RETURN
',
),
array(
'title'=>'Clean',
'code'=>'
// Hello World in Clean

module hello

Start :: String
Start = "Hello World!\n"
',
),
array(
'title'=>'CLP',
'code'=>'
/* Hello World in CLP for the IBM AS/400 */
PGM
SNDPGMMSG  MSG("Hello World !") MSGTYPE(*COMP)
 
ENDPGM
',
),
array(
'title'=>'Cocoa',
'code'=>'
// Hello World in Cocoa Obj-C (OS X)

#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    NSLog(@"Hello, World!");
    [pool release];
    return 0;
}
',
),
array(
'title'=>'ColdFusion',
'code'=>'
<!---Hello world in ColdFusion--->

<cfset message = "Hello World">
<cfoutput> #message#</cfoutput>
',
),
array(
'title'=>'CommandScript',
'code'=>'
#Hello World in Command Script 3.1
#Meta.Name: "Hello World"

#Block(Main).Start
    echo "Hello World!"
#Block(Main).End
',
),
array(
'title'=>'Console-Postscript',
'code'=>'
%% Hello World in Console PostScript

serverdict begin 0 exitserver
/Courier findfont
48 scalefont setfont
22 22 moveto
(Hello World!) show
showpage

%% End
',
),
array(
'title'=>'CoolBasic',
'code'=>'
" Hello World in CoolBasic

print "hello world"
wait key
',
),
array(
'title'=>'CSS',
'code'=>'
/* Hello World in CSS */
body:before {
    content: "Hello World";
}
',
),
array(
'title'=>'CYBOL',
'code'=>'
<!-- Hello World in Cybernetics Oriented Language (CYBOL) -->
<model>
    <part name="send_message" channel="inline" abstraction="operation" model="send">
        <property name="channel" channel="inline" abstraction="character" model="shell"/>
        <property name="message" channel="inline" abstraction="character" model="Hello, World!"/>
    </part>
    <part name="exit_application" channel="inline" abstraction="operation" model="exit"/>
</model>
',
),
array(
'title'=>'D',
'code'=>'
// Hello World in D

import std.stdio;

void main()
{
   writefln("Hello World!");
}
',
),
array(
'title'=>'Darkbasic',
'code'=>'
` Hello World in Darkbasic

print "Hello World!"
wait key
',
),
array(
'title'=>'Databasic',
'code'=>'
PROGRAM HELLO.B
# Hello World in Databasic
CRT "HELLOW WORLD"
END
',
),
array(
'title'=>'Dataflex',
'code'=>'
// Hello World in Dataflex Procedural

/tela

Hello world

/*

clearscreen

page tela
',
),
array(
'title'=>'dBase',
'code'=>'
* Hello World in dBase IV

? "Hello World!"
',
),
array(
'title'=>'dc',
'code'=>'
#!/usr/bin/dc
# Hello world! in dc (Unix desk calculator)
[Hello world!]p
',
),
array(
'title'=>'DCL',
'code'=>'
$! Hello world in Digital/Compaq/HP DCL (Digital Command Language)
$ write sys$output "Hello World"
',
),
array(
'title'=>'Delphi',
'code'=>'
// Hello World in Delphi
Program Hello_World;

{$APPTYPE CONSOLE}

Begin
  WriteLn("Hello World");
End.
',
),
array(
'title'=>'Dialect',
'code'=>'
# Hello World in Dialect

print "Hello World"
',
),
array(
'title'=>'DM',
'code'=>'
// Hello World in DM (Dream Maker)

mob
    Login()
        ..()

        world << "Hello World!"
',
),
array(
'title'=>'DML',
'code'=>'
! Hello World in DML (Gembase database language)

PROCEDURE_FORM MAIN
        PRINT/NOWAIT ("Hello world")
END_FORM
',
),
array(
'title'=>'DWIM',
'code'=>'
Hello World in DWIM ("Do what I mean").
Comments are not necessary in this language.

DWIM
',
),
array(
'title'=>'Dylan',
'code'=>'
module:			hello-world
author:			Homer
copyright:		(c) 1994 Homer
version:		1.0

// Hello World in DYLAN

define method main (#rest args)
  princ("Hello world!");
end;

main();
',
),
array(
'title'=>'DynaMorph',
'code'=>'
<<!! Hello World in DynaMorph >>
<#setString foo {Hello World!}#>
<html>
<head>
	<title>DynaMorph</title>
</head>
<body>
<#getString foo#>
</body>
</html>
',
),
array(
'title'=>'E',
'code'=>'
# Hello World in E

println("Hello, world!")
',
),
array(
'title'=>'easm',
'code'=>'
// Hello World in easm (Win32).

subsystem cui

section imports

    from msvcr70.dll import printf
    from kernel32.dll import ExitProcess

section data

    string pszOutput = "Hello World!"

section code

    call printf (&pszOutput;)
    call ExitProcess (00h)
',
),
array(
'title'=>'Eiffel',
'code'=>'
indexing "Hello World in Eiffel"

class HELLO

creation
   run

feature

   run is
     local
       io : BASIC_IO;
     do
       !!io;
       io.put_string("Hello World!");
       io.put_newline;
     end; -- run

end; -- class HELLO
',
),
array(
'title'=>'Elan',
'code'=>'
(* Hello World in ELAN *)

putline ("Hello World!");
',
),
array(
'title'=>'Elliott',
'code'=>'
:: Hello World in Elliott Autocode
SETF PUNCH
SETR 1
1)TELEPRINTER
LINE
TITLE Hello World.;
STOP
START 1
',
),
array(
'title'=>'Erlang',
'code'=>'
%% Hello World in Erlang

-module(hello).

-export([hello/0]).

hello() ->
   io:format("Hello World!~n", []).
',
),
array(
'title'=>'Euphoria',
'code'=>'
-- Hello World in Euphoria

puts(1, "Hello World!\n")
',
),
array(
'title'=>'Ferite',
'code'=>'
/**
 *   start script -- Hello world in Ferite ( www.ferite.org )
 */
uses "console";
Console.println("Hello World");
/* end script */
',
),
array(
'title'=>'Filemaker',
'code'=>'
#Hello World in Filemaker Script
Show Custom Dialog ["Hello World" ; "Hello World"]
',
),
array(
'title'=>'Flaming Thunder',
'code'=>'
# Write "Hello world" in Flaming Thunder.

Write "Hello world".
',
),
array(
'title'=>'Focal',
'code'=>'
1.01 COMMENT HELLO WORLD IN FOCAL
1.02 TYPE "HELLO WORLD", !
1.03 QUIT
',
),
array(
'title'=>'FOCUS',
'code'=>'
-* Hello World in FOCUS

-TYPE Hello world
',
),
array(
'title'=>'Forth',
'code'=>'
" Hello World in Forth

: HELLO
  ." Hello World!" CR
;
',
),
array(
'title'=>'Fortran',
'code'=>'
C     Hello World in Fortran

      PROGRAM HELLO
      WRITE (*,100)
      STOP
  100 FORMAT (" Hello World! " /)
      END
',
),
array(
'title'=>'Fortran77',
'code'=>'
C     Hello World in Fortran 77

      PROGRAM HELLO
      PRINT*, "Hello World!"
      END
',
),
array(
'title'=>'Fortran90',
'code'=>'
! Hello World in Fortran 90 and 95

PROGRAM HelloWorld
     WRITE(*,*)  "Hello World!"
END PROGRAM
',
),
array(
'title'=>'FortranIV',
'code'=>'
       PROGRAM HELLO
c
C      Hello World in Fortran IV (supposedly for a TR440)
c
       WRITE (6,"("" Hello World!"")")
       END
',
),
array(
'title'=>'Fortress',
'code'=>'
(* Hello World in Fortress *)

export Executable
run(args) = print "Hello, world!"
',
),
array(
'title'=>'FreeBASIC',
'code'=>'
"Hello World in FreeBASIC

print "Hello World"
',
),
array(
'title'=>'Frink',
'code'=>'
// Hello World in Frink

println["Hello World!"]
',
),
array(
'title'=>'G-Code',
'code'=>'
%
O1000
(PROGRAM NAME - HELLOWORLD)
(DATE=DD-MM-YY - 30-06-05 TIME=HH:MM - 19:37)
N10G20
N20G0G17G40G49G80G90
/N30G91G28Z0.
/N40G28X0.Y0.
/N50G92X0.Y0.Z0.
( 1/16 FLAT ENDMILL TOOL - 1 DIA. OFF. - 1 LEN. - 1 DIA. - .0625)
(CONTOUR)
N60T1M6
N70G0G90X0.Y1.A0.S5000M3
N80G43H1Z.5
N90Z.25
N100G1Z-.005F2.
N110Y0.F20.
N120G0Z.5
N130X.5
N140Z.25
N150G1Z-.005F2.
N160Y1.F20.
N170G0Z.5
N180Y.6106
N190Z.25
N200G1Z-.005F2.
N210X0.F20.
N220G0Z.5
N230X.6157Y.4712
N240Z.25
N250G1Z-.005F2.
N260X.6039Y.4135F20.
N270X.6Y.351
N280X1.1
N290G3X1.0098Y.6202R.4333
N300X.8941Y.6971R.2625
N310X.7255Y.6538R.1837
N320X.6157Y.4712R.332
N330G0Z.5
N340X.6Y.351
N350Z.25
N360G1Z-.005F2.
N370X.6039Y.2885F20.
N380G3X.7255Y.0481R.385
N390X.9745R.1853
N400X1.0843Y.2308R.332
N410G0Z.5
N420X1.2039Y0.
N430Z.25
N440G1Z-.005F2.
N450Y1.F20.
N460G0Z.5
N470X1.3098
N480Z.25
N490G1Z-.005F2.
N500Y0.F20.
N510G0Z.5
N520X1.4706Y.125
N530Z.25
N540G1Z-.005F2.
N550X1.502Y.0817F20.
N560G3X1.6176Y.0048R.2625
N570X1.7863Y.0481R.1837
N580X1.9118Y.351R.3957
N590X1.8216Y.6202R.4333
N600X1.7059Y.6971R.2625
N610X1.5373Y.6538R.1837
N620X1.4157Y.4135R.358
N630X1.4706Y.125R.4611
N640G0Z.5
N650X1.9853Y0.
N660Z.25
N670G1Z-.005F2.
N680X2.0422Y.1442F20.
N690G0Z.5
N700X2.5706Y1.
N710Z.25
N720G1Z-.005F2.
N730X2.6961Y0.F20.
N740X2.8216Y1.
N750X2.9451Y0.
N760X3.0706Y1.
N770G0Z.5
N780X3.2961Y.6538
N790Z.25
N800G1Z-.005F2.
N810X3.2608Y.6202F20.
N820G3X3.1745Y.2885R.4408
N830X3.2961Y.0481R.385
N840X3.5451R.1853
N850X3.6706Y.351R.3957
N860X3.5804Y.6202R.4333
N870X3.4647Y.6971R.2625
N880X3.2961Y.6538R.1837
N890G0Z.5
N900X3.7461Y.7019
N910Z.25
N920G1Z-.005F2.
N930Y0.F20.
N940G0Z.5
N950Y.3654
N960Z.25
N970G1Z-.005F2.
N980X3.7637Y.4663F20.
N990G2X3.8422Y.6587R.4948
N1000X3.9167Y.7019R.0929
N1010G1X4.0755
N1020G2X4.15Y.6587R.0929
N1030X4.1951Y.5769R.246
N1040G0Z.5
N1050X4.3255Y1.
N1060Z.25
N1070G1Z-.005F2.
N1080Y0.F20.
N1090G0Z.5
N1100X4.9275
N1110Z.25
N1120G1Z-.005F2.
N1130Y1.F20.
N1140G0Z.5
N1150X5.0314
N1160Z.25
N1170G1Z-.005F2.
N1180Y.2981F20.
N1190G0Z.5
N1200X4.9275Y.274
N1210Z.25
N1220G1Z-.005F2.
N1230X4.8941Y.1731F20.
N1240G2X4.7627Y.0192R.3255
N1250X4.5529Y.0481R.1862
N1260X4.4314Y.2885R.358
N1270X4.5176Y.6202R.4408
N1280X4.6333Y.6971R.2625
N1290X4.802Y.6538R.1837
N1300X4.8941Y.5288R.3457
N1310G1X4.9275Y.4279
N1320G0Z.5
N1330X5.0314Y.149
N1340Z.25
N1350G1Z-.005F2.
N1360Y0.F20.
N1370G0Z.5
N1380M5
N1390G91G28Z0.
N1400G28X0.Y0.A0.
N1410M30
%
',
),
array(
'title'=>'Gambas',
'code'=>'
"************************************
" Hello world in Gambas
"************************************
PUBLIC SUB Main()

   PRINT "Hello World"

END
',
),
array(
'title'=>'GameMonkey Script',
'code'=>'
// Hello World in GameMonkey Script

print("Hello World");
',
),
array(
'title'=>'Gentee-simple',
'code'=>'
// Hello World in Gentee (simple version)

func hello<main> : @"Hello, World!"
',
),
array(
'title'=>'Gentee',
'code'=>'
// Hello World in Gentee

func hello <main>
{
   print( "Hello, World!" )
   getch()
}
',
),
array(
'title'=>'GML',
'code'=>'
// Hello World in GML (Game Maker Language)
draw_text(10,10,"Hello World")
screen_refresh()
keyboard_wait()
',
),
array(
'title'=>'Gofer',
'code'=>'
-- Hello World in Gofer
-- Simple version

helloWorld:: String
helloWorld = "Hello World!\n"


-- Hello World in Gofer
-- Dialog version

helloWorld :: Dialogue
helloWorld resps = [AppendChan stdout "Hello world!"]
',
),
array(
'title'=>'GoogleGadgets',
'code'=>'
<?xml version="1.0" encoding="UTF-8" ?> 
<!-- Hello World as a Google gadget -->

<Module>
  <ModulePrefs title="hello world example" /> 
  <Content type="html">
     <![CDATA[ 
       Hello, world!
     ]]>
  </Content> 
</Module>
',
),
array(
'title'=>'GRAMophone',
'code'=>'
//Hello World in GRAMophone

composition "Hello, World!" of "Composer"
{
 %
 player player1 {
    grammar lindenmayer
    %
    axiom->print("Hello, World!");
 }

 player player2 {
    grammar chomsky
    %
       @composition->print("Hello, World!");
 }
}
',
),
array(
'title'=>'Gri',
'code'=>'
# Hello World in Gri
show "hello world"
',
),
array(
'title'=>'Groovy',
'code'=>'
// Hello World in Groovy

println "Hello World"
',
),
array(
'title'=>'GynkoSoft',
'code'=>'
; Hello World in GynkoSoft
; Simple version
0.00 Protocol "Hello, World!"


; Hello World in GynkoSoft
; Dialog box output
0.00 Message "Hello, World!"
',
),
array(
'title'=>'Haskell',
'code'=>'
-- Hello World in Haskell
 
main = putStrLn "Hello World"
',
),
array(
'title'=>'HDX',
'code'=>'
# Hello World as bdehaldia.exe external command

proc hdx_info {} {
  set ::Titel [TRA "&Hello; World"]
  set ::Menu GMA
}

proc hdx_run {} {
  tk_messageBox -type ok -message [TRA "Hello World!"]
  destroy .
}
',
),
array(
'title'=>'HP-41C',
'code'=>'
Hello World for the HP 41C. No comment character exists.

01 LBL "HELLO"
02 "HELLO WORLD"
03 AVIEW
',
),
array(
'title'=>'HP-48',
'code'=>'
<<
@ Hello World for the HP-48
@ << and >> are one char each
"HELLO WORLD"
>>
',
),
array(
'title'=>'HQ9+',
'code'=>'
Hello World in HQ9+ and HQ9++. No comment character exists.

H
',
),
array(
'title'=>'HTML',
'code'=>'
<HTML>
<!-- Hello World in HTML -->
<HEAD>
<TITLE>Hello World!</TITLE>
</HEAD>
<BODY>
Hello World!
</BODY>
</HTML>
',
),
array(
'title'=>'HyperTalk',
'code'=>'
-- Hello World in HyperTalk

answer "Hello, world!"
',
),
array(
'title'=>'IBM-Exec',
'code'=>'
Hello World for IBM EXEC (under VM/CMS)

&CONTROL;
*
&TYPE; Hello World!
*
&EXIT; 0
',
),
array(
'title'=>'IBM-Exec2',
'code'=>'
Hello World for IBM EXEC2 (under VM/CMS)

&TRACE; OFF
*
&TYPE; Hello World!
*
&EXIT; 0
',
),
array(
'title'=>'ici',
'code'=>'
# Hello World in ici (http://www.zeta.org.au/~atrn/ici/)
printf("Hello World!\n");
',
),
array(
'title'=>'ICL SCL',
'code'=>'
@ HELLO WORLD IN ICL SCL
PROC HELLO_WORLD IS ()
BEGIN
    SEND_MESSAGE("HELLO WORLD")
END
',
),
array(
'title'=>'Icon',
'code'=>'
# Hello world in Icon (http://www.cs.arizona.edu/icon/)

procedure main()
    write("Hello world")
end
',
),
array(
'title'=>'IDC',
'code'=>'
// Hello World in IDC-script language for IDA disaasembler

#include <idc.idc>

static main(void)
{
  Message("Hello World!");
}
',
),
array(
'title'=>'IDL',
'code'=>'
IDL> ; Hello World in IDL (Interactive Data Language)
IDL> print, "Hello World"
',
),
array(
'title'=>'Inform',
'code'=>'
!  "Hello world" in Inform
[ Main;
  print "Hello world^";
];
',
),
array(
'title'=>'Informix-4GL',
'code'=>'
# Hello World in Informix 4GL

MAIN

  DISPLAY "Hello World"

END MAIN
',
),
array(
'title'=>'Ingres-ABF',
'code'=>'
/* Hello World in Ingres ABF */
procedure hello =
begin
  message "Hello, World" with style=popup;
end
',
),
array(
'title'=>'InstallScript',
'code'=>'
// Hello World in InstallScript
// (Scripting language of InstallShield, a Windows install generator)

program
	MessageBox("Hello World!",INFORMATION);
endprogram
',
),
array(
'title'=>'Io',
'code'=>'
// Hello World in io programming language
"Hello world!" print
',
),
array(
'title'=>'Iptscrae',
'code'=>'
; Hello World in Iptscrae.
; 1. from the chat prompt:

/ "Hello World!" SAY

; 2. in a cyborg:

ON OUTCHAT {
    {  "Hello World!" SAY
    } CHATSTR "say it" == IF
}

; 3. in a room script:

ON SELECT {
    "Hello World!" SAY
}
',
),
array(
'title'=>'Jako',
'code'=>'
# Hello World in Jako

use sys;

sys::print("Hello, world!\n");
',
),
array(
'title'=>'Java',
'code'=>'
// Hello World in Java

class HelloWorld {
  static public void main( String args[] ) {
    System.out.println( "Hello World!" );
  }
}
',
),
array(
'title'=>'Java-Mobile',
'code'=>'
// Hello World on a mobile Java device

package helloworld;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

public class HelloWorld extends MIDlet {

  public HelloWorld()
  {
    Form form = new Form("Hello World");
    form.append("Hello world!");
    Display.getDisplay(this).setCurrent(form);
  }

  protected void pauseApp() {  }
  protected void startApp() throws
    javax.microedition.midlet.MIDletStateChangeException {  }
  protected void destroyApp(boolean parm1) throws
    javax.microedition.midlet.MIDletStateChangeException {  }
}
',
),
array(
'title'=>'Java-Server-Pages',
'code'=>'
<!-- Hello World for Java Server Pages -->

<%@ page language="java" %>
<%="Hello World!" %>
',
),
array(
'title'=>'Java-Servlet',
'code'=>'
import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;

//
// Hello World Java Servlet
//
public class HelloWorld extends HttpServlet {
public void service(HttpServletRequest request,
HttpServletResponse response)
throws IOException {

response.setContentType("text/html");
PrintWriter out = response.getWriter();

out.println("<html><body>");
out.println("Hello World!");
out.println("</body></html>");
}
}
',
),
array(
'title'=>'Java-Swing',
'code'=>'
// Hello World in Java using Swing GUI

class HelloWorldSwing {
  static public void main(String args[]) {
    javax.swing.JOptionPane.showMessageDialog(null,"Hello world!");
  }
}
',
),
array(
'title'=>'JavaScript',
'code'=>'
<html>
<body>
<script language="JavaScript" type="text/javascript">
// Hello World in JavaScript
document.write("Hello World");
</script>
</body>
</html>
',
),
array(
'title'=>'JCL',
'code'=>'
//HERIB    JOB  ,"HERIBERT OTTEN",PRTY=12
//* Hello World for MVS
//HALLO    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD SYSOUT=T
//SYSUT1   DD *
Hello World!
/*
//
',
),
array(
'title'=>'JudoScript',
'code'=>'
// Hello World in JudoScript (a Java scripting layer)

. "Hello World";
',
),
array(
'title'=>'Kylix',
'code'=>'
{Hello World in Kylix} 

program Hello_World;
 
 uses
    QDialogs;
 
 begin
   ShowMessage("Hello World");
 end.
',
),
array(
'title'=>'LaTeX',
'code'=>'
% Hello World! in LaTeX
\documentclass{article}
\begin{document}
Hello World!
\end{document}
',
),
array(
'title'=>'LibertyBASIC',
'code'=>'
"hello world in Liberty BASIC
PRINT "Hello World"
END
',
),
array(
'title'=>'LilyPond',
'code'=>'
% Hello World in LilyPond

\markup { Hello World! }
',
),
array(
'title'=>'Limbo',
'code'=>'
Hello World in Limbo.
Limbo is the programming language of the Inferno OS
(from Lucent Bell Labs).


implement Cmd;

include "sys.m";
include "draw.m";

Cmd : module {
    init : fn (ctxt : ref Draw->Context, args : list of string);
};

init(nil : ref Draw->Context, nil : list of string)
{
    sys := load Sys Sys->PATH;
    sys->print("Hello World\n");
}
',
),
array(
'title'=>'LIMS-Basic',
'code'=>'
"Hello World in LIMS Basic
msgbox("hello world")
',
),
array(
'title'=>'Lingo',
'code'=>'
Hello World in Lingo (Macromedia Director)

on startmovie
  alert "Hello World" 
end
',
),
array(
'title'=>'Lisp',
'code'=>'
;;; Hello World in Common Lisp

(defun helloworld ()
  (print "Hello World!")
)
',
),
array(
'title'=>'Lisp-Emacs',
'code'=>'
;;; Hello World in Emacs Lisp.

(defun hello-world()
  "Display the string hello world."
  (interactive)
  (message "hello world"))
',
),
array(
'title'=>'Logo',
'code'=>'
; Hello World in Logo

DRUCKEZEILE [Hello World!]
',
),
array(
'title'=>'Logo-graphical',
'code'=>'
; Hello World in LOGO, graphical output.

go 20 , left 180,
go 40 , left 180,
go 20 , right 90,
go 20 , left 90 ,
go 20 , left 180,
go 40 , left 90 ,
go 20 , left 90 ,
go 20 , right 90 ,
go 20 , right 90 ,
go 10 , right 90 ,
go 20 , left 90 ,
go 10 , left 90 ,
go 30 , left 90 ,
go 40 , left 180,
go 40 , left 90 ,
go 20 , left 90 ,
go 40 , left 180,
go 40 , left 90 ,
go 40 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 60 , left 90 ,
go 40 , left 180,
go 40 , left 90 ,
go 20 , left 90 ,
go 20 , left 180,
go 20 , left 90 ,
go 20 , left 90 ,
go 40 , left 180,
go 40 , left 90 ,
go 40 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 40 , left 90 ,
go 20 , right 90,
go 20 , right 90,
go 5 , left 90  ,
go 5 , left 90  ,
go 25 , left 180,
go 40 , left 90 ,
go 40 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 20 , left 90 ,
go 40 , left 180,
go 40 ,
',
),
array(
'title'=>'LOLCODE',
'code'=>'
BTW Hello World in LOLCODE
HAI
CAN HAS STDIO?
VISIBLE "HAI WORLD!"
KTHXBYE
',
),
array(
'title'=>'LOTOS',
'code'=>'
(* Hello World in LOTOS (Language Of Temporal Ordering Specifications) *)
 
process HelloWorld [v]: exit :=
  v! "Hello World!";
  exit
endproc
',
),
array(
'title'=>'Lotus-Note-Formula',
'code'=>'
REM "Lotus Note Formula Language";
@Prompt([ok];"Hi there";"Hello World");
',
),
array(
'title'=>'Lotus-Script',
'code'=>'
" Hello World in Lotus Script
Sub Initialize
        Msgbox "Hello world", 0, "Hi there!"
End Sub
',
),
array(
'title'=>'LS-DYNA',
'code'=>'
$ "Hello World" for LS-DYNA
*KEYWORD
*PART
                                                                                
         1         1         1
*SECTION_BEAM
         1         1       1.0         2         1
       1.0       1.0       0.0       0.0         1         1
*MAT_ELASTIC
         1    1.0E-9    1000.0       0.3
*ELEMENT_BEAM
       1       1       2       3       1
       2       1       3       4       1
       3       1       3       6       1
       4       1       6       5       1
       5       1       8      11       1
       6       1      11      12       1
       7       1      12       9       1
       8       1       9       8       1
       9       1       8       7       1
      10       1       7      10       1
      11       1      14      16       1
      12       1      16      15       1
      13       1      13      15       1
      14       1      15      17       1
      15       1      19      21       1
      16       1      21      20       1
      17       1      18      20       1
      18       1      20      22       1
      19       1      23      24       1
      20       1      24      26       1
      21       1      26      25       1
      22       1      25      23       1
      23       1      27      28       1
      24       1      28      29       1
      25       1      29      30       1
      26       1      30      31       1
      27       1      32      33       1
      28       1      33      35       1
      29       1      35      34       1
      30       1      34      32       1
      31       1      36      37       1
      32       1      37      38       1
      33       1      40      42       1
      34       1      42      41       1
      35       1      39      41       1
      36       1      41      43       1
      37       1      44      45       1
      38       1      45      47       1
      39       1      48      47       1
      40       1      47      46       1
      41       1      46      44       1
*NODE
       1             0.0             0.0             1.0
       2
       3             0.0             2.0
       4             0.0             4.0
       5             2.0
       6             2.0             2.0
       7             3.0
       8             3.0             1.0
       9             3.0             2.0
      10             5.0
      11             5.0             1.0
      12             5.0             2.0
      13             6.0
      14             6.0             4.0
      15             7.0
      16             7.0             4.0
      17             8.0
      18             9.0
      19             9.0             4.0
      20            10.0
      21            10.0             4.0
      22            11.0
      23            12.0
      24            12.0             2.0
      25            14.0
      26            14.0             2.0
      27            16.0             2.0
      28            16.5
      29            17.0             1.0
      30            17.5
      31            18.0             2.0
      32            19.0
      33            19.0             2.0
      34            21.0
      35            21.0             2.0
      36            22.0
      37            22.0             2.0
      38            24.0             2.0
      39            25.0
      40            25.0             4.0
      41            26.0
      42            26.0             4.0
      43            27.0
      44            28.0
      45            28.0             2.0
      46            30.0
      47            30.0             2.0
      48            30.0             4.0
*END
',
),
array(
'title'=>'LSL',
'code'=>'
// Hello World in Linden Scripting Language (LSL)

default
{
    state_entry()
    {
        llSay(0, "Hello World");
    }
}
',
),
array(
'title'=>'Lua',
'code'=>'
# Hello World in Lua

print "Hello world"
',
),
array(
'title'=>'m4',
'code'=>'
# Hello World for the m4 macro processor
Hello
',
),
array(
'title'=>'MACRO-10',
'code'=>'
TITLE HELLO WORLD
; HELLO WORLD IN MACRO 10 FOR TOPS-10
ENTRY OUTPUT
SEARCH UUOSYM

LAB:    ASCIZ /HELLO WORLD
/
OUTPUT: OUTSTR LAB              ; OUTPUT MESSAGE
        MONRT.                  ; RETURN TO MONITOR          
        END OUTPUT
',
),
array(
'title'=>'MACRO-11',
'code'=>'
;       "Hello, world!" in MACRO-11 for RT-11

        .MCALL  .EXIT,.PRINT
START:  .PRINT  #$1
        .EXIT
$1:     .ASCIZ  /Hello, world!/
        .END    START
',
),
array(
'title'=>'Macromedia-Flex',
'code'=>'
<?xml version="1.0" encoding="utf-8"?>
<mx:Application xmlns:mx="http://www.macromedia.com/2003/mxml">
     <!-- Hello Word in Macromedia Flex -->
     <mx:Label text="Hello World"/>
</mx:Application>
',
),
array(
'title'=>'Malbolge',
'code'=>'
Hello World in Malbolge. No comment character exists.

(=<`$9]7<5YXz7wT.3,+O/o"K%$H""~D|#z@b=`{^Lx8%$Xmrkpohm-kNi;gsedcba`_^]\[ZYXWVUTSRQPONMLKJIHGFEDCBA@?>=<;:9876543s+O<oLm
',
),
array(
'title'=>'MAMASH',
'code'=>'
/* Hello World in MAMASH */

TQWD LGYEA NXKIA HELLO_WORLD )1(
 
DWLH CXBZ_YKX
 
ID& HELLO_WORLD YED "HELLO WORLD" .
',
),
array(
'title'=>'Maple',
'code'=>'
# Hello World in Maple

>> printf("Hello World!");
',
),
array(
'title'=>'Mathematica',
'code'=>'
(* Hello World in Mathematica *)

Hello[] := Print["Hello, World!"]
',
),
array(
'title'=>'MATLAB',
'code'=>'
% Hello World in MATLAB.

disp("Hello World");
',
),
array(
'title'=>'MAXScript',
'code'=>'
/*Hello World in MAXScript (the script language of 3ds Max).
Hello World is both printed as text and in full 3D.*/

Print "Hello World"
Text text:"Hello World"
',
),
array(
'title'=>'MEL',
'code'=>'
// Hello World in MEL scripting language for Alias Maya

proc helloWorld () {
   print "Hello World!\n";
}

helloWorld;
',
),
array(
'title'=>'Microtik',
'code'=>'
#Hello World in Mikrotik RouterOS Scripting Host; :put ("Hello, World!");
',
),
array(
'title'=>'mIRC-Alias',
'code'=>'
;Hello World for mIRC (alias section)

helloworld: /echo -a Hello World!
',
),
array(
'title'=>'mIRC-Commandline',
'code'=>'
; Hello World! for mIRC (command line version)

echo Hello World!
',
),
array(
'title'=>'mIRC-Script',
'code'=>'
;Hello World for mIRC script

alias helloworld {
  /echo -a Hello World!
}
',
),
array(
'title'=>'MivaScript',
'code'=>'
<MvCOMMENT>Hello World in Miva Script</MvCOMMENT>
<MvEVAL EXPR="{"Hello World"}">
',
),
array(
'title'=>'MML-AXE10',
'code'=>'
! Hello world program in MML for Ericsson"s AXE10 telephone exchange
IOTXP:Hello World;
',
),
array(
'title'=>'Modula-2',
'code'=>'
(* Hello World in Modula-2 *)

MODULE HelloWorld;
FROM InOut IMPORT WriteString,WriteLn;
BEGIN
  WriteString("Hello World!");
  WriteLn;
END HelloWorld.
',
),
array(
'title'=>'MoHAA-Script',
'code'=>'
// Hello World in the Medal of Honor Allied Assault scripting language

iprintln "Hello World!"
',
),
array(
'title'=>'Mouse',
'code'=>'
~ Hello World in Mouse

"HELLO, WORLD."
$
',
),
array(
'title'=>'MPD',
'code'=>'
# Hello World in MPD.

resource helloworld()
  write("Hello World")
end
',
),
array(
'title'=>'MSDOS',
'code'=>'
@ECHO OFF
REM Hello World for DOS batch

ECHO Hello World!
',
),
array(
'title'=>'MSIL',
'code'=>'
//Hello World in MSIL (.NET assembler)

.assembly helloworld {}
.class helloworld
{
 .method static void Main() cil managed
 {
  .entrypoint
  ldstr "Hello World!"
  call void [mscorlib]System.Console::WriteLine(string)
  ret
 }
}
',
),
array(
'title'=>'MuLisp',
'code'=>'
; Hello, World! in MuLisp

(print "Hello\,\ world\!)
',
),
array(
'title'=>'Mumps',
'code'=>'
; Hello World in Mumps-M
 w !,"Hello World"
',
),
array(
'title'=>'Natural',
'code'=>'
 * Hello World in Natural (by Software AG)
Write "Hello, World!".
',
),
array(
'title'=>'Nemerle',
'code'=>'
// Hello World in Nemerle (a functional programming language for .NET)

System.Console.WriteLine("Hello World");
',
),
array(
'title'=>'newLISP',
'code'=>'
;; Hello World in newLISP

(println "Hello World")
',
),
array(
'title'=>'NewtonScript',
'code'=>'
// Hello World in NewtonScript

baseview :=
   {viewBounds: {left: -3, top: 71, right: 138, bottom: 137},
    viewFlags: 581,
    declareSelf: "base,
    _proto: protoFloatNGo,
    debug: "baseview"
   };

textview := * child of baseview *
   {text: "Hello World!",
    viewBounds: {left: 33, top: 24, right: 113, bottom: 46},
    viewFlags: 579,
    _proto: protoStaticText,
    debug: "textview"
   };
',
),
array(
'title'=>'Nice',
'code'=>'
//Hello World in Nice

void main(String[] args){ 
   println("hello world"); 
}
',
),
array(
'title'=>'NSIS',
'code'=>'
; Hello World in Nullsoft Software Install Script (NSIS)

Caption "Hello World!"
OutFile ".\HelloWorld.exe"
SilentInstall silent

Section ""
        MessageBox MB_OK "Hello World!"
SectionEnd
',
),
array(
'title'=>'Oberon.oberon',
'code'=>'
MODULE HelloWorld;

(* Hello World in Oberon for the Oberon System *)

  IMPORT Oberon, Texts;

  VAR
      W: Texts.Writer;

  PROCEDURE Do*;
    BEGIN
        Texts.WriteString(W,"Hello World!");
            Texts.WriteLn(W);
                Texts.Append(Oberon.Log,W.buf)
                  END Do;

BEGIN Texts.OpenWriter(W)
END HelloWorld.
',
),
array(
'title'=>'Oberon.std',
'code'=>'
(* Hello World in Oberon for standard operating systems *)

MODULE HelloWorld;
IMPORT Out;
BEGIN
  Out.String("Hello World!");
  Out.Ln;
END HelloWorld;
',
),
array(
'title'=>'OCaml',
'code'=>'
(* Hello World in OCaml *)

print_string "Hello World!\n";;
',
),
array(
'title'=>'Occam',
'code'=>'
PROGRAM Hello
-- Hello world in Occam
#USE ioconv

SEQ
  write.full.string(screen,"Hello World!")
',
),
array(
'title'=>'Octave',
'code'=>'
#Hello World in Octave (http://www.octave.org/)
printf("Hello World\n");
',
),
array(
'title'=>'Omnimark',
'code'=>'
; Hello World in Omnimark


process
     output "Hello World!%n"
',
),
array(
'title'=>'Ook',
'code'=>'
Hello World in Ook. No comments possible.

Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook.
Ook! Ook. Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook?
Ook! Ook! Ook? Ook! Ook? Ook. Ook. Ook. Ook! Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook. Ook! Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook! Ook. Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook. Ook! Ook.
Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook! Ook? Ook? Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook? Ook! Ook! Ook? Ook! Ook? Ook. Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook.
Ook? Ook. Ook? Ook. Ook? Ook. Ook? Ook. Ook! Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook! Ook. Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook.
Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook! Ook!
Ook! Ook. Ook. Ook? Ook. Ook? Ook. Ook. Ook! Ook. Ook! Ook? Ook! Ook! Ook? Ook!
Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook. Ook.
Ook. Ook. Ook. Ook. Ook! Ook.
',
),
array(
'title'=>'OpenVMS',
'code'=>'
$! Hello World in OpenVMS DCL

$ write sys$output "Hello World"
',
),
array(
'title'=>'OPL.simple',
'code'=>'
REM Hello World for OPL (Psion Organizer 3a)
REM Simple version

PROC HELLO:
  PRINT "Hello World!"
  GET
ENDP
',
),
array(
'title'=>'Pascal',
'code'=>'
{Hello World in Pascal}

program HelloWorld(output);
begin
  WriteLn("Hello World!");
end.
',
),
array(
'title'=>'Pascal-Windows',
'code'=>'
{ Hello World in Borland Pascal 7 for MS-Windows}

PROGRAM HelloWorld;

USES
  WinCRT;

BEGIN
  InitWinCRT;
  WriteLn("Hello World!");
  ReadLn;
  DoneWinCRT;
END.
',
),
array(
'title'=>'Pawn',
'code'=>'
/* Hello World in Pawn */

main()
{
    printf "Hello World!"
}
',
),
array(
'title'=>'PBASIC',
'code'=>'
" Hello World in PBASIC (for the Boe-Bot Robot)

" {$STAMP BS2}
" {$PBASIC 2.5}

DEBUG "Hello World!"

END
',
),
array(
'title'=>'PDF',
'code'=>'
%Hello World in Portable Document Format (PDF)
%PDF-1.2
1 0 obj
<<
/Type /Page
/Parent 5 0 R
/Resources 3 0 R
/Contents 2 0 R
>>
endobj
2 0 obj
<<
/Length 51
>>
stream
BT
/F1 24 Tf
1 0 0 1 260 600 Tm
(Hello World)Tj
ET
endstream
endobj
3 0 obj
<<
/ProcSet[/PDF/Text]
/Font <</F1 4 0 R >>
>>
endobj
4 0 obj
<<
/Type /Font
/Subtype /Type1
/Name /F1
/BaseFont /Arial
>>
endobj
5 0 obj
<<
/Type /Pages
/Kids [ 1 0 R ]
/Count 1
/MediaBox
[ 0 0 612 792 ]
>>
endobj
6 0 obj
<<
/Type /Catalog
/Pages 5 0 R
>>
endobj
trailer
<<
/Root 6 0 R
>>
',
),
array(
'title'=>'PEARL',
'code'=>'
MODULE (HELLO);
/* Hello World in PEARL (Process and Experiment Automation Realtime Language) */
  SYSTEM;
   TERMINAL:DIS<->SDVLS(2);
  PROBLEM;
   SPC TERMINAL DATION INOUT
         ALPHIC DIM(,) TFU MAX
         FORWARD CONTROL (ALL);
   MAIN:TASK;
     DCL TEXT INV CHAR(30)
          INIT("HELLO WORLD!");
    OPEN TERMINAL;
    PUT TEXT TO TERMINAL;
    CLOSE TERMINAL;
   END;
MODEND;
',
),
array(
'title'=>'PeopleCode',
'code'=>'
/* Hello World in PeopleCode 8.45

&MsgText; = MsgGetText(66666666, 999999999, "Hello World!");
',
),
array(
'title'=>'Perl',
'code'=>'
# Hello world in perl

print "Hello World!\n";
',
),
array(
'title'=>'PHP',
'code'=>'
<?php
  // Hello World in PHP
  echo "Hello World!";
?>
',
),
array(
'title'=>'PHP+GD',
'code'=>'
<?
// Hello World in PHP + GD library
header("Content-type: image/gif");
$rscImage    = imagecreatetruecolor(80, 25);
$intFontC    = imagecolorallocate($rscImage, 255, 255, 255);
$intBGC        = imagecolorallocate($rscImage, 0, 0, 0);
imagestring($rscImage, 2, 5, 5, "Hello World!", $intFontC);
imagegif($rscImage);
imagedestroy($rscImage);
?>
',
),
array(
'title'=>'Pike',
'code'=>'
// Hello world in Pike (pike.roxen.com)

int main(){
        write("Hello World!\n");
}
',
),
array(
'title'=>'PL-SQL',
'code'=>'
-- Hello World in Oracle PL/SQL (sqlplus)

set serveroutput on

begin
  dbms_output.enable(10000);
  dbms_output.put_line("Hello World");
end;
/
',
),
array(
'title'=>'PL1',
'code'=>'
/* Hello World in PL/1 */

Hello: procedure options(main);
       put skip list("Hello World!");
end Hello;
',
),
array(
'title'=>'Pocket-Calculator',
'code'=>'
Hello World for standard pocket calculators (7-segment display).
Type in and turn calculator upside down.

0.7734
',
),
array(
'title'=>'POP-11',
'code'=>'
;;; Hello World in POP-11

: vars outputtext;
: "Hello World" -> outputtext;
: outputtext =>

** Hello World
',
),
array(
'title'=>'PostgreSQL',
'code'=>'
-- Hello World in PL/pgSQL (PostgreSQL Procedural Language)
-- In old versions replace "$$" by double qoutes

CREATE FUNCTION hello_world() RETURNS text AS $$
BEGIN
RETURN "Hello World";
END
$$ LANGUAGE plpgsql;

SELECT hello_world();
',
),
array(
'title'=>'Postscript',
'code'=>'
% Hello World in Postscript
%!PS
/Palatino-Roman findfont
100 scalefont
setfont
100 100 moveto
(Hello World!) show
showpage
',
),
array(
'title'=>'POV-Ray',
'code'=>'
#include "skies.inc"

camera{location <0,1,-5> look_at y}
light_source{<2,4,-7>,2}
sky_sphere{S_Cloud1}
plane{y 0 pigment{checker rgb 1,0} finish{reflection .5}}
text{ttf "timrom.ttf" "Hello World!" .3,0
  pigment {agate scale .2} translate -2.5*x
}
',
),
array(
'title'=>'Powerbasic',
'code'=>'
" Hello World in Powerbasic Console Compiler

FUNCTION PBMAIN () AS LONG

    PRINT "Hello World"

END FUNCTION
',
),
array(
'title'=>'Powerflex',
'code'=>'
// "Hello World" in PowerFlex

showln "Hello World"
system
',
),
array(
'title'=>'PowerScript',
'code'=>'
// Hello World in PowerScript

MessageBox("","Hello World!")
',
),
array(
'title'=>'Powershell',
'code'=>'
# Hello World in Microsoft Powershell

"Hello World!"
',
),
array(
'title'=>'PPL',
'code'=>'
;Hello World in PPL (PCBoard Programming Language)
PRINTLN "Hello, World!"
',
),
array(
'title'=>'PQN-PROC',
'code'=>'
PQN
C Hello World in PQN/PROC
O Hello World
',
),
array(
'title'=>'PRAAT',
'code'=>'
# Hello World in praat (www.praat.org)
echo Hello World!
',
),
array(
'title'=>'ProC',
'code'=>'
/* Hello World in Pro*C, the Oracle"s embedded SQL environment */

#include <stdio.h>

EXEC SQL INCLUDE SQLCA;

int main() {
   char hello[15];
   char *user = "the_user";
   char *password = "the_password";
   char *sid = "the_sid";

   EXEC SQL CONNECT :user IDENTIFIED BY :password USING :sid;

   EXEC SQL
      SELECT "Hello World" INTO :hello
      FROM DUAL;

   printf("%s\n", hello);

   EXEC SQL COMMIT RELEASE;
 
   return 0;
}
',
),
array(
'title'=>'Profan',
'code'=>'
" Hello World in Profan (http://www.profan.de/)

cls
print "Hello World!"
waitkey
',
),
array(
'title'=>'Progress',
'code'=>'
/* Hello World in Progress */

message "Hello World" view-as alert-box.
',
),
array(
'title'=>'Prolog',
'code'=>'
% Hello World in Prolog

hello :- display("Hello World!") , nl .
',
),
array(
'title'=>'PureBasic-Console',
'code'=>'
; Hello World in PureBasic (console program)

OpenConsole()
   ConsoleTitle ("Hello World!")
   PrintN ("Hello World!")
CloseConsole()
',
),
array(
'title'=>'PureBasic-Messagebox',
'code'=>'
; Hello World in PureBasic (message box)

MessageRequester("Hello World Messagebox","Hello World!")
',
),
array(
'title'=>'PureBasic-Window',
'code'=>'
; Hello World in PureBasic (Window)

If OpenWindow(0, 216, 0, 268, 133,  #PB_Window_SystemMenu | #PB_Window_TitleBar | #PB_Window_ScreenCentered , "Hello World Window")
 If CreateGadgetList(WindowID())
   TextGadget(1, 100, 60, 60, 20, "Hello World!")
 EndIf
EndIf

Repeat    ; Message Loop
Until WaitWindowEvent() = #PB_EventCloseWindow
',
),
array(
'title'=>'Python 3000',
'code'=>'
# Hello World in Python 3000
print("hello World")
',
),
array(
'title'=>'Python',
'code'=>'
# Hello World in Python
print "Hello World"
',
),
array(
'title'=>'qore',
'code'=>'
#!/usr/local/bin/qore
# Hello World in qore

class HelloWorld
{
    constructor()
    {
	background $.output("Hello, world!");
    }
    output($arg)
    {
	printf("%s\n", $arg);
    }
}

new HelloWorld();
',
),
array(
'title'=>'QuakeC',
'code'=>'
// Hello World in QuakeC. This should be put somewhere, for instance
// PutClientInServer() in Client.qc.

bprint("Hello World\n");
',
),
array(
'title'=>'QuickBASIC',
'code'=>'
REM Hello World in QuickBASIC
PRINT "Hello World!"
END
',
),
array(
'title'=>'R',
'code'=>'
# Hello World for R
cat("Hello world\n")
',
),
array(
'title'=>'ratfor',
'code'=>'
# hello.world.in.ratfor
print *, "hello, world"
end
',
),
array(
'title'=>'REALbasic',
'code'=>'
" Hello World in REALbasic (http://www.realsoftware.com/)

msgBox "Hello World!"
',
),
array(
'title'=>'RealText',
'code'=>'
<window width="320" height="160">
<!-- Hello World in RealText  -->
	<font size="12" name="Arial">
	<center>Hello World!</center>
	</font>
</window>
',
),
array(
'title'=>'Rebol-view',
'code'=>'
Hello World in Rebol-view.

rebol[]
view layout[
text "Hello World!"
]
',
),
array(
'title'=>'Redcode',
'code'=>'
; Hello World in Redcode
; Should work with any MARS >= ICWS-86
; (with 128x64 gfx core support, of course!)
;
Start   MOV     0,2455
        MOV     0,2458
        MOV     0,2459
        MOV     0,2459
        MOV     0,2459
        MOV     0,2459
        MOV     0,2459
        MOV     0,2460
        MOV     0,2465
        MOV     0,2471
        MOV     0,2471
        MOV     0,2471
        MOV     0,2479
        MOV     0,2482
        MOV     0,2484
        MOV     0,2484
        MOV     0,2484
        MOV     0,2486
        MOV     0,2486
        MOV     0,2486
        MOV     0,2486
        MOV     0,2488
        MOV     0,2493
        MOV     0,2493
        MOV     0,2493
        MOV     0,2493
        MOV     0,2497
        MOV     0,2556
        MOV     0,2559
        MOV     0,2560
        MOV     0,2565
        MOV     0,2570
        MOV     0,2575
        MOV     0,2578
        MOV     0,2585
        MOV     0,2588
        MOV     0,2589
        MOV     0,2592
        MOV     0,2593
        MOV     0,2596
        MOV     0,2597
        MOV     0,2603
        MOV     0,2605
        MOV     0,2608
        MOV     0,2667
        MOV     0,2670
        MOV     0,2671
        MOV     0,2676
        MOV     0,2681
        MOV     0,2686
        MOV     0,2689
        MOV     0,2696
        MOV     0,2699
        MOV     0,2700
        MOV     0,2703
        MOV     0,2704
        MOV     0,2707
        MOV     0,2708
        MOV     0,2714
        MOV     0,2716
        MOV     0,2719
        MOV     0,2778
        MOV     0,2778
        MOV     0,2778
        MOV     0,2778
        MOV     0,2778
        MOV     0,2779
        MOV     0,2779
        MOV     0,2779
        MOV     0,2782
        MOV     0,2787
        MOV     0,2792
        MOV     0,2795
        MOV     0,2802
        MOV     0,2805
        MOV     0,2806
        MOV     0,2809
        MOV     0,2810
        MOV     0,2810
        MOV     0,2810
        MOV     0,2810
        MOV     0,2812
        MOV     0,2818
        MOV     0,2820
        MOV     0,2823
        MOV     0,2882
        MOV     0,2885
        MOV     0,2886
        MOV     0,2891
        MOV     0,2896
        MOV     0,2901
        MOV     0,2904
        MOV     0,2911
        MOV     0,2912
        MOV     0,2913
        MOV     0,2914
        MOV     0,2917
        MOV     0,2918
        MOV     0,2919
        MOV     0,2922
        MOV     0,2928
        MOV     0,2930
        MOV     0,2933
        MOV     0,2992
        MOV     0,2995
        MOV     0,2996
        MOV     0,3001
        MOV     0,3006
        MOV     0,3011
        MOV     0,3014
        MOV     0,3021
        MOV     0,3022
        MOV     0,3023
        MOV     0,3024
        MOV     0,3027
        MOV     0,3028
        MOV     0,3030
        MOV     0,3032
        MOV     0,3038
        MOV     0,3040
        MOV     0,3103
        MOV     0,3106
        MOV     0,3107
        MOV     0,3107
        MOV     0,3107
        MOV     0,3107
        MOV     0,3107
        MOV     0,3108
        MOV     0,3108
        MOV     0,3108
        MOV     0,3108
        MOV     0,3108
        MOV     0,3109
        MOV     0,3109
        MOV     0,3109
        MOV     0,3109
        MOV     0,3109
        MOV     0,3111
        MOV     0,3111
        MOV     0,3111
        MOV     0,3120
        MOV     0,3121
        MOV     0,3124
        MOV     0,3124
        MOV     0,3124
        MOV     0,3126
        MOV     0,3129
        MOV     0,3130
        MOV     0,3130
        MOV     0,3130
        MOV     0,3130
        MOV     0,3130
        MOV     0,3131
        MOV     0,3131
        MOV     0,3131
        MOV     0,3131
        MOV     0,3135
        JMP     0
',
),
array(
'title'=>'REFAL-2',
'code'=>'
* Hello, World! in REFAL-2
start
entry go
extrn PROUT
go = <prout "Hello, world!">
end
',
),
array(
'title'=>'Regular-Expression',
'code'=>'
Hello World as a regular expression.
Replaces everything with "Hello World".
For use with vi, sed, etc.

Search String :  ^.*$
Replace String: "Hello World"
',
),
array(
'title'=>'Revolution',
'code'=>'
-- Hello World in Revolution (formerly called Transcript)
answer "Hello World!"
',
),
array(
'title'=>'Rexx.simple',
'code'=>'
/* Hello World in Rexx, simple version (writes to standard output) */

say "Hello World!"
exit
',
),
array(
'title'=>'Rexx.window',
'code'=>'
/* Hallo World in Rexx, opens window */

call RxFuncAdd "SysLoadFuncs", "RexxUtil", "SysLoadFuncs"
call SysLoadFuncs
call RxMessageBox "Hello World!", "Hello World Window", "OK", "EXCLAMATION"
exit
',
),
array(
'title'=>'RPG-IV',
'code'=>'
H* Hello World in RPG IV

D msg             S             32    inz(*blank)
D cmd             S             64

C                   eval      msg = "Hello World"

C     msg           dsply

C                   eval      cmd = "DLYJOB DLY(30)"
C                   call      "QCMDEXC"
C                   parm                    cmd
C                   parm      64            len              15 5

C                   eval      *inlr = *on
',
),
array(
'title'=>'RPL',
'code'=>'
Hello World in RPL for the HP-28, HP-48, HP-49 and HP-50 series pocket calculators. No comments possible.

<<
    "HELLO WORLD"
    1 DISP
    60 FREEZE
>>
',
),
array(
'title'=>'RSL',
'code'=>'
// Hello World in RSL (RS-Bank Language)

[Hello World!];
',
),
array(
'title'=>'Ruby',
'code'=>'
# Hello World in Ruby
puts "Hello World!"
',
),
array(
'title'=>'S-Plus',
'code'=>'
# Hello World for S-Plus
cat("Hello world\n")
',
),
array(
'title'=>'SAL',
'code'=>'
// Hello World in SAL

proc main()
    MsgBox("Hello from SAL", "Hello, World!")
end
',
),
array(
'title'=>'SApp',
'code'=>'
comment: Hello World in SApp
popup "Hello ## World!" ,
',
),
array(
'title'=>'SAS',
'code'=>'
/* Hello world in SAS */
* Writes as output title;
TITLE &ldquo;Hello World!&rdquo;;
* writes to the log;
PUT Hello world!;
',
),
array(
'title'=>'Sather',
'code'=>'
-- Hello World in Sather

    class HELLO is
       main is #OUT + "Hello World!\n" end
    end
',
),
array(
'title'=>'Scala',
'code'=>'
// Hello World in Scala

object HelloWorld with Application {
     Console.println("Hello world!");
   }
',
),
array(
'title'=>'Seed7',
'code'=>'
# Hello World in Seed7

$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln("Hello World!");
  end func;
',
),
array(
'title'=>'Self',
'code'=>'
(|  "Hello World in Self"

  hello = (| | "Hello World!" print)
|)
',
),
array(
'title'=>'SenseTalk',
'code'=>'
Hello World in SenseTalk.

on run put "Hello World!" end run 
',
),
array(
'title'=>'Setl2',
'code'=>'
-- Hello World in Setl2

procedure Hello();
   print "Hello World!";
end Hello;
',
),
array(
'title'=>'Shakespeare',
'code'=>'
The Infamous Hello World Program in Shakespeare.

Romeo, a young man with a remarkable patience.
Juliet, a likewise young woman of remarkable grace.
Ophelia, a remarkable woman much in dispute with Hamlet.
Hamlet, the flatterer of Andersen Insulting A/S.


                    Act I: Hamlet"s insults and flattery.

                    Scene I: The insulting of Romeo.

[Enter Hamlet and Romeo]

Hamlet:
 You lying stupid fatherless big smelly half-witted coward!
 You are as stupid as the difference between a handsome rich brave
 hero and thyself! Speak your mind!

 You are as brave as the sum of your fat little stuffed misused dusty
 old rotten codpiece and a beautiful fair warm peaceful sunny summer"s
 day. You are as healthy as the difference between the sum of the
 sweetest reddest rose and my father and yourself! Speak your mind!

 You are as cowardly as the sum of yourself and the difference
 between a big mighty proud kingdom and a horse. Speak your mind.

 Speak your mind!

[Exit Romeo]

                    Scene II: The praising of Juliet.

[Enter Juliet]

Hamlet:
 Thou art as sweet as the sum of the sum of Romeo and his horse and his
 black cat! Speak thy mind!

[Exit Juliet]

                    Scene III: The praising of Ophelia.

[Enter Ophelia]

Hamlet:
 Thou art as lovely as the product of a large rural town and my amazing
 bottomless embroidered purse. Speak thy mind!

 Thou art as loving as the product of the bluest clearest sweetest sky
 and the sum of a squirrel and a white horse. Thou art as beautiful as
 the difference between Juliet and thyself. Speak thy mind!

[Exeunt Ophelia and Hamlet]


                    Act II: Behind Hamlet"s back.

                    Scene I: Romeo and Juliet"s conversation.

[Enter Romeo and Juliet]

Romeo:
 Speak your mind. You are as worried as the sum of yourself and the
 difference between my small smooth hamster and my nose. Speak your
 mind!

Juliet:
 Speak YOUR mind! You are as bad as Hamlet! You are as small as the
 difference between the square of the difference between my little pony
 and your big hairy hound and the cube of your sorry little
 codpiece. Speak your mind!

[Exit Romeo]

                    Scene II: Juliet and Ophelia"s conversation.

[Enter Ophelia]

Juliet:
 Thou art as good as the quotient between Romeo and the sum of a small
 furry animal and a leech. Speak your mind!

Ophelia:
 Thou art as disgusting as the quotient between Romeo and twice the
 difference between a mistletoe and an oozing infected blister! Speak
 your mind!

[Exeunt]
',
),
array(
'title'=>'SilverBasic',
'code'=>'
//Hello World in SilverBasic

PRINT "Hello World!"
',
),
array(
'title'=>'SIMPLE',
'code'=>'
[::PROGRAM:Hello World program in SIMPLE
A EQL @0
MSG A
END
]
{::DATA:Data part
@0:T
Hello World$$M
$$@
}
',
),
array(
'title'=>'Simula',
'code'=>'
! Hello World in Simula;

BEGIN
    OutText("Hello World!");
    OutImage;
END
',
),
array(
'title'=>'SinclairBasic',
'code'=>'
10 REM Hello World in Sinclair BASIC
20 PRINT ("Hello World");
',
),
array(
'title'=>'Smalltalk.simple',
'code'=>'
"Hello World in Smalltalk (simple version)"

Transcript show: "Hello World!".
',
),
array(
'title'=>'Smalltalk.window',
'code'=>'
"Hello World in Smalltalk (in an own window)"
"(to be entered in a special browser)"

VisualComponent subclass: #HelloWorldView
	instanceVariableNames: ""
	classVariableNames: ""
	poolDictionaries: ""
	category: "test"

displayOn: aGraphicsContext

	"Hello World!" asComposedText displayOn: aGraphicsContext.

open

	|window|
	window := ScheduledWindow new.
	window label: "Hello World Demo:".
	window component: self new.
	window open.
',
),
array(
'title'=>'SMIL',
'code'=>'
<!-- Hello World in SMIL -->
<smil>
 <head>
  <layout>
   <root-layout width="300" height="160" background-color="white"/>
   <region id="text_region" left="115" top="60"/>
  </layout>
 </head>
 <body>
  <text src="data:,Hello%20World!" region="text_region">
   <param name="fontFace" value="Arial"/>
  </text>
 </body>
</smil>
',
),
array(
'title'=>'SML',
'code'=>'
(* Hello World in SML *)

fun hello() = output(std_out, "Hello World!");
',
),
array(
'title'=>'Snobol',
'code'=>'
* Hello World in Snobol

        OUTPUT = "Hello World!"
',
),
array(
'title'=>'Spiral',
'code'=>'
Hello World in Spiral. No comment character exists.

e0v ***   *eXlv**   *lX      *2X       **oXi
v * * *   *     *   * 2      * o      **   v*
* * * *   * *****   * v      * v      * *iX *
* * * *   * *       * ^      v *      * * w *
* *** *   * *****   * v      * *      * * v *
*     *   *     ^   * ^      * *      * * * *
* *** *   * ****v   * v      * *      v * * *
* * * *   * *       * *      * *      ^ * * *
* * * *   * *****   * *****  * *****  * *** *
* * * *   *     *   *     *  *     *  **   **
*** ***   *******   *****v^  *******   *****

*wX ***    **3Xp    *rX4..   d5*      qd**  
* 3 * *   **   v^   *    ..  * *      *  ***
* v * ^   * #pX v   * ..  .  * *      *    **
* *** v   * # r #   * ..  .  * *      * !q* *
* * * *   * # v #   * 54 ..  * *      * * * *
* * * *   * # * #   *   @X   * *      * * * *
* * * *   * # * #   *   v    * *      * * * *
* * * *   * # * #   * * **   * *      * * * *
*  *  *   * # * #   * ** **  * *      * *** *
*  *  *   * #v* ^   * *** *  * *****  *    **
** * **   **   *v   * * * *  *     *  *  ***
 *****     *v^**    *** ***  *******  **** 
',
),
array(
'title'=>'SPL',
'code'=>'
 HELLO: PROCEDURE OPTIONS(MAIN);
*
 /* Hello World in SPL4 (Siemens) */
*
 DCL PRINTC ENTRY;
*
 CALL PRINTC("Hello World!", 12);
 RETURN;
*
 END HELLO;
',
),
array(
'title'=>'SPSS',
'code'=>'
* SPSS Syntax
* "Hello World" title in the Output Window of SPSS via SPSS Syntax.

TITLE "Hello World".
',
),
array(
'title'=>'SQL-Advantage',
'code'=>'
-- Hello World in SQL for Advantage Database
 
select "Hello World" from system.iota
',
),
array(
'title'=>'SQL-DB2',
'code'=>'
-- Hello World in SQL for DB2
VALUES("hello world")
',
),
array(
'title'=>'SQL-Oracle',
'code'=>'
# Hello World in SQL for Oracle

SELECT "Hello World" FROM dual;
',
),
array(
'title'=>'SQL',
'code'=>'
# Hello World in SQL

SELECT "Hello World";
',
),
array(
'title'=>'sqlplus',
'code'=>'
-- Hello World in Oracle SQL*Plus

prompt Hello World!
',
),
array(
'title'=>'ST-Guide',
'code'=>'
## Hello World for ST-Guide

@node "Hello World!"

Hello World!

@endnode
',
),
array(
'title'=>'SVG',
'code'=>'
<?xml version="1.0" encoding="utf-8" standalone="no"?>
<!-- Hello World in SVG -->

<svg width="240" height="100" viewBox="0 0 240 100" zoomAndPan="disable"
     xmlns="http://www.w3.org/2000/svg"  xmlns:xlink="http://www.w3.org/1999/xlink">
  <title>Hello World</title>
    <g>
      <text x="10" y="50">Hello World</text>
      <animate attributeName="opacity" values="0;1" dur="4s" fill="freeze" begin="0s"/>
    </g>
</svg>
',
),
array(
'title'=>'T-SQL',
'code'=>'
-- Hello World in T-SQL
PRINT "Hello World"
',
),
array(
'title'=>'T9-Mobile',
'code'=>'
How to enter Hello World on a standard T9 numeric
keypad of an SMS-enabled mobile phone.
T9 predictive text has to be off.

44 33 555 555 666 0 9 666 777 555 3 11111
',
),
array(
'title'=>'TAL',
'code'=>'
!     Hello world in Tandem TAL (Transaction Application Language)
      proc Hello^World main;
            begin
            int    .term[0:12] := [ 12 * [ &ldquo;  &ldquo; ] ],
 .out [0:19];
            string .sout := @out &lsquo;<<&rsquo; 1, .sp;
 
            call myterm ( term[1] );
            call open ( term[1], term );
if <> then call abend;
 
sout &lsquo;:=&rsquo; &ldquo;Hello World&rdquo; -> @sp;
            call write ( term, out, @sp&rsquo;-&lsquo;@sout );
if <> then call abend;
end;
',
),
array(
'title'=>'Tcl',
'code'=>'
#!/usr/local/bin/tclsh
# Hello World in Tcl

puts "Hello World!"
',
),
array(
'title'=>'TECO',
'code'=>'
!Hello World in TECO
!The $ symbol below wouldn"t actually be a printing character -
!it"s the [escape] character, \u001b!
FTHello World$
',
),
array(
'title'=>'TeX',
'code'=>'
% Hello World in plain \TeX
\immediate\write16{Hello World!}
\end
',
),
array(
'title'=>'Texinfo',
'code'=>'
\input texinfo
@c Hello World for Texinfo

@setfilename hello
@settitle Hello World

@node Top, Hello, (dir), (dir)

@menu
* Hello:: Hello World
@end menu

@node Hello, , Top, Top

Hello World!

@contents
@bye
',
),
array(
'title'=>'Thue',
'code'=>'
a::=~Hello World!
::=
a
',
),
array(
'title'=>'TI-59',
'code'=>'
Hello World for the TI-59 with PC-100C thermo printer.
No comment character exists.

The TI-59/PC-100C can print up to 20 characters per line (upper case
only). They are coded as 2-digit decimal numbers (see manual for
details) in up to four print registers (of five characters each)
and then printed.
Before entering the program, press LRN to switch into learn mode.
After entering the program, cancel learn mode with LRN, turn on the
printer, and run the program with A.
A pleasant sound, and what a font! Real TRUE-TYPE!

The output looks like this:

      +--------------------+
      |        HELLO WORLD!|
      |                    |
      +--------------------+


      TI59 Code   Comment

      LBL A       Start of program: label A
      OP 00       Clear the four print registers
      23          "H"
      OP 02       Write into print register 2
      17          "E"
      27          "L"
      27          "L"
      32          "O"
      00          " "
      OP 03       Write into print register 3
      43          "W"
      32          "O"
      35          "R"
      27          "L"
      16          "D"
      73          "!"
      OP 04       Write into print register 4
      OP 05       Start printing
      ADV         Line feed (optional)
      R/S         End program
',
),
array(
'title'=>'TI-8x',
'code'=>'
Hello World for TI 8x/9x basic (tested on a TI-83)

:ClrHome
:Disp "HELLO WORLD"
',
),
array(
'title'=>'TI-BASIC-Extended',
'code'=>'
10 REM Hello World in Extended BASIC
20 REM for the TI99 series computer
100 CALL CLEAR :: DISPLAY AT(10,5):"Hello World" :: ACCEPT AT(20,4):A$
',
),
array(
'title'=>'TI-BASIC',
'code'=>'
10 REM Hello World in TI BASIC
20 REM for the TI99 series computer
100 CALL CLEAR
110 PRINT "HELLO WORLD"
120 GOTO 120
',
),
array(
'title'=>'Tk',
'code'=>'
#!/usr/local/bin/wish -f
# Hello World in Tk

label .l -text "Hello World!"
pack .l
',
),
array(
'title'=>'Toy',
'code'=>'
# Hello World code in Toy Programming Language (generic way)

<< "Hello World";
',
),
array(
'title'=>'Trans',
'code'=>'
// Hello World in Trans (Transmuter Programming Language)
import Console
Console.write("Hello World!")
',
),
array(
'title'=>'troff',
'code'=>'
\"	"Hello, world!" in troff

Hello, world!
',
),
array(
'title'=>'TSO-CLIST',
'code'=>'
PROC 0
/* Hello World in TSO CLIST */
write Hello World!
',
),
array(
'title'=>'Turing-Machine',
'code'=>'
Hello World as a Turing machine.

State   Read   |   Write     Step    Next state
---------------|---------------------------------
1       empty  |   H         >       2
2       empty  |   e         >       3
3       empty  |   l         >       4
4       empty  |   l         >       5
5       empty  |   o         >       6
6       empty  |   blank     >       7
7       empty  |   W         >       8
8       empty  |   o         >       9
9       empty  |   r         >       10
10      empty  |   l         >       11
11      empty  |   d         >       12
12      empty  |   !         >       STOP
',
),
array(
'title'=>'Turing',
'code'=>'
% Hello World in Turing
put "Hello World!"
',
),
array(
'title'=>'UniComal',
'code'=>'
// Hello World in UniComal

PRINT "Hello World"
',
),
array(
'title'=>'Unix-Shell',
'code'=>'
# Hello World for the Unix shells (sh, ksh, csh, bash, ...)

echo "Hello World!"
',
),
array(
'title'=>'unlambda',
'code'=>'
# Hello World in unlambda

`r```````````.H.e.l.l.o. .w.o.r.l.di
',
),
array(
'title'=>'UnrealScript',
'code'=>'
// Hello World for UnrealScript

class HelloWorldHUD extends HudBase;

simulated function DrawHudPassC (Canvas C)
{
  C.SetPos( 0.50*C.ClipX , 0.50*C.ClipY);
  C.DrawText("Hello World!");
}

defaultproperties
{
}
',
),
array(
'title'=>'Ursala',
'code'=>'
# hello world in Ursala

#executable&

f = -[hello world]-!
',
),
array(
'title'=>'Vatical',
'code'=>'
+ Hello World in Vatical

LITURGY:
	PRAY "Hello World!"
AMEN.
',
),
array(
'title'=>'VAX-11-Macro',
'code'=>'
; Hello World in VAX-11 MACRO

        .title hello
term_name:      .ascid /SYS$INPUT/
term_chan:      .blkw 1
out_iosb:       .blkq 1
msg:    .asciz  /Hello, world!/

        .entry start,0

        ; establish a channel for terminal I/O
        $assign_s devnam=term_name,-
                chan=term_chan
        blbc r0,error

        ; queue the I/O request
        $qio_s chan=term_chan,-
                func=#io$_writevblk,-
                iosb=out_iosb,-
                p1=msg,-
                p2=#13
        blbc r0,error

        $exit_s ; normal exit

error:  halt ; error condition

        .end start
',
),
array(
'title'=>'VAX-Macro',
'code'=>'
Hello World in VAX Macro.

        .title  helloworld
        .ident  /hello world/
;
        .library        /sys$library:lib/
        $libdef
        $lib$routinesdef


        .psect  $data,wrt,noshr,noexe,long

hello:  .ascid  /Hello World!/

        .psect  $code,nowrt,shr,exe,long

        .entry  helloworld,^m<r9,r10,r11>

        pushaq  hello                   ; output the
message
        calls   #1,g^lib$put_output     ;

        ret                             ; GTFOH
        .end    helloworld              ;
',
),
array(
'title'=>'VBScript',
'code'=>'
" Hello World in VBScript (Windows Scripting Host)
msgbox "Hello, World!"
',
),
array(
'title'=>'Velocity',
'code'=>'
<HTML>
<!-- Hello World in Velocity -->
<BODY>
  #set( $foo = "Hello World" )
  $foo
</BODY>
</HTML>
',
),
array(
'title'=>'Verilog',
'code'=>'
/* Hello World in Verilog. */

module main;

 initial
   begin
     $display("Hello, World");
     $finish ;
   end

 endmodule
',
),
array(
'title'=>'VHDL',
'code'=>'
--Hello World in VHDL

ENTITY helloworld IS
END helloworld;

ARCHITECTURE hw OF helloworld IS

BEGIN

ASSERT FALSE
REPORT "HELLO, WORLD!"
SEVERITY NOTE;

END hw;
',
),
array(
'title'=>'Visual-FoxPro',
'code'=>'
*Hello World in Microsoft Visual FoxPro 5-9
? "Hello World!"
',
),
array(
'title'=>'VisualBasic',
'code'=>'
REM Hello World in Visual Basic for Windows

VERSION 2.00
Begin Form Form1
   Caption         =   "Form1"
   ClientHeight    =   6096
   ClientLeft      =   936
   ClientTop       =   1572
   ClientWidth     =   6468
   Height          =   6540
   Left            =   876
   LinkTopic       =   "Form1"
   ScaleHeight     =   6096
   ScaleWidth      =   6468
   Top             =   1188
   Width           =   6588
   Begin Label Label1
      Caption         =   "Hello World!"
      Height          =   372
      Left            =   2760
      TabIndex        =   0
      Top             =   2880
      Width           =   972
   End
End
Option Explicit
',
),
array(
'title'=>'VisualBasic.NET',
'code'=>'
"Hello World in Visual Basic .NET (VB.NET)

Imports System.Console

Class HelloWorld

    Public Shared Sub Main()
        WriteLine("Hello, world!")
    End Sub

End Class
',
),
array(
'title'=>'VisualBasic6',
'code'=>'
" Hello World in Visual Basic 6

Private Sub Form_Load()
Print "Hello World"
End Sub
',
),
array(
'title'=>'VisualWorksSmalltalk',
'code'=>'
"Hello World! in VisualWorks Smalltalk"

Dialog warn: "Hello World!".
',
),
array(
'title'=>'VMS-DCL',
'code'=>'
$ ! Hello World in Digital Command Language for the VMS operating system

$ WRITE SYS$OUTPUT "Hello World!"
',
),
array(
'title'=>'VRML',
'code'=>'
#VRML V2.0 utf8
# Hello World in VRML

Shape
        {
        geometry Text
                {string "Hello World!"}
        }
',
),
array(
'title'=>'VSL',
'code'=>'
/* "hello, world" in VSL (the script language of Virtools),
to be used in a "Run VSL" building block */
 
void main()
{
    bc.OutputToScreen("hello, world");
}
',
),
array(
'title'=>'Whitespace',
'code'=>'
Hello #World #in #Whitespace	* # #	* # # #
+	*[Space]
+ #is #marked #with"#" # #[tab]	#with"*"	*line-feed #with #"+"	* #	*so
+it	#would
+be #easier #to #write #again... #All	*the	*non-whitespace-characters #are	*ignored...	* # #
+	*
+ # # # # #	*	* #	*	* # #
+	*
+ # # # # #	*	* #	*	*	*	*
+	*
+ # # # # #	* # # # # #
+	*
+ # # # # #	* #	* #	*	*	*
+	*
+ # # # # #	*	* #	*	*	*	*
+	*
+ # # # # #	*	*	* # #	* #
+	*
+ # # # # #	*	* #	*	* # #
+	*
+ # # # # #	*	* # #	* # #
+	*
+ # # # # #	* # # # #	*
+	*
+ # # # # #	* #	* #
+	*
+ # #
+
+
+
',
),
array(
'title'=>'wml',
'code'=>'
# Hello World in Wesnoth Markup Language (wml)
#define HELLOWORLD
  [message]
    speaker="narrator"
    message=_"Hello World"
  [/message]
#enddef
',
),
array(
'title'=>'WSH',
'code'=>'
// Hello World for the Windows Scripting Host
WScript.Echo("Hello World!");
',
),
array(
'title'=>'X++',
'code'=>'
// Hello World in X++ (Microsoft Axapta)

class classHelloWorld
{
}

static void main(args Args)
{
    dialog   dialog;
    ;
    dialog = new dialog();
    dialog.addText("Hello World!");
    dialog.run();
}
',
),
array(
'title'=>'xblite',
'code'=>'
" Hello World in xblite, Windows GUI mode

	IMPORT "gdi32"
	IMPORT "user32"

DECLARE FUNCTION Entry ()

FUNCTION Entry ()
	MessageBoxA (0, &"Hello World!", &"Hello World Window", $$MB_OK)
END FUNCTION
END PROGRAM
',
),
array(
'title'=>'XHTML',
'code'=>'
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<!-- Hello World in XHTML -->
<html
 xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>
      Hello World!
    </title>
  </head>
  <body>
    <p>
      Hello World!
    </p>
  </body>
</html>
',
),
array(
'title'=>'XML',
'code'=>'
<?xml version="1.0" encoding="ISO-8859-1"?>
<?xml-stylesheet type="text/xsl" href="HelloWorld.xsl" ?>
<!-- Hello World in XML -->
<text><string>Hello, World</string></text>
',
),
array(
'title'=>'XPL0',
'code'=>'
\Hello World in XPL0
code Text=12;
Text(0, "Hello World!
")
',
),
array(
'title'=>'XQuery',
'code'=>'
(: Hello World with XQuery :)
let $i := "Hello World"
return $i
',
),
array(
'title'=>'XSL-FO',
'code'=>'
<?xml version="1.0" encoding="utf-8"?>
<!-- Hello World in XSL-FO -->
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
    <fo:layout-master-set>
        <fo:simple-page-master master-name="LetterPage" page-width="8.5in" page-height="11in">?
            <fo:region-body region-name="PageBody" margin="0.7in"/>
        </fo:simple-page-master>
    </fo:layout-master-set>
    <fo:page-sequence master-reference="LetterPage">
        <fo:flow flow-name="PageBody">
            <fo:block font-size="12pt" font-family="courier">Hello, World</fo:block>
        </fo:flow>
    </fo:page-sequence>
</fo:root>
',
),
array(
'title'=>'XSLT',
'code'=>'
<?xml version="1.0" encoding="UTF-8"?>
<!-- Hello World in XSLT -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
        <xsl:text>Hello World!</xsl:text>
    </xsl:template>
</xsl:stylesheet>
',
),
array(
'title'=>'XUL',
'code'=>'
<?xml version="1.0"?>
<!-- Hello World in XUL -->
<window xmlns="http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul">
<label value="Hello World!"/>
</window>
',
),
array(
'title'=>'ZIM',
'code'=>'
% Hello World in ZIM (database and development language)

out "Hello World"
',
),
);
?>
