;.OPT
;UADE15; > Uade15
;.TTL
;Fileserver file UADE15

.UADE15

	;*  C O M M A N D  P R O C E S S O R*

.CMND
{
	JSR GETBB;Get big buffer !

	JSR VSTRIN
	EQUB CR,"Starting - "
	NOP


.CLPA
	LDX #MONON
	STX MONFLG;Monitor ON to start with

	;SET UP RXCB

	INX
	STX EVCHAR;initialise event flag
	STX RIPPLE;Rxcb counter

	JSR OPENRX;open CB1

	LDA &220
	STA OLDEV
	LDA &220 + 1
	STA OLDEV + 1

	LDA #LO(EVENT)
	STA &220
	LDA #HI(EVENT)
	STA &220 + 1

	LDA #14
	LDX #2
	JSR OSBYTE;enable KB event
	LDA #229;disable escape
	LDX #1
	JSR OSBYTE

	JSR VSTRIN
	EQUB "Ready",CR,CR,15
	NOP


	LDA #53
	JSR OSBYTE;** 13/2/84 **
	; sever any REMOTE connection now

	LDX #LO(PROT)
	LDY #HI(PROT)
	LDA #&13; Net OSWORD
	JSR OSWORD; Set protection mask


	; Poll KB and Rxcb's in the I/O processor
	
.CPOLXX
	JSR PRTIM;** only call dongle every 30 or so seconds **
	LDA #85;APPROX 30 SECONDS
	STA TCOUNT

.CLPD;CLPD
.CPOLL0

	LDA EVCHAR;look for event
	BEQ COMRTS
	LDX #0
	STX EVCHAR;reset the flag
	CMP #ESC
	BEQ DOCHDI
	AND #&DF;force upper case
	CMP #'M'
	BEQ DOMONT;switch monitor status
	CMP #'Q'
	BEQ DOQUIT;call HALT

	;else fall through ..
	;Now poll the control blocks using the MOS

.*COMRTS;come here for new command

	LDX RXTAB;get number of CB to poll
	STX RXCBN;save this for later use
	LDY #1;APPROX .36 SECOND TIMEOUT
	JSR WAIT3;Poll receive
	BMI DOCMND;leap off to deal with the Rx'd packet
	
	DEC TCOUNT
	BPL CPOLL0
	BMI CPOLXX

.DOCHDI;DOCHDI
	LDA #&7E
	JSR OSBYTE;acknowledge escape condition here
	JSR NOEVNT;disable events
	LDA #8
	JSR SETRTN;ensure free space table
	JSR USRMAN
	JSR CHDISC;*** CHANGE DISC **
	JMP CLPA;back to the start

.DOQUIT;DOQUIT
	JSR NOEVNT;disable events
	LDA #8;ensure all users free space ** 3/10/84 **
	JSR SETRTN
	JSR USRMAN;no RC from this
	JSR FLUSYS
	LDX #LO(DOBYE)
	LDY #HI(DOBYE)
	JSR OSCLI;*bye the discs
	JMP INITCO;Back to COMMAND prompt

.DOBYE
	EQUB "Bye",CR


.DOMONT;DOMONT

	;Switch monitor and go back to loop start.

	JSR VSTRIN
	EQUB CR,"Monitor "
	NOP

	LDX MONFLG
	CPX #MONOFF
	BEQ DOMONL
	JSR VSTRIN
	EQUB "OFF"
	NOP
	LDA #MONOFF
	JMP DOMONJ

.DOMONL
	JSR VSTRIN
	EQUB "ON"
	NOP
	LDA #MONON
.DOMONJ
	STA MONFLG

.DBNCEX
	JSR OSCRLF
	JMP CLPD;Go back to main RX loop



.DOCMND;DOCMND

	JSR UPDTE;just in case DATE is used

	;Reception has happened, so check
	;the function code in the RXbuffer
	;and jump to the operation.

	LDA RXCBN;save CB number
	PHA ;altered in OPENRX
	JSR OPENRX;open a new Rxcb
	
	PLA ;RXTAB updated to new CB here
	STA RXCBN;reset CB number
	JSR RWRXCB;read the control block

IF DBG2
	lda #'R'
	jsr OSWRCH
	jsr prtrxb
ENDIF

	; the structure set up in RXCBV is of the form
	; CB number (1) ;in RXCBN
	; CB data (12)

	; Thus QPTR is always zero as only one CB is
	; read at any time

	LDA #&00
	STA QPTR;standard offset
	SEC
	LDA RXCBV + 4;get low byte of address
	SBC #LO(RXBUF);calulate buffer offset
	STA BPTR;point at the buffer
	TAX

	LDA RXBUF,X;Reply port at top of buffer
	STA RPLYPT;Store reply port

	LDA CBBUF
	STA OLDRXB;STORE OLD BUFFER PTR.
	LDA CBBUF + 1
	STA OLDRXB + 1


	;Provide p.0 pointer to control block (used
	;in XMIT/REPLY etc.)

	CLC
	LDA QPTR
	ADC #LO(RXCBV)
	STA NETCB
	LDA #HI(RXCBV)
	ADC #0
	STA NETCB + 1

	LDA RXBUF + 1,X;Get fn. code
	BEQ FCOK;Don't monitor commands immediately

	PHA
	JSR MONITR;*** Do monitor **
	PLA

	CMP #FCMAX + 1;Is < biggest fn. code +1 ?
	BCC FCOK;Yes => ok

	LDA #COERRA

	;Send error return to machine

	JSR EXTERR

	;Finish command (don't matter if error
	;didn't get through).

	JMP COMRTS

.FCOK;FCOK

	;FN. code is ok, so get routine entry address

	ASL A
	TAX
	
	LDY BPTR
	LDA CPUFD,y
	STA UMHUFD
	LDA CPLIB,Y
	STA UMHLIB
	LDA CPCSD,Y
	STA UMHCSD
		
	LDA FNTAB,X
	STA GENPTR
	LDA FNTAB + 1,X
	STA GENPTR + 1

	;ENTER ROUTINE ..

	JMP (GENPTR)


	;T A B L E S

.FNTAB;FNTAB

	;Addresses of functions
	EQUW CLINE;Decode command line
	EQUW SAVE;1
	EQUW LOAD;2
	EQUW EXAMIN;3
	EQUW CATHDR;4
	EQUW LOAD;Load command for execution
	EQUW FIND;6
	EQUW CPSHUT;7
	EQUW GETBYT;8
	EQUW PUTBYT;9
	EQUW GBYTES;10
	EQUW PBYTES;11
	EQUW CPRDAR;=> GETARGS
	EQUW CPSTAR;=> SETARGS
	EQUW DISCS;14
	EQUW CPUSRS;15
	EQUW CPDATE;16
	EQUW CPEOF;17
	EQUW CPINFO;18
	EQUW CPSTAT;Set object attributes
	EQUW CPDEL;Delete from fn. code
	EQUW USRENV;21
	EQUW CPSOPT;Set user option bits
	EQUW FCBYE;Logoff by function code
	EQUW CPUINF;Single user info.
	EQUW CPVERN;25
	EQUW CPSPAC;26 - returns disc free space
	EQUW CDIRFN;27 - cdir from function code, specify size
	EQUW CPSETD;28 - Set date and time from program
	EQUW SAVE; 29 - Do a 'create', like 'save' but no data
	EQUW RDFREE;30 - read callers free space
	EQUW WRFREE;31 - write users free space
	EQUW CPWHO;32 - return client user name
	EQUW CPUSRS;33 - ?
	EQUW CPUINF;34 - ?


	;COMMAND LINE DECODER ....


.COMTAB
	EQUB "DELETE"
	EQUB CLIDLM
	EQUB LO(DELETE), HI(DELETE)
	EQUB "INFO"
	EQUB CLIDLM
	EQUB LO(INFO), HI(INFO)
	EQUB "DIR"
	EQUB CLIDLM
	EQUW SELDIR
	EQUB "SDISC"
	EQUB CLIDLM
	EQUW SELDSC
	EQUB "CDIR"
	EQUB CLIDLM
	EQUW CDIR
	EQUB "ACCESS"
	EQUB CLIDLM
	EQUW SETACC
	EQUB "PASS"
	EQUB CLIDLM
	EQUW SETPW
	EQUB "I AM"
	EQUB CLIDL1
	EQUW LOGON
	EQUB "BYE"
	EQUB CLIDLM
	EQUW USROFF
	EQUB "NEWUSER"
	EQUB CLIDLM
	EQUW NEWUSE
	EQUB "PRIV"
	EQUB CLIDLM
	EQUW STPRIV
	EQUB "LIB"
	EQUB CLIDLM
	EQUW SLIB
	EQUB "REMUSER"
	EQUB CLIDLM
	EQUW REMUSE
	EQUB "RENAME"
	EQUB CLIDLM
	EQUW RENAME
	EQUB 0;Indicates table end


.CLINE
	LDX #&FF
	CLD
.THUNK
	LDY BPTR
	JSR SPACES
	DEY
.FIRCH
	INY
	INX
	LDA COMTAB,X
	BEQ BADCOM;Last delimiter is zero
	BMI CHKLST
	EOR MIDRX,Y;Compare
	AND #&DF;Force cases
	BEQ FIRCH
	DEX
.MINUS
	INX
	LDA COMTAB,X
	BPL MINUS

	LDA MIDRX,Y
	CMP #'.'
	BNE MINNXT
	INY
	JMP CLINE3

.CHKLST
	LDA MIDRX,Y;Otherwise check char. after cmnd. is non-alpha
	JSR ISCHAR
	BCS CLINE3;Is non-alpha => end of command
.MINNXT
	INX
	INX ;Inc. past CLIDLM and address
	JMP THUNK


.CLINE3
	LDA COMTAB + 1,X;Set jump addresses
	STA COZERO
	LDA COMTAB + 2,X
	STA COZERO + 1
	JMP (COZERO)


.BADCOM

	;UNRECOGNISED COMMAND

	JSR BUFTXT
	BNE BDEXIT;Quotes error
	LDX #0;Now move from TXTBUF to TXBUF
.BCLOOP
	LDA TXTBUF,X
	STA MIDTX,X
	INX
	CMP #CR
	BNE BCLOOP

	CPX #1
	BNE BCONA
	LDA #WOTERR
	JSR EXTERR;Do WHAT? error immediately
	JMP BDEXIT
.BCONA
	TXA
	CLC
	ADC #TXHDR;Message length
	LDY #CCCMND
	STY CCODE
	JSR REPLYC
.BDEXIT
	JMP COMRTS
}


.MONITR;MONITR
{
	BIT MONFLG
	BPL MONEX;bit7=0 -> no monitor

	LDX #MONTL;length of the 'monitored' function codes table

.MON1
	CMP MONT1,X;look for function match
	BEQ MON2;found one
	DEX
	BPL MON1;if fall thru' then exit
.MONEX
	RTS

.MON2
	STX COTEMP+1;save this for later use
	JSR PRTMC;print the machine number

	JSR VSTRIN
	EQUB ": "
	NOP

	LDY #0
	LDX COTEMP+1
	BEQ MON11;CLI here

.MON3
	LDA MONT3,Y;read though the text
	BEQ MON4;count the number so far
	INY
	BNE MON3
.MON4
	INY
	DEX
	BNE MON3
	LDX COTEMP+1

.MON5
	LDA MONT3,Y;now at the correct operation
	BEQ MON6
	JSR WRCH;print ascii chars only
	INY ;step thru'
	BNE MON5;stop on zero

.MON6
	JSR PRTSPC;print a space
.MON11
	LDA MONT2,X;determine objectname offset (if any)
	BMI MON9;none here

	CLC
	ADC BPTR
	TAY ;Y has offset within the buffer

.MON7
	LDA MIDRX,Y;get chars from Rx buffer
	CMP #CR;end of name
	BEQ MON8
	JSR WRCH;print the name
	INY
	BNE MON7

.MON8
	JSR PRTSPC;print another space

.MON9
	LDA MONT4,X;determine size offset (if any)
	BMI MON10
	CLC
	ADC BPTR
	TAY ;Y has offset within the buffer

	LDA MIDRX+2,Y
	JSR WHEX
	LDA MIDRX+1,Y
	JSR WHEX
	LDA MIDRX,Y
	JSR WHEX

.MON10
	JMP OSCRLF


.WRCH
	CMP #&7F
	BCS MONEX
	CMP #&20
	BCC MONEX
	JMP OSASCI;print ascii characters

.MONT1
	EQUB 0,1,2,5,20,23,27,29;function codes
.MONT2
	EQUB 0,11,0,0,0,-1,1,11;name offsets
.MONT3
	EQUB 0;CLI here
	EQUB "Save",0
	EQUB "Load",0
	EQUB "Run",0
	EQUB "Delete",0
	EQUB "Bye",0
	EQUB "Cdir",0
	EQUB "Create",0
.MONT4
	EQUB -1,8,-1,-1,-1,-1,-1,8;length offsets
MONTL =  MONT2-MONT1-1
}


.PRTMC
{
	LDA CBSTID+1
	BEQ PRTMC3

	JSR MKDEC;make decimal
	JSR PRTMC2;print the network number
	
	LDA #'.'
	JSR OSWRCH
	JMP PRTMC5
	
.PRTMC3
	LDX #4

.PRTMC4
	LDA #' '
	JSR OSWRCH
	DEX
	BNE PRTMC4
	
.PRTMC5
	LDA CBSTID;print the station number
	JSR MKDEC
	JSR PRTMC2
	
	LDX #4
	LDA CBFLG
	AND #&78
	BEQ PRTMC1
	
	LSR A
	LSR A
	LSR A
	LSR A
	PHA
	LDA #'['
	JSR OSWRCH
	PLA
	JSR WHEXD
	LDA #']'
	JSR OSWRCH

	LDX #1
.PRTMC1
	LDA #SPACE
	JSR OSWRCH
	DEX
	BNE PRTMC1;print spaces

	RTS

.PRTMC2
	LDA COWORK;msig digit
	JSR OSWRCH
	LDA COWORK+1
	JSR OSWRCH
	LDA COWORK+2
	JMP OSWRCH;print all three characters in COWORK
}

.MKDEC;convert to decimal
{
	TAY

	LDA #LO(-1)
	STA TEMPA;leading zero flag

	LDA #100
	JSR MKDEC1
	STA COWORK
	LDA #10
	JSR MKDEC1
	STA COWORK+1
	LDA #1

.MKDEC1
	STA COWORK+3
	TYA
	LDX #'0'-1
	SEC

.MKDEC2
	INX
	SBC COWORK+3
	BCS MKDEC2
	ADC COWORK+3

	CPX #'0'
	BNE MKDEC3
	BIT TEMPA
	BPL MKDEC3
	LDX #SPACE
	BNE MKDEC4

.MKDEC3
	INC TEMPA;print zeros
.MKDEC4
	TAY ;restore original parameter
	TXA
	STA COWORK+2;save this for now
	RTS
}

.CHDISC;CHDISC
{
	;1) READ DRIVE NUMBER (IF DRIVES>1)
	;2) GET DISC NUMBER FOR DRIVE
	;3) FLUSH ALL OBJECTS FROM STORE
	;4) ENSURE DISC MAP
	;5) RESTART (MAPMAN)
	;6) RESTART (AUTMAN)

	LDA DRIVES
	SEC
	SBC #1
	BEQ CHDILA;ONE DRIVE, CONTINUE

	JSR RDDRIV
	BNE CHDIQ;IF QUIT TYPED, QUIT ** 13/9/84

.CHDILA
	PHA ;STORE DRIVE NO.
	JSR CHMSG;"CHANGING DRIVE - ??"
	PLA
	STA CURDRV
	JSR DRVINF;GET DISC NO.

	BNE CHDIAB;ABORT IF ERROR

	LDA #8
	JSR SETRTN
	JSR STRMAN;*** FLUSH STORE **
	BNE CHDIAB

	LDA #6
	JSR SETRTN
	JSR MAPMAN;*** ENSURE DISC **
	BNE CHDIAB

.CHDILB
	JSR CHMSGA;"LOAD NEW DISC"
	BNE CHDIQ;IF ABORT, ABORT ** 13/9/84 **

	LDA CURDRV
	LDY #ARGB
	STA (NEWARG),Y
	LDA #9
	JSR SETRTN
	JSR MAPMAN;*** RESTART DRIVE **
	BEQ CHDIOK;ALL WELL, FINISH

	JSR USRERR;ERROR, PRINT MESSAGE
	JMP CHDILB

.CHDIOK
	JSR GETPWF;Restart AUTMAN
	JSR VSTRIN
	EQUB CR,"Restarting - "
	NOP

	RTS

.CHDIAB
	JSR INTERR
.CHDIQ
	JSR FLUSYS;clear out the system
	JMP INITCO;** 13/9/84 ** return to command prompt after abort
}

.RDDRIV;RDDRIV
{
	;READ DRIVE NUMBER FROM CMND. LINE

	JSR VSTRIN
	EQUB CR,"Drive: "
	NOP

	JSR RDLINE

	LDY #0
	LDA (COMPTR),Y
	AND #&DF;force upper case
	CMP #ABTCH;ABORT ?? "Q"
	BNE RDDONA;** 13/9/84 **

	LDA #&FF
	BNE RDDREX;ABORT ...

.RDDONA
	JSR GETINT;GET DRIVE NO.
	BNE RDDRIV;DO AGAIN IF NESC.
	CPX DRIVES
	BCS RDDRIV;INTEGR>=DRIVES -> ERROR
	TXA

	LDX #0;GIVE Z SET EXIT
.RDDREX
	RTS
}


.CHMSG;CHMSG

	PHA
	JSR VSTRIN
	EQUB CR,"Changing drive - "
	NOP

	PLA
	JSR WHEX
	JMP OSCRLF



.CHMSGA;CHMSGA
{
	JSR VSTRIN
	EQUB CR,"Load new disc",CR
	NOP

.CHMALP
	JSR OSECHO;fudge around ESCAPE problem ** 28/9/84 **
	CMP #SPACE
	BEQ CHMAEX
	AND #&DF;force upper case ** 13/9/84 **
	CMP #ABTCH
	BNE CHMALP

	LDA #&FF;GIVE ABORT EXIT

.CHMAEX
	RTS
}

.GETPWF
{
	;Call AUTMAN restart and write message
	;if PW file not found.

	LDA #6
	LDY #ARGA
	STA (NEWARG),Y
	JSR AUTMAN
	BEQ GETPWX

	JSR VSTRIN
	EQUB CR,"WARNING - PW File not found"
	NOP

.GETPWX
	RTS
}

.EVENT
{
	PHP ;save status over routine
	PHA
	CMP #2
	BNE EVENT1
	STY EVCHAR
	PLA
	PLP
	RTS

.EVENT1
	PLA
	PLP
	JMP (OLDEV);unknown event
}

.NOEVNT
	LDA #15
	LDX #0
	JSR OSBYTE;flush buffer

	LDA #13
	LDX #2
	JSR OSBYTE;disable event
	LDA #229
	LDX #0
	JSR OSBYTE;re-enable escape

	LDA OLDEV
	STA &220
	LDA OLDEV + 1
	STA &220 + 1

	LDX RXCBN;get CB number
	LDA #52
	JMP OSBYTE;delete open CB

.OPENRX
{
	;Set flag, port and stid, and buffer pointers
	;The start of a buffer for one cb. is 1+end of buffer
	;fr previous cb (kept in TEMP). So the end address
	;is stored in TEMP and incremented adter setting
	;buffer pointers.
	;The one byte gap between the buffers
	;is to prevent corruption when a zero is placed
	;at the end of received data to prevent
	;catastrophe when a bad file name is being
	;decoded.

	JSR MKRXCB
	JSR RWRXCB
	
	LDA RXCBN
	STA RXTAB;save CB number
	BNE CONC

	LDA #IERRAI;unable to open Rxcb
	JMP INTERR

.CONC
	RTS
}

.MKRXCB
{
	LDA RIPPLE
	INC RIPPLE
	LSR A;flag in carry

	LDX #(CTABA3-CTABA1-1)
	BCC CONB
	LDX #(CTABA4-CTABA1-1)

.CONB
	LDY #12
.CLPC
	LDA CTABA1,X
	STA RXCBN,Y

	CPX #(CTABA3-CTABA1)
	BNE CLPJ
	LDX #(CTABA2-CTABA1)
.CLPJ
	DEX
	DEY
	BPL CLPC

	RTS
	
.CTABA1;CTABA
	;Initial receive control block
	EQUB 0;Block number
	EQUB RXFLAG
	EQUB COPORT;Command port
	EQUB 0
	EQUB 0
.CTABA2
	EQUD RXBUF
	EQUD RXBUF + RXBUFL
.CTABA3
	EQUD RXBUF +RXBUFL + 1
	EQUD RXBUF +RXBUFL +RXBUFL + 1
.CTABA4
}

.RWRXCB
if DBG2 AND FALSE
	lda #'O'
	jsr OSWRCH
	jsr prtrxb
endif

	LDX #LO(RXCBN)
	LDY #HI(RXCBN)
	LDA #&11
	JMP OSWORD


IF DBG2
.prtrxb
{
	pha
	tya
	pha
	txa
	pha
	
	ldx #0
.dbg2lp
	lda RXCBN,x
	jsr WHEX
	jsr PRTSPC
	inx
	cpx #13
	bne dbg2lp
	jsr OSCRLF
	
	pla
	tax
	pla
	tay
	pla
	rts
}
ENDIF

;.LNK
;UADE16
