;.OPT
;UADE20;FILE > Uade20
;.TTL
;Fileserver file UADE20

.UADE20

	;C O M M A N D  P R O C E S S O R

	;U T I L I T I E S




.RCODE

	;Check return code in A. If 0, send
	;3 byte message. If not, go to error
	;routine
	;Command code always set 0.

	LDX #0
	STX CCODE
	TAY
	BEQ REPLYA
	JMP EXTERR


.REPLYA

	;Sends a zero return code to client.
	;Sets RC into TXBUF and drops through into
	;REPLY, passing a the header length as message length

	LDA #TXHDR
.REPLYC
	LDX #0;Set R.code
	STX RTCODE

.REPLY

	;Sends a message of length A to client
	;on the reply port (in var RPLYPT).

	LDX RPLYPT

.REPLYB

	;Sends a message of length A on port X
	;to client.


	LDY QPTR;Get CB pointer
	CLC
	ADC #LO(TXBUF)
	STA CBBUFE,Y;Buffer end ptr. (lo)
	LDA #0;Assumed msg. length < 255

	STA CBBUF + 2,Y
	STA CBBUF + 3,Y;hi order addresses
	STA CBBUFE + 2,Y
	STA CBBUFE + 3,Y;from 2nd processor

	ADC #HI(TXBUF)
	STA CBBUFE + 1,Y;Buffer end ptr (hi)

	LDA #LO(TXBUF)
	STA CBBUF,Y;Buffer ptr lo
	LDA #HI(TXBUF)
	STA CBBUF + 1,Y;BUffer ptr hi
	TXA ;Set port
	STA CBPORT,Y


.SEND
	;Entry point used when CB set up elsewhere
	;(e.g. LOAD/EXAMINE). Assumed all is
	;set but TX flag.

	LDA #TXFLAG
	ORA CBFLG,Y
	STA CBFLG,Y

.XMIT
{
	;NETCB = page zero pointer to control block

	LDA #TXRPT;no. of times to try transmit
	STA TXJ
	LDA #TXDLY;Delay between tries
	STA TXD
	
	LDY #0
	LDA (NETCB),Y
	STA TXF;flag byte
	
.XLP
	LDX NETCB;Pointers to CBlock
	LDY NETCB + 1
	LDA #&10
	JSR OSWORD;call Tx

	LDY #0
	LDA (NETCB),Y;check for Tx on Tx
	BNE XLP2
	LDA TXF
	STA (NETCB),Y
	BNE XLP;restore flga byte and try again

.XLP2
	LDA #50
	JSR OSBYTE
	TXA
	BMI XLP2;poll 'til done
	
	BEQ XMITEX;Yes => success => exit
	DEC TXJ;No. of times decrement
	BEQ XERR;All done => failure
	LDY TXD;Delay time
	JSR MSDELY;Do delay
	JMP XLP;Do again
	
.XERR
	TXA ;Return flag in A
.XMITEX
	RTS
}

.WAIT
	;Wait to receive from client.
	;Control block pointed by QPTR, wait
	;is a constant (WAITCL) no. of msecs.

	LDA #RXFLAG
	STA CBFLG;CB is ammended for Rx, so open it

	LDX #&00
	STX RXCBN
	JSR RWRXCB

	LDY #50;APPROX 1 MINUTE
	
.WAIT2
{
	JSR WAIT3
	JSR RWRXCB;read the control block
	LDA #0
	BIT RXCBN + 1;read flag byte
	BMI WAITOK
	
	LDA #WAITER;timeout

.WAITOK
	RTS
}

	;ENTRY : RXCBN = BLOCK, Y = TIMEOUT
	;EXIT  : N FLAG SET IF DATA RECEIVED
.WAIT3
{
	STY TIMER2
	LDX RXCBN
	LDA #51
	JSR OSBYTE;TRAP FOR EMULATOR

	;Total inner loop time = 14 cycles
	;So ONEMS = 71 (decimal)

	LDX #WAITCL
	STX TIMER1
	
.WAITLA
	LDA #ONEMS;One msec loop   ;=5
	STA TIMER;(2cycles)
	
.WAITLB
	LDX RXCBN;(4usecs)
	LDA #51
	JSR OSBYTE;poll Rx
	TXA
	BMI WAITEX;(2) ;N=1
	
	DEC TIMER;(5) timer not p.0
	BNE WAITLB;(3)
	DEC TIMER1;(2)
	BNE WAITLA;(3)
	DEC TIMER2
	BNE WAITLA
	
	;N=0
	
.WAITEX
	RTS
}

.ERROR
	JSR EXTERR
	JMP COMRTS

	;EXTERR

	;Puts error code in A into TXBUF +02.
	;Looks A up in ERRTAB and if found, puts text into
	;message.
	;If not found, uses text F.S. ERROR xx (xx is A
	;in hex).
	;Message then send to client using REPLY.
	;CCODE always set to zero in LOOKER

.EXTERR
	PHA
	JSR LOOKER
	PLA

	CMP #RDERRH;** 17/9/84 **
	BNE EXOPLA

	LDA ERMCNO + 1;net number
	BEQ EXOPL1
	JSR EXOPR1;write into the buffer
	LDA #'.'
	STA MIDTX,X;add separator
	INX

.EXOPL1
	LDA ERMCNO;station number
	JSR EXOPR1;append to end of message
	
	LDA #'('
	STA MIDTX,X
	LDA ERMCNO + 2
	BEQ EXOPLB
	INX
	JSR EXOPR1
	LDA #')'
	STA MIDTX,X
	INX
	
.EXOPLB
	LDA #CR
	STA MIDTX,X

.EXOPLA
	TXA ;Message length for REPLY
	SEC ;Set carry to add 1 for CR
	ADC #TXHDR;Add header length to message length
	JMP REPLY;Send and return

.EXOPR1
	STX COTEMP
	JSR MKDEC

	LDY #LO(-3)
	LDX COTEMP;restore X value
.EXOPR2
{
	LDA COWORK - LO(-3),Y;get results from MKDEC
	CMP #SPACE
	BEQ EXOPR3
	STA MIDTX,X;put text in buffer
	INX
.EXOPR3
	INY
	BNE EXOPR2
	RTS
}

.LOOKER
{
	;Look error in A up and put relevant message
	;at MIDTX.

	STA RTCODE
	PHA

	LDA #LO(ERRTAB)
	STA GENPTR
	LDA #HI(ERRTAB)
	STA GENPTR + 1
	LDY #0
	STY CCODE;Set command code to zero
.ERRLUP
	PLA
	CMP (GENPTR),Y;Check error number
	BEQ ERRFND;Found

	PHA
	LDA #CR
.INCWHY
	INY
	BNE EXTELA
	INC GENPTR + 1
.EXTELA
	CMP (GENPTR),Y;Move to end of this text
	BNE INCWHY

	INY
	BNE EXTONA
	INC GENPTR + 1
.EXTONA
	LDA (GENPTR),Y
	BNE ERRLUP;Non-zero => not end of table

	;If here, number not found, so construct
	;default error message.

	LDX #0
.EELP
	LDA ERRMSG,X;Load error text
	BMI EELPE;Terminated by NOP
	STA MIDTX,X;Store in transmit buffer
	INX
	BNE EELP

.EELPE
	PLA ;Get error number back
	JSR WRTERR;Put A in hex in message at X

	LDA #CR
	STA MIDTX,X;Terminate message
	RTS

.ERRFND
	LDX #&FF;Error found, move text to message
.ERRFLA
	INX
	INY
	BNE ERRFON
	INC GENPTR + 1
.ERRFON
	LDA (GENPTR),Y
	STA MIDTX,X
	CMP #CR
	BNE ERRFLA
	RTS ;Complete, exit X - length-1
}


.WRTERR
{
	PHA
	LSR A
	LSR A
	LSR A
	LSR A
	JSR WERR;TOP NYBBLE

	PLA
.WERR
	AND #&F
	CMP #&A
	BCC WEON
	ADC #6
.WEON
	ADC #&30
	STA MIDTX,X
	INX
	RTS
}

.ERRMSG
	EQUB "F.S. Error "
	NOP


.SENDBB;SENDBB

	;Send big block on port in A.
	;Size of message is in OUTBSZ,
	;assumes station already set.

.SENDBC

	LDY QPTR
	STA CBPORT,Y
	CLC
	LDA BBUF;Buffer address
	STA CBBUF,Y
	ADC OUTBSZ
	STA CBBUFE,Y
	LDA BBUF + 1
	STA CBBUF + 1,Y
	ADC OUTBSZ + 1
	STA CBBUFE + 1,Y

	JMP SEND;Send message  ...

.SENDIO
{
	LDY QPTR;routine to send the IO side buffer
	STA CBPORT,Y;in byte-size chunks ** 19/1/84 **

	CLC
	LDA IOBUF
	STA COTEMP
	ADC OUTBSZ
	STA OUTBSZ
	LDA IOBUF +1
	STA COTEMP +1
	ADC OUTBSZ +1
	STA OUTBSZ +1;COTEMP is low end of buffer

	LDA #&FF
	STA CBBUF+2,Y
	STA CBBUF+3,Y
	STA CBBUFE+2,Y
	STA CBBUFE+3,Y;initialise top bytes

.SENDLP
	LDY QPTR
	CLC

	LDA COTEMP
	STA CBBUF,Y
	ADC #LO(BUFSZ)
	STA CBBUFE,Y
	STA COTEMP

	LDA COTEMP+1
	STA CBBUF+1,Y
	ADC #HI(BUFSZ)
	STA CBBUFE+1,Y
	STA COTEMP+1;buffer pointers set, new COTEMP set

	LDA CBBUFE,Y
	CMP OUTBSZ
	LDA CBBUFE+1,Y
	SBC OUTBSZ+1
	BCC SENDL1;C=1 if cbbufe>= outbsz

	LDA OUTBSZ
	STA CBBUFE,Y
	LDA OUTBSZ+1
	STA CBBUFE+1,Y;setup last transmission

.SENDL1
	PHP
	JSR SEND;do transmit
	PLP ;restore carry
	TAX
	BNE SENDL2;bad transmit
	BCC SENDLP
.SENDL2
	RTS
}


.MSDELY
{
	CPY #0
	BEQ DELYEX;If no delay, Exit
	PHA ;Keep Acc.
	TXA
	PHA ;Keep X
	TYA ;Keep delay time (in msecs)
	PHA
	LDX #0
.DLPP
	DEX
	BNE DLPP;1msec loop
	DEY
	BNE DLPP;Outer loop
.DLPP1
	PLA
	TAY ;Reset delay time
	PLA
	TAX ;Reset X
	PLA ;Reset A
.DELYEX
	RTS
}


	;*** NON - NET  UTILITIES ***


.CPDNAM
{
	;Read disc name of disc number at ARGF on stack
	;into MIDTX offset by A.

	PHA ;Push MIDTX offset

	LDY #ARGF
	LDA (NEWARG),Y;GET DISC NO.
	PHA
	INY
	LDA (NEWARG),Y;SET ON STACK AT ARGB
	LDY #ARGC
	STA (NEWARG),Y
	DEY
	PLA
	STA (NEWARG),Y
	LDA #&B;MAPMAN ENTRY PT.
	JSR SETRTN
	JSR MAPMAN
	BNE CPDNMX

	LDY #ARGB
	LDA (NEWARG),Y
	STA GENPTR
	INY
	LDA (NEWARG),Y
	STA GENPTR + 1;SET PTR. TO DISC NAME

	PLA ; Offset from MIDTX
	TAX
	LDY #0
.CHDRLD
	LDA (GENPTR),Y
	STA MIDTX,X
	INX
	INY
	CPY #DNAMLN
	BNE CHDRLD

	;Shared with DIRINF
.CPDNMX
	RTS
}

.DIRIND
	STA COZERO
	JSR STKUSA;Stack user info at ARGA
	LDX #&C1
	BNE DIRIN1

	;Call DIRMAN.EXAMINE to get last component of
	;file title (may be CR) and disc number and cycle
	;number of directory.
	;On entry, A is offset from MIDTX to put dir. name
	;Dir. name arg. is assumed at MIDRX

.DIRINF
	STA COZERO
	JSR STKUSA;Stack user info at ARGA
	LDX #&C0
.DIRIN1
	LDA #HDRLEN
.DIRIN2;Entry point from CPINFO
	STX COZERO + 1;save parameter for DIRMAN
	JSR SETFTP;MOVE FILE TITLE PTR. TO STACK
	INY
	LDA COZERO;Load offset
	CLC
	ADC #LO(MIDTX)
	STA (NEWARG),Y;SET RESULT AREA FOR EXAMINE
	LDA #HI(MIDTX)
	ADC #0
	INY
	STA (NEWARG),Y
	INY
	LDA #4
	STA (NEWARG),Y;ARG. FOR EXAMINE => GET TITLE
	LDA #7
	JSR SETRTN

	LDY #ARGK
	LDA COZERO + 1;passed parameter
	STA (NEWARG),Y;wild card flag

	JMP DIRMAN;GET INFO. and return


.SINDSC;SINDSC
{
	;Copy SIN/Disc no. from DANDS to
	;stack. Y already set as stack ptr.

	LDX #0
.SDLP
	LDA DANDS,X
	STA (NEWARG),Y
	INY
	INX
	CPX #5
	BNE SDLP
	RTS
}

.IBLOCK;IBLOCK

	;Copy disc block information to stack, starting
	;at Y.

	;Info is:
	;1) Current disc block start (CURBLK)
	;2) No. of blocks to read/write (DIVPAR)
	;3) Address to read/write to/from (BBUF)

	LDA CURBLK
	STA (NEWARG),Y
	INY
	LDA CURBLK + 1
	STA (NEWARG),Y

	INY
	LDA DIVPAR
	STA (NEWARG),Y
	INY
	LDA DIVPAR + 1
	STA (NEWARG),Y

	INY
	LDA IOBUF
	STA (NEWARG),Y
	INY
	LDA IOBUF + 1
	STA (NEWARG),Y

	RTS




.SBUFPT

	LDA #0

	;Set pointer on stack pointing to
	;text buffer.
	;Also set 0 in byte after receive buffer
	;Offset in A on entry to SETTXP

.SETTXP

	INY
	CLC
	ADC #LO(TXTBUF)
	STA (NEWARG),Y
	LDA #HI(TXTBUF)
	INY
	ADC #0
	STA (NEWARG),Y
	LDX BPTR
	LDA #0
	STA RXBUFT,X
	RTS




.SETFTP

	;Sets file title pointer to stack
	;at Y.
	;On entry, A is offset of file title
	;in RX buffer.

	;This routine also puts 0 in the byte
	;after the receive buffer so that
	;analysis of the file title doesn't
	;get carried away.

	INY
	CLC
	ADC BPTR;Add file title offset to buffer offset
	ADC #LO(RXBUF)
	STA (NEWARG),Y
	LDA #0
	ADC #HI(RXBUF)
	INY
	STA (NEWARG),Y
	TXA
	PHA ;Store X just in case
	LDX BPTR
	LDA #0
	STA RXBUFT,X;RX buffer terminater
	PLA
	TAX ;Restore X
	RTS

.SAVNAM
{
	;Returns the last component of the pathname handed to save
	;in MIDTX [3:13]

	LDX BPTR
.SAVNMA
	INX
	LDA MIDRX + 11,X
	CMP #CR
	BNE SAVNMA;look for CR
.SAVNMB
	DEX
	LDA MIDRX + 11,X
	CMP #'.'
	BEQ SAVNMC
	CPX BPTR;maybe just one component here
	BNE SAVNMB
	DEX
.SAVNMC
	INX
	LDY #0
.SAVNMG
	LDA MIDRX + 11,X
	CMP #CR
	BEQ SAVNMD
	STA MIDTX + 3,Y
	INX
	INY
	BNE SAVNMG
.SAVNMD
	LDA #&20
.SAVNME
	CPY #12
	BCS SAVNMF
	STA MIDTX + 3,Y
	INY
	BNE SAVNME
.SAVNMF
	LDA #&80
	STA MIDTX + 3,Y
	RTS ;terminate with CR
}
	
.GETUSR
{
	JSR GETUSE
	BEQ GETUSZ
	JSR EXTERR
	LDA #&FF
	RTS

.*GETUSE;GETUSE

	;Call FINDMC using machine number
	;in net control block.

	;Then set CSD handle from RX block into
	;user table.


	LDY QPTR
	LDA CBSTID,Y
	STA MCNUMB
	LDA CBSTID + 1,Y
	STA MCNUMB + 1
	
	LDA CBFLG,Y 
	AND #&78
	STA MCNUMB + 2
	
	JSR FINDMC
	BNE GETUSZ

	LDX BPTR
	LDA CPCSD,X;GET CSD SENT FROM CLIENT
	LDY #UTHSLD
	STA (USTPTR),Y;SET CSD
	LDA #0;Indicate successful exit
	
.GETUSZ
	RTS
}


.SETUSE;SETUSE

	;Move user info. pointer in USTPTR
	;to stack.

	INY
	LDA USTPTR
	STA (NEWARG),Y
	LDA USTPTR + 1
	INY
	STA (NEWARG),Y
	RTS


.STKUSE;STKUSE

	;GET USER INFO AND PUT ON STACK AT ARGB
	JSR GETUSR
	BNE STKUEX
.STKUSA
	LDY #ARGA
	JSR SETUSE;NOTE SETUSE DOES INY FIRST, SO ARGA CORRECT
	LDA #0

.STKUEX
	RTS


.STKHND;STKHND

	;PUT THREE HANDLES FROM RX BUFFER
	;ON STACK AT Y.

	LDX BPTR
	LDA CPUFD,X
	STA (NEWARG),Y
	INY
	LDA CPCSD,X
	STA (NEWARG),Y
	INY
	LDA CPLIB,X
	STA (NEWARG),Y
	RTS


.SETRTN;SETRTN

	LDY #ARGA
	STA (NEWARG),Y
	RTS



.SCOWPT;SCOWPT

	;Set pointer to COWORK on stack

	INY
	LDA #LO(COWORK)
	STA (NEWARG),Y
	INY
	LDA #HI(COWORK)
	STA (NEWARG),Y
	RTS



.OBJCLR
{
	;SIN/disc no. of object are on
	;stack (usually just after a call
	;of PRESERVE).

	;assumes that GETUSE result is valid ** 3/10/84 **

	;If SIN <> 0 ...

	;1) Clear object from store.
	;2) Delete object from map
	;3) Ensure map.

	JSR SINZED;Check SIN is zero (also used in RENAME)

	BEQ OBJRTS;If zero, exit

	LDA #5;STRALL function
	JSR SETRTN
	JSR STRMAN;Flush from store
	BNE SETRTN

.OBJJES
	LDY #ARGF
	JSR SETUSE;pass pointer to user info ** 3/10/84 **

	LDA #MAPFS;Map free store
	JSR SETRTN
	JSR MAPMAN;Delete from map
	BNE OBJABT;No good => abort

	LDA #MAPENS;Map ensure fn.
	JSR SETRTN
	JSR MAPMAN;Ensure map
	BEQ OBJRTS;OK => exit

.OBJABT
	JSR INTERR;Not ok => stop

.OBJRTS
	RTS
}

.SINZED
	LDY #ARGD
	LDA (NEWARG),Y
	INY
	ORA (NEWARG),Y
	INY
	ORA (NEWARG),Y
	RTS


.STKLST

	;Gets entry point, no. of entries and a pointer
	;to BIGBFR +1 onto stack. Used by DISCS and USERS

	TYA
	TAX ;pointer into MIDRX
	LDY #ARGB
	LDA MIDRX,X
	STA (NEWARG),Y;Start point in list
	INY
	LDA MIDRX + 1,X
	STA (NEWARG),Y;Number of entries
	INY
	LDA BBUF
	CLC
	ADC #TXHDR + 1;Result area
	STA (NEWARG),Y
	INY
	LDA BBUF + 1
	ADC #0
	STA (NEWARG),Y
	RTS



.LSTRPY

	;Given no. of entries found and ptr. to
	;end of result area on stack, sends off the
	;result of DISC/USERS in big buffer.
	;On entry A = continue code.


	STA TEMPA
	LDY #ARGB
	LDA (NEWARG),Y
	PHA ;Number of entries found
	LDA BBUF
	STA GENPTR
	LDA BBUF + 1
	STA GENPTR + 1
	LDY #0
	LDA TEMPA;Continue code
	STA (GENPTR),Y
	TYA ;A:=0 = Return code
	INY
	STA (GENPTR),Y
	PLA
	INY
	STA (GENPTR),Y;Number of entries found

	;Set control block for reply

	LDA RPLYPT
	LDX QPTR
	STA CBPORT,X
	LDA BBUF
	STA CBBUF,X;Ptr. to message
	LDA BBUF + 1
	STA CBBUF + 1,X
	LDY #ARGC
	LDA (NEWARG),Y;End of result buffer
	STA CBBUFE,X
	INY
	LDA (NEWARG),Y
	STA CBBUFE + 1,X
	LDY QPTR;Arg to SEND
	JMP SEND;Send reply and exit



.LDRETR

	LDA #LODFTO

.CPRETR
{
	;Set stack and call DIRMAN retrieve
	;to put details into COWORK buffer.
	;On entry A = offset in RXCB of file name

	PHA
	LDA #DRRTR;Dirman retreive
	JSR SETUSB;PLACE ADDR OF USERINFO ONTO NEWARG STACK
	PLA ;Pull f.t. offset
	JSR SETFTP;Set file title pointer
	JSR SCOWPT;Set pointer to COWORK
	LDA #&C0;allow wild cards in LOAD
	LDY #ARGH
	STA (NEWARG),Y;wild card flag

	JMP DIRMAN;Get info. to COWORK and return
}



	;COMMAND LINE UTILS


.BUFTXT

	;Get string (possibly quoted) from
	;CLI string (at MIDRX) and transfer
	;to TXTBUF offset by X (normally zero)
	;NOTE - ACKS and NACKS ignored here !!

	LDX #0
.BTXTA
{
	LDA #0
	STA QUOTED;Quoted flag

	JSR SPACES
	CMP #'"'
	BNE GCHON
	DEC QUOTED;Quoted flag on
	DEX

.GCHLP
	INX
.GCHLPA
	INY
.GCHON
	LDA MIDRX,Y
.GCHFIX
	STA TXTBUF,X

	CMP #ACK
	BEQ GCHLPA;Inc. string ptr., but not buffer
	CMP #NACK
	BEQ GCHLPA

	CMP #CR
	BEQ GCHEND
	CMP #SPACE
	BNE GCHONA
	BIT QUOTED
	BMI GCHLP;Spaces OK in quoted string

.GCFIXA
	LDA #CR;Finish with CR as terminator in destn.
	BNE GCHFIX

.GCHONA
	CMP #'"'
	BNE GCHLP
	INY ;Step C.line ptr. past final delimiter
	INC QUOTED;Switch off quoted flag
	BEQ GCFIXA;Exit, with CR terminator

.GCHEND
	LDA QUOTED
	BEQ BTXTZ

	LDA #NAMERR
	PHP
	JSR EXTERR;Send error message
	PLP
	
.BTXTZ
	RTS ;Give Z unset exit
}

.SPACER
{
	DEY
.SPACLP
	INY
.*SPACES
	LDA MIDRX,Y
	CMP #SPACE
	BEQ SPACLP
	RTS
}

.RDTITL

	;Move file title into TXTBUF and
	;check to end of line for syntax.

	JSR BUFTXT
	BNE COERTS

.COMEND
	JSR SPACES
	CMP #CR
	BEQ COERTS
	LDA #SYNERR
	JSR EXTERR
	LDA #SYNERR;Give non-zero exit
.COERTS
	RTS

	;** E R R O R  T A B L E **

.ERRTAB
	EQUB URERRA,"Who are you?",CR
	EQUB WOTERR
	EQUB "Bad Command",CR
	EQUB NAMERR
	EQUB "Bad string",CR
	EQUB DRERRA
	EQUB "Bad file name",CR
	EQUB DRERRB
	EQUB "Broken dir",CR
	EQUB DRERRC
	EQUB "Not found",CR
	EQUB DRERRD
	EQUB "Not a directory",CR
	EQUB DRERRE
	EQUB "Insufficient access",CR
	EQUB DRERRG
	EQUB "Entry locked",CR
	EQUB ATERRB
	EQUB "User not known",CR
	EQUB ATERRC
	EQUB "Wrong password",CR
	EQUB ATERRE
	EQUB "Bad password",CR
	EQUB MPERRB
	EQUB "Disc full",CR
	EQUB MPERRA
	EQUB "Disc changed",CR
	EQUB SAERRA
	EQUB "Bad attribute",CR
	EQUB ATERRA
	EQUB "PW file not found",CR
	EQUB &54
	EQUB "Insert a Fileserver disc",CR
	;EQUB "INSERT A FILE SERVER DISC",CR
	EQUB RDERRB
	EQUB "Channel",CR
	EQUB RDERRJ
	EQUB "EOF",CR
	EQUB RDERRL
	EQUB "Outside file",CR
	EQUB RDERRH
	EQUB "Already open at station ",CR
	EQUB RDERRN	;RDERRH
	EQUB "File read only",CR
	EQUB DCERRE
	EQUB "Disc read only",CR
	EQUB DCERRF
	EQUB "Disc fault",CR
	EQUB MPERRL
	EQUB "Map fault",CR
	EQUB RDERRC
	EQUB "Too many open files",CR
	EQUB URERRB
	EQUB "Too many users",CR
	EQUB LODERA
	EQUB "Is a directory",CR
	EQUB ATERRD
	EQUB "Insufficient privilege",CR;** 13/4/83 **
	EQUB DRERRJ
	EQUB "Dir. not empty",CR
	EQUB DRERRM
	EQUB "Dir. full",CR
	EQUB ATERRF
	EQUB "Already a user",CR
	EQUB RNAMQQ
	EQUB "Bad Rename",CR
	EQUB DRERRK
	EQUB "Types don't match",CR
	EQUB URERRE
	EQUB "Not logged on",CR
	EQUB ATERRG
	EQUB "Bad user name",CR
	EQUB RDERRO;** 16/11/84
	EQUB "Write only",CR
	EQUB MPERRN
	EQUB "Insufficient space",CR
	EQUB &8C
	EQUB "Bad privilege letter", CR
	EQUB 0;Table terminator

.FNSH

LEFT =  FRESTR - FNSH;get free store
IF HI(LEFT) = 0 ;must be less than a page
ELSE
	PRINT "FRESTR incorrect"
ENDIF

	;** T H E  E N D ***
.END
;
