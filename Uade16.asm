;.OPT
;UADE16; > Uade16
;.TTL
;Fileserver file UADE16

.UADE16

	;SAVE/LOAD/ ETC.

	;*************
	;*  S A V E  *
	;*************

	;1) Do FINDMC, get disc number from
	;DIRMAN file title -> disc number

	;2) Call CREATESPACE in MAPMAN using above args
	;and size given in operation request
	;for create goto 7
	;3) Send first reply
	;4) Receive blocks one at a time into big buffer
	;5) Write to disc (DSCMAN)
	;6) If disc error, continue to receive,
	;but set error flag
	;7) All received OK => call PRESERVE in DIRMAN
	;8) Send final return code
	;9) Possibly DELETE previous file from map,
	;flush from store and ensure map.

.SAVE; Also enterd here for a CREATE 11/6/84 BC
{
	JSR STKUSE;Set m/c no. and call FINDMC
	BNE SAVRT2

	LDA #SAVFTO
	JSR SETFTP
	LDA #&A;File title -> disc number
	JSR SETRTN

	LDY #UTHUFD
	LDA (USTPTR),Y
	STA UMHUFD

	LDY #ARGF
	LDA #&80;no wild cards in last name
	STA (NEWARG),Y

	JSR DIRMAN;Disc  number on stack
	BNE SAVERR

	LDX BPTR
	LDY #ARGD
.SAVLPA;SAVLPA

	;Move size from message buffer to
	;arg. stack for create space call

	LDA MIDRX + 8,X
	STA (NEWARG),Y
	INX
	INY
	CPY #ARGD + 3
	BNE SAVLPA

	LDA #1
	JSR SETRTN

	LDY #ARGF;pass info @ ARGG,H
	JSR SETUSE;put userinfo on stack

	JSR MAPMAN;** CREATE SPACE **
	BNE SAVERR

	LDX #4
	LDY #ARGF
.SAVLPB
	LDA (NEWARG),Y
	STA DANDS,X;Save disc. no. and SIN
	DEY
	DEX
	BPL SAVLPB

	; New stuff for create 11/6/84 BC
	LDX BPTR
	LDA FCODE,X
	EOR #FCCRET; The function code for create
	BNE SAVSAV
	
	LDA #0	;MM A ALREADY ZERO!
	STA CCODE; Blat the command return code
	JMP SAVOND

.SAVRT2
	JMP COMRTS
.SAVERR
	JMP ERROR
	
.SAVSAV
	LDA #PSAVD
	STA MIDTX;Save data port

	LDA #LO(BUFSZ);** 6/2/84 **
	STA MIDTX + 1;Size of save buffer
	LDA #HI(BUFSZ);** 6/2/84 **
	STA MIDTX + 2;Size of save buffer hi

	LDA #0
	STA RTCODE
	LDA #3;offset in buffer of name
	STA CCODE

	JSR SAVNAM;get filename
	TYA ;Y=length of filename
	CLC
	ADC #TXHDR+ 7

	JSR REPLY;Send message with 0 RC and C.code
	BNE SAVAB2

	;If we get here, ready to receive
	;data, so set up for receive loop.

	LDA #0
	STA DATAIN
	STA DATAIN + 1
	STA DATAIN + 2;DATAIN -> no. of bytes received
	STA DSCERR;Error flag
	LDX BPTR
	LDA MIDRX + 8,X;Check if zero length file
	ORA MIDRX + 9,X
	ORA MIDRX +&A,X
	BNE SAVONS;Nope -> carry on
	JMP SAVOK;Yep -> send final reply

.SAVONS
	LDA #0
	LDY #ARGG
	STA (NEWARG),Y;Current disc block start
	INY
	STA (NEWARG),Y;Current disc block start (hi)

.SALOOP;SALOOP

	;Set buffer pointers in control block

	LDY QPTR
	LDA IOBUF
	STA CBBUF,Y
	LDA IOBUF + 1;Buffer (hi)
	STA CBBUF + 1,Y

	LDA IOEND
	STA CBBUFE,Y;Buffer end (lo)
	LDA IOEND + 1
	STA CBBUFE + 1,Y;Buffer end (hi)

	LDA #&FF
	STA CBBUF + 2,Y
	STA CBBUF + 3,Y
	STA CBBUFE + 2,Y
	STA CBBUFE + 3,Y;high order addresses

	LDA #PSAVD;Set data port
	STA CBPORT,Y

	JSR WAIT;Station set
	BEQ SAVONQ;Ok, continue
	
.SAVAB2
	JMP SAVABT;No contact => abort

.SAVONQ
	;Now subtract buffer received limits to
	;get how much data was received.

	LDY QPTR;** Y corrupted in WAIT **
	SEC
	LDA CBBUFE,Y
	SBC CBBUF,Y
	STA DIVPAR

	LDA CBBUFE + 1,Y
	SBC CBBUF + 1,Y
	STA DIVPAR + 1

	LDA #0
	STA DIVPAR + 2;Top byte always 0

	;Add no. received to total no. of
	;bytes received.

	CLC
	LDA DIVPAR
	ADC DATAIN
	STA DATAIN
	LDA DIVPAR + 1
	ADC DATAIN + 1
	STA DATAIN + 1
	LDA DIVPAR + 2;For superstitions sake
	ADC DATAIN + 2
	STA DATAIN + 2

	LDA DSCERR

	;If error code set, ignore received
	;data and jump past disc write.

	BNE SAVONB

	JSR DIVIDE;Get no. of disc blocks to write (in BREGA)

	LDY #ARGI
	LDA DIVPAR
	STA (NEWARG),Y
	INY
	LDA DIVPAR + 1
	STA (NEWARG),Y;No. of blocks to write
	INY
	LDA IOBUF
	STA (NEWARG),Y;Address of data to write
	INY
	LDA IOBUF + 1
	STA (NEWARG),Y;As above (hi)

	LDY #ARGB
	JSR SINDSC;Set SIN and disc number

	LDA #4;write from IO side
	JSR SETRTN
	JSR DSCMAN
	BEQ SAVONB
	STA DSCERR

	;If error, store error code

.SAVONB;SAVONB

	;Now compare amount received (in DATAIN)
	;with file size (in receive message buffer).

	LDY BPTR
	SEC
	LDA MIDRX + 8,Y
	SBC DATAIN
	STA BREGA
	LDA MIDRX + 9,Y
	SBC DATAIN + 1
	STA BREGA + 1
	LDA MIDRX +&A,Y
	SBC DATAIN + 2

	ORA BREGA
	ORA BREGA + 1
	BEQ SAVOK;If equal, SAVE finished...
	BCS SAVONC;If rx'd < size, carry on

	;Otherwise, client has sent too
	;much data, so send error code and
	;abort.

	LDA #SAVERA;Error code
	JMP SAVER1

.SAVONC;SAVONC

	;Add no. of blocks saved to block
	;start to get block start for next
	;reception of data.

	LDY #ARGI
	LDA (NEWARG),Y
	DEY
	DEY
	CLC
	ADC (NEWARG),Y
	STA (NEWARG),Y
	INY
	LDA (NEWARG),Y
	INY
	INY
	ADC (NEWARG),Y
	DEY
	DEY
	STA (NEWARG),Y

	;Now send ack. on ack. port sent from client

	LDY BPTR
	LDX CPUFD,Y;Ack. port
	LDA #1;Message length
	JSR REPLYB
	BNE SAVABT
	JMP SALOOP;Go off round loop again.

.SAVOK

	;At this point, have received the
	;correct amount of data, so check
	;error flag to see if all data was
	;written, and if not, send the
	;error off as a return code.

	LDA DSCERR
	BNE SAVER1
.SAVOND
	LDY #ARGO;SIN GOES IN ARGQ->ARGS (!)

	;Now prepare to do PRESERVE to
	;keep the data on disc.

	JSR SINDSC;Get SIN/DISC to stack
	JSR STKUSA

	LDA #SAVFTO
	JSR SETFTP

	;Move load/exec addresses from
	;receive message buffer to stack

	LDX BPTR
	LDY #ARGF
.SAVLPC
	LDA MIDRX,X
	STA (NEWARG),Y
	INX
	INY
	CPY #ARGF + 8
	BNE SAVLPC

	LDA #ACCDEF;Access default
	STA MIDTX;Store for final reply
	STA (NEWARG),Y

	LDY #ARGO
	LDA DATE
	STA MIDTX + 1;Store for final reply
	STA (NEWARG),Y
	INY
	LDA DATE + 1
	STA MIDTX + 2;Store for final reply
	STA (NEWARG),Y

	LDA #DRPRS
	JSR SETRTN

	LDY #ARGT
	LDA #&80
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;*** DO PRESERVE **
	BNE SAVER1

	LDY #ARGG;** 4/8/83 **
	LDA (NEWARG),Y;access status
	STA MIDTX
	LDA #TXHDR + 3;Otherwise reply with access + date
	JSR REPLYC

	JSR OBJCLR;Delete object if neccessary (see PRESERVE)
	BNE SAVER1;**** 17/3/83 ****

.SAVRTS
	JMP COMRTS;Finished  ... exit


.SAVER1
	JSR EXTERR;Send error to client

.SAVABT

	;Abort exit ... clear entry from
	;map of created space using FREESTORE,
	;and exit.

	LDY #ARGB
	JSR SINDSC;Get disc/sin to stack
	LDA #2
	JSR SETRTN

	LDY #ARGF;pass pointer to user info (ARGG,H)
	JSR SETUSE;** 3/10/84 **

	JSR MAPMAN;** Free space **
	BEQ SAVRTS
	JSR INTERR
}


	;*************
	;*  L O A D  *
	;*************


	;1) RETRIEVE object info. (DIR. MAN)
	;check access and type.

	;2) Get handle of file, and open for
	;reading, just in case.

	;3) REPLY
	;4) Loop round DSCMAN doing getdata from disc
	;into Big Buffer, and sending to client.
	;If disc error, set error flag but continue
	;to pad out data until loop end.
	;5) Return code reply.


.LOAD
{
	JSR GETUSR;Get user info. using FINDMC
	BNE LODEX2
	
	LDY #UTHUFD
	LDA (USTPTR),Y
	STA UMHUFD

	JSR LDRETR;Do retrieve
	BEQ LODONA;OK, continue

	LDX BPTR
	LDY FCODE,X
	CPY #FCCMND;Do a command ??
	BNE LODONQ;Nope, send not found error.

	LDA CPLIB,X;Get lib. handle
	LDY #UTHSLD
	STA (USTPTR),Y;Note, assumes USTPTR still OK from STKUSE.

	JSR LDRETR;Do another retrieve.
	BEQ LODONA;OK, continue
	CMP #DRERRC
	BEQ LODONX;"not found"
	CMP #DRERRA;Is bad file title ?
	BNE LODONQ;Nope, carry on
.LODONX
	LDA #WOTERR
.LODONQ
	JMP ERROR
.LODEX2
	JMP COMRTS


.LODONA
	LDY #ARGB
	LDA #TYPDIR;Directory type
	AND (NEWARG),Y;0 => is file
	BEQ LODONB;OK, carry on

	LDA #LODERA;Not ok, external error
	BNE LODONQ

.LODONB
	LDA #READAC
	AND (NEWARG),Y
	BNE LODONC;Read access => OK

	LDA #LODERB;Not ok, external error
	BNE LODONQ

.LODONC
	LDA DETDIS
	STA DANDS;Store Disc. no.
	LDA DETDIS + 1
	STA DANDS + 1

	LDX #2
.LODLPC
	LDA DETSIN,X;Store SIN
	STA DANDS + 2,X
	DEX
	BPL LODLPC

	LDA #2;arg to RNDMAN.INFO
	JSR SETRTN
	INY
	JSR SINDSC;put disc number & SIN on stack
	JSR RNDMAN
	BNE LODONJ;opject not open at all
	LDY #ARGB;test mode of access to object
	LDA (NEWARG),Y
	AND #&02;test bit 1 (write bit)
	BEQ LODONJ
	LDA #DRERRI;object in use
	BNE LODONQ

.LODONJ

	;Move load/exec info. from COWORK to transmit buffer
	;Note CPYLXS copies date and access as well

	LDX #0;Offset from MIDTX
	JSR CPYLXS;Note corrupts COTEMP

	LDX #10
.LODLKB
	LDA COWORK,X
	STA MIDTX + 14,X
	DEX
	BPL LODLKB

	LDA #&20
	STA MIDTX + 24
	STA MIDTX + 25
	STX MIDTX + 26;negative byte here

	LDX #2
.LODLPA
	LDA DETSZ,X;Move file size to TOSEND variable
	STA TOSEND,X
	DEX
	BPL LODLPA

	LDA #0
	STA RTCODE
	LDA #14;offset of name string in buffer
	STA CCODE

	LDA #TXHDR + 28;Message length (load/exec/size/acc/date)
	JSR REPLY;Send reply
	BEQ LODOND
	JMP LOADEX;Reply failed

.LODOND
	LDA #0
	STA DSCERR;Set disc error flag
	STA FINFLG;Set finish flag
	STA CURBLK
	STA CURBLK + 1
	
.LOLOOP
	;Loop round sending data to client
	;in blocks of size BBSIZE. TOSEND
	;is the amount left to send, decremented
	;by BBSIZE each loop.
	;If a disc error occurs, the disc
	;error number is put in DSCERR and
	;the loop continues, padding out the
	;data until the right amount has
	;been sent. The disc error number
	;is then sent as a return code.

	;First, subtract BBSIZE from TOSEND

	SEC
	LDA TOSEND
	SBC IOBSIZ
	STA COWORK
	LDA TOSEND + 1
	SBC IOBSIZ + 1
	STA COWORK + 1
	LDA TOSEND + 2

	;Note BBSIZE is TWO bytes !!

	SBC #0
	STA COWORK + 2

	ORA COWORK
	ORA COWORK + 1

	;If result +ve TOSEND > BBSIZE, so send
	;a block of size BBSIZE.

	BEQ LODONW
	BCS LODONE

	;If BBSIZE >= TOSEND, send remaining
	;data (in TOSEND) and set flag.

.LODONW
	LDA TOSEND
	STA OUTBSZ
	LDA TOSEND + 1
	STA OUTBSZ + 1
	DEC FINFLG;Set loop finish flag=#FF
	ORA TOSEND
	BEQ LODLPE;Zero length file => send final reply only
	BNE LODONF

.LODONE
	LDA IOBSIZ
	STA OUTBSZ
	LDA IOBSIZ + 1
	STA OUTBSZ + 1

.LODONF;LODONF

	;The size of the block to send (in
	;bytes) is now in OUTBSZ. So divide
	;by disc block size to get number
	;of disc blocks to transfer.

	LDA OUTBSZ
	STA DIVPAR
	LDA OUTBSZ + 1
	STA DIVPAR + 1
	LDA #0
	STA DIVPAR + 2;3 byte arg.
	JSR DIVIDE
	LDY #ARGB
	JSR SINDSC;Set disc/SIN on stack
	JSR IBLOCK;Set disc block parms on stack
	LDA #3;Read disc function
	JSR SETRTN
	JSR DSCMAN;*** DO DISC XFER TO BIG BUFFER **
	BEQ LODONG;OK => continue
	STA DSCERR;Otherwise set disc error

.LODONG
	LDY BPTR
	LDA CPUFD,Y;Get data port sent from client

	JSR SENDIO;Send big block
	BNE LOADEX;Contact lost with client, so give up.

	;At end of loop, test finish flag,
	;and set CURBLK to be start disc
	;block for next iteration.
	;Also set TOSEND from COWORK as decremented counter

	BIT FINFLG
	BMI LODLPE;#FF => exit

	CLC
	LDA CURBLK
	ADC DIVPAR;Add no. of blocks xfered
	STA CURBLK
	LDA CURBLK + 1
	ADC DIVPAR + 1
	STA CURBLK + 1

	LDA COWORK
	STA TOSEND
	LDA COWORK + 1
	STA TOSEND + 1
	LDA COWORK + 2
	STA TOSEND + 2

	JMP LOLOOP;*** LOOP RETURN **

.LODLPE
	LDA DSCERR
	JSR RCODE;Send return code

.LOADEX;LOADEX

	;No cleaning up to do, so exit

	JMP COMRTS
}


	;***************
	;* D E L E T E *
	;***************


.DELETE
{
	;1) CALL DELETE IN DIRMAN, WHICH SUPPLIES
	;SIN AND DISC NO. OF OBJECT TO BE DELETED
	;AFTER REMOVING IT FROM THE APPROPRIATE
	;DIRECTORY.

	;2) CALL UTILITY OBJCLR TO REMOVE OBJECT FROM
	;STORE, FROM THE MAP, AND ENSURE THE MAP.


	STY COTEMP;Store command line pointer
	JSR STKUSE
	BNE DELEX

	LDY COTEMP
	JSR RDTITL
	BNE DELEX

	LDY #ARGC
	JSR SBUFPT
	LDA #3
	JSR SETRTN
	LDY #ARGG
	LDA #&80
	STA (NEWARG),Y;specify full last name

	JSR DIRMAN;*** DIRECTORY DELETE **
	BNE DELER;IF ERROR IN DIRMAN, EXIT

	JSR OBJCLR;CLEAR OLD OBJECT

.DELER
	JSR RCODE;**** 17/3/83 ****

.DELEX
	JMP COMRTS
}


	;**********************
	;* C A T  H E A D E R *
	;**********************

.CATHDR
{
	;1) DIRMAN CALL RETRIEVE TO GET DIR. NAME
	;AND DISC. NO.
	;2) MAPMAN CALL TO GET DISC NAME FOR GIVEN NO.
	;3) FORMAT ABOVE WITH INDICATION OF WHETHER
	;OWNER OR PUBLIC ACCESS TO DIR. (ALSO
	;GOT FROM EXAMINE CALL)

	JSR GETUSR
	BNE CHDREX

	LDA #0;Offset from MIDTX to put dir. title
	JSR DIRIND;Get dir. title/access/disc no.
	BNE CHDRAB

	LDA #SPACE
	STA MIDTX +NAMLNT
	LDY #ARGD
	LDA (NEWARG),Y
	AND #OWNER
	BEQ CHDRLB
	LDA #'O'
	BNE CHDRLC
.CHDRLB
	LDA #'P';PUBLIC OR OWNER ACCESS
.CHDRLC
	STA MIDTX +&B
	LDA #SPACE
	STA MIDTX +&C
	STA MIDTX +&D
	STA MIDTX +&E

	LDA #&F;Offset from MIDTX to put disc name
	JSR CPDNAM;Read disc name of current disc
	BNE CHDRAB

	LDA #CR
	STA MIDTX,X;Note assumes X is offset remaining from CPDNAM
	LDA #EXTERM;BLOCK DELIM.
	STA MIDTX + 1,X

	TXA
	CLC
	ADC #TXHDR + 2;GET MESSAGE LENGTH
	JSR REPLYC
.*CHDREX
	JMP COMRTS
.*CHDRAB
	JMP ERROR
}


.EXAMIN
{
	;Mainly concerned with getting args
	;from RXBUF to NEWARG, calling DIRMAN,
	;and getting args back again.

	JSR STKUSE;CALL FINDMC AND SET USTPTR ON STACK
	BNE CHDREX

	LDA #EXAFTO
	JSR SETFTP;SET FILE TITLE POINTER ON STACK


	INY
	CLC
	LDA BBUF
	ADC #TXHDR + 2
	STA (NEWARG),Y;SET POINTER TO RESULT AREA

	;RESULT OF CALL OF DIRMAN IS PLACED IN THE BIG
	;BUFFER, OFFSET BY TWO BYTES FOR THE MESSGE HEADER
	;TO BE INSERTED

	INY
	LDA BBUF + 1
	ADC #0
	STA (NEWARG),Y

	;NOW COPY 3 ARGS FROM RXBUF TO STACK

	LDX BPTR;OFFSET IN RXBUF FOR THIS RX BUFFER
	INY
	LDA MIDRX,X
	STA (NEWARG),Y
	INY
	LDA MIDRX + 1,X
	STA (NEWARG),Y
	INY
	LDA MIDRX + 2,X
	STA (NEWARG),Y

	LDA #7;FUNCTION NUMBER IN DIRMAN
	JSR SETRTN

	LDY #ARGK
	LDA #&C1;specify directory here
	STA (NEWARG),Y;pass parameter

	JSR DIRMAN;*** DIRECTORY MANAGER CALL **
	BNE CHDRAB

	LDA BBUF
	STA GENPTR
	LDA BBUF + 1
	STA GENPTR + 1
	LDY #0
	TYA ;A:=0
	STA (GENPTR),Y;STORE 0 RETURN CODE IN MESSAGE
	INY
	STA (GENPTR),Y;STORE 0 COMMAND CODE

	LDY #ARGD
	LDA (NEWARG),Y;GET NUMBER OF ENTRIES RETURNED
	PHA ;Store for later
	INY
	LDA (NEWARG),Y
	LDY #TXHDR + 1;Store cycle no. of directory
	STA (GENPTR),Y
	PLA ;Restore no. of entries returned
	DEY
	STA (GENPTR),Y;Set in message to client


	;NOW SET UP CONTROL BLOCK

	LDA RPLYPT;REPLY PORT
	LDX QPTR;C.B POINTER
	STA CBPORT,X

	LDA BBUF;POINTER TO MESSAGE START
	STA CBBUF,X
	LDA BBUF + 1
	STA CBBUF + 1,X

	LDY #ARGB
	LDA (NEWARG),Y;POINTER TO END OF MESSAGE,
					;RETURNED BY DIR. MAN.

	STA CBBUFE,X
	INY
	LDA (NEWARG),Y
	STA CBBUFE + 1,X

	LDY QPTR;SET ARG FOR SEND
	JSR SEND;*** SEND INFO **
	JMP COMRTS
}


;***********
;* I N F O *
;***********


.INFO;INFO
{
	;GET INFO ON A FILE IN A CHAR STRING
	;FROM DIRMAN INTO BIG BUFFER AND SEND

	JSR RDTITL
	BNE INFEX
	JSR STKUSE
	BNE INFEX

	JSR SBUFPT
	INY
	CLC
	LDA BBUF
	ADC #TXHDR
	STA (NEWARG),Y

	;RESULT POINTER PASSED TO DIRMAN IS BBUF +A BIT
	;TO ALLOW ROOM FOR THE MESSAGE HEADER
	;IN THE FINAL MESSAGE BACK TO THE CLIENT.

	INY
	LDA BBUF + 1
	ADC #0;RESULT AREA TOP BYTE
	STA (NEWARG),Y

	LDA #8
	JSR SETRTN

	LDY #ARGH
	LDA #&C0
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;*** GET THE INFO **
	BEQ INFONB;OK, CONTINUE

	JMP ERROR

.INFONB
	LDA BBUF
	STA GENPTR
	LDA BBUF + 1
	STA GENPTR + 1

	LDY #0
	LDA #CCINF
	STA (GENPTR),Y;Command code
	INY
	LDA #0;Zero r.code
	STA (GENPTR),Y

	;FROM HERE, SET UP TRANSMIT CONTROL BLOCK

	LDX QPTR
	LDA RPLYPT
	STA CBPORT,X

	LDA BBUF
	STA CBBUF,X;MESSAGE BUFFER
	LDA BBUF + 1
	STA CBBUF + 1,X

	LDY #ARGB;GET MESSAGE BUFFER END FROM STACK
	LDA (NEWARG),Y
	STA CBBUFE,X
	INY
	LDA (NEWARG),Y
	STA CBBUFE + 1,X

	LDY QPTR
	JSR SEND;OFF WE GO ....

.INFEX
	JMP COMRTS
}
;.LNK
;UADE17
;
