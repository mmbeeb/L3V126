;.OPT
;UADE19;File > Uade19
;.TTL
;Fileserver file UADE19

.UADE19

	;**********
	;* GETBYT *
	;**********

	;Read single byte from file.
	;All work done by RNDMAN.

	;Entry: BPTR is offset of buffer in RXBUF
	;QPTR is offset of control block in RXCBV

.GETBYT
{
	JSR STKUSE;Put user info ptr on stack
	BNE GTBYTZ

	LDX BPTR
	LDA RXBUF + 2,X;Get file handle
	LDY #ARGD
	STA (NEWARG),Y

	LDX QPTR;Get seq no. from control block
	LDA RXCBV,X
	AND #1;Mask out reception flag
	INY
	STA (NEWARG),Y

	LDA #7;RNDMAN.RDCH
	JSR SETRTN

	JSR RNDMAN;*** Call RNDMAN.RDCH **
	BNE GTBYTX;Failed

	LDY #ARGB
	LDA (NEWARG),Y;Get byte read
	STA MIDTX;Put in TX buffer

	INY
	LDA (NEWARG),Y;Get EOF flag
	STA MIDTX + 1;Put in TX buffer
	LDA #TXHDR + 2;Message length

	;Note that sequence no. is still in control block

	JSR REPLYC;Send reply

.GTBYTZ
	JMP COMRTS;exit

.GTBYTX
	JMP ERROR
}



	;**********
	;* PUTBYT *
	;**********

	;Write single byte to file.
	;All work done by RNDMAN.

	;Entry: BPTR is offset of buffer in RXBUF
	;QPTR is offset of control block in RXCBV

.PUTBYT
{
	JSR STKUSE;Put user info ptr on stack
	BNE PTBYTZ

	LDX BPTR
	LDA RXBUF + 2,X;Get file handle
	LDY #ARGD
	STA (NEWARG),Y

	LDX QPTR;Get seq no. from control block
	LDA RXCBV,X
	AND #1;Mask out reception flag
	INY
	STA (NEWARG),Y

	LDX BPTR
	LDA RXBUF + 3,X;Get byte to be written
	INY
	STA (NEWARG),Y;Put on call stack

	LDA #8;RNDMAN.WRCH
	JSR SETRTN
	JSR RNDMAN;*** Call RNDMAN.WRCH **

	;Note that sequence no. is still in control block

	JSR RCODE;Send return code

.PTBYTZ
	JMP COMRTS;exit
}


	;* CPRDAR *

	;Read specified part of file info

	;Most work done in RNDMAN.

.CPRDAR
{
	JSR GETUSR;USTPTR := USERTB entry
	BNE CPRDAX
	
	JSR SETUSR;Copy user info to NEWARG stack
	LDX BPTR;Offset of request buffer
	LDA MIDRX,X;Get handle
	LDY #ARGD
	STA (NEWARG),Y;Put handle on NEWARG stack

	LDA #&B;RNDMAN.RDAR
	JSR SETRTN
	JSR RNDMAN;*** Call RNDMAN.RDAR **
	BNE CPRDAB;Not OK, send error code and exit

	;Have now got sequential file ptr, file hwm,
	;and file size on NEWARG stack.
	;The one to be sent back is determined by
	;arg in request block.
	;Arg is 0,1,2 resp. to get the above.

	;Set Y to appropriate offset on NEWARG stack

	LDX BPTR;Offset of request buffer
	LDA MIDRX + 1,X;Arg from request
	BNE CPRDAC;Not 0

	LDY #ARGB;Offset of file ptr
	BNE CPRDAF;Always jumps

.CPRDAC
	CMP #1;Test arg
	BNE CPRDAD;Not 0 or 1

	LDY #ARGE;Offset of file HWM
	BNE CPRDAF;Always jumps

.CPRDAD
	CMP #2;Test arg
	BNE CPRDAE;Not 0, 2, or 1

	LDY #ARGH;Offset of file size
	BNE CPRDAF;Always jumps

	;Invalid arg in request

.CPRDAE
	LDA #ARGERR;"Bad arg to RDAR"
	
.CPRDAB
	JMP ERROR

	;Y now contains the offset on NEWARG
	;of the required 3 bytes.

.CPRDAF
	LDA (NEWARG),Y;LS byte
	STA MIDTX;Put in TX buffer
	INY
	LDA (NEWARG),Y;CS byte
	STA MIDTX + 1
	INY
	LDA (NEWARG),Y;MS byte
	STA MIDTX + 2
	LDA #TXHDR + 3;Mesage length
	JSR REPLYC;Send reply

.*CPRDAX
	JMP COMRTS;Exit
}


	;* CPSTAR *

	;Set sequential file pointer

.CPSTAR
{
	JSR GETUSR;USTPTR := ptr to user info
	BNE CPSTAX

	JSR SETUSR;Copy user info to stack
	LDX BPTR;Offset of request buffer
	LDA MIDRX,X;Get handle
	LDY #ARGD
	STA (NEWARG),Y;Put handle on stack

	;Put arg saying what to set on stack

	LDA MIDRX + 1,X
	INY
	STA (NEWARG),Y

	;Put value to be set on stack

	LDA MIDRX + 2,X;value (LS)
	INY
	STA (NEWARG),Y
	LDA MIDRX + 3,X;value (CS)
	INY
	STA (NEWARG),Y
	LDA MIDRX + 4,X;value (MS)
	INY
	STA (NEWARG),Y
	LDA MIDRX + 5,X;Some vals have 4 bytes
	INY
	STA (NEWARG),Y

	LDA #&C;RNDMAN.STAR
	JSR SETRTN
	JSR RNDMAN;*** Call RNDMAN.STAR **
	JSR RCODE;Transmit reply

.CPSTAX
	JMP COMRTS;Exit
}


	;************
	;* PUTBYTES *
	;************

	;Write several bytes to specified file offset.

	;1) Set up pointer to user info
	;2) (abolished)
	;3) Send first reply, giving data port
	;and max. blocksize.
	;4) LOOP: receive block into big buffer
	;5) LOOP: write to disc (RNDMAN.PUTBYTES)
	;unless disc error flag set
	;6) LOOP: If disc error, then set flag but
	;continue to receive.
	;7) LOOP: If more to receive, adjust OFFSET
	;and loop to 4).
	;8) Send final RC (disc error flag)

	;Entry: BPTR is offset of buffer in RXBUF
	;QPTR is offset of control block in RXCBV

.PBYTES
{
	JSR GETUSR;USTPTR := ptr to user info
	BNE PBYTEK;not logged on ** 6/9/84 **

	JSR RMSUBO;Prepare for some PUTBYTES calls
	BEQ PBYTEJ

.PBYTEC
	JSR EXTERR;Not OK, send RC
.PBYTEK
	JMP COMRTS

	;Send first reply, giving data port and max blocksize

.PBYTEJ
	LDA #PSAVD;Use same data port as SAVE
	STA MIDTX

	LDA #LO(BUFSZ);Big buffer size (2 bytes)
	STA MIDTX + 1
	LDA #HI(BUFSZ)
	STA MIDTX + 2

	LDA #0;RTCODE := CCODE := 0
	STA RTCODE
	STA CCODE

	LDA #TXHDR + 3;Message length

	JSR REPLY;Send message
	BNE PBYTEK

	;Ready to receive data, so set up for
	;reception loop.

	LDA #0;DATAIN := 0 (3 bytes)
	STA DATAIN
	STA DATAIN + 1
	STA DATAIN + 2
	STA DSCERR;Clear disc err flag
	STA FINFLG;Clear end-of-transfer flag

	;Test specially for transfer of zero bytes,
	;to prevent 64K net control block being
	;set up! Jump straight to final reply.

	LDX BPTR
	LDA MIDRX + 2,X;Ls byte of size
	ORA MIDRX + 3,X;CS
	ORA MIDRX + 4,X;MS
	BNE PBLOOP;Not zero bytes
	JMP PBYTED;Jump straight to final reply

.PBLOOP
	;Loop which receives data and writes it to disc

	;Set buffer pointers in RX control block

	LDY QPTR;Buffer start = BBUF
	LDA BBUF
	STA CBBUF,Y
	LDA BBUF + 1
	STA CBBUF + 1,Y

	LDA BBEND;Buffer end = BBEND
	STA CBBUFE,Y
	LDA BBEND + 1
	STA CBBUFE + 1,Y

	LDA #PSAVD;Set data port
	STA CBPORT,Y

	;Station already set from first reception

	JSR WAIT;Wait for reception
	BNE PBYTEK;No contact => abort

	;Now subtract buffer limits to see how
	;much data was received.

	LDY QPTR
	SEC ;COWORK := CBBUFE-CBBUF
	LDA CBBUFE,Y
	SBC CBBUF,Y
	STA COWORK;LS byte
	LDA CBBUFE + 1,Y
	SBC CBBUF + 1,Y
	STA COWORK + 1;MS byte

	;Add amount received to total number of bytes
	;received.

	CLC ;DATAIN +:= COWORK
	LDA COWORK;LS byte
	ADC DATAIN
	STA DATAIN
	LDA COWORK + 1;CS byte
	ADC DATAIN + 1
	STA DATAIN + 1
	BCC PBLOOA
	INC DATAIN + 2
.PBLOOA

	;Compare total amount received (in DATAIN)
	;with total number of bytes to be written.
	;(in original message buffer).
	;Set FINFLG if exactly the right amount
	;has been received.
	;If too much has been received, then send
	;a return code.

	LDX BPTR
	SEC
	LDA MIDRX + 2,X;LS byte of size
	SBC DATAIN
	STA BREGA;Use BREGA as workspace
	LDA MIDRX + 3,X;CS byte of size
	SBC DATAIN + 1
	STA BREGA + 1
	LDA MIDRX + 4,X;MS byte
	SBC DATAIN + 2

	;Carry set if received <= size

	ORA BREGA;OR 3 bytes of result
	ORA BREGA + 1;for zero test
	BEQ PBLOOC;Equal => last block
	BCS PBLOOD;Recd < size => carry on

	;Client sent too much data.
	;Send error code and stop.

	LDA #PBERRA;Error code
	JMP PBYTEC;Send RC and exit

.PBLOOC
	LDA #&FF;Last block; set FINFLG
	STA FINFLG

	;If disc error flag set, ignore received data,
	;and jump round disc write.

.PBLOOD
	LDA DSCERR;Disc error flag
	BNE PBLOOE;Jump if error

	;Set up for call of RNDMAN

	LDX BPTR
	LDA MIDRX,X;Get file handle
	LDY #ARGD
	STA (NEWARG),Y;Put on call stack

	LDA BBUF;Buffer addr (Big Buffer)
	INY
	STA (NEWARG),Y
	LDA BBUF + 1
	INY
	STA (NEWARG),Y

	LDA COWORK;No. of bytes (calculated above)
	INY
	STA (NEWARG),Y;LS byte
	LDA COWORK + 1
	INY
	STA (NEWARG),Y;MS byte

	LDA #&A;RNDMAN.PUTBYTES
	JSR SETUSB;Stack user info

	JSR RNDMAN;*** RNDMAN.PUTBYTES **
	BEQ PBLOOE;OK, continue
	STA DSCERR;Store error code

	;If FINFLG is set, then we have just written
	;the last block.
	;Otherwise, adjust the file offset and carry on.

.PBLOOE
	LDA FINFLG;Finished?
	BNE PBYTED;Exit from loop if yes

	;Send ack to client's ack port

	LDY BPTR
	LDX CPUFD,Y;Ack port
	LDA #1;Message length

	JSR REPLYB;Send byte (random contents)
	BNE PBYTEZ;Contact lost => abort
	JMP PBLOOP;Round loop again

.PBYTED
	;Have received all the data.
	;Final RC is in DSCERR.
	;If RC is zero, send back amount xferred (always amount requested
	;if no error). If non-zero, send error.

	LDA DSCERR
	STA RTCODE
	BNE PBYTEX;RC <> 0 => error
	STA CCODE;Set command code = 0
	STA MIDTX+4;send 32 bit number
	LDX BPTR
	LDA MIDRX + 2,X
	STA MIDTX + 1
	LDA MIDRX + 3,X
	STA MIDTX + 2
	LDA MIDRX + 4,X;Move length of data to MIDTX
	STA MIDTX + 3
	LDA #TXHDR + 5
	JSR REPLYC;Send reply (note MIDTX undefined)

.PBYTEZ
	JMP COMRTS;Exit from command

.PBYTEX
	JSR RCODE;Send error reply
	JMP PBYTEZ
}


	;************
	;* GETBYTES *
	;************

	;Read several bytes from specified file offset.

	;1) Set up pointer to user info
	;2) Save size of transfer.
	;SEND first RC to client
	;3) LOOP: read chunk into big buffer (RNDMAN.GETBYTES)
	;unless disc error flag set
	;4) LOOP: If disc error, then set flag but
	;continue to send data.
	;5) LOOP: send to client
	;6) LOOP: If more to send, adjust OFFSET
	;and loop to 3).
	;7) Send final RC (disc error flag)

	;Entry: BPTR is offset of buffer in RXBUF
	;QPTR is offset of control block in RXCBV

.GBYTES
{
	LDA #0
	STA GBBXFD;GBBXFD := 0
	STA GBBXFD + 1
	STA GBBXFD + 2
	STA GBEFLG;GBEFLG := 0

	JSR GETUSR;USTPTR := ptr to user info
	BNE GBYTEC;OK, continue ** 6/9/84 **

	JSR RMSUBO;Set up RNDMAN for GETBYTES calls
	BEQ GBYTEF

	JSR EXTERR;Not OK, send RC
.GBYTEC
	JMP COMRTS

.GBYTEF
	JSR RCODE;Send "OK" rc
	BNE GBYTEC;lost contect so ABORT

	;TOSEND := size of transfer
	LDX BPTR
	LDA MIDRX + 2,X;Size (LS)
	STA TOSEND
	LDA MIDRX + 3,X;Size (CS)
	STA TOSEND + 1
	LDA MIDRX + 4,X;Size (MS)
	STA TOSEND + 2

	;Use the big buffer.
	;Read from disc chunks of size BUFSZ, and transmit
	;them to the client.
	;Note that, in general, each chunk will be badly
	;aligned with respect to disc blocks. RNDMAN tries
	;to be efficient about this for each chunk.
	;It may be worth adding extra optimization here if
	;very large (> BUFSZ) transfers are common.

	LDA #0
	STA DSCERR;No disc error yet
	STA FINFLG;Not finished yet

	;Test specially for transfer of zero
	;bytes to prevent 64K net control
	;block being set up!

	LDA TOSEND;Size (LS)
	ORA TOSEND + 1;CS
	ORA TOSEND + 2;MS
	BNE GBLOOP;OK - not zero bytes
	JMP GBYTEB;Jump straight to final reply


	;Loop, sending data to client in blocks of
	;size BUFSZ.  TOSEND is the amount left to
	;send, OFFSET the current file position.
	;If an error occurs, the error code is put
	;in DSCERR and the loop continues, padding
	;out the transmission until the right
	;amount of data has been sent.
	;FINFLG gets set on the last time round
	;the loop.

.GBLOOP
	SEC ;COWORK := TOSEND - BUFSZ
	LDA TOSEND
	SBC #LO(BUFSZ)
	STA COWORK
	LDA TOSEND + 1
	SBC #HI(BUFSZ)
	STA COWORK + 1
	LDA TOSEND + 2
	SBC #0;BUFSZ only 2 bytes
	STA COWORK + 2

	ORA COWORK;OR 3 bytes for zero test
	ORA COWORK + 1
	BEQ GBLOOA;TOSEND = BUFSZ
	BCS GBLOOB;TOSEND < BUFSZ

	;BUFSZ >= TOSEND: send remaining data
	;and set finish flag.

.GBLOOA
	LDA TOSEND;OUTBSZ := TOSEND
	STA OUTBSZ
	LDA TOSEND + 1
	STA OUTBSZ + 1
	DEC FINFLG;Set loop finish flag to $FF
	BNE GBLOOC

.GBLOOB
	LDA #LO(BUFSZ);OUTBSZ := BUFSZ
	STA OUTBSZ
	LDA #HI(BUFSZ)
	STA OUTBSZ + 1

.GBLOOC
	;The size of the block to send is in OUTBSZ.
	;Call RNDMAN.GETBYTES to get the data into
	;the big buffer.

	LDX BPTR
	LDA MIDRX,X;Get file handle
	LDY #ARGD
	STA (NEWARG),Y;Put handle on stack

	LDA BBUF;Put buffer address on stack
	INY
	STA (NEWARG),Y
	LDA BBUF + 1
	INY
	STA (NEWARG),Y

	LDA OUTBSZ;Put no. of bytes on stack
	INY
	STA (NEWARG),Y
	LDA OUTBSZ + 1
	INY
	STA (NEWARG),Y

	LDA #9;RNDMAN.GETBYTES
	JSR SETUSB
	JSR RNDMAN;*** Call RNDMAN.GETBYTES **
	BEQ GBLOOD;OK, continue
	STA DSCERR;Otherwise set error code

.GBLOOD

	;Add number of bytes actually read to
	;GBBXFD, and OR end-of-file flag
	;into GBEFLG.

	CLC
	LDY #ARGB
	LDA (NEWARG),Y;Bytes xferred (LS)
	ADC GBBXFD
	STA GBBXFD;LS total
	INY
	LDA (NEWARG),Y
	ADC GBBXFD + 1
	STA GBBXFD + 1;CS total
	BCC GBLOOJ
	INC GBBXFD + 2;MS total
.GBLOOJ
	INY
	LDA (NEWARG),Y;EOF flag
	ORA GBEFLG;OR with flag so far
	STA GBEFLG

	LDY BPTR
	LDA CPUFD,Y;Get client's data port
	JSR SENDBC;Send big block (size in OUTBSZ)
	BNE GBYTEZ

	;End of loop. Test finish flag, and set
	;TOSEND from COWORK.

	BIT FINFLG;Test finish flag
	BMI GBYTEB;Exit from loop if finished

	LDA COWORK;TOSEND := COWORK
	STA TOSEND
	LDA COWORK + 1
	STA TOSEND + 1
	LDA COWORK + 2
	STA TOSEND + 2

	JMP GBLOOP;Loop return

	;Exit from GBLOOP: RC in DSCERR

.GBYTEB
	LDA DSCERR
	BNE GBYTEE;Jump if error

	;Send back end-of-file flag and
	;count of bytes actually read.

	LDA GBEFLG;EOF flag
	STA MIDTX;Put in TX buffer
	LDA GBBXFD;Bytes xferred (LS)
	STA MIDTX + 1
	LDA GBBXFD + 1
	STA MIDTX + 2;CS
	LDA GBBXFD + 2
	STA MIDTX + 3;MS
	LDA #0
	STA MIDTX+4;send 32 bit number ** 26/2/85 **
	LDA #TXHDR + 5;Message length
	JMP DATOUT;Send reply

.GBYTEE
	JSR RCODE;Send return code

.GBYTEZ
	JMP COMRTS;Exit from command
}


	;*** Call RNDMAN to set up bytes operation

.RMSUBO
{
	LDA #&E;**** RNDMAN.SUBO ***
	JSR SETUSB
	LDX BPTR
	LDA MIDRX + 0,X;Get file handle
	INY
	STA (NEWARG),Y

	LDX QPTR;Get seq no from control block
	LDA RXCBV,X
	AND #1;Get just seq bit
	INY
	STA (NEWARG),Y

	;The flag, no of bytes, and offset
	;are in the correct order in rx block

	LDX BPTR
.RMSUBA
	LDA MIDRX + 1,X
	INY
	STA (NEWARG),Y
	INX
	CPY #ARGL
	BNE RMSUBA;More to copy

	JMP RNDMAN;**** Call RNDMAN.SUBO & return ***
}

.CPWHO
{
	;ROUT;** 19/3/85 **

	;Yet another interface - this one allows an application
	;to determine the userid under which it is logged-on

	;This is probably most useful for MAILing type progs

	JSR GETUSR
	BNE Y20;nb this will deal with the error

	LDX #0
	LDY #UTUSID
.Y10
	LDA (USTPTR),Y;copy the user name
	STA MIDTX,X
	INY
	INX
	CMP #CR;copy only upto CR (pw follows - dont want that)
	BNE Y10

	TXA
	JMP DATOT2
.Y20
	JMP COMRTS
}

;.LNK
;UADE20
