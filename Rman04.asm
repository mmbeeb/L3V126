;.OPT
;RMAN04;RNDMAN   >  Rman04
;.TTL
;Fileserver file RMAN04

.RMAN04

	;************ RDGBTS ************


	;RDGBTS: Get bytes from file

	;Entry: ARGA  = func code (9)
	;ARGB) = ptr to user info (LS)
	;ARGC) =  "   "  "    "   (MS)
	;ARGD  = file handle
	;ARGE) = buffer address (LS)
	;ARGF) =   "       "    (MS)
	;ARGG) = no. of bytes   (LS)
	;ARGH)   to be read     (MS)

	;Exit:  ARGA  = rc
	;ARGB) = no. of bytes actually read (LS)
	;ARGC)                              (MS)
	;ARGD  = flag: normally zero
	;$80 if this read includes
	;the last byte OR if it is
	;completely outside the file.

	;The read is done starting at the current SFP.
	;A call of RNDMAN.RDSUBO must be done at the start of each
	;external GETBYTES operation, followed by one or more calls
	;of RDGBTS.

	;*** N.B. Attempting a read partially or
	;completely outside the file is not an error
	;here.  Transfer completely outside file
	;will have been deletected in RDSUBO.

	;1) Get HANDTB entry from handle
	;2) Get RANDTB entry
	;3) BTSLFT := file size - SFP (bytes left)
	;4) EOFFLG := 0 (end of file flag)
	;5) IF transfer size < BTSLFT
	;THEN BTSLFT := transfer size
	;ELSE EOFFLG := $80 (end-of-file)
	;6) BTSXFD := BTSLFT (bytes transferred)
	;7) The transfer is done in 3 parts (any of which
	;may be null).
	;If the start address is not badly aligned on
	;a disc block boundary, then STRMAN is called to
	;get the required disc block into the cache.
	;(It may still be there from the last GETBYTES call).
	;The relevant bytes are copied into the buffer.
	;We are now aligned to a disc block boundary, so
	;can do a direct read by calling DSCMAN, provided
	;at least 256 bytes remain to be read.
	;The final partial block (if any) is read through
	;the cache.

.RDGBTS
{
	JSR INITRD;GENPTR := user info; MCNUMB := machine no.

	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDGBTZ;Not found - exit

	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDGBTZ;Not found (object not file)

	LDY #HTMODE
	LDA (HNDPTR),Y;check that file open for reading
	AND #READAC
	BNE RDGBJA
	LDA #RDERRO
	BNE RDGBTZ

.RDGBJA
	CLC ;CHECK CORRECT LIMIT
	JSR RDBLFT;BTSXFD := BTSLFT :=
				;min(transfer size, bytes left in file)
				;Also sets EOFFLG

	JSR RDSBFA;Set RDBFAD

	JSR RDGBT1;Read first partial disc block
	BNE RDGBTZ;Failed

	JSR RDGBT2;Read complete disc blocks in middle
	BNE RDGBTZ;Failed

	JSR RDGBT3;Read final partial disc block
	;BNE RDGBTZ;Failed

	;Reading worked OK

	;LDA #0

	;Exit: error code in A
	;Bytes transferred in BTSXFD

.RDGBTZ
	PHA ;Save error code

	LDY #ARGB;Return bytes tranferred
	LDA BTSXFD
	STA (ARGPTR),Y;LS byte
	INY
	LDA BTSXFD + 1
	STA (ARGPTR),Y;MS byte

	INY
	LDA EOFFLG;Return EOF flag
	STA (ARGPTR),Y

	PLA ;Restore error code
	JMP RDEXIT;Exit: RC in A
}


	;************ RDPBTS ************


	;RDPBTS: Put bytes to file

	;Entry: ARGA  = func code (10)
	;ARGB) = (LS) ptr to user info
	;ARGC) = (MS)
	;ARGD  = file handle
	;ARGE) = (LS) buffer address
	;ARGF) = (MS)
	;ARGG) = (LS) number of bytes to be written
	;ARGH) = (MS)

	;Exit:  ARGA  = RC

	;The write is done starting from the current SFP.
	;The file is extended if necessary.
	;A call of RNDMAN.RDSUBO must be done at the
	;start of each external PUTBYTES operation,
	;followed by one or more calls of RDPBTS.

	;1) Get HANDTB entry from handle
	;2) Get RANDTB entry
	;3) Check open for writing
	;4) (abolished)
	;5) BTSLFT := file size - SFP (bytes left)
	;6) IF transfer size > BTSLFT
	;THEN extend file
	;BTSLFT := transfer size
	;7) BTSXFD := BTSLFT (bytes transferred)
	;8) The write is done in 3 parts
	;(any of which may be null).
	;They are (a) via the cache,
	;(b) directly to disc, and
	;(c) via the cache, as for RDGBTS (q.v.).
	;9) Adjust file HWM

.RDPBTS
{
	JSR INITRD;GENPTR := user info; MCNUMB:=machine no.

	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDPBTJ;Not found - exit

	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDPBTJ;Not found - exit

	LDY #HTMODE;Check file open for update
	LDA (HNDPTR),Y;Get mode
	AND #WRITAC;Write access
	BNE RDPBTA;OK to write

	LDA #RDERRN;"Not open for update"
.RDPBTJ
	BNE RDPBTZ;Always jumps

.RDPBTA
	SEC ;CHECK CORRECT LIMIT
	JSR RDBLFT;BTSXFD := BTSLFT :=
				;min(transfer size, bytes left in file)

	LDA EOFFLG;Get end-of-file flag
	BEQ RDPBTB;Not end of file

	;Must call MAPMAN to extend file

	CLC
	LDY #RTCSFP;NEWFSZ := SFP + xfer size
				;rounded up modulo 256 (whole block)
	LDA (RTEPTR),Y;SFP LS byte
	LDY #ARGG
	ADC (ARGPTR),Y;xfer size (LS)
	STA NEWFSZ
	INY
	LDA (ARGPTR),Y
	LDY #RTCSFP + 1
	ADC (RTEPTR),Y
	STA NEWFSZ + 1;CS
	INY
	LDA (RTEPTR),Y
	ADC #0
	STA NEWFSZ + 2;MS

	CLC
	LDA #&FF;Add 255 to round up
	ADC NEWFSZ;Don't want LS byte
	LDA #4;Add extra 1K
	ADC NEWFSZ + 1
	STA NEWFSZ + 1
	BCC RDPBTK
	INC NEWFSZ + 2
	
.RDPBTK
	LDA #0
	STA NEWFSZ;Clear LS byte
	;ADC NEWFSZ + 2
	;STA NEWFSZ + 2

	JSR RDCHSZ;Extend file
	BNE RDPBTZ;Failed

.RDPBTB
	LDY #ARGG;BTSLFT := xfer size
	LDA (ARGPTR),Y
	STA BTSLFT
	INY
	LDA (ARGPTR),Y
	STA BTSLFT + 1

	JSR RDSBFA;Set RDBFAD

	JSR RDPBT1;Write 1st partial block
	BNE RDPBTZ;Failed

	JSR RDPBT2;Write complete disc blocks
	BNE RDPBTZ;Failed

	JSR RDPBT3;Write final partial block
	BNE RDPBTZ;Failed

	;Writing worked OK

	JSR RDSHWM;Adjust file HWM
	LDA #0;=> successful return
.RDPBTZ
	JMP RDEXIT;EXIT: RC IN A
}

	;*** RDBLFT ***

	;Initialize BTSLFT, BTSXFD & EOFFLG
	;for PUTBYTES/GETBYTES.
	;Carry is used as entry condition ** 24/1/84 **

.RDBLFT
{
	LDA #0;EOFFLG := 0
	STA EOFFLG
	LDY #RTFSZE;BTSLFT := filesize - SFP
	BCS RDBLF1;C=1 -> use filesize, use HWM
	LDY #RTHWM
	SEC

.RDBLF1
	LDA (RTEPTR),Y;** 24/1/84 **
	INY
	STY RNDTMP
	LDY #RTCSFP
	SBC (RTEPTR),Y
	STA BTSLFT;LS byte
	LDY RNDTMP;** 24/1/84 **
	LDA (RTEPTR),Y
	LDY #RTCSFP + 1
	SBC (RTEPTR),Y
	STA BTSLFT + 1;CS Byte
	INC RNDTMP;** 24/1/84 **
	LDY RNDTMP;next byte
	LDA (RTEPTR),Y
	LDY #RTCSFP + 2
	SBC (RTEPTR),Y;MS byte left in A

	BCS RDBLFA;Size >= SFP

	;If filesize < SFP set BTSLFT to 0

	LDA #0;BTSLFT := 0
	STA BTSLFT
	STA BTSLFT + 1

	;Test xfer size > bytes left
	;> if A non-zero (MS byte)
	;or if xfer size > BTSLFT

.RDBLFA
	BNE RDBLFE;A not 0

	SEC ;BTSLFT - xfer size
	LDA BTSLFT
	LDY #ARGG
	SBC (ARGPTR),Y
	STA RNDTMP;Store LS byte
	LDA BTSLFT + 1
	INY
	SBC (ARGPTR),Y
	ORA RNDTMP;for zero test
	BEQ RDBLFB;BTSLFT = xfer size
	BCC RDBLFB;BTSLFT < xfer size

	;BTSLFT > xfer size: transfer OK
	;Set BTSLFT := xfer size

.RDBLFE
	LDY #ARGG
	LDA (ARGPTR),Y
	STA BTSLFT
	INY
	LDA (ARGPTR),Y
	STA BTSLFT + 1

	JMP RDBLFC

	;BTSLFT < xfer size: transfer too long
	;or outside file
	;Set EOFFLG to $80

.RDBLFB
	LDA #&80
	STA EOFFLG

	;BTSLFT now says how many bytes can be
	;transferred: set BTSXFD to this.

.RDBLFC
	LDA BTSLFT
	STA BTSXFD
	LDA BTSLFT + 1
	STA BTSXFD + 1

	RTS
}

	;*** RDSBFA ***

	;Set RDBFAD from call stack

.RDSBFA
	LDY #ARGE
	LDA (ARGPTR),Y
	STA RDBFAD
	INY
	LDA (ARGPTR),Y
	STA RDBFAD + 1
	RTS


	;*** RDGBT1 ***

	;Read the first partial block of a GETBYTES
	;transfer, taking the SFP to a disc block
	;boundary.

.RDGBT1
{
	JSR RDSZE1;Get no. of bytes in A
	CMP #0
	BEQ RDGB1X;Nothing to do

	STA RNDTMP;Save no. of bytes

	JSR RDGBLK;Get required block in cache
	BNE RDGB1Z;Failed

	;Copy from cache buffer to user buffer

	LDA RDBFAD;MOVETO := RDBFAD
	STA MOVTO
	LDA RDBFAD + 1
	STA MOVTO + 1

	LDY #RTCSFP;Offset in cache blk
	LDA (RTEPTR),Y;given by LS byte of SFP
	CLC
	ADC GENPTR
	STA MOVFRM
	LDA GENPTR + 1
	ADC #0
	STA MOVFRM + 1

	LDX RNDTMP;No. of bytes to copy
	JSR MOVE;Copy
	JSR RDADJ1;Adjust BTSLFT, RDBFAD, SFP

.RDGB1X
	LDA #0;Empty partial block
.RDGB1Z
	RTS
}

	;*** RDPBT1 ***

	;Write the first partial block of a PUTBYTES
	;transfer, taking the SFP to a disc block
	;boundary.

.RDPBT1
{
	JSR RDSZE1;Get no. of bytes in A
	CMP #0
	BEQ RDPB1X;Nothing to do

	STA RNDTMP;Save no. of bytes

	JSR RDGBLK;Get required block in cache
	BNE RDPB1Z;Failed

	;Copy from user buffer to cache buffer

	LDA RDBFAD;MOVFRM := RDBFAD
	STA MOVFRM
	LDA RDBFAD + 1
	STA MOVFRM + 1

	LDY #RTCSFP;Offset in cache blk
	LDA (RTEPTR),Y;given by LS byte of SFP
	CLC
	ADC GENPTR
	STA MOVTO
	LDA GENPTR + 1
	ADC #0
	STA MOVTO + 1

	LDX RNDTMP;No. of bytes to copy
	JSR MOVE;Copy
	JSR MRKDRT;Mark cache buffer dirty
	JSR RDADJ1;Adjust BTSLFT, RDBFAD, SFP
	
.RDPB1X
	LDA #0;Empty partial block
.RDPB1Z
	RTS
}

	;*** RDSZE1 ***

	;Return size of 1st partial block in A.
	;size1 := (0 - LS byte of SFP) mod 256
	;size  := min(size1, BTSLFT)

.RDSZE1
{
	LDA #0
	SEC
	LDY #RTCSFP
	SBC (RTEPTR),Y;0 - LS(SFP)

	;A := min(A, BTSLFT)

	LDX BTSLFT + 1;Is BTSLFT > 256
	BNE RDSZEZ

	CMP BTSLFT
	BCC RDSZEZ;A < BTSLFT

	LDA BTSLFT;A > BTSLFT
.RDSZEZ
	RTS
}

	;*** RDADJ1 ***

	;Adjust BTSLFT, RDBFAD, SFP by RNDTMP

.RDADJ1
{
	SEC ;BTSLFT -:= RNDTMP
	LDA BTSLFT
	SBC RNDTMP
	STA BTSLFT
	BCS RDAJ1K
	DEC BTSLFT + 1
.RDAJ1K
	CLC ;RDBFAD +:= RNDTMP
	LDA RDBFAD
	ADC RNDTMP
	STA RDBFAD
	BCC RDADJ3
	INC RDBFAD + 1
}

	;RDADJ3 entry adjusts SFP only

.RDADJ3
	CLC ;SFP +:= RNDTMP
	LDY #RTCSFP
	LDA (RTEPTR),Y
	ADC RNDTMP
	STA (RTEPTR),Y
	INY
	LDA (RTEPTR),Y
	ADC #0
	STA (RTEPTR),Y
	INY
	LDA (RTEPTR),Y
	ADC #0
	STA (RTEPTR),Y

	RTS


	;*** RDGBT2 *** and *** RDPBT2 ***

	;Do middle part of GETBYTES or PUTBYTES transfer
	;i.e. that part consisting of whole disc blocks.

	;The code is identical apart from the disc action.

.RDGBT2
	LDA #1;DSCMAN.READ
	BNE RDBT2A;Always jumps

.RDPBT2
	LDA #2;DSCMAN.WRITE - PUTBYTES entry

.RDBT2A
{
	STA RNDTMP;Store disc action for later
	;Determine how many whole blocks there
	;are to be transferred.
	;*** Assumes blocksize = 256 bytes ***

	LDA BTSLFT + 1;Blocks = ms byte of bytes left
	BEQ RDBT2X;Nothing to do

	;Must flush all blocks of object from
	;cache before doing direct transfer.
	JSR RDFALL;Leaves DISC & SIN on NEWARG stack
	BNE RDBT2Z;Failed

	;Call DSCMAN to READ/WRITE directly from/to
	;caller's buffer.

	;Start block no. is given by MS
	;two bytes of SFP.

	LDY #RTCSFP +  1
	LDA (RTEPTR),Y;SFP (CS)
	LDY #ARGG
	STA (NEWARG),Y;Block no. (LS)
	LDY #RTCSFP + 2
	LDA (RTEPTR),Y;SFP (MS)
	LDY #ARGH
	STA (NEWARG),Y;Block no. (MS)

	;Number of blocks is given by MS
	;byte of BTSLFT.

	LDA BTSLFT + 1
	INY
	STA (NEWARG),Y
	LDA #0;MS byte of block count
	INY ;always zero (in 64K machine)
	STA (NEWARG),Y

	LDA RDBFAD;Put buffer address on stack
	INY
	STA (NEWARG),Y
	LDA RDBFAD + 1
	INY
	STA (NEWARG),Y

	;Operation code was placed in RNDTMP
	;on entry to this routine.

	LDA RNDTMP
	JSR SETRTN
	JSR DSCMAN;*** DSCMAN.READ or DSCMAN.WRITE **
	BNE RDBT2Z;Failed

	;Must adjust SFP in RANDTB, RDBFAD, and BTSLFT
	;*** Assumes blocksize 256 bytes ***

	LDA BTSLFT + 1;No. of blocks
	LDY #RTCSFP + 1;Add to CS byte of SFP
	CLC
	ADC (RTEPTR),Y
	STA (RTEPTR),Y;Put back
	INY
	LDA #0
	ADC (RTEPTR),Y;Carry to MS byte of SFP
	STA (RTEPTR),Y;Put back

	;Adjust MS byte of RDBFAD by no. of blocks

	LDA BTSLFT + 1;No. of blocks
	ADC RDBFAD + 1;MS byte of buffer addr
	STA RDBFAD + 1;Put back

	;Only a partial block remains, so MS
	;byte of BTSLFT is now zero

	LDA #0
	STA BTSLFT + 1

	;JMP RDBT2X;Exit
.RDBT2X
	LDA #0;RC
.RDBT2Z
	RTS ;No whole blocks in transfer
}

	;*** RDGBT3 ***

	;Read the last partial block of a GETBYTES
	;transfer

.RDGBT3
{
	LDA BTSLFT;Get no. of bytes in A
	BEQ RDGB3X;Nothing to do

	JSR RDGBLK;Get required block in cache
	BNE RDGB3Z;Failed

	;Copy from cache buffer to user buffer

	LDA RDBFAD;MOVTO := RDBFAD
	STA MOVTO
	LDA RDBFAD + 1
	STA MOVTO + 1

	LDA GENPTR;MOVFRM := GENPTR
	STA MOVFRM
	LDA GENPTR + 1
	STA MOVFRM + 1

	LDX BTSLFT;No. of bytes to copy
	STX RNDTMP;Store for RDADJ3 below
	JSR MOVE;Copy
	JSR RDADJ3;Adjust SFP

.RDGB3X
	LDA #0;Empty partial block
.RDGB3Z
	RTS
}

	;*** RDPBT3 ***

	;Write the last partial block of a PUTBYTES
	;transfer

.RDPBT3
{
	LDA BTSLFT;Get no. of bytes in A
	BEQ RDPB3X;Nothing to do

	JSR RDGBLK;Get required block in cache
	BNE RDPB3Z;Failed

	;Copy from user buffer to cache buffer

	LDA RDBFAD;MOVFRM := RDBFAD
	STA MOVFRM
	LDA RDBFAD + 1
	STA MOVFRM + 1

	LDA GENPTR;MOVTO := GENPTR
	STA MOVTO
	LDA GENPTR + 1
	STA MOVTO + 1

	LDX BTSLFT;No. of bytes to copy
	STX RNDTMP;For RDADJ3 call below
	JSR MOVE;Copy
	JSR MRKDRT;Mark cache buffer dirty
	JSR RDADJ3;Adjust SFP
	
.RDPB3X
	LDA #0;Empty partial block
.RDPB3Z
	RTS
}

	;********* RDEOF *****************

	;RDEOF: read "end of file" condition

	;Entry: ARGA = fn. code (#0F)
	;ARGB = ptr. to user info (LS)
	;ARGC = ptr. to user info (MS)
	;ARGD = handle

	;Exit : ARGA = RC
	;ARGB = zero if SFP <= HWM
	;#FF  if SFP => HWM

.RDEOF
{
	JSR INITRD
	LDY #ARGD
	LDA (ARGPTR),Y
	JSR FNDHND;Check handle/machine pair
	BNE RDEOFZ;Error -> exit
	JSR SETRPT;Set Random table ptr. (& check is file)
	BNE RDEOFZ;Not a file -> exit
	JSR CMPHWP;Compare HWM with PTR
	BEQ RDEOFL;Ptr = HWM -> exit w/$FF
	
	LDA #0
	BCS RDEOFX;Ptr < HWM -> exit w/zero

.RDEOFL
	LDA #&FF

.RDEOFX
	LDY #ARGB
	STA (ARGPTR),Y;Set result
	LDA #0;Set return code
.RDEOFZ
	JMP RDEXIT;Return
}

;.LNK
;RMAN05
