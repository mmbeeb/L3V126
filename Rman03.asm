;.OPT
;RMAN03;RNDMAN   >  Rman03
;.TTL
;Fileserver file RMAN03

.RMAN03

	;*** RDCHNT ***

	;Checks the cache descriptor hint in a
	;RANDTB entry.

	;Entry: HNDPTR points to HANDTB entry
	;RTEPTR points to RANDTB entry

	;Exit:  A = RC: zero    => hint used, GENPTR set
	;non-zero=> must call STRMAN instead
	;GENPTR points to cache buffer (iff RC=0)

.RDCHNT
{
	LDY #RTDESC;GENPTR := Cache descriptor hint
	LDA (RTEPTR),Y;from RANDTB entry
	STA GENPTR
	INY
	LDA (RTEPTR),Y
	STA GENPTR + 1

	;Hint is invalid if it is zero (as it
	;is when file first opened)

	ORA GENPTR;OR both bytes of hint
	BEQ RDCHNZ;Hint no good (zero)

	;Must now check that the hint still points
	;to the cache descriptor we want. (If it
	;is not zero, it will always point to
	;SOME cache descriptor).
	;The descriptor is valid only if it has the
	;right disc number and SIN, and if its start
	;block is the one we want.

	;*** Assumptions:
	;***  1) Blocksize = 256 bytes (=> easy
	;***     calculation of block number
	;***  2) Cache buffers contain only one
	;***     disc block

	;Compare SINs first (more likely to
	;be different than disc number).

	LDY #HTSIN
	LDA (HNDPTR),Y
	LDY #CESIN
	CMP (GENPTR),Y;Compare LS bytes
	BNE RDCHNZ;Different
	LDY #HTSIN + 1
	LDA (HNDPTR),Y
	LDY #CESIN + 1
	CMP (GENPTR),Y;Compare CS bytes
	BNE RDCHNZ;Different
	LDY #HTSIN + 2
	LDA (HNDPTR),Y
	LDY #CESIN + 2
	CMP (GENPTR),Y;Compare MS bytes
	BNE RDCHNZ;Different

	;SIN OK - check disc no.

	LDY #HTDISC;Compar disc nos.
	LDA (HNDPTR),Y
	LDY #CEDISC
	CMP (GENPTR),Y;Compare LS bytes
	BNE RDCHNZ;Different - hint no good
	LDY #HTDISC + 1
	LDA (HNDPTR),Y
	LDY #CEDISC + 1
	CMP (GENPTR),Y;Compare MS bytes
	BNE RDCHNZ;Different

	;Disc no. OK - Check block no.
	;(should be equal to top 2 bytes of
	;sequential file pointer).

	LDY #CEBKNO
	LDA (GENPTR),Y;LS byte of block no.
	LDY #RTCSFP + 1
	CMP (RTEPTR),Y;Comp with CS byte of SFP
	BNE RDCHNZ;Different
	LDY #CEBKNO + 1
	LDA (GENPTR),Y;MS byte of block no.
	LDY #RTCSFP + 2
	CMP (RTEPTR),Y;Comp with MS byte of SFP
	BNE RDCHNZ;Different

	;The hint is OK.
	;Copy buffer address to GENPTR (which
	;currently points to cache descriptor).

	LDY #CESTA
	LDA (GENPTR),Y;LS byte of buff addr
	PHA ;Save for now
	INY
	LDA (GENPTR),Y;MS byte of buff addr
	STA GENPTR + 1;GENPTR := buff addr
	PLA
	STA GENPTR;LS byte

	LDA #0;Success - hint used
	RTS

.RDCHNZ
	LDA #&FF;Hint no good
	RTS
}


	;************ RDCLAF ************


	;RDCLAF: Close all files for the calling machine

	;Entry:  ARGB = Ptr to user info (LS)
	;ARGC = Ptr to user info (MS)

	;Exit:   ARGA = Return code


.RDCLAF
{
	JSR INITRD;Set GENPTR & MCNUMB & RDUPTR (extra copy of user ptr.)
	JSR INITHD;Set HNDPTR and X

.RDCLFA
	LDY #HTMCNO
	LDA (HNDPTR),Y;Get mc no. from HANDTB entry
	CMP MCNUMB;Same as client machine?
	BNE RDCLFB;LS bytes differ
	INY
	LDA (HNDPTR),Y;CS byte
	CMP MCNUMB + 1
	BNE RDCLFB;CS bytes differ
	INY
	LDA (HNDPTR),Y;MS byte
	CMP MCNUMB + 2
	BNE RDCLFB;MS bytes differ

	;Entry is for this machine.
	;See if the object is a file.

	LDY #HTACC
	LDA (HNDPTR),Y;Object type & access
	AND #TYPE
	CMP #TYPDIR;Is it a directory?		;MM NOT NECESSARY
	BEQ RDCLFB;Jump if yes

	;Entry is for a file for this mc.

	LDA RDUPTR
	STA GENPTR
	LDA RDUPTR + 1
	STA GENPTR + 1;Set pointer to user info (corrupted over CLRHTE)

	TXA ;Save X	;;MM WHY?
	PHA
	JSR CLRHTE;Close the file

	TAX
	PLA ;tidy stack
	TXA ;return error code
	BNE RDCLFX;**** 17/3/83 ****

.RDCLFB
	JSR INCHND;Inc pointer
	BNE RDCLFA;Look at next entry

	LDA #0;Return code
.RDCLFX
	JMP RDEXIT;Return
}

	;************ RDSUBO ************

	;RDSUBO: Set up for PUTBYTES/GETBYTES operation

	;Entry:  ARGB = LS ptr to user info
	;ARGC = MS  "   "   "   "
	;ARGD = file handle
	;ARGE = Seq. no. received
	;ARGF = Flag: 0 => use given offset
	;NOT 0 => use SFP in RANDTB
	;ARGG = LS total no. of bytes
	;ARGH = CS   "    "   "   "
	;ARGI = MS   "    "   "   "
	;ARGJ = LS Offset in file
	;ARGK = CS (only if flag=0)
	;ARGL = MS   "
	;
	; Side effect:
	;    OLDSZE is set to old HWM
	;    OLDSZE+3 is zero if area of disc has
	;    to be zeroed on a write
;
.RDSUBO
{
	LDA #1
	STA OLDSZE +3
	JSR INITRD
	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDSUBZ;Not found - exit

	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDSUBZ;Not found

	JSR RDCSEQ;Check seq no and set SFP accordingly

	LDY #ARGF
	LDA (ARGPTR),Y;A := flag
	BNE RDSUBA;Offset not supplied

	;Offset supplied : set SFP to offset

	LDY #RTHWM;GET CURRENT HWM
	LDA (RTEPTR),Y
	STA OLDSZE
	INY
	LDA (RTEPTR),Y
	STA OLDSZE + 1
	INY
	LDA (RTEPTR),Y
	STA OLDSZE + 2
	LDA #0;ZERO FLAG
	STA OLDSZE + 3
	LDY #ARGJ;CHECK IF NEW SIZE LARGER THAN OLD
	LDA OLDSZE
	CMP (ARGPTR),Y
	INY
	LDA OLDSZE + 1
	SBC (ARGPTR),Y
	INY
	LDA OLDSZE + 2
	SBC (ARGPTR),Y
	BCC RDSSI
	INC OLDSZE + 3
.RDSSI
	LDY #ARGJ
	LDA (ARGPTR),Y;Offset (LS)
	LDY #RTCSFP
	STA (RTEPTR),Y;LS byte
	LDY #ARGK
	LDA (ARGPTR),Y
	LDY #RTCSFP + 1
	STA (RTEPTR),Y;CS byte
	LDY #ARGL
	LDA (ARGPTR),Y
	LDY #RTCSFP + 2
	STA (RTEPTR),Y;MS byte

.RDSUBA
	JSR RDCKSP;Check SFP inside file
	BEQ RDSUBZ

	PHA
	JSR RDBACK;restore SFP to old SFP
	PLA	

	;LDA #0;rc
.RDSUBZ
	JMP RDEXIT
}

;.LNK
;RMAN04
;
