;.OPT
;RMAN05;RNDMAN > Rman05
;.TTL
;Fileserver file RMAN05

.RMAN05

	;** RNDMAN UTILS **

	;*** GETRND ***

	;Get free slot in RANDTB.

	;Exit: A = RC
	;RTEPTR = Ptr to free RANDTB entry

.GETRND
{
	LDA RANDTB;RTEPTR := RANDTB
	STA RTEPTR
	LDA RANDTB + 1
	STA RTEPTR + 1

	LDX RTNENT;X := no. of entries in RANDTB
	LDY #RTINUS;Y := offset of "in use" byte in entry

.GETRNB
	LDA (RTEPTR),Y;Get an "in use" byte
	BEQ GETRNA;Free entry found

	CLC ;RTEPTR +:= entry size
	LDA #RTENSZ
	ADC RTEPTR;LS byte
	STA RTEPTR
	BCC GETRNC
	INC RTEPTR + 1;MS byte
.GETRNC
	DEX ;Any entries left?
	BNE GETRNB;Jump if yes

	;No entries left

	LDA #RDERRG;"RANDTB full"
.GETRNA
	RTS
}


	;*** RDCKAC ***

	;Check that the requested access to a file is allowed.
	;Check first that the access is allowed in
	;the directory entry, and secondly that it can
	;be granted within a multiple reader / single
	;writer regime (by scanning HANDTB).

	;Entry: ARGD (on ARGPTR) = requested access
	;ARGE = allowed access (from directory entry)
	;RNDPTR points to object details

	;Exit:  A = RC
	;HNDPTR is corrupted

.RDCKAC
{
	LDY #ARGD
	LDA (ARGPTR),Y;Get requested access
	AND #RDWRAC;Mask out type bit
	STA RNDTMP;Keep masked copy
	INY
	AND (ARGPTR),Y;AND with allowed access
	CMP RNDTMP;Should equal requested access
	BEQ RDCKAD;Access allowed

	LDA #RDERRK;"Access not allowed"
	RTS

.RDCKAD
	JSR INITHD;HNDPTR := HANDTB; X := no. of entries

.RDCKAB
	LDY #HTHAND
	LDA (HNDPTR),Y;Is this an empty entry?
	BEQ RDCKAA;Skip if empty

	;A used HANDTB entry: check disc no. and SIN
	;against those of file we want to open.

	LDY #INFDIS;Compare disc numbers
	LDA (RNDPTR),Y
	LDY #HTDISC
	CMP (HNDPTR),Y
	BNE RDCKAA;LS bytes differ
	LDY #INFDIS + 1
	LDA (RNDPTR),Y
	LDY #HTDISC + 1
	CMP (HNDPTR),Y
	BNE RDCKAA;MS bytes differ

	LDY #INFSIN;Compare SINs
	LDA (RNDPTR),Y;A := LS byte of SIN for OPEN
	LDY #HTSIN
	CMP (HNDPTR),Y
	BNE RDCKAA;LS bytes differ
	LDY #INFSIN + 1
	LDA (RNDPTR),Y
	LDY #HTSIN + 1
	CMP (HNDPTR),Y
	BNE RDCKAA;CS bytes differ
	LDY #INFSIN + 2
	LDA (RNDPTR),Y
	LDY #HTSIN + 2
	CMP (HNDPTR),Y
	BNE RDCKAA;MS bytes differ

	;Have found an existing HANDTB entry for the SIN
	;we want to open.
	;Open can be permitted only if it is for reading,
	;and the file is not already open for writing.

	LDY #HTMODE
	LDA (HNDPTR),Y;A := mode in which already open
	LDY #ARGD
	ORA (ARGPTR),Y;OR requested access with that already granted
	AND #WRITAC;Look at write access bit
	BEQ RDCKAA;Both for reading - proceed with search

	;The file is already open, and:
	;either the request includes write access, or the
	;file is already open with write access.

	JSR STMCNO
	LDA #RDERRH;"File in use"
	RTS

	;Have not yet found a reason to disallow the
	;OPEN. Keep looking!

.RDCKAA
	JSR INCHND;Point HNDPTR at next entry; X -:= 1
	BNE RDCKAB;back to start of loop

	;Have searched all of HANDTB - open may proceed

	LDA #0;set RC
	RTS
}

.STMCNO;USED BY EXTERR
	LDY #HTMCNO;OWNER'S M/C NO
	LDA (HNDPTR),Y
	STA ERMCNO
	INY
	LDA (HNDPTR),Y
	STA ERMCNO + 1
	INY	
	LDA (HNDPTR),Y
	STA ERMCNO + 2
	RTS

	;*** CLRRTE ***

	;Clear entry in RANDTB pointed to by HANDTB entry
	;whose address is in HNDPTR.

.CLRRTE
{
	JSR SETRPT;RTEPTR := address of RANDTB entry

	LDY #RTENSZ - 1;X := size of RANDTB entry
	LDA #0;Set up for clearing loop

.CLRRTA
	STA (RTEPTR),Y;Clear whole entry to zero
	DEY
	BPL CLRRTA;Loop if more to do

	RTS ;No result
}


	;*** RDFLSH ***

	;Flush all blocks of an object from cache.
	;Called from CLRHTE when closing a file.

	;Entry: HNDPTR contains address of HANDTB entry

.RDFALL

	LDA #5;STRMAN.remove all blocks
	BNE RDFLLA;always taken
	;** 5/8/83 **
.RDFLSH
	LDA #9;STRMAN.FLUSH ALLDIRTYWONDOWS
.RDFLLA
	JSR SETRTN
	JSR RDDSIN;Put disc and SIN on stack

	JMP STRMAN;*** Call STRMAN **


	;*** RDSHWM ***

	;IF seq file ptr > high water mark
	;THEN high water mark := seq file ptr

	;Entry: RTEPTR points to RANDTB entry

.RDSHWM
{
	JSR CMPHWP
	BCS RDSHWX;SFP <= HWM, return

	LDY #RTCSFP;SFP > HWM, so copy
	LDA (RTEPTR),Y;SFP to HWM field
	LDY #RTHWM
	STA (RTEPTR),Y;LS byte
	LDY #RTCSFP + 1
	LDA (RTEPTR),Y
	LDY #RTHWM + 1
	STA (RTEPTR),Y;CS byte
	LDY #RTCSFP + 2
	LDA (RTEPTR),Y
	LDY #RTHWM + 2
	STA (RTEPTR),Y;MS byte
.RDSHWX
	RTS
}

.CMPHWP

	;Compare High Water Mark with Seq. file
	;ptr. and return CC if SFP>HWM, CS if
	;SFP<=HWM.
	;Also returns Z flag indicating EQ or not.

	;NOTE - corrupts RNDTMP

	SEC
	LDY #RTHWM;Subtract SFP from HWM
	LDA (RTEPTR),Y;LS byte of HWM
	LDY #RTCSFP
	SBC (RTEPTR),Y;Subtract LS bytes
	STA RNDTMP
	LDY #RTHWM + 1
	LDA (RTEPTR),Y
	LDY #RTCSFP + 1
	SBC (RTEPTR),Y;Subtract CS bytes
	STA RNDTMP + 1
	LDY #RTHWM + 2
	LDA (RTEPTR),Y
	LDY #RTCSFP + 2
	SBC (RTEPTR),Y;Sets carry if SFP <= HWM
	ORA RNDTMP
	ORA RNDTMP + 1;Return Z flag indicating EQ or not
	RTS
	
;.LNK
;UADE0A
