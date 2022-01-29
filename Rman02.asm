
;.OPT
;RMAN02;RNDMAN > Rman02
;.TTL
;Fileserver file RMAN02

.RMAN02

	;**************** RDRDCH ****************

	;RDRDCH: read one byte from an open file

	;Entry: ARGB = (LS) ptr to user info
	;ARGC = (MS) ptr to user info
	;ARGD = file handle
	;ARGE = sequence no. (in LS bit)

	;Exit:  ARGA = return code
	;ARGB = byte read
	;ARGC = end-of-file flag
	;normally zero,
	;#X80 if this is the last byte in file
	;#XC0 if this is byte after end of file

	;1) Get HANDTB entry from handle
	;2) Check that object is a file
	;3) Get RANDTB entry
	;4) Compare sequence no. in ARGD with that
	;in RANDTB entry.
	;Same => old seq. file ptr := current SFP
	;increment sequence no. in RANDTB entry
	;Different => repeat of last read;
	;current SFP := old SFP
	;5) Check sequential file pointer against file size
	;SFP =  size => return $FE and EOFFLG = $C0
	;SFP >  size => end of file; return RC
	;SFP <  size => OK
	;6) Work out disc address of block containing
	;required byte, and offset of byte in block.
	;7) Find / get required block in cache (STRMAN)
	;8) Fetch required byte and return it
	;Increment current SFP in RANDTB

.RDRDCH
{
	JSR INITRD;GENPTR := user info; MCNUMB := machine no.
				;[RNDPTR := rubbish]

	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDRDCZ;Not found - exit


	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDRDCZ;Not found (object not file)

	LDY #HTMODE
	LDA (HNDPTR),Y;check that file is open for input
	AND #READAC
	BNE RDRDCA
	LDA #RDERRO;file not open for input ** 15/11/84 **
	BNE RDRDCZ

.RDRDCA
	JSR RDCSEQ;Compare sequence numbers
				;and choose file ptr; if equal
				;then increment seq no.
	JSR RDCKEX;Check SFP against file length
	BNE RDRDCZ;End of file
	LDA #&FE;Byte after last
	BIT EOFFLG
	BVS RDRDLA;Reading byte after last
				;so return $FE


	JSR RDGBLK;Get required block into cache
				;GENPTR := store address
	BNE RDRDCZ;error

	;*** Assuming blocksize is 256 bytes ***
	;Offset of required byte in block is given
	;by LS byte of seq. file pointer.

	LDY #RTCSFP
	LDA (RTEPTR),Y;LS byte of SFP
	TAY
	LDA (GENPTR),Y;Get required byte
.RDRDLA
	LDY #ARGB
	STA (ARGPTR),Y;Return byte read
	INY
	LDA EOFFLG
	STA (ARGPTR),Y;Return end-of-file flag

	JSR RDISFP;Increment Current SFP

	LDA #0
.RDRDCZ
	JMP RDEXIT;Exit: Rc in A
}

	;**************** RDWRCH ****************

	;RDWRCH: write one byte to an open file

	;Entry: ARGB = (LS) ptr to user info
	;ARGC = (MS) ptr to user info
	;ARGD = file handle
	;ARGE = sequence no. (in LS bit)
	;ARGF = byte to be written

	;Exit:  ARGA = return code

	;1) Get HANDTB entry from handle
	;2) Check that object is a file
	;3) Check file open for update
	;4) Get RANDTB entry
	;5) Compare sequence no. in ARGD with that
	;in RANDTB entry.
	;Same => old seq. file ptr := current SFP
	;increment sequence no. in RANDTB entry
	;Different => repeat of last read;
	;current SFP := old SFP
	;6) Check sequential file pointer against file size
	;SFP >= size => end of file; extend file
	;SFP <  size => OK
	;7) Work out disc address of block containing
	;required byte, and offset of byte in block.
	;8) Find / get required block in cache (STRMAN)
	;9) Write byte to cache; mark cache buffer dirty
	;(but do not cause it to be written to disc: this will
	;happen when the cache space is needed for
	;something else, or when the file is closed.
	;The cache buffer is left unlocked between
	;calls of RDWRCH).
	;10) IF SFP > HWM THEN HWM := SFP
	;11) Increment current SFP

.RDWRCH
{
	JSR INITRD;GENPTR := user info; MCNUMB := machine no.
				;[RNDPTR := rubbish]

	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDWRCZ;Not found - exit

	LDY #HTMODE;Check file open for update
	LDA (HNDPTR),Y;Get mode
	AND #WRITAC;Extract write access bit
	BNE RDWRCA;OK to write

	LDA #RDERRN;"File not open for update"
	BNE RDWRCZ;Always jumps

.RDWRCA
	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDWRCZ;Not found (object not file)

	JSR RDCSEQ;Compare sequence numbers
				;and choose file ptr; if equal
				;then increment seq no.
	JSR RDCKSP;Check SFP against file length
	BIT EOFFLG;If PTR = EXT, need to extend
	BVS RDWRCC;V set => PTR = EXT => extend
	BEQ RDWRCB;Otherwise, Z set if inside file
				;so don't extend

.RDWRCC

	;Must extend file
	;Allocate another block (1K bytes)

	LDA #0
	STA NEWFSZ;Ls byte of new size
	LDA #4;Add 1K to 3-byte size
	CLC
	LDY #RTFSZE + 1;CS byte of size
	ADC (RTEPTR),Y
	STA NEWFSZ + 1
	INY
	LDA #0
	ADC (RTEPTR),Y;MS byte
	STA NEWFSZ + 2
	JSR RDCHSZ;Change file size
	BNE RDWRCZ;Error


.RDWRCB
	JSR RDGBLK;Get required block into cache
				;GENPTR := store address
	BNE RDWRCZ;error

	;*** Assuming blocksize is 256 bytes ***
	;Offset of required byte in block is given
	;by LS byte of seq. file pointer.

	LDY #ARGF
	LDA (ARGPTR),Y;Byte to be written
	TAX ;Save it
	LDY #RTCSFP
	LDA (RTEPTR),Y;LS byte of file pointer
	TAY
	TXA
	STA (GENPTR),Y;Write byte to cache

	JSR MRKDRT;Mark buffer dirty (addr in GENPTR)
	JSR RDISFP;Increment Current SFP
	JSR RDSHWM;Set high water mark

	LDA #0;Success

.RDWRCZ
	JMP RDEXIT;Return - RC in A
}


	;****** RDRDAR ******

	;RDRDAR: read RANDTB info on open file

	;Entry: ARGB (LS) ptr to user info
	;ARGC (MS) ptr to user info
	;ARGD = file handle

	;Exit:  ARGA = return code
	;ARGB = (LS) Seq file ptr
	;ARGC = (CS)  "   "    "
	;ARGD = (MS   "   "    "
	;ARGE = (LS) File high water mark
	;ARGF = (CS)  "    "     "
	;ARGG = (MS)  "    "     "
	;ARGH = (LS) File size
	;ARGI = (CS)  "    "
	;ARGJ = (MS)  "    "

	;The file handle is used to find the HANDTB
	;entry, which points to the RANDTB entry
	;from which the info is extracted.

.RDRDAR
{
	JSR INITRD;GENPTR:=ptr to user info
	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HNDPTR := HANDTB entry
	BNE RDRDAZ;Not found - exit

	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDRDAZ;Not found

	;Return current seq file ptr

	LDY #RTCSFP
	LDA (RTEPTR),Y;LS byte of SFP
	LDY #ARGB
	STA (ARGPTR),Y
	LDY #RTCSFP + 1
	LDA (RTEPTR),Y;CS byte
	LDY #ARGC
	STA (ARGPTR),Y
	LDY #RTCSFP + 2
	LDA (RTEPTR),Y;MS
	LDY #ARGD
	STA (ARGPTR),Y

	;Copy 6 bytes from RANDTB entry to ARGPTR
	;stack.  (The 2 fields are in the same
	;order in each place).

	LDX #6;Loop count
	LDA #ARGE;1st stack offset
	STA RNDTMP;Temp workspace
	LDY #RTHWM;Offset in RANDTB entry

.RDRDAA
	LDA (RTEPTR),Y;Get data from RANDTB
	STY RNDTMP + 1;Save RANDTB entry offset
	LDY RNDTMP;Get stack offset
	STA (ARGPTR),Y;Put data on stack
	INY
	STY RNDTMP;Save next stack offset
	LDY RNDTMP + 1;Restore entry offset
	INY
	DEX ;Loop count
	BNE RDRDAA;Loop if not zero
	JSR RDCKSP;check for EOF ** 26/5/83 **
	BEQ RDRDAJ;skip except on error

	LDY #ARGJ
	STY RNDTMP
	LDY #ARGD
	LDX #2;copy size to pointer area
.RDRDAK
	STY RNDTMP + 1
	LDY RNDTMP
	LDA (ARGPTR),Y;move the data
	LDY RNDTMP + 1
	STA (ARGPTR),Y
	DEC RNDTMP
	DEY
	DEX
	BPL RDRDAK;three bytes

.RDRDAJ
	LDA #0;All copied - set RC

.RDRDAZ
	JMP RDEXIT;Return - RC in A
}


	;****** RDSTAR ******

	;RDSTAR: set sequential file ptr

	;Entry: ARGB = (LS) ptr to user info
	;ARGC = (MS)  "   "   "   "
	;ARGD = file handle
	;ARGE = arg saying what to set
	;0: SFP
	;1: HWM ** 5/9/84 **
	;ARGF = (LS) New value to set
	;ARGG = (CS)  "   "    "   "
	;ARGH = (MS)  "   "    "   "
	;ARGI = 4th byte if any

	;Exit:  ARGA = return code

	;The file handle is used to find the RANDTB entry.
	;The arg in ARGE is inspected to find
	;out what is being set.

.RDSTAR
{
	JSR INITRD;GENPTR -> user info
	LDY #ARGD
	LDA (ARGPTR),Y;A := handle
	JSR FNDHND;HANDPTR := HANDTB entry
	BNE RDSTAE

	JSR SETRPT;RTEPTR := RANDTB entry
	BNE RDSTAE;Not found

	;Inspect arg in ARGE

	LDY #ARGE
	LDA (ARGPTR),Y
	LSR A
	BEQ RDSTAB;0 => set SFP
				;1 => set HWM

	LDA #RDERRM;"Bad arg to RDSTAR"

.RDSTAE
	JMP RDSTAZ;Exit


	;Set seq file ptr

	;IF newSFP < size THEN oldSFP = newSFP
	;ELIF openforwriting THEN resize file: HWM = newSFP: oldSFP = newSFP
	;ELSE IF newSFP <> size THEN error ELSE oldSFP = newSFP

.RDSTAB
	BCS RDJHWM;A = 1 => set extent of file
	SEC
	LDY #ARGF
	LDA (ARGPTR),Y
	LDY #RTFSZE
	SBC (RTEPTR),Y;Do RNDTMP := SFP - size
	STA RNDTMP
	LDY #ARGG
	LDA (ARGPTR),Y
	LDY #RTFSZE + 1
	SBC (RTEPTR),Y
	STA RNDTMP + 1
	LDY #ARGH
	LDA (ARGPTR),Y
	LDY #RTFSZE + 2
	SBC (RTEPTR),Y
	ORA RNDTMP
	ORA RNDTMP + 1
	BCC RDSTAF;Set SFP = new SFP size > SFP

	;Here SFP => size, so attempt to point
	;outside file. If open for update, resize
	;else if SFP <> size then error else set SFP = new SFP

	PHP ;Push Z flag indicating SFP = size
	LDY #HTMODE
	LDA (HNDPTR),Y;Mode handle opened for
	AND #WRITAC;Check if open for writing
	BNE RDSTAG;Yes, so resize

	PLP ;Not open for writing, so check if SFP=size
	BEQ RDSTAF;Yes, so set SFP
	LDA #RDERRL;No => "attempt to point outside file"
	BNE RDSTAZ;Error exit

.RDSTAG
	PLP ;Restore stack, and resize file
	LDY #ARGH
	LDA (ARGPTR),Y;Set NEWFSZ to seq. ptr.
	STA NEWFSZ + 2
	DEY
	LDA (ARGPTR),Y
	STA NEWFSZ + 1
	DEY
	LDA (ARGPTR),Y
	STA NEWFSZ
	JSR RDCHSZ;Change size of file
	BNE RDSTAZ;Error => exit

	JSR RDJHW9

.RDSTAF

	;Copy SFP into RANDTB

	LDA #ARGF
	STA OFF1
	LDA #RTCSFP
	JSR MOVRND;Set current seq. ptr.
	LDA #RTOSFP;Set old seq. ptr. as well
	JSR MOVRND
.RDSTAX
	LDA #0;Return code

.RDSTAZ
	JMP RDEXIT;Return - RC in A


.RDJHWM
	LDY #HTMODE
	LDA (HNDPTR),Y
	AND #WRITAC
	BNE RDJHW2;open for update

	LDA #RDERRN;not open for update
	BNE RDSTAZ

.RDJHW2

	LDY #ARGF
	LDX #LO(-3)
.RDJHW1
	LDA (ARGPTR),Y
	STA NEWFSZ - LO(-3),X	; :LSB:(-3),X
	INY
	INX
	BNE RDJHW1

	LDY #RTHWM+2
	LDX #2
.RDJHW4
	LDA (RTEPTR),Y
	CMP NEWFSZ,X
	BNE RDJHW5
	DEY
	DEX
	BPL RDJHW4

	;exactly the same size so NOP here

	BMI RDJHWK;exit, all ok

.RDJHW5
	LDY #RTHWM
	LDA (RTEPTR),Y
	STA TEMPA;save current extent
	ROL A
	STA TEMPB;save carry as well

	;C=0 => extending the file

	ROR A
	BCC RDJHWA

	;if decreasing file then must flush all blocks
	;else cache may be left with antiquated data

	LDA #5
	JSR SETRTN
	JSR RDDSIN
	JSR STRMAN;flush all blocks
	BNE RDSTAZ

.RDJHWA
	JSR RDCHSZ;call changesize
	BNE RDSTAZ

.RDJHWK
	JSR RDJHW9
	JMP RDSTAX

.RDJHW9
	LDA #ARGF
	STA OFF1
	LDA #RTHWM

.MOVRND

	;Little space-saving routine
	STA OFF2
	LDX #ARGPTR
	LDY #RTEPTR
	LDA #3
	JMP MOVBLK
}

	;*** RDCHSZ ***

	;Change the size of a file to the 3-byte value
	;in NEWFSZ.  HNDPTR points at its HANDTB entry,
	;and RTEPTR at the RANDTB entry.

	;MAPMAN.CHANGESIZE is called, and the new
	;size is recorded in the RANDTB entry.

	;RC in A on exit.


.RDCHSZ
{
	JSR RDDSIN;Put disc and SIN on stack

	;Put new size on stack

	LDA NEWFSZ;LS byte
	LDY #ARGG
	STA (NEWARG),Y
	LDA NEWFSZ + 1
	INY
	STA (NEWARG),Y;CS byte
	LDA NEWFSZ + 2
	INY
	STA (NEWARG),Y;MS byte

	JSR JUSINF;** 3/10/84 **

	LDA #3;MAPMAN.CHANGESIZE
	JSR SETRTN
	JSR MAPMAN
	BNE RDCHSX;Failed, return code

	;zero the new area if needed

	JSR RDDSIN;** 20/9/84 **

	LDY #RTFSZE
	LDA (RTEPTR),Y
	LDY #ARGG
	STA (NEWARG),Y

	LDY #RTFSZE+1
	LDA (RTEPTR),Y
	LDY #ARGH
	STA (NEWARG),Y

	LDY #RTFSZE+2
	LDA (RTEPTR),Y
	LDY #ARGI
	STA (NEWARG),Y

	LDA #13
	JSR SETRTN
	JSR MAPMAN
	BNE RDCHSX;failed, return code

	;Size changed: record in RANDTB

	LDA #6
	JSR SETRTN;Ensure map on disc
	JSR MAPMAN;Note assumes disc no. still on stack

	BNE RDCHSX;Error -> leave

	LDY #RTFSZE
	LDA NEWFSZ
	STA (RTEPTR),Y;LS byte
	LDA NEWFSZ + 1
	INY
	STA (RTEPTR),Y;CS byte
	LDA NEWFSZ + 2
	INY
	STA (RTEPTR),Y;MS byte

	LDA #0;RC
.RDCHSX
	RTS
}

	;*** SETRPT ***

	;Set RTEPTR to point to RANDTB entry

	;Entry: HNDPTR points at HANDTB entry

	;Exit:  A = return code
	;RTEPTR points to corresponding RANDTB entry


.SETRPT
{
	LDY #HTACC
	LDA (HNDPTR),Y;A := type of object (& access)
	AND #TYPE;Mask type
	CMP #TYPFIL;Is it a file?
	BEQ SETRPA;Jump if file

	LDA #RDERRI;"Object not a file"
	RTS ;Return

.SETRPA
	LDY #HTRPTR
	LDA (HNDPTR),Y;LS byte
	STA RTEPTR;LS byte
	INY
	LDA (HNDPTR),Y
	STA RTEPTR + 1;MS byte

	LDA #0;return code
	RTS
}

	;*** RDDSIN ***

	;Put disc number and SIN on NEWARG
	;stack from ARGB - ARGF.

.RDDSIN

	LDA #HTDISC;"Move from" offset
	STA OFF1
	LDA #ARGB;"move to" offset
	STA OFF2
	LDX #HNDPTR;Move from
	LDY #NEWARG;Move to
	LDA #5;Size of disc no. + SIN
	JMP MOVBLK;Note -> assumes DISC/SIN contiguous


	;*** RDCKEX ***

	;Check SFP against HWM (For read operation)

.RDCKEX
	LDY #RTHWM
	BNE RDCKLA;share code with RDCKSP


	;*** RDCKSP ***

	;Check sequential file pointer against file size

	;Entry: RTEPTR points to RANDTB entry

	;Exit:  A = return code: 0 => OK, SFP < size
	;EOFFLG = 0 normally
	;= #X80 if this is last byte of file
	;= $C0 If byte after last

.RDCKSP
	LDY #RTFSZE
.RDCKLA
{
	STY RNDTMP +2
	LDA #0;First set EOFFLG to zero
	STA EOFFLG
	SEC
	LDY RNDTMP +2;Subtract SFP from file size
	INC RNDTMP +2
	LDA (RTEPTR),Y;LS byte of file size
	LDY #RTCSFP
	SBC (RTEPTR),Y;Subtract LS bytes
	STA RNDTMP;Use RNDTMP as workspace
	LDY RNDTMP +2
	INC RNDTMP +2
	LDA (RTEPTR),Y
	LDY #RTCSFP + 1
	SBC (RTEPTR),Y;CS bytes
	STA RNDTMP + 1
	LDY RNDTMP +2
	INC RNDTMP +2
	LDA (RTEPTR),Y
	LDY #RTCSFP + 2
	SBC (RTEPTR),Y;Sets carry if SFP <= size

	TAX ;Save MS byte for EOFFLG test
	ORA RNDTMP;OR all 3 bytes of result
	ORA RNDTMP + 1;for zero test
	BEQ RDCKSC;Jump if SFP = file size
	BCC RDCKSA;Jump if SFP > file size

	;OK: SFP < file size | HWM
	;Set EOFFLG to #X80 if this is the last
	;byte of the file (size - SFP = 1)

	CMP #1;Is OR of all bytes 1?
	BNE RDCKSB;No
	TXA ;OR MS two bytes
	ORA RNDTMP + 1;CS byte
	BNE RDCKSB;MS 2 bytes not both 0

	LDA #&80;(size - SFP)=1 so set
	STA EOFFLG;end-of-file flag

.RDCKSB
	LDA #0;OK: SFP <= file size
	RTS

	;SFP = file size
	;Set end of file marker to indicate
	;reading/writing byte AFTER end of file.

.RDCKSC
	LDA #&C0;Last AND one after last
	STA EOFFLG
	BNE RDCKSB;Exit, Z set, A=0

.RDCKSA
	LDA #RDERRJ;"End of file"
	RTS
}


	;*** RDCSEQ ***

	;Compare sequence number received with that recorded
	;in RANDTB.  If they are equal, then increment
	;the sequence number in RANDTB and set
	;old SFP := current SFP
	;Otherwise, this is a repetition of the
	;last operation, so don't change the sequence number
	;and set current SFP := old SFP

	;Entry: RTEPTR points to RANDTB entry
	;ARGE on ARGPTR stack holds received
	;seq no (in LS bit)

	;Exit:  Sequence number
	;in RANDTB entry incremented if and only if
	;sequence numbers matched.

.RDCSEQ

	LDY #ARGE
	LDA (ARGPTR),Y;A := received seq no.
	LDY #RTINUS;Seq no. byte of RANDTB entry
	EOR (RTEPTR),Y;A := XOR of seq nos.
	AND #1;Mask out "in use" bit
	BEQ RDCSEA;Jump if seq nos. equal

	;Seq nos different, so back up SFP:
	;current SFP := old SFP

.RDBACK
	LDY #RTOSFP;** 5/8/83 **
	LDA (RTEPTR),Y
	LDY #RTCSFP
	STA (RTEPTR),Y
	LDY #RTOSFP + 1
	LDA (RTEPTR),Y
	LDY #RTCSFP + 1
	STA (RTEPTR),Y
	LDY #RTOSFP + 2
	LDA (RTEPTR),Y
	LDY #RTCSFP + 2
	STA (RTEPTR),Y
	RTS ;Seq nos. different; return

.RDCSEA
	LDA (RTEPTR),Y;Get seq. no.
	EOR #1;Flip seq no., leaving "in use" flag
	STA (RTEPTR),Y

	;old SFP := current SFP

	LDY #RTCSFP
	LDA (RTEPTR),Y
	LDY #RTOSFP
	STA (RTEPTR),Y
	LDY #RTCSFP + 1
	LDA (RTEPTR),Y
	LDY #RTOSFP + 1
	STA (RTEPTR),Y
	LDY #RTCSFP + 2
	LDA (RTEPTR),Y
	LDY #RTOSFP + 2
	STA (RTEPTR),Y
	RTS ;Return


	;*** RDISFP ***

	;Increment sequential file ptr (3 bytes)

.RDISFP
	CLC
	LDA #1
	LDY #RTCSFP
	ADC (RTEPTR),Y
	STA (RTEPTR),Y;LS byte
	INY
	LDA (RTEPTR),Y
	ADC #0
	STA (RTEPTR),Y;CS byte
	INY
	LDA (RTEPTR),Y
	ADC #0
	STA (RTEPTR),Y;MS byte

	RTS ;Return


	;*** RDGBLK ***

	;Gets the required block into the cache.
	;In order to speed up the operation, a means
	;is provided of usually avoiding a call of STRMAN.
	;Each entry in RANDTB contains a hint - in the form
	;of a pointer to the cache descriptor for the block
	;last used.  STRMAN guarantees that this will
	;continue to point to SOME cache descriptor, but if
	;there has been a store crisis in the cache, then
	;it may not still point to the block we want.
	;Therefore the hint is carefully checked for validity
	;before use.
	;If the hint is no good, then the required block
	;is read from disc by calling STRMAN.DISC ADDRESS->
	;STORE ADDRESS. The address of the cache descriptor
	;is put in the RANDTB entry as the next hint.

	;Entry: HNDPTR points to handle table entry
	;RTEPTR points to RANDTB entry

	;Exit:  A = RC
	;GENPTR contains store address of cache buffer.
	;Hint field of RANDTB entry points to cache
	;descriptor (or is zero).

.RDGBLK
{
	JSR RDCHNT;Check hint; set GENPTR if OK
	BEQ RDGBLZ;GENPTR already set; return

	JSR RDDSIN;Put disc and SIN on stack

	;Logical block number of the block containing the
	;current byte is given by the top 2 bytes of the
	;sequential file pointer.
	;*** This firmly assumes that the blocksize is
	;*** 256 bytes (as does DIVIDE in MAPMAN).

	LDY #RTCSFP + 1
	LDA (RTEPTR),Y;Seq file ptr (CS)
	LDY #ARGG
	STA (NEWARG),Y;Logical block no. (LS)
	LDY #RTCSFP + 2
	LDA (RTEPTR),Y;Seq file ptr (MS)
	LDY #ARGH
	STA (NEWARG),Y;Logical block no. (MS)

	;Ask for one block only.  This simplifies the
	;arithmetic elsewhere.
	;(Would using units of > 1 block give a
	;significant speed improvement?)

	LDA #0
	LDY #ARGJ
	STA (NEWARG),Y
	LDA #1
	DEY
	STA (NEWARG),Y

	;LDA #1;STRMAN: Disc address -> store address
	JSR SETRTN
	JSR STRMAN;*** STRMAN.DISC -> STORE ADDRESS **
	BNE RDGBLZ;Failed; return

	LDY #ARGB;GENPTR := store address
	LDA (NEWARG),Y
	STA GENPTR;(LS)
	INY
	LDA (NEWARG),Y
	STA GENPTR + 1;(MS)

	INY ;Store cache desc. addr. in RANDTB entry
	LDA (NEWARG),Y;LS byte
	LDY #RTDESC
	STA (RTEPTR),Y
	LDY #ARGE
	LDA (NEWARG),Y;MS byte
	LDY #RTDESC + 1
	STA (RTEPTR),Y

	;Must unlock cache window, so that it is not
	;locked between byte transfers.  Note that if
	;parallelism is ever introduced into the file
	;server, care must be taken that RNDMAN does
	;not relinquish control between deciding that
	;it has found a block in the cache and using it.

	;Very conveniently, STRMAN has left the buffer
	;address in the right place on the stack.

	LDA #3;STRMAN.UNLOCKWINDOW
	JSR SETRTN
	JSR STRMAN;*** STRMAN.UNLOCKWINDOW **

.RDGBLZ
	RTS ;exit
}

;.LNK
;RMAN03
;
