;.OPT
;UADE14; > Uade14
;.TTL
;Fileserver file UADE14

.UADE14
.DSCTOP



	;*********************************
	;*           D S C M A N         *
	;*********************************



	;DSCMAN: THE DISC MANAGER
	;THIS MODULE READS & WRITES OBJECTS FROM
	;AND TO THE DISC.
	;NOTE THAT IT USES MANY OF THE POINTERS,VARIABLES,
	;AND ROUTINES PROVIDED BY MAPMAN.


	;ENTRY: ARGA = (1=>READ,2=>WRITE)
	;ARGB = LS(DISC NO)
	;ARGC = MS(DISC NO)
	;ARGD = LS(SIN)
	;ARGE = CS(SIN)
	;ARGF = MS(SIN)
	;ARGG = LS(START BLOCK NO)
	;ARGH = MS(START BLOCK NO)
	;ARGI = LS(NUMBER OF BLOCKS)
	;ARGJ = MS(NUMBER OF BLOCKS)
	;ARGK = LS(STORE ADDRESS)
	;ARGL = MS(STORE ADDRESS)

	;EXIT: ARGA = RC



	;********** MAIN ROUTINE **********

.DSCMAN
{
	LDX #4
	LDA #MODDSC
	JSR ENTRY
	LDA DCRTNS,X
	STA DCJUMP + 1
	LDA DCRTNS + 1,X
	STA DCJUMP + 2
.DCJUMP
	JMP DCJUMP;N.B. NO NEED FOR SETFS-NOT CALLING ANY MODULE
.DCRTNS
	EQUW DCREAD;1 => READ
	EQUW DCWRIT;2 => WRITE
	EQUW DCIORD;3 => READ (I/O processor)
	EQUW DCIOWR;4 => WRITE (I/O processor)
}
.DCEXIT
	BEQ DCEXEX
	LDX #&FF
	STX ODSCMN;invalidate cache on disc error
	STX ODSCMN + 1
.DCEXEX
	JMP EXIT



	;************ DCREAD ************


	;DCREAD: READ AN OBJECT FROM THE DISC.
	;NOTE IF A SECTOR HAS NOT BEEN WRITTEN THEN
	;THE APPROPRIATE AREA OF STORE IS CLEARED TO ZERO.


.DCIORD
{
	JSR DCCACH;check call against last caller
	BEQ DCREDX;all done !!!!!

	LDX #&FF;for I/O side operation
	BNE DCRDL1
.*DCREAD
	LDX #0
.DCRDL1
	STX DSCCB+TRADD+ 2
	STX DSCCB+TRADD+ 3

	JSR DCINIT;INITIALISE FOR DSCMAN VARIABLES
	BNE DCREDZ

	LDA #8
	STA SAVCOM;SAVCOM := DOS DISC READ COMMAND

.DCREDB
	JSR SETDSE;DCSECT := SECTOR NUMBER


	JSR SETDES;SETUP DOS DISC DRIVE DESCRIPTOR


	LDA DCRASH
	BNE DCREDX;If a disc error occured in NMI
				;task...error.

	JSR XFERIN;TRANSFER MPTMPD SECTORS
	JSR MOVEON;FIND NEXT SECTORS FROM DISC MAP
	BEQ DCREDB
	CMP #1
	BNE DCREDZ;NO MORE SECTORS TO READ
	LDA #0
.DCREDZ
	TAX
	LDA DCRASH
	BNE DCREDX;Check no crash on last BG job
	TXA
.DCREDX
	JMP DCEXIT
}


	;************ DCWRIT ************


	;DCWRIT: WRITE AN OBJECT TO DISC
	;NOTE THAT IF A SECTOR IS WRITTEN FOR THE FIRST TIME
	;IT IS MARKED DIRTY IN THE MAP (IN WHICH CASE
	;THE MAP IS MARKED DIRTY).


.DCIOWR
{
	JSR DCCACF;copy new stack into variables
	LDX #&FF;for I/O side operation
	BNE DCWRL1
.*DCWRIT
	JSR DCSIN;check invalidation of cache
	LDX #0
.DCWRL1
	STX DSCCB+TRADD+ 2
	STX DSCCB+TRADD+ 3
	JSR DCINIT
	BNE DCWRTZ

	LDA #&0A
	STA SAVCOM;SAVCOM:=COMMAND TO WRITE
.DCWRTB
	JSR SETDSE;DCSECT:=SECTOR NUMBER
	JSR SETDES;SETUP DRIVE DESCRIPTOR FOR DOS

	LDA DCRASH
	BNE DCWRTZ

	JSR XFERIN;START TRANSFR OF ONE SECTOR

	JSR MOVEON
	BEQ DCWRTB

	TAX
	LDA DCRASH
	BNE DCWRTZ
	TXA
	CMP #1
	BNE DCWRTZ;NO MORE SECTORS TO READ
	LDA #0
.DCWRTZ
	JMP DCEXIT
}



	;DCINIT: INITIALISE DSCMAN VARIABLES FOR TRANSFER
	;0) GET APPROPRIATE DISC MAP LOADED
	;1) LDRNB := LOGICAL DRIVE NUMBER (GOT FROM MAPTB ENTRY)
	;2) NBLKS := NUMBER OF DISC BLOCKS TO TX
	;3) DCSTAD := STORE ADDRESS OF SECTOR
	;4) CHECK SIN IS FOR START OF OBJECT
	;5) MAPTMP := SIN, MPSCPT:=PTR TO RELEVENT MAP ENTRY
	;6) DSCTMP := PTR TO MAP'S DIRTY FLAG



.DCINIT
{
	LDA #0
	STA DCRASH;Initialise disc error flag
	JSR FNDMAP;MAPPTR & MAPENT PT MAPTB ENTRY
	BNE DCINTZ

	LDA MPDRVE
	STA LDRNB;LDRNB:= LOGICAL DRIVE NUMBER

	;FIRST CHECK THAT THE SPECIFIED SIN IS SENSIBLE

	LDY #ARGD
	LDA (ARGPTR),Y
	STA MPTMPA
	INY
	LDA (ARGPTR),Y
	STA MPTMPA + 1;MPTMPA:=SIN
	INY
	LDA (ARGPTR),Y
	STA MPTMPA + 2
	CLC
	JSR RDMPBK
	BNE DCINTZ

	LDA MAPTMP
	STA MPMBPT
	LDA MAPTMP + 1
	STA MPMBPT + 1
	LDA #0
	STA NBLKS + 1
	LDY #ARGI
	LDA (ARGPTR),Y
	STA NBLKS;NBLKS:=NUMBER OF SECTORS TO BE TX
	BNE DCINTD
.DCINTC
	LDA #DCERRA;INVALID NUMBER OF BLOCKS
.DCINTZ
	RTS

.DCINTD
	INY
	LDA (ARGPTR),Y
	BNE DCINTC

	LDY #ARGK
	LDA (ARGPTR),Y
	STA DCSTAD
	INY
	LDA (ARGPTR),Y
	STA DCSTAD + 1;DCSTAD:=STORE ADDRESS


	;NOW GET TO THE CORRECT STARTING POINT IN THE OBJECT

	LDY #ARGG
	LDA (ARGPTR),Y
	STA BREGA
	INY
	LDA (ARGPTR),Y
	STA BREGA + 1;BREGA:=START BLOCK NUMBER

.*DCENT2
	LDA #LO(MBENTS)
	STA MPTMPB
	LDA #HI(MBENTS)
	STA MPTMPB + 1
	CLC
	LDA MPMBPT
	ADC MPTMPB
	STA MPSCPT
	LDA MPMBPT + 1
	ADC MPTMPB + 1
	STA MPSCPT + 1
.DCINTS
	SEC
	LDY #3
	LDA BREGA
	SBC (MPSCPT),Y
	STA BREGA
	LDA BREGA + 1
	INY
	SBC (MPSCPT),Y
	STA BREGA + 1
	BCC DCINTM
	JSR NXTEN
	BEQ DCINTS
	RTS
	
.DCINTM
	CLC
	LDY #3
	LDA (MPSCPT),Y
	ADC BREGA
	STA BREGA
	INY
	LDA (MPSCPT),Y
	ADC BREGA + 1
	STA BREGA + 1
	LDY #0
	CLC
	LDA (MPSCPT),Y
	ADC BREGA
	STA MAPTMP
	INY
	LDA (MPSCPT),Y
	ADC BREGA + 1
	STA MAPTMP + 1
	INY
	LDA (MPSCPT),Y
	ADC #0
	STA MAPTMP + 2
	SEC
	INY
	LDA (MPSCPT),Y
	SBC BREGA
	STA MPTMPD
	INY
	LDA (MPSCPT),Y
	SBC BREGA + 1
	STA MPTMPD + 1
	JSR SETSZE

	LDA #0
	RTS
}

.SETSZE
{
	LDA NBLKS
	CMP MPTMPD
	LDA NBLKS + 1
	SBC MPTMPD + 1
	BCS STSZND
	LDA NBLKS
	STA MPTMPD
	LDA NBLKS + 1
	STA MPTMPD + 1
.STSZND
	RTS
}

.NXTEN
{
	CLC
	LDA MPTMPB
	ADC #LO(ENSZ)
	STA MPTMPB
	LDA MPTMPB + 1
	ADC #HI(ENSZ)
	STA MPTMPB + 1
.NXENLP
	CLC
	LDA MPMBPT
	ADC MPTMPB
	STA MPSCPT
	LDA MPTMPB + 1
	ADC MPMBPT + 1
	STA MPSCPT + 1
	LDY #0
	LDA (MPSCPT),Y
	INY
	ORA (MPSCPT),Y
	INY
	ORA (MPSCPT),Y
	BEQ NXENNG
	LDA MPTMPB
	CMP #LO(LSTENT)
	LDA MPTMPB + 1
	SBC #HI(LSTENT)
	BCC NXENND
	LDY #0
	LDA (MPSCPT),Y
	STA MPTMPA
	INY
	LDA (MPSCPT),Y
	STA MPTMPA + 1
	INY
	LDA (MPSCPT),Y
	STA MPTMPA + 2
	LDA DSCCB+TRADD+ 2
	PHA
	LDA DCSTAD
	PHA
	LDA DCSTAD + 1
	PHA
	LDA SAVCOM
	PHA
	CLC
	JSR RDMPBK
	BNE NXENGA
	PLA
	STA SAVCOM
	PLA
	STA DCSTAD + 1
	PLA
	STA DCSTAD
	PLA
	STA DSCCB+TRADD+ 2
	STA DSCCB+TRADD+ 3
	LDA MAPTMP
	STA MPMBPT
	LDA MAPTMP + 1
	STA MPMBPT + 1
	LDA #LO(MBENTS)
	STA MPTMPB
	LDA #HI(MBENTS)
	STA MPTMPB + 1
	JMP NXENLP
.NXENGA
	TAX
	PLA
	PLA
	PLA
	PLA
	TXA
	RTS
.NXENNG
	LDA #DCERRC
	RTS
.NXENND
	LDY #0
	LDA (MPSCPT),Y
	STA MAPTMP
	INY
	LDA (MPSCPT),Y
	STA MAPTMP + 1
	INY
	LDA (MPSCPT),Y
	STA MAPTMP + 2
	INY
	LDA (MPSCPT),Y
	STA MPTMPD
	INY
	LDA (MPSCPT),Y
	STA MPTMPD + 1
	LDA #0
	RTS
}

.SETDSE
	LDA MAPTMP;DCSECT := MAPTMP (I.E SECTOR NUMBER)
	STA DCSECT
	LDA MAPTMP + 1
	STA DCSECT + 1
	LDA MAPTMP + 2
	STA DCSECT + 2
	RTS


	;SETDES: SETUP DOS'S DISC DRIVE DESCRIPTOR.
	;SETUP CORRECT SECTOR NUMBER IN DOS DESCRIPTOR
	;AND SELECT APPROPRIATE DRIVE, WHEN ACTUAL DRIVES
	;CHANGE.
	;A DRIVE MUST BE "DRVSEL" SELECTED & STARTED
	;BEFORE TRANSFERS CAN BE INITIATED.
	;HOWEVER WE WANT TO AVOID CALLING"DRVSEL" BETWEEN
	;THE SECTORS OF A MULTI-SECTOR TRANSFER.

	;ENTRY: OLDFDR = LAST ACTUAL DRIVE NUMBER USED(SETBY SETDES)
	;LDRNB = LOGICAL DRIVE NO(I.E. DISC UNIT)
	;DCSECT = LOGICAL SECTOR NUMBER
	;MAPENT = PTR TO RELEVENT MAPTB ENTRY(MAPMAN/FNDMAP)

	;FUNCTION:-
	;SECTSD := NO OF SECTORS ON A SIDE OF THIS DISC;
	;IF DCSECT >= SECTSD #NO OF SECTORS ON A SIDE#
	;THEN SECTOR NO=SECTDC-SECTSD
	;DRIVNO = LDRNB + 2
	;ELSE SECTOR NO=DCSECT;
	;DRIVNO := LDRNB
	;FI;
	;IF DRIVNO <> OLDFDR
	;THEN FDRIVE = DRIVE NUMBER
	;JSR DRVSEL;
	;IF DRIVNO <> othersideof OLDFDR THEN JSR ZTRKSL
	;FI;


.SETDES
{
	LDA LDRNB
	STA DRIVNO

	LDY #MPSECS
	LDA (MAPENT),Y
	STA SECTSD
	INY
	LDA (MAPENT),Y
	STA SECTSD + 1;SECTSD:=NUMBER OF SECTORS ON A SIDE
	INY
	LDA (MAPENT),Y
	STA SECTSD + 2

.STDSLP
	LDA DCSECT
	CMP SECTSD
	LDA DCSECT + 1
	SBC SECTSD + 1
	LDA DCSECT + 2
	SBC SECTSD + 2
	BCC SETDSF

	;DCSECT >= SECTSD

	CLC
	LDY #MPDRNC
	LDA DRIVNO
	ADC (MAPPTR),Y
	STA DRIVNO
	SEC
	LDA DCSECT
	SBC SECTSD
	STA DCSECT
	STA RBDA
	LDA DCSECT + 1
	SBC SECTSD + 1;SUBT NO. OF SECTORS ON A SIDE
	STA DCSECT + 1
	STA RBDA + 1
	LDA DCSECT + 2
	SBC SECTSD + 2
	STA DCSECT + 2
	STA RBDA + 2
	JMP STDSLP

	;DCSECT < SECTSD

.SETDSF
	LDA DCSECT
	STA RBDA
	LDA DCSECT + 1
	STA RBDA + 1
	LDA DCSECT + 2
	STA RBDA + 2
	RTS
}

	;MOVEON: SET NXT SECTOR
	;FUNCTION:- NBLKS -:=1
	;DCSTAD +:= BLKSZE
	;MAPTMP := NXT SECTOR NUMBER
	;MPSCPT := PTR TO NXT MAP ENTRY

	;EXIT: A = 0  => CARRY ON
	;A = 1  => NO MORE BLOCKS TO TX
	;A > 1 => ERROR

.MOVEON
{
	SEC
	LDA NBLKS
	SBC MPTMPD
	STA NBLKS
	LDA NBLKS + 1
	SBC MPTMPD + 1
	STA NBLKS + 1
	ORA NBLKS
	BNE MOVENB
	LDA #1;RC:=NO MORE BLOCKS
	RTS

.MOVENB
	CLC ;INCREMENT STORE ADDRESS
	LDA DCSTAD + 1
	ADC MPTMPD
	STA DCSTAD + 1
	BCC MOVEND
	LDA #DCERRB;STORE ADDR GONE ILLEGAL
	JSR INTERR
	
.MOVEND
	JSR NXTEN;UPDATE MAPTMP & MPSCPT
	JMP SETSZE
}


.XFER
{
	LDA #1
	STA DDRSZE + 1
	LDA #0
	JMP XFERST

.*XFERIN
	LDA MPTMPD
	STA DDRSZE+ 1
	LDA MPTMPD + 1
.XFERST
	STA DDRSZE
	LDA DCSTAD
	STA DDRSTA
	LDA DCSTAD + 1
	STA DDRSTA+ 1
	LDA SAVCOM;A:=TYPE OF TRANSFER (READ/WRITE)
	JSR TRANS
	BEQ XFEROK;is ok, end NMI
	STA DCRASH
.XFEROK
	RTS
}


	;Block read and write
	;   On entry::
	;      DRIVNO holds the drive number
	;      DDRSTA holds the start address
	;      CSECTR holds the start sector number
	;      CTRACK holds the start track number
	;      DDRSZE holds the number of sectors to transfer
.BLKRD
	LDA #&08;COMMAND FOR READ
.TRANS
{
	STA DSCCB+CMD
	
	JSR GTDRNO
	ORA RBDA+ 2
	STA DSCCB+CMD+ 1
	LDA RBDA+ 1
	STA DSCCB+CMD+ 2
	LDA RBDA
	STA DSCCB+CMD+ 3

	;test for writing to sector zero

	LDA RBDA
	ORA RBDA+1
	ORA RBDA+2
	BNE TRANSJ

	LDA #IERRAK;writing to sector zero
	JMP INTERR;***

.TRANSJ
	LDA DDRSTA
	STA DSCCB+TRADD; SAVE THE ADDRESS
	LDA DDRSTA + 1
	STA DSCCB+TRADD+ 1
	LDA DDRSZE + 1;CHECK FOR END OF TRANSFER
	STA DSCCB+LENGTH+ 1
	LDA DDRSZE
	STA DSCCB+LENGTH+ 2
	LDA #0
	STA DSCCB
	STA DSCCB+LENGTH
	STA DSCCB+LENGTH+ 3
	STA DSCCB+CMD+ 4
	STA DSCCB+CMD+ 5
	;JMP DSCCMD;DO THE TRANSFER
}

	;DSCCMD
	;   SEND A COMMAND TO THE DISC CONTROLLER
	;   DSCCB IS THE CONTROL BLOCK AND MUST BE SET UP
.DSCCMD
{
	LDA #0
	STA DSCCB

IF DBG1
	ldx #0
.dsclp
	lda DSCCB,x
	jsr WHEX
	lda #' '
	jsr OSWRCH
	inx
	cpx #12
	bne dsclp
	jsr OSCRLF
ENDIF

	LDX #LO(DSCCB)
	LDY #HI(DSCCB)
	LDA #&72
	JSR OSWORD
	LDA DSCCB
	BEQ LHNOER; REPORT DISC ERRORS TO SCREEN 5/6/86 LH
	JSR LHDSCR

.LHNOER
	RTS

.LHDSCR
	PHA
	LDA #&73
	LDX #LO(DSCCB)
	LDY #HI(DSCCB)
	JSR OSWORD
	JSR VSTRIN
	EQUB "Disc Error No. "
	NOP
	LDA DSCCB+3
	JSR WHEX
	JSR VSTRIN
	EQUB " At Disc Address "
	NOP
	LDA DSCCB+2
	JSR WHEX
	LDA DSCCB+1
	JSR WHEX
	LDA DSCCB
	JSR WHEX
	PLA
	RTS
}


.GTDRNO
	LDA DRIVNO
	LSR A
	ROR A
	ROR A
	ROR A;bits 5,6,7 = drive
	RTS

.DCCACH
	LDY #ARGJ;get MS number of blocks to load
	LDA (ARGPTR),Y
	BNE DCCACD;exit 'cos its too big
	DEY ;get LS number of blocks
	LDA (ARGPTR),Y
	CMP IOBSIZ + 1
	BEQ DCCACF
	BCS DCCACD;overflow here

.DCCACF
{
	LDY #ARGL
	LDX #10
	LDA #0
	STA GENPTR;spare space

.DCCACA
	LDA (ARGPTR),Y
	CMP ODSCMN,X;look against old stack
	BEQ DCCACB
	INC GENPTR;unzero flag
.DCCACB
	STA ODSCMN,X;save stack
	DEY
	DEX
	BPL DCCACA;loop until done

	LDA GENPTR;load flag
.*DCCACD
	RTS
}

.DCSIN
{
	LDY #ARGB
	LDX #0
.DCSINA
	LDA (ARGPTR),Y
	CMP ODSCMN,X;compare against old cache
	BNE DCSINB
	INY
	INX
	CPX #5;test disc number & SIN
	BNE DCSINA
	LDX #&FF
	STX ODSCMN;invalidate cache
	STX ODSCMN + 1
.DCSINB
	RTS ;return to main routine
}

;.LNK
;UADE15
;
