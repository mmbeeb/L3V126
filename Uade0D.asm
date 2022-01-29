;.OPT
;UADE0D;FILE > Uade0D
;.TTL
;Fileserver file UADE0D

.UADE0D

	;********************************
	;*       DIRMAN UTILITIES       *
	;********************************

	;******** GETDIR ROUTINE *********

	;GETDIR: LOADS THE DIRECTORY SPECIFIED BY A FILE TITLE.

	;EXIT: A = RC
	;DIRSTA = ADDR OF DIR IN MEMORY

	;IF TITLE = <root><terminator> OR
	;TITLE = <terminator>
	;THEN load 'current dir' #set by INITDV#
	;ELSE call RETENT #retrieve entry#
	;check that entry is for a directory
	;DIRSIN := SIN of current entry
	;load new current directory
	;FI

	;NOTE that GETDIR in cases where an error occurs
	;will UNLOCK anything that it loaded.

.GETDRD
{
	JSR TSTROT;<root>$<terminator>?
	BEQ GETDRY
	JSR TSTERM;<terminator>$?
	BEQ GETDRY
	
	INY
	LDA (NAMPTR),Y
	DEY
	CMP #TERMIN
	BNE GETDRF
	
	LDA (NAMPTR),Y
	JSR TSTSYM
	BEQ GETDRY
	
.GETDRF
	BIT DIRFLG
	BMI GETDRE
	JSR RETENT
	JMP GETDRC
.GETDRE
	JSR RETEND
.GETDRC
	BNE GETDRZ
	
	JSR TSTPSP;TEST TXTNAM STARTS WITH '^' FOLLOWED BY SPACE
	BEQ GETDRX;YES
	
	JSR TSTDIR;SEE IF ITS A DIR-TSTDIR WILL UNLOCK IF REQD.
	BNE GETDRZ
	JSR SETSIN
	
.GETDRX
	JSR UNLOCK
.GETDRY
	JSR LOADDR
.GETDRZ
	RTS
}

	;CHKTIT: CHECKS THE SYNTAX OF A FILE TITLE.
	;ENTRY:
	;NAMPTR => PTR TO FILE TITLE,
	;TO BE RECOGNISED.

	;EXIT :
	;A = RC (0=>SUCCESS)
	;LASTNM => ADDRESS OF THE LAST
	;TEXT NAME IN THE FILE TITLE.

	;<file title>::=$<title component>|<file name>
	;<file name)::= <text name><title component>
	;<title component>::=<terminator>|<separator><file name>

.CHKTIT
{
	LDA DIRWC
	AND #&C3;WAS &C0
	STA DIRWC;top bits only
	LDY #0;Y:=PTR TO CHAR
	STY DIRTMP;counter for loop here
	JSR SETLST;SETUP LASTNM
	LDA (NAMPTR),Y
	CMP #ROOT
	BEQ CHKTTB
	JSR TSTSYM
	BNE CHKTTH
	
	;SEE IF ITS A <file name>
	
.CHKTTB	
	INY
	LDA (NAMPTR),Y
	CMP #SEPART;IS IT A <separator>
	BEQ CHKTTG
	CMP #TERMIN;IS IT A <terminator>
	BEQ CHKTTA
	BNE CHKTTY

	;TRY TO RECOGNISE <file name>
.CHKTTG
	INY
.CHKTTH
	STY DIRTMP + 1;save offset
	JSR SETLST;SETUP LASTNM
.CHKTTJ
	JSR CHKTEX;RECOGNISE <text name>
	BNE CHKTTZ
	LDA (NAMPTR),Y
	CMP #TERMIN
	BNE CHKTTM

	INC DIRTMP
	LDX DIRTMP
	CPX #2
	BEQ CHKTTA;match here

	LDA DIRWC
	AND #&3F
	BIT DIRWC
	BVC CHKTTC
	ORA #&80
	
.CHKTTC
	STA DIRWC
	LDY DIRTMP + 1;restore Y
	JMP CHKTTJ;do loop again

.CHKTTA
	LDA #0;RC := 0
	RTS
.CHKTTM
	CMP #SEPART
	BEQ CHKTTG
.CHKTTY
	LDA #DRERRA;RC:=INVALID SEPARATOR
.CHKTTZ
	RTS

	;CHKTEX: RTN TO RECOGNISE <text name>

.CHKTEX
	LDX #NAMLNT;X:=MAX NO. OF CHARS IN <text name>
.CHKTXA
	LDA (NAMPTR),Y
	CMP #&21
	BCC CHKTXX
	CMP #&7F
	BCS CHKTXX;Nope => error

	BIT DIRWC
	BPL CHKTLA
	CMP #'*'
	BEQ CHKTXV
	CMP #'#'
	BEQ CHKTXV

.CHKTLA
	CMP #'"';** 21/9/83 **
	BEQ CHKTXX
	CMP #':'
	BEQ CHKTXX
	CMP #'*'
	BEQ CHKTXX
	CMP #'.'
	BEQ CHKTXX
	CMP #':'
	BEQ CHKTXX
	CMP #'#'
	BEQ CHKTXX;Nope => error
	CMP #'$'
	BEQ CHKTXX;** 18/9/84 **

.CHKTXV
	INY
	DEX
	BNE CHKTXA
.CHKTXX
	LDA #0
	CPX #NAMLNT;TEST LENGTH OF <text name>
	BNE CHKTXZ
	LDA #DRERRA;INVALID TEXT NAME
.CHKTXZ
	TAX ;SET FLAGS
	RTS
}

.SETLST
	TYA ;RTN TO SETUP LASTNM
	CLC
	ADC NAMPTR
	STA LASTNM
	LDA NAMPTR + 1
	ADC #0
	STA LASTNM + 1
	RTS

	;FNDTEX: SEARCH CUR DIR FOR NAME IN TXTNAM.
	;N.B. DIR ENTRIES ARE SORTED,
	;KEY=<file title>.

	;ENTRY: DIRSTA=STORE ADDR OF DIR TO BE SEARCHED.

	;EXIT : A=0 => NAME FOUND
	;PREVEN => PTR TO PREVIOUS ENTRY
	;CRNTEN => PTR TO CURRENT ENTRY

.FNDTEX
{
	JSR INTPRE;PREVEN := addr of 1st entry
	LDA #0
	STA WILD1;flag for wild cards
.FNDTXA
	JSR SETCRN
	BEQ FNDTXB;end of dir ?
	LDY #DRTITL
	LDX #0

	JSR FNDM1;look up name with wild cards
	BCS FNDTXE
	PHP
	LDA WILD1
	BNE FNDTXP
	PLP
	BMI FNDTXB
	PHP
.FNDTXP
	PLP
.*FNDTEZ
	JSR MOVCRN
	JMP FNDTXA;repeat until end

.FNDTXB
	LDA #DRERRC
	RTS
.FNDTXE
	LDA #0
	RTS ;exit ok

.FNDM1
	LDA TXTNAM,X
	CMP #'*'
	BNE FNDM4
	INC WILD1
	INX
.FNDM6
	TXA
	PHA
	TYA
	PHA
	JSR FNDM1
	PLA
	TAY
	PLA
	TAX
	BCS FNDMR
	INY
	CPY #12
	BCC FNDM6

.FNDMT
	LDA TXTNAM,X
	CMP #&20
	BEQ FNDMR
	CMP #&0D
	BEQ FNDMR
	CPX #10;returns C=1 at end-of-name
	RTS

.FNDM4
	CPY #12
	BCS FNDMT
	JSR CPCHAR
	BNE FNDMF
	INX
	INY
	BNE FNDM1

.FNDMF
	CLC
.FNDMR
	RTS

.CPCHAR
	CMP #'#'
	BEQ CPRET
	JSR CAPS
	PHA
	LDA (CRNTEN),Y
	JSR CAPS
	STA WILD2
	PLA
	CMP WILD2
	RTS

.CPRET
	INC WILD1;set flag
	LDA (CRNTEN),Y;must NOT match a space
	CMP #SPACE
	PHP
	PLA
	EOR #2;flip Z flag
	PHA
	PLP ;return Z=1 if not a space
	RTS

.CAPS
	STA WILD2
	CMP #'A'
	BCC CAPSL1
	CMP #'z' + 1
	BCS CAPSL1
	CMP #'Z' + 1
	BCC CAPSL3
	CMP #'a'
	BCS CAPSL2
.CAPSL1
	SEC
	BCS CAPSL3
.CAPSL2
	CLC
.CAPSL3
	LDA #&F9
	ROR A
	ROR A
	ROR A
	AND WILD2
	RTS
}

.INTPRE
	LDA DIRSTA;PREVEN := DIRSTA
	STA PREVEN
	LDA DIRSTA + 1
	STA PREVEN + 1
	RTS

.MOVCRN
	LDA CRNTEN;PREVEN := CRNTEN
	STA PREVEN
	LDA CRNTEN + 1
	STA PREVEN + 1
	RTS

.SETCRN
	CLC ;RTN TO SETUP CRNTEN (CURRENT ENTRY) PTR
	LDY #DRLINK
	LDA (PREVEN),Y
	ADC DIRSTA
	STA CRNTEN
	INY
	LDA (PREVEN),Y
	ADC DIRSTA + 1
	STA CRNTEN + 1
	LDY #0
	LDA (PREVEN),Y
	INY
	ORA (PREVEN),Y;IF [PREVEN] = 0 THEN A:=0
	RTS

	;INITDV: INITIALISES SEVERAL DIRMAN VARIABLES.

	;ENTRY:
	;ARGB = LS
	;ARGC = MS(PTR TO USERINFO)
	;ARGD = LS
	;ARGE = MS(PTR TO FILE TITLE)

	;FUNCTION:-

	;DRUSIN => PTR TO USERINFO
	;MCNUMB => MACHINE NO OF CALLER
	;NAMPTR => PTR TO FILE TITLE
	;DRDISC => SELECTED DISC NUMBER
	;DIRACC => IF CHAR[0] OF FILE TITLE = ROOT
	;THEN IF SYST PRIVILEGED
	;THEN OWNER ACCESS
	;ELSE ANYBODY ACCESS
	;FI
	;ELSE ACCESS TO CURRENTLY SELECTED DIR
	;FI ! TYPDIR

	;DIRSIN => IF DISC NAME PRESENT
	;THEN SIN OF ROOT DIRECTORY ON NAMED DISC
	;ELIF CHAR[0] OF FILE TITLE = ROOT
	;THEN SIN OF ROOT DIRECTORY
	;ELSE SIN OF CURRENTLY SELECTED DIR
	;FI
	;TDATE  => IF ROOT DIR IMPLIED
	;THEN DATE ROOT DIR CREATED (SEE MAPTB)
	;FI
	;DIRSTT := 0 START BLK NO

	;EXIT: A = RC

.INITDV
{
	LDY #ARGB
	LDA (ARGPTR),Y
	STA DRUSIN
	INY
	LDA (ARGPTR),Y
	STA DRUSIN + 1;DRUSIN:=PTR TO USER INFO

	INY
	LDA (ARGPTR),Y
	STA NAMPTR
	INY
	LDA (ARGPTR),Y
	STA NAMPTR + 1;NAMPTR := PTR TO FILE TITLE

	LDY #UTMCNO
	LDA (DRUSIN),Y
	STA MCNUMB
	INY
	LDA (DRUSIN),Y
	STA MCNUMB + 1
	INY
	LDA (DRUSIN),Y
	STA MCNUMB + 2;MCNUMB := MACHINE NO OF CALLER

	;PRECEEDED BY A DISC NAME?

	LDY #0
	LDA (NAMPTR),Y
	CMP #DISCIN;DISC NAME PRESENT??
	BEQ INTSTR;YES
	JMP INTDVC

.INTSTR
	LDX #DNAMLN;X:=MAX LENGTH OF DISC NAME
	LDA #LO(DRDSNM)
	STA MOVTO
	LDA #HI(DRDSNM)
	STA MOVTO + 1;MOVTO:=PTR TO DRDSNM BUFFER

	INC NAMPTR
	BNE INTDVF
	INC NAMPTR + 1;NAMPTR+:=1
.INTDVF

	LDA (NAMPTR),Y
.INTDVA
	STA (MOVTO),Y;COPY DISC NAME INTO DRDSNM
	CMP #TERMIN;TERMINATOR?
	BEQ INTDBA
	CMP #SEPART;SEPARATOR?
	BNE INTDVB
	LDA #TERMIN
	STA (MOVTO),Y;TERMINATE DISC NAME
	INY

	LDA (NAMPTR),Y
	CMP #'^';PARENT?
	BEQ INTDVG
	CMP #'&';USER ROOT?
	BEQ INTDVG
	CMP #'%';USER LIB?
	BNE INTDBA

.INTDVG
	INY
	LDA (NAMPTR),Y
	DEY 
	CMP #TERMIN
	BEQ INTDVR;ERROR
	CMP #SEPART
	BEQ INTDVR;ERROR
	BNE INTDBA
	
.INTDVB
	INY
	LDA (NAMPTR),Y
	DEX
	BNE INTDVA
	
	LDA #TERMIN
	STA (MOVTO),Y;TERMINATE DISC NAME
	LDA (NAMPTR),Y
	CMP #TERMIN
	BEQ INTDBA
	CMP #SEPART
	BEQ INTDBC
.INTDVR
	LDA #DRERRA;BAD FILE TITLE
.INTDVX
	RTS

.INTDBC
	INY
.INTDBA
	CLC
	TYA
	ADC NAMPTR
	STA NAMPTR
	BCC INTDBD
	INC NAMPTR + 1;NAMPTR:=PTR TO FILE TITLE

	;GET DISC NUMBER OF NAMED DISC

.INTDBD
	LDA #&A
	JSR SETRTN
	INY
	LDA #LO(DRDSNM)
	STA (NEWARG),Y
	INY
	LDA #HI(DRDSNM)
	STA (NEWARG),Y;PTR TO DISC NAME
	JSR MAPMAN;MAPMAN.DISCNAME->DISCNUMB
	BNE INTDVX

	LDY #ARGB
	LDA (NEWARG),Y
	STA DRDISC
	INY
	LDA (NEWARG),Y
	STA DRDISC + 1
	JMP INTDVD

	;WAS ROOT DIR SPECIFIED

.INTDVC
	JSR TSROOT;IS ROOT DIR SPECIFIED?
	BNE INTDV3
	
	LDY #UTDISC
	LDA (DRUSIN),Y
	STA DRDISC
	INY
	LDA (DRUSIN),Y
	STA DRDISC + 1;DRDISC:=SELECTED DISC NUMBER
.INTDVD
	LDA #(ANYBDY OR TYPDIR)
	STA DIRACC;DIRACC:=ANYBDY!TYPDIR
	LDY #UTPRIV
	LDA (DRUSIN),Y
	AND #SYSTPV
	BEQ INTDVE
	LDA #(OWNER OR TYPDIR)
	STA DIRACC;DIRACC:=OWNER!TYPDIR

	;GET SIN OF ROOT ON SELECTED DISC

.INTDVE
	LDA #4;(SIN OF ROOT)
	JSR STDSN2;PLACE DISC NO ON NEWARG STACK
	JSR MAPMAN;CALL MAPMAN.SINOFROOT
	BNE INTDVZ
	LDY #ARGB
	LDA (NEWARG),Y
	STA DIRSIN
	INY
	LDA (NEWARG),Y
	STA DIRSIN + 1
	INY
	LDA (NEWARG),Y
	STA DIRSIN + 2;DIRSIN:=SIN OF ROOT
	INY
	LDA (NEWARG),Y
	STA TDATE;TDATE:=DATE OF ROOT
	INY
	LDA (NEWARG),Y
	STA TDATE + 1
	JMP INTDVT
	
	;USER'S ROOT
.INTDV1
	LDA UMHUFD
	BCS INTDV5;ALWAYS
	
	;USER'S LIB
.INTDV2
	LDA UMHLIB
	BCS INTDV5;ALWAYS

	;NOT ROOT, SO?
.INTDV3
	INY
	LDA (NAMPTR),Y
	CMP #SEPART
	BEQ INTDV4
	CMP #TERMIN
	BNE INTDVH
	
.INTDV4
	DEY
	LDA (NAMPTR),Y
	CMP #'^';PARENT?
	BEQ INTDV6
	CMP #'&';USER'S ROOT?
	BEQ INTDV1
	CMP #'%';USER'S LIB?
	BEQ INTDV2

	;LOOK UP ACCESS, DISC NUMBER, & SIN OF SELECTED DIR

.INTDVH
	LDY #UTHSLD;OFFSET OF HANDLE FOR SELECTED DIR
	LDA (DRUSIN),Y;A:=HANDLE FOR SELECTED DIR

.INTDV5
	JSR FNDHND;FIND HANDLE IN HANDTB
	BNE INTDVZ

	;CHECK THAT ITS A DIR

	LDY #HTACC
	LDA (HNDPTR),Y
	AND #TYPDIR
	BNE INTDVJ
	LDA #DRERRD;OBJECT NOT A DIR
	RTS

.INTDVJ
	CLC
	LDA HNDPTR;HNDPTR SETUP BY FNDHND
	ADC #HTACC
	STA MOVFRM
	LDA HNDPTR + 1
	ADC #0
	STA MOVFRM + 1
	LDA #LO(DIRACC)
	STA MOVTO
	LDA #HI(DIRACC)
	STA MOVTO + 1
	LDX #6;DIRACC,DRDISC,DIRSIN
	JSR MOVE;COPY INFO FROM HANDTB INTO DIRMAN VARIABLES

.INTDVT
	LDA #0
	STA DIRSTT
	STA DIRSTT + 1;DIRSTT:=0
.INTDVZ
	RTS
	
	;PARENT
.INTDV6
	JSR INTDVH
	LDY #UTDISC
	LDA (DRUSIN),Y
	STA DRDISC
	INY
	LDA (DRUSIN),Y
	STA DRDISC + 1
	JSR LDPAR;LOAD PARENT DIR
	BNE INTDVZ
	JSR UNLOCK
	BEQ INTDVT
}

.LDDR
{
	LDA #5
	JSR SETRTN
	JSR SETDIR
	JSR MAPMAN;MAPMAN.SIZEINFO
	BNE LDDRZ
	LDY #ARGB
	LDA (NEWARG),Y
	STA DIRSZE
	INY
	LDA (NEWARG),Y
	STA DIRSZE + 1;DIRSZE:=SIZE OF DIR IN BYTE
	INY
	LDA (NEWARG),Y
	STA DIRSZE + 2
	INY ;Y:=ARGE
	LDA (NEWARG),Y
	STA DIRBKS
	INY
	LDA (NEWARG),Y
	STA DIRBKS + 1;DIRBKS:=NO OF BLOCKS USED BY DIR
	LDA #0
	STA DIRSTT
	STA DIRSTT + 1;DIRSTT:=0(START ADDR IN DIR)
	JSR SETDIR
	LDA #1;STRMAN.DISC ADDR TO STORE ADDR
	JSR SETRTN
	JSR STRMAN
	BNE LDDRZ
	LDY #ARGB
	LDA (NEWARG),Y
	STA DIRSTA
	STA DPTRLD
	INY
	LDA (NEWARG),Y
	STA DIRSTA + 1
	STA DPTRLD + 1;DPTRLD:=PTR TO 1ST BYTE OF DIR
	LDA #0;RC:=0
.LDDRZ
	RTS
}

.LOADDR
{
	JSR LDDR;TRY AND LOAD THE DIR
	BNE LOADRZ

	;SEE IF LEADING AND TRAILING SEQUENCE NUMBERS MATCH
	;IN THE PROCESS SETUP DPTRTR (PTR TO LAST BYTE OF DIR)

	LDY #DRSQNO
	LDA (DPTRLD),Y
	TAX ;X:=LEADING SQ. NO.
	SEC
	LDA DPTRLD
	SBC #1
	STA DPTRTR
	LDA DPTRLD + 1
	SBC #0
	STA DPTRTR + 1;DPTRTR:=DPTRLD-1
	CLC
	LDA DPTRTR
	ADC DIRSZE
	STA DPTRTR
	LDA DPTRTR + 1
	ADC DIRSZE + 1
	STA DPTRTR + 1;DPTRTR:=PTR TO LAST BYTE OF DIR
	TXA
	LDY #0
	CMP (DPTRTR),Y;A:=TRAILING SQ. NO.
	BEQ LOADRY
	LDA #DRERRB;BROKEN DIR
	JMP UNLOCK
.LOADRY
	LDY #ARGA
	LDA (NEWARG),Y;RC
.LOADRZ
	RTS
}

.SETDIR
	LDA #LO(DRDISC);MOVE DISC NO->DIRBKS ONTO NEWARG STACK
	STA MOVFRM
	LDA #HI(DRDISC)
	STA MOVFRM + 1
	CLC
	LDA NEWARG
	ADC #ARGB
	STA MOVTO
	LDA NEWARG + 1
	ADC #0
	STA MOVTO + 1
	LDX #9
	JMP MOVE

.STAARG
	LDA DIRSTA;MOVE DIRSTA ONTO ARG STACK FOR STRMAN
	LDY #ARGB
	STA (NEWARG),Y
	STA GENPTR;SETUP GENPTR FOR MRKDRT RTN
	LDA DIRSTA + 1
	INY
	STA (NEWARG),Y
	STA GENPTR + 1
	RTS

.UNLOCK
{
	PHA
	LDA #3;CALL STRMAN.UNLOCK
	JSR SETRTN
	JSR STAARG
	JSR STRMAN
	BEQ UNLKZ
	JSR INTERR
.UNLKZ
	PLA
	RTS
}

.ENSRIT
	LDA #2;CALL STRMAN.ENSURE FOR DIR
	JSR SETRTN
	JSR STAARG
	JSR MRKDRT
	LDY #ARGD
	LDA #UNLKIT
	STA (NEWARG),Y;ENSURE AND UNLOCK THE OBJECT

	;NOW INC SEQ. NOS.

	LDY #DRSQNO
	LDA (DPTRLD),Y
	TAX
	INX
	TXA ;A:=NEW SQ NO
	STA (DPTRLD),Y;1ST SQ NO
	LDY #0
	STA (DPTRTR),Y;LST SQ NO

	JMP STRMAN

	;RETDIR: RETRIEVE DIR BEFORE LAST COMPONENT OF FILE TITLE.

	;EXIT: A = RC
	;NAMPTR = PTR TO LAST TEXT NAME.
	;DIRSTA = ADDR OF DIR IN MEMORY

	;FUNCTION: IF SYNTAX OF FILE TITLE OK
	;THEN RETRIEVE PENULTIMATE DIR
	;FI

.RETDIR
{
	JSR CHKTIT;CHECK SYNTAX OF FILE TITLE
	BNE RETDRZ

	;** 11/4/84 **
	;attempts to delete "$" fail in eyecatching manner

	JSR ISLNAM
	BNE RETDRA;ok if not EQ
	JSR TSTROT;look for ROOT here
	BEQ RETDRE
	JSR TSTSYM
	BNE RETDRA
	JSR TSTCR1;FOLLOWED BY TERMINATOR?
	BNE RETDRA

.RETDRE
	LDA #DRERRC;'not found', well it cant be can it !
.RETDRX
	RTS

.RETDRA
	JSR LOADDR;LOAD CURRENT DIR
	BNE RETDRX

	;IF THE FIRST NAME=ROOT THEN WE HAVE TO ADJUST NAMPTR

	JSR TSROOT;SEE IF FIRST NAME COMPONENT=ROOT
	BEQ RETDRC
	
	JSR TSTSYM
	BNE RETDRB
	INY
	LDA (NAMPTR),Y
	CMP #SEPART
	BNE RETDRB
	
.RETDRC
	CLC
	LDA NAMPTR
	ADC #2
	STA NAMPTR
	BCC RETDRB
	INC NAMPTR + 1;NAMPTR+:=2

.RETDRB
	JSR ISLNAM
	BEQ RETDRY;BRANCH IF ARRIVED AT LAST TEXT NAME

	;COPY NEXT TEXT NAME COMPONENT INTO TXTNAM
	;& MOVE NAMPTR TO NEXT COMPONENT

	JSR LOADTX
	
	JSR TSTPSP;TEST TXTNAM STARTS WITH '^' FOLLOWED BY SPACE
	BNE RETDRD;NO
	
	JSR UNLOCK
	JSR LDPAR;LOAD PARENT DIR
	BEQ RETDRB
	RTS

	;CHECK DIR EXISTS
.RETDRD
	JSR FNDTEX;SEARCH DIR FOR TEXT NAME
	BNE RETDRU

	;FIRST CHECK THAT ITS A DIR

.RETDRH
	JSR ISDIR
	BEQ RETDRJ
	JSR FNDTEZ;try on next match
	BEQ RETDRH
.RETDRU
	JMP UNLOCK;unlock the entry

	;NOW PREPARE FOR NEXT DIR TO BE LOADED, AND
	;UNLOCK THE CURRENT ONE

.RETDRJ
	JSR SETSIN;DIRSIN := SIN OF NEXT DIR TO BE LOADED
	JSR UNLOCK;UNLOCK CURRENT DIR
	JSR LOADDR
	BNE RETDRZ
	BEQ RETDRB
.RETDRY
	LDA #0
.RETDRZ
	RTS
}

.ISDIR
	LDY #DRACCS
	LDA (CRNTEN),Y;check type found
	AND #TYPDIR
	CMP #TYPDIR
	RTS
	
.ISLNAM
{
	LDA NAMPTR
	CMP LASTNM
	BNE ISLNAX
	LDA NAMPTR + 1
	CMP LASTNM + 1
.ISLNAX
	RTS
}

.RETENT
{
	JSR RETDIR;FIND APPROPRIATE DIR
	BNE RETETZ
	JSR LOADTX;COPY LAST TEXTNAME INTO TXTNAM
	LDA #&02
	BIT DIRWC
	BEQ RETETA
	
	JSR TSTPSP;TEST TXTNAM STARTS WITH '^' FOLLOWED BY SPACE
	BNE RETETA;NO
	JSR UNLOCK
	JMP LDPAR;LOAD PARENT DIR
	
.RETETA
	JSR FNDTEX;SEARCH FOR LAST TEXT NAME
	BEQ RETETZ
.RETUNL
	JSR UNLOCK
.RETETZ
	RTS

.*RETEND
	JSR RETENT
	BNE RETETZ

	JSR TSTPSP;TEST TXTNAM STARTS WITH '^' FOLLOWED BY SPACE
	BEQ RETETZ

.RETEN1
	JSR ISDIR
	BEQ RETETZ
	JSR FNDTEZ
	BEQ RETEN1
	BNE RETUNL
}

.TSTPSP;TEST TXTNAM STARTS WITH '^' FOLLOWED BY SPACE
{
	LDA TXTNAM
	CMP #'^'
	BNE TSTPSA
	LDA TXTNAM + 1
	CMP #' '
.*LDPARX
.TSTPSA
	RTS
}

.LDPAR;LOAD PARENT DIR
{
	LDY #UTPRIV
	LDA (DRUSIN),Y
	AND #SYSTPV
	BNE LDPARA;SYSTEM USER
	
	;MCNUMB = MACHINE NUMBER
	LDA UMHUFD
	JSR FNDHND
	BNE LDPARE;NOT FOUND
	
	LDA #LO(DRDISC);DISC NO. OF CURRENT DIR
	STA MOVFRM
	LDA #HI(DRDISC)
	STA MOVFRM+1
	
	CLC
	LDA HNDPTR
	ADC #HTDISC
	STA MOVTO
	LDA HNDPTR+1
	ADC #0
	STA MOVTO+1
	LDX #5
	JSR COMPAR
	BNE LDPARA;DISC NO/SINS DON'T MATCH
	
	LDA #TYPDIR;ACCESS BYTE OF 'PARENT DIR' ENTRY IS ZERO, SO SET AS DIR
	STA DIRACC
	
.LDPARA
	JSR LOADDR;LOAD CURRENT DIR
	BNE LDPARX;EXIT
	
	;FIRST DIRECTORY ENTRY
	LDY #DRSTAR;DRLINK == &FFFF?
	LDA #&FF
	CMP (DPTRLD),Y
	BNE LDPARE;NO
	INY
	CMP (DPTRLD),Y
	BNE LDPARE;NO PARENT DIR ENTRY
	
	;DIRSIN = SIN OF PARENT DIR
	LDY #DRSTAR + DRSIN
	LDA (DPTRLD),Y
	STA DIRSIN
	INY
	LDA (DPTRLD),Y
	STA DIRSIN + 1
	INY
	LDA (DPTRLD),Y
	STA DIRSIN + 2
	
	JSR UNLOCK
	JSR LOADDR
	BNE LDPARX;EXIT
	
	LDA #0;OK
	RTS
.LDPARE
	LDA #DRERRC;NOT FOUND
	JMP UNLOCK
}


.TSTDIR
{
	JSR ISDIR
	BEQ TSTDRZ
	JSR UNLOCK;UNLOCK CURRENT DIR IF ENTRY NOT FOR A DIR
	LDA #DRERRD;RC := OBJECT NOT A DIR
.TSTDRZ
	RTS
}

.SETSIN
	LDY #DRSIN;DIRSIN := SIN OF CRNTEN
	LDA (CRNTEN),Y
	STA DIRSIN
	INY
	LDA (CRNTEN),Y
	STA DIRSIN + 1
	INY
	LDA (CRNTEN),Y
	STA DIRSIN + 2
	RTS

	;MAXACC: CALC ACCESS TO A PARTICULAR DIR ENTRY
	;EXIT: A:=OBJACC:= TYPE & MAX ACCESS TO OBJECT

.MAXACC
{
	LDY #DRACCS
	LDA (CRNTEN),Y
	AND #TLAMSK
	STA OBJACC;OBJACC:=TYPE & ACCESS SPECIFIED IN DIR ENTRY
	AND #TYPDIR
	BEQ MAXACF

	;ITS A DIR SO OBJACC := TYPDIR!DIRACC

	LDA DIRACC
	AND #OWNER;** 30/9/84 **
	ORA OBJACC
	JMP MAXACZ

	;ITS A FILE SO
	;IF (DIRACC & OWNER) = OWNER
	;THEN TYPFIL!((OBJACC SHR 2)&RDWRACC)!(OBJACC&RDWRAC)
	;ELSE TYPFIL ! (OBJACC & RDWRAC)
	;FI

.MAXACF
	LDA DIRACC
	AND #OWNER
	BEQ MAXACG;BRANCH IF ANYBODY

	;OWNER ACCESS

	LDA OBJACC
	LSR A
	LSR A
	AND #RDWRAC
	TAX ;X:= A & RDWRAC
	LDA OBJACC
	AND #LOCKED;** 28/3/85 **
	STA OBJACC
	TXA
	ORA OBJACC
	JMP MAXACZ

	;ANYBODY ACCESS

.MAXACG
	LDA OBJACC
	AND #RWLACC;ANYBODY ACCESS
.MAXACZ
	STA OBJACC
	LDA DIRACC
	AND #OWNER
	ORA OBJACC;Include owner bit in result
	STA OBJACC
	RTS
}

.LOADTX
{
	LDA #LO(TXTNAM);MOVE TEXT NAME INTO TXTNAM
	STA TXTPTR
	LDA #HI(TXTNAM)
	STA TXTPTR + 1
	LDY #0
.LDTXA
	LDA (NAMPTR),Y
	CMP #TERMIN
	BEQ LDTXC
	CMP #SEPART
	BEQ LDTXC
	STA (TXTPTR),Y;MOVE CHAR INTO TXTNAM
	INY
	JMP LDTXA

	;MAKE NAMPTR PT TO 1ST CHAR AFTER <terminator>|<separator>

.LDTXC
	TYA
	SEC ;ADD 1
	ADC NAMPTR
	STA NAMPTR
	BCC LDTXF
	INC NAMPTR + 1

	;FILL REMAINDER OF TXTNAM WITH SPACES

.LDTXF
	CPY #NAMLNT+1;** 5/9/84 **
	BEQ LDTXZ
	LDA #SPACE
	STA (TXTPTR),Y
	INY
	BNE LDTXF
.LDTXZ
	RTS
}

.SETFRE
	CLC ;SETUP DIRFRE (ADDR OF FREE CHAINPTR IN DIR)
	LDA DIRSTA
	ADC #DRFREE
	STA DIRFRE
	LDA DIRSTA + 1
	ADC #0
	STA DIRFRE + 1
	LDY #0;TEST FOR [DIRFRE]=0
	LDA (DIRFRE),Y
	INY
	ORA (DIRFRE),Y;A=0 IF NO FREE ENTRIES
	RTS

	;DELCHK: RTN TO CHECK WHETHER A DIR ENTRY MAY BE DELETED

	;EXIT: A = RC

	;FUNCTION: IF ENTRY 'LOCKED'
	;THEN ERROR (I.E. A>0)
	;ELIF OBJECT OPEN
	;THEN ERROR
	;ELIF TYPE = TYPDIR ANDF NOT EMPTY
	;THEN ERROR
	;ELSE OBJECT MAY BE DELETED (I.E. A = 0)
	;FI

.DELCHK
{
	LDY #DRACCS
	LDA (CRNTEN),Y
	AND #LOCKED
	BEQ DELCKD
	LDA #DRERRG
	RTS

	;SAVE SIN OF OBJECT TO BE DELETED

.DELCKD
	JSR CRNSIN;OBJSIN := SIN OF OBJECT

	;SEE IF THE OBJECT TO BE DELETED IS OPEN

	LDA #2
	JSR STDSN2;SETUP DISC NO AND SIN ON NEWARG STACK
	JSR RNDMAN;RNDMAN.INFO
	BNE DELCKG
	LDA #DRERRI;CANNOT DELETE OPENED OBJECT
	RTS

	;IF IT IS DIR MAKE SURE THAT IT IS EMPTY

.DELCKG
	LDY #DRACCS
	LDA (CRNTEN),Y
	AND #TYPDIR
	BEQ DELCKZ

	;LOAD FIRST BLOCK OF THE DIR
	;TO SEE IF IT IS EMPTY

	LDA #1
	JSR STDSN2;PLACE DISC NO AND SIN ON NEWARG STACK
	LDY #ARGG
	LDA #0
	STA (NEWARG),Y;LS
	INY
	STA (NEWARG),Y;MS(BLK NO) START BLOCK NO := 0
	INY ;Y:=ARGI
	LDA #1
	STA (NEWARG),Y
	INY
	LDA #0
	STA (NEWARG),Y;NO OF BLOCKS TO BE LOADED := 1
	JSR STRMAN
	BNE DELCKZ
	LDY #ARGB
	LDA (NEWARG),Y
	STA GENPTR
	INY
	LDA (NEWARG),Y
	STA GENPTR + 1;GENPTR:=STORE ADDRESS OF LOADED DIR
	LDY #DRFRST;PTR TO FIRST DIR ENTRY IN USE
	LDA (GENPTR),Y
	INY
	ORA (GENPTR),Y
	PHA ;PUSH A
	LDA #4
	JSR SETRTN
	JSR STRMAN;STRMAN.FLUSH WINDOW
	BEQ DELCKQ
	TAX
	PLA
	TXA
	RTS
.DELCKQ
	PLA
	BEQ DELCKZ
	LDA #DRERRJ
.DELCKZ
	RTS
}

.FREECH
	JSR SETFRE
	LDY #0;RTN TO PLACE ENTRY ON FREE CHAIN
	LDA (DIRFRE),Y
	STA (CRNTEN),Y
	INY
	LDA (DIRFRE),Y
	STA (CRNTEN),Y;[CRNTEN] := [FREE ENTRY PTR]

	SEC ;CALC RELATIVE ADDRESS OF CRNTEN
	LDA CRNTEN
	SBC DIRSTA
	LDY #0
	STA (DIRFRE),Y
	LDA CRNTEN + 1
	SBC DIRSTA + 1
	INY
	STA (DIRFRE),Y;[DIRFRE] := DIR ENTRY
	RTS

.STDSN2
	JSR SETRTN

.SETDSN
	LDY #ARGB;PUT DISC NO & OBJSIN ON NEWARG STACK
	LDA DRDISC
	STA (NEWARG),Y
	INY
	LDA DRDISC + 1
	STA (NEWARG),Y
	INY
	LDA OBJSIN
	STA (NEWARG),Y
	INY
	LDA OBJSIN + 1
	STA (NEWARG),Y
	INY
	LDA OBJSIN + 2
	STA (NEWARG),Y
	RTS

.ARGDSN
	LDY #ARGB;PLACE DISC NO & OBJSIN ON ARGPTR STACK
	LDA DRDISC
	STA (ARGPTR),Y
	INY
	LDA DRDISC + 1
	STA (ARGPTR),Y
	INY
	LDA OBJSIN
	STA (ARGPTR),Y
	INY
	LDA OBJSIN + 1
	STA (ARGPTR),Y
	INY
	LDA OBJSIN + 2
	STA (ARGPTR),Y
	RTS

.CRNSIN
	LDY #DRSIN;OBJSIN := SIN OF CRNTEN
	LDA (CRNTEN),Y
	STA OBJSIN
	INY
	LDA (CRNTEN),Y
	STA OBJSIN + 1
	INY
	LDA (CRNTEN),Y
	STA OBJSIN + 2
	RTS

.SINSZE
	;CALL MAPMAN.SIZE FOR OBJSIN
	LDA #5;MAPMAN.SIZEINF
	JSR STDSN2
	JMP MAPMAN

.RETDSZ
	LDY #INFDIS;MOVE DISC NO & SIZE INTO INFPTR
	LDA DRDISC
	STA (INFPTR),Y
	INY
	LDA DRDISC + 1
	STA (INFPTR),Y
	JSR SINSZE
	PHA ;Record MAPMAN return code
	LDY #ARGB;MOVE SIZE ON NEWARG STACK TO INFPTR
	LDA (NEWARG),Y
	LDY #INFSZE
	STA (INFPTR),Y
	LDY #ARGC
	LDA (NEWARG),Y
	LDY #INFSZE + 1
	STA (INFPTR),Y
	LDY #ARGD
	LDA (NEWARG),Y
	LDY #INFSZE + 2
	STA (INFPTR),Y;MS(SIZE)
	PLA ;Restore MAPMAN return code
	RTS

.TSROOT
	LDY #0;TEST FOR <root>
	LDA (NAMPTR),Y
	CMP #ROOT;I.E. '$'
	RTS


.TSTROT
	JSR TSROOT;TEST FOR <root><terminator>
.TSTRT1
	BNE TSTRTZ
.TSTCR1;NEXT CHAR IS TERMINATOR
	INY
.TSTCR2;THIS CHAR IS TERMINATOR
	LDA (NAMPTR),Y
	CMP #TERMIN
.TSTRTZ
	RTS

.TSTERM;TSTERM

	;IS FILE TITLE <term> only ??

	LDY #0
	BEQ TSTCR2
	
	
.TSTPAR;TEST FOR <parent><terminator>

	LDY #0
	LDA (NAMPTR),Y
	CMP #'^'
	JMP TSTRT1
	
.TSTSYM;TEST FOR SPECIAL SYMBOL
{
	CMP #'^';Parent directory of CSD
	BEQ TSTSYX 
	CMP #'&';User's root directory
	BEQ TSTSYX 
	CMP #'%';User's lib directory
	BEQ TSTSYX 
	CMP #'@';Currently selected directory (CSD)
.TSTSYX
	RTS
}

.TSTOWN
	LDA DIRACC;TEST FOR OWNER ACCESS
	AND #OWNER
	CMP #OWNER
	BEQ TSTONZ
	LDA #DRERRE;INSUFFICIENT ACCESS
.TSTONZ
	RTS

.SETGEN
	LDA DIRSTA;GENPTR := [DIRSTA]
	STA GENPTR
	LDA DIRSTA + 1
	STA GENPTR + 1
	RTS

	;OUTPUT LOAD AND EXECUTE ADDRESSES
	;FOR THE CURRENT ENTRY.

.OUTLEX
	LDY #DRLOAD
	JSR OUTHEX
	JSR OUTSP
	LDY #DREXEC
	JSR OUTHEX
	LDX #3
	JMP OUTSPS;SPACE AND EXIT

;.LNK
;UADE0E
;
