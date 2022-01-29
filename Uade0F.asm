
;.OPT
;UADE0F;> Uade0F
;.TTL
;Fileserver file UADE0F

.UADE0F

	;**********************
	;*    A U T M A N     *
	;**********************

	;AUTMAN: THE AUTHENTICATION MANAGER
	;THE PASSWORD FILE CONTAINS A NUMBER
	;OF ENTRIES, EACH OF WHICH HAS THE FOLLOWING FORM:-
	;0) USERID (10 BYTES)
	;1) PASSWORD (6 BYTES)
	;4) Free space (4 bytes)
	;3) FLAG (1 BYTE) - INUSE & PRIVILEGE FLAG


APWIND =  4;window size in blocks

	;***** MAIN ROUTINE *****

.AUTMAN
{
	LDX #10;AUTMAN HAS 10 ENTRY POINTS
	LDA #MODAUT;A:=NAME OF THIS MODULE
	JSR ENTRY
	LDA ATRTNS,X
	STA ATJUMP+1
	LDA ATRTNS+1,X
	STA ATJUMP+2
	LDA #0
	STA ATDRIV
	LDA #9
	STA ATWORK+2;** 31/10/83
	;<>0 -> dont keep window with empty slot
	;unset in ATNWUS
	JSR SETFS
.ATJUMP
	JMP ATJUMP
.ATRTNS
	EQUW ATNWUS;1 => NEW USER
	EQUW ATCKPW;2 => CHECK PASSWORD
	EQUW ATSTPW;3 => SET PASSWORD
	EQUW ATDLUS;4 => DELETE USER
	EQUW ATSPRV;5 => SET PRIVILEGE
	EQUW ATREST;6 => RESTART
	EQUW ATSOPT;7 => SET "USER OPTION"
	EQUW ATENS;8 => Update user disc space in PW file ** 2/10/84 **
	EQUW ATFREE;9 => Return user disc space
	EQUW ATWRIT;10=> Write user disc space
}
.ATEXIT
	JMP EXIT

.PWTITL
.PWFILE
	EQUB "$.PASSWORDS",CR

	;***** ATNWUS *****

	;ATNWUS: NEW USER - MAKE AN ENTRY IN THE PASSWORD FILE

	;ENTRY:
	;ARGB = LS(PTR TO USERINFO)
	;ARGC = MS(PTR TO USERINFO)
	;ARGD = LS(PTR TO USERID)
	;ARGE = MS(PTR TO USERID)

	;EXIT:
	;ARGA = RC

	;FUNCTION:-
	;0) CHECK THAT CALLER HAS SYSTEM PRIV
	;1) CHECK THAT USERID DOES NOT EXIST ALREADY
	;2) CHECK SYNTAX OF USERID
	;3) MAKE AN ENTRY IN PW FILE

{
.ATNWSG
	STA ATWORK+2
	JSR APENRG
	BNE ATEXIT

.*ATNWUS
	LDA #0
	STA DOTFLG
	JSR CHKPRV;CHECK THAT CALLER HAS SYSTEM PRIV
				;GENPTR:=PTR TO USERINFO
	BNE ATEXIT

	JSR SETAUT;AUTPTR:=PTR TO USERID

	;SEE IF USERID EXISTS ALREADY

	LDA #0
	STA ATDRIV
	JSR FUSIDD
	BNE ATNWSA

	LDA #ATERRF;USERID ALREADY IN USE
	BNE ATNWXA
	
.ATNWSA
	LDA #0
	STA ATWORK+2
	STA ATDRIV
	JSR CHKPRV;GENPTR:=PTR TO USERINFO
	LDY #UTDISC;ARGA,B=DISC NO.
	LDA (GENPTR),Y
	TAX
	INY
	LDA (GENPTR),Y
	LDY #ARGC
	STA (NEWARG),Y
	TXA
	DEY;Y:=ARGB
	STA (NEWARG),Y
	LDA #15
	JSR SETRTN
	JSR MAPMAN;FIND DRIVE WITH DISC NO.
	BNE ATNWSD
	LDY #ARGB
	LDA (NEWARG),Y
	STA ATDRIV
	
.ATNWSD
	JSR FUSIDD
	BEQ ATNWSB
	CMP #ATERRB;User ID not found
	BEQ ATNWSG
	BNE ATEXIT

	;CHECK SYNTAX OF USERID

.ATNWSB
	LDY #0
	STY DOTFLG
.DOTLP
	LDX #MAXID
	LDA (AUTPTR),Y;Check first char is alpha only
	JSR ISCHAR
	BCS ATNWSR;Nope -> error

.ATNWSC
	LDA (AUTPTR),Y
	JSR ISDCHA;Is digit or char
	BCS ATNWSF;Nope
	INY
	DEX
	BNE ATNWSC

	LDA (AUTPTR),Y;Check char after user id. is terminator
.ATNWSF
	CMP #CR
	BEQ ATNWSH; => continue

	CMP #DOT
	BNE ATNWSR

	LDA DOTFLG
	BNE ATNWSR
	INC DOTFLG

	CPY #MAXID+1;should have had a "." before this
	BCS ATNWSR
	INY
	BCC DOTLP

.ATNWSR
	LDA #ATERRG;SYNTAX ERROR IN USERID
.ATNWXA
	JMP ATNWSX

	;SEE IF THERE IS ANY ROOM IN PW FILE

.ATNWSH
	TYA ;Check if ptr. zero
	BEQ ATNWSR; => bad user id.
	EOR #MAXUNM
	STA DOTFLG;0=> maximum length

	LDY #PWUSID
.ATNWSN
	LDA (AUTPTR),Y;Set userid (terminated CR)
	CMP #DOT
	BNE ATNIND

	LDX DOTFLG
	BNE ATNIND

	LDA PWFPTR
	BNE ATNWSP
	DEC PWFPTR + 1
.ATNWSP
	DEC PWFPTR
	JMP ATNDID

.ATNIND
	STA (PWFPTR),Y
	CMP #CR
	BEQ ATNWSQ;Check for end of userid
.ATNDID
	INY
	BNE ATNWSN

.ATNWSQ
	LDA DOTFLG
	BNE ATNWPI

	INC PWFPTR
	BNE ATNWPI
	INC PWFPTR+1

.ATNWPI
	LDY #PWPASS
	LDA #TERMIN
	STA (PWFPTR),Y;PASSWORD = ""

	LDY #PWFREE;space allocation always maximum
	LDA #4
	LDX #UTFRLN-1
.ATNWFR
	STA (PWFPTR),Y;** 2/10/84 **
	INY
	DEX
	BNE ATNWFR
	TXA
	STA (PWFPTR),Y

	LDY #PWFLAG
	LDA #INUSE
	STA (PWFPTR),Y
	JSR MARKDT;MARK PW FILE DIRTY
	LDA #0
.ATNWSX
	JSR APENFL
	JMP ATEXIT
}

	;***** ATCKPW *****

	;ATCKPW: CHECK PASSWORD

	;ENTRY:
	;ARGB & ARGC = SPARE
	;ARGD = LS(PTR TO USERID FOLLOWED BY PW)
	;ARGE = MS(PTR TO USERID FOLLOWED BY PW)

	;EXIT:
	;ARGA = RC
	;ARGB-E = LS User disc space ** 2/10/84 **
	;ARGF = FLAG BYTE FROM PW FILE

.ATCKPW
{
	JSR SETAUT;AUTPTR:=PTR TO USERID
	JSR FUSIDD;SEARCH PW FILE FOR USERID
	BNE ATCKWX

	JSR ATFIND;is user already logged on
	BCC ATCKPJ;yes

	LDA #PWFREE
	LDX #PWFPTR
	BNE ATCKPK

.ATCKPJ
	JSR ATFPTR;make pointer
	LDA #UTFREE
	LDX #GENPTR

.ATCKPK
	STA OFF1
	LDY #ARGPTR
	LDA #ARGB
	STA OFF2
	LDA #4
	JSR MOVBLK;general move routine

	LDY #PWFLAG
	LDA (PWFPTR),Y
	LDY #ARGF
	STA (ARGPTR),Y
	
	INY
	LDA ATDRIV
	STA (ARGPTR),Y

	;NOW SEE IF PASSWORDS MATCH

	JSR CHEKPW
	JSR APENFL

.ATCKWX
	JMP ATEXIT
}

	;***** ATSTWP *****

	;ATSTPW: SET PASSWORD

	;ENTRY:
	;ARGB = LS(PTR TO USERINFO)
	;ARGC = MS
	;ARGD = LS(PTR TO OLD PW FOLLOWED BY NEW PW)
	;ARGE = MS

	;EXIT:
	;ARGA = RC

	;FUNCTION:- IF OLD PW MATCHES
	;EXISTING PW IN PW FILE THEN CHANGE PW.

.ATSTPW
{
	JSR SETAP1;Set AUTPTR to point to user id.
	JSR FUSIDD;SEARCH PW FILE FOR USERID
	BNE ATSTWY

	;NOW SEE IF OLD PW MATCHES THAT IN PW FILE

	LDY #ARGD
	LDA (ARGPTR),Y
	STA AUTPTR
	INY
	LDA (ARGPTR),Y
	STA AUTPTR+1;AUTPTR USED BY CHEKPW
	LDY #0;Offset for first pw
	JSR CHEKP1
	BNE ATSTWX

	;CHECK SYNTAX OF NEW PW

	JSR STEPUR;Step Y past first PW
	STY OFF1;OFFSET FOR START OF NEW PW
	LDX #MAXPW
.ATSTWD
	LDA (AUTPTR),Y
	CMP #TERMIN
	BEQ ATSTWG
	JSR ISDCHA;Is digit or character ?
	BCS ATSTWF;Nope => error

	INY
	DEX
	BNE ATSTWD
	LDA (AUTPTR),Y
	CMP #TERMIN
	BEQ ATSTWG;SYNTAX OF PW OK
.ATSTWF
	LDA #ATERRE
	BNE ATSTWX

	;NOW COPY NEW PW INTO PW FILE

.ATSTWG
	LDA #PWPASS;Copy new PW into PW file
	STA OFF2;Note OFF1 already set
	LDX #AUTPTR;"From" ptr.
	LDY #PWFPTR;"To" ptr.
	LDA #MAXPW
	JSR MOVBLK

	JSR MARKDT;MARK PW FILE STORE BUFFER DIRTY
	LDA #0
.ATSTWX
	JSR APENFL
.ATSTWY
	JMP ATEXIT
}

	;***** ATDLUS *****

	;ATDLUS: DELETE USER FROM PASSWORD FILE

	;ENTRY: ARGB = LS(PTR TO USERINFO)
	;ARGC = MS(PTR TO USERINFO)
	;ARGD = LS(PTR TO USERID)
	;ARGE = MS(PTR TO USERID)

	;EXIT:  ARGA = RC

.ATDLUS
{
	JSR CHKPRV;SEE IF CALLER HAS SYSTEM PRIV
	BNE ATDLSY

	JSR SETAUT;AUTPTR:=PTR TO USERID
	JSR FUSIDD;SEARCH PW FILE FOR USERID
	BNE ATDLSY

	;NOW CLEAR PASSWORD FILE ENTRY

	LDX #PWENSZ
	LDY #0
	TYA
.ATDLSB
	STA (PWFPTR),Y
	INY
	DEX
	BNE ATDLSB
	JSR MARKDT;MARK PW FILE BUFFER DIRTY
	LDA #0
	JSR APENFL

.*ATDLSY
	JMP ATEXIT
}

	;***** ATSPRV *****

	;ATSPRV: SET PRIVILEGE OF A SPECIFIED USER

	;ENTRY:
	;ARGB = LS(PTR TO USERINFO)
	;ARGC = MS(PTR TO USERINFO)
	;ARGD = LS(PTR TO USERID)
	;ARGE = MS(PTR TO USERID)
	;ARGF = PRIV. FLAG:
			;1 => system privileges
			;2 => no privileges
			;else normal user privileges

	;EXIT:  ARGA = RC

{
.ATSTBL
	EQUB 0, SYSTPV, LOCKPV, 0
	
.*ATSPRV
	JSR CHKPRV;CHECK THAT CALLER HAS SYSTEM PRIV
	BNE ATDLSY;NO!

	JSR SETAUT;AUTPTR:=PTR TO USERID
	JSR FUSIDD;PWFPTR:=PW FILE ENTRY FOR GIVEN USER
	BNE ATDLSY

	LDY #ARGF
	LDA (ARGPTR),Y;Load priv. flag
	AND #3
	TAY
	LDA ATSTBL,Y
	STA ATWORK+2
	
	LDY #PWFLAG
	LDA (PWFPTR),Y
	AND #NTSYST;Mask off SYST bit
	JMP ATENPW;OR in ATWORK+2, ensure PW file
}

	;***** ATREST *****

	;ATREST: AUTMAN.RESTART
	;FINDOUT THE DISC NUMBER OF THE DISC
	;WHICH CONTAINS THE PASSWORD FILE.
	;NOTE IF PW FILE NOT FOUND A <>0 RC IS RETURNED

	;EXIT:
	;ARGA = RC

.ATREST
{
	LDA #LO(ATUSRI)
	STA ATUSPT
	LDA #HI(ATUSRI)
	STA ATUSPT+1;ATUSPT := PTR TO AUTMAN'S USERINFO AREA

	LDA #ATERRA;CANNOT FIND PASSWORD FILE
	STA ATWORK+2

	LDY #UTPRIV
	LDA #(SYSTPV OR INUSE)
	STA (ATUSPT),Y;AUTMAN HAS SYSTEM PRIV
	LDA #0
	STA ATDRIV;DRIVE NUMBER
.ATRSTB
	LDA ATDRIV;A:=DRIVE NUMBER
	JSR DRVINF;MAPMAN.DRIVEINFO
	BNE ATRSTG
	JSR RETPWF
	BEQ ATRSTD
	CMP #DRERRC;IF RC=CANNOT FIND OBJECT THEN TRY NEXT DRIVE
	BEQ ATRSTG
	JSR INTERR
.ATRSTD
	LDA #0
	STA ATWORK+2
.ATRSTG
	INC ATDRIV;ATDRIV+:= 1
	
	LDA ATDRIV
	CMP DRIVES
	BCC ATRSTB
	LDA ATWORK+2
	JMP ATEXIT
}

	;***** ATSOPT *****

	;Set user option bits in PW flag (bottom two bits)

	;ENTRY:
	;ARGB/C = ptr. to user info (lo/hi)
	;ARGD   = new option bits (bottom two bits)

	;EXIT :
	;ARGA   = RC

.ATSOPT
{
	JSR SETAP1;Set AUTPTR -> uRV id.
	JSR FUSIDD;Find user id. in pw file
	BNE ATSOEX
	LDY #ARGD
	LDA (ARGPTR),Y
	AND #OPTMSK;Mask off option bits
	STA ATWORK+2
	LDY #PWFLAG
	LDA (PWFPTR),Y;Read PW flag byte
	AND #NTOPT;Mask off all but option bits

.*ATENPW

	;Exit code shared by ATSPRV

	ORA ATWORK+2;OR in option bits
	CMP (PWFPTR),Y
	BEQ ATSOEZ
	STA (PWFPTR),Y
	JSR MARKDT;Mark PW file dirty
.ATSOEZ
	LDA #0;Return code (restored over ENSPWF)
	JSR APENFL
.*ATSOEX
	JMP ATEXIT
}

	;***** ATENS *****

	;ensure that user disc allocation is up to date
	;on disc.

	;ENTRY:
	;ARGB,C unset
	;ARGD,E pointer to user id
	;ARGF,G,H,I value to write

.ATENS
{
	JSR SETAUT;point to userid
	JSR FUSIDD;lookup the name
	BNE ATFREX

	LDA #ARGF
	STA OFF1
	LDA #PWFREE
	STA OFF2
	LDX #ARGPTR
	LDY #PWFPTR
	LDA #0;SET POINTERS, BUT DON'T MOVE YET
	JSR MOVBLK;copy the data
	LDX #UTFRLN
	JSR COMPAR
	BEQ ATENSB;NO CHANGE
	LDX #UTFRLN
	JSR MOVE;MOVE NOW
	JSR MARKDT
.ATENSB
	JSR APENLV;unlock PW file
	LDA #0
	BEQ ATFREX;write the file back to the disc
}

	;***** ATFREE *****

	;ENTRY:
	;ARGB,C ptr to callers info
	;ARGD,E ptr to user name

	;EXIT:
	;ARGB,C,D,E user free space

.ATFREE
{
	JSR ATFIND;is user logged on
	BNE ATFREX;some error here
	BCC ATFREA;user logged on

	JSR SETAUT
	JSR FUSIDD
	BNE ATFREX;not found

	LDA #PWFREE
	STA OFF1
	LDX #PWFPTR
	SEC
	BCS ATFREB

.ATFREA
	JSR ATFPTR;grasp pointer from stack
	LDA #UTFREE
	STA OFF1
	LDX #GENPTR
	CLC

.ATFREB
	LDA #ARGB
	STA OFF2
	LDY #ARGPTR

	PHP
	LDA #4
	JSR MOVBLK;copy the info to the appropriate place
	PLP
	LDA #0
	BCC ATFREX
	JSR APENFL

.*ATFREX
	JMP ATEXIT;and return to caller
}


	;***** ATWRIT *****

	;ENTRY:
	;as for ATFREE+ ARGF-I value to write

.ATWRIT
{
	JSR CHKPRV
	BNE ATWRIX;check for priv

	LDY #ARGD
	LDA (ARGPTR),Y
	TAX
	INY
	LDA (ARGPTR),Y
	LDY #ARGC
	STA (NEWARG),Y
	DEY
	TXA
	STA (NEWARG),Y;pointer to name

	LDA #ARGF
	STA OFF1
	LDX #ARGPTR
	LDA #ARGD
	STA OFF2
	LDY #NEWARG
	LDA #4
	JSR MOVBLK

	LDA #9
	JSR SETRTN;call update user free space
	JSR USRMAN
	BNE ATWRIX

	JSR SETAUT
	JSR FUSIDD
	BNE ATWRIX;not found

	LDA #PWFREE;area to copy to
	STA OFF2
	LDY #PWFPTR
	LDA #ARGF
	STA OFF1
	LDX #ARGPTR

	LDA #4
	JSR MOVBLK

	JSR MARKDT;indicate that PW file is dirty
	LDA #0
	JSR APENFL

.ATWRIX
	JMP ATEXIT
}

.ATFPTR
	LDY #ARGF;setup Zpage pointer from ARGF
	LDA (NEWARG),Y
	STA GENPTR
	INY
	LDA (NEWARG),Y
	STA GENPTR+1
	RTS

.ATFIND
{
	LDX #4;EXIT Z=1 => ok user: C=1 => not logged on
	LDY #ARGB
.ATFINA
	LDA (ARGPTR),Y
	STA (NEWARG),Y
	INY
	DEX
	BNE ATFINA;copy stack for USRMAN

	LDA #6;find user
	JSR SETRTN
	JSR USRMAN
	CLC
	BEQ ATFINB;user found in table

	CMP #URERRE;'not logged on'
.ATFINB
	RTS
}

.SETAUT
	LDY #ARGD
	LDA (ARGPTR),Y
	STA AUTPTR
	INY
	LDA (ARGPTR),Y
	STA AUTPTR+1;AUTPTR:=ADDRESS OF USERID
	RTS

.CHKPRV
{
	LDY #ARGB
	LDA (ARGPTR),Y
	STA GENPTR
	INY
	LDA (ARGPTR),Y
	STA GENPTR+1
	LDY #UTPRIV
	LDA (GENPTR),Y
	AND #SYSTPV
	BNE CHKPVX
	LDA #ATERRD;INSUFFICIENT PRIV
	RTS
.CHKPVX
	LDA #0
	RTS
}

	;RETPWF: RETRIEV PW FILE FROM DISC (ATUSRI HOLDS DISCNO)

	;FIRST SETUP ARGS FOR DIRMAN.RETRIEVE
{
.RETPTB
	EQUB 2;DIRMAN.RETRIEVE
	EQUW ATUSRI;userinfo
	EQUW PWTITL;PW file title
	EQUW ATINF;result area
	EQUB 0
	
.*RETPWF
	LDY #ARGB
	LDA (NEWARG),Y
	LDY #UTDISC
	STA (ATUSPT),Y
	LDY #ARGC
	LDA (NEWARG),Y
	LDY #UTDISC + 1
	STA (ATUSPT),Y

	LDY #ARGA
.RETPWL
	LDA RETPTB -ARGA,Y
	STA (NEWARG),Y
	INY
	CPY #ARGI
	BNE RETPWL

	JSR DIRMAN
	BNE RETPFZ
	LDY #ARGB
	LDA (NEWARG),Y
	AND #TYPDIR
	CMP #TYPFIL
	BEQ RETPFY
	LDA #ATERRI;$.PASSWORDS WRONG TYPE
	JSR INTERR
.RETPFY
	LDA #0;RC:=0
.RETPFZ
	PHP
	CMP #MPERRA;DISC NOT FOUND
	BNE RETPFX
	LDA #ATERRA
.RETPFX
	PLP
	RTS


}

.DSCSIN
	JSR SETRTN
	LDY #ARGB;PLACE DISC NO & SIN ONTO NEWARG STACK
	LDA ATINF+INFDIS;disc number (lo)
	STA (NEWARG),Y
	INY
	LDA ATINF+INFDIS+1;disc number (hi)
	STA (NEWARG),Y
	INY
	LDA ATINF+INFSIN
	STA (NEWARG),Y;SIN (ls)
	INY
	LDA ATINF+INFSIN+1
	STA (NEWARG),Y;SIN (cs)
	INY
	LDA ATINF+INFSIN+2
	STA (NEWARG),Y;SIN (ms)
	RTS

.MARKDT
	LDA ATSTRA
	STA GENPTR
	LDA ATSTRA+1
	STA GENPTR+1
	JMP MRKDRT
	
	
	;FUSIDD: SEARCH THE PW FILE FOR GIVEN USERID ACROSS DISCS
	
	;AUTPTR:=PTR TO USERID
	;SEARCH PW FILE FOR USERID
	
	;ENTRY:
	;IF ATWORK?2 == 0, ONLY CHECK ATDRIV
	
	;EXIT:
	;AS FDUSID
.FUSIDD
{
	LDA ATDRIV;A:=DRIVE NUMBER
	LDY ATWORK+2
	BNE FUSIDA
	DEY ;Y:=&FF
	STY ATDRIV

.FUSIDA
	JSR DRVINF;MAPMAN.DRIVEINFO
	BEQ FDUSID;IF OK
	
.*FUSIDB
	INC ATDRIV
	LDA ATDRIV
	CMP DRIVES
	BCC FUSIDA;TRY NEXT DRIVE

	LDA #ATERRB;userid not found error
	RTS
}
	

	;FDUSID: SEARCH THE PW FILE FOR GIVEN USERID

	;ENTRY:
	;AUTPTR = POINTER TO USERID

	;EXIT:
	;A = RC
	;PWFPTR = PTR TO RELEVENT ENTRY IN PW FILE
	;ATNWEN = PTR TO FIRST FREE PW FILE ENTRY(=0=>NONE)
	;**** 11/4/83 ****

.FDUSID
{
	JSR APINIT;get first block of entries
	BEQ FDUSDB
	CMP #DRERRC
	BEQ FUSIDB

.FDUSDB
	LDA #0;initialise the flag for every lookup
	STA DOTFLG

	LDY #PWFLAG
	LDA (PWFPTR),Y
	AND #INUSE;IS THE ENTRY IN USE?
	BEQ FDUNDK

	LDA ATWORK+2
	BEQ FDUSDH

	LDY #PWUSID
	STY TEMPA;use registers to hold pointers
	STY TEMPB;pointer into PW file
.FDUSDC
	LDY TEMPA
	LDA (AUTPTR),Y
	JSR ISCHAR;C=1 -> not alpha
	LDY TEMPB
	EOR (PWFPTR),Y;Compare with PW file
	BCS FDUSJW
	AND #&DF;Compare with case forcing
.FDUSJW
	BNE FDUSDH

	LDA (PWFPTR),Y;Here are equal, so check if are terminators
	CMP #TERMIN
	BEQ FDUSDJ;Both terminators => found

	CPY #PWPASS -1; *** LH  24/10/85
	BNE FDUSDK;Not last pair so continue
	LDY TEMPA;Check USERID terminated
	INY
	LDA (AUTPTR),Y
	CMP #TERMIN
	BEQ FDUSDJ;End and terminated
	BNE FDUSDH;Not terminated so step on

.FDUSDK
	INC TEMPA
	INC TEMPB
	LDY TEMPA
	LDA (AUTPTR),Y
	CMP #DOT
	BNE FDUNDD
	INC DOTFLG

	CPY #MAXID;only skip if the dot could have been omitted
	BNE FDUNDD;from the PW file

	LDY TEMPB;just look for "."
	EOR (PWFPTR),Y
	BNE FDUNDE;dont adjust if it is there
	INC TEMPB
.FDUNDE
	INC TEMPA;step past in AUTPTR, not PWFPTR

.FDUNDD
	LDY TEMPA
	CPY #MAXID+1;check that we have had a dot in first 11 chars
	BNE FDUSDC;  LH 24/10/85  check next match

	LDA DOTFLG
	BEQ FDUSDH;should have found one by now
	BNE FDUSDC;Dot found so continue

.FDUSDJ
	LDA #0;FOUND IT
.FDUNDZ
	RTS

.FDUNDK
	LDA ATWORK+2
	BEQ FDUNDZ
	
	;NOW MAKE PWFPTR POINT TO THE NEXT ENTRY

.FDUSDH
	JSR APTEST;check for end-of-window
	BNE FDUSDS;go again if not
	JSR APEND;check end-of-file
	BEQ FDUEOF
	CMP #LO(-1);check alternative RC
	BNE FDUNDZ;return the error

	JSR APNEXT;get next window
	BNE FDUNDZ
.FDUSDS
	JMP FDUSDB
	
	;EOF
.FDUEOF
	LDA #0
	JSR APENFL
	BNE FDUNDZ
	LDA ATWORK + 2
	BEQ FDUSFE
	JMP FUSIDB

.FDUSFE
	LDA #ATERRB;userid not found error
	RTS ;else return to caller
}

	;CHEKPW: CHECK PASSWORD SUPPLIED BY THE USER WITH
	;THAT IN THE PASSWORD FILE.

	;ENTRY: AUTPTR = PTR TO USERID (TERMINATED CR) FOLLOWED BY PW.
	;PWFPTR = PTR TO PW FILE ENTRY.

	;EXIT:  A = RC

.CHEKPW
	JSR STEPUR;Step Y past userid. to password
.CHEKP1;Entry from SETPASSWORD (Y already set)
{
	LDA #PWPASS;Set up offsets for PW comparison
	STA OFF2

.CHEKWB
	LDA (AUTPTR),Y
	STY OFF1
	LDY OFF2;Offset into PW file
	CPY #PWPASS+MAXPW;Check if reached end of pw in file
	BEQ CHEKWD;If so, check that pw from AUTPTR is terminated

	EOR (PWFPTR),Y
	INC OFF2;Increment PW ptr. for next time
	LDY OFF1;Get offset from AUTPTR
	INY ;Increment for next time
	AND #&DF;Complete comparison
	BNE CHEKWC;Not the same -> exit with error

	DEY ;Point to char. off AUTPTR
	LDA (AUTPTR),Y;The same -> check if end of string
	INY ;Restore Y in case going back to CHEKWB
	SEC
	SBC #TERMIN
	BNE CHEKWB
	RTS
	
.CHEKWD
	SEC
	SBC #TERMIN
	BEQ CHEKWZ;Both are terminators => exit with zero

.CHEKWC
	LDA #ATERRC;=> not found
.CHEKWZ
	RTS

.*STEPUR
	LDY #0
.CHEKWA
	LDA (AUTPTR),Y
	INY
	CMP #TERMIN;Move Y to point to posn. after userid
	BNE CHEKWA;Not there yet
	RTS
}

.SETAP1;Set pointer to callers user id
	CLC
	LDY #ARGB
	LDA (ARGPTR),Y
	ADC #UTUSID
	STA AUTPTR
	INY
	LDA (ARGPTR),Y
	ADC #0
	STA AUTPTR+1;AUTPTR:=PTR TO CALLER'S USERID
	RTS

	;**** Start of PW paging routines ****

	;start pointer := 0
	;end pointer := 0
	;current pointer := 0 ;initialisation of variables

	;REPEAT
	;  load window ( end pointer DIV 256, window size)
	;  End pointer+:= amount loaded
	;  Current pointer := Current pointer REM 256
	;  REPEAT
	;    perform operation (current pointer)
	;    start pointer+:= entry size
	;    current pointer+:= entry size
	;  UNTIL end pointer - start pointer < entry size OR ...
	;  IF end pointer <> start pointer THEN end pointer -:= 256
	;UNTIL end pointer >= file size OR ...

	;**** Initialisation of variables ****

.APINIT
	LDA #0
	STA STRPTR
	STA STRPTR+1;start := 0
	STA PTREND
	STA PTREND+1;end := 0
	STA PWFPTR
	STA PWFPTR+1;current := 0
	JSR RETPWF;read the password file info
	BEQ APNEX1;first call has no window to release
	CMP #DRERRC
	BNE APNEX1
	TAX
	RTS

	;**** Grab next chunk of PW file ****

.APNEXT
{
	LDA #0;stiff entry code
	JSR APENFL;release the old window
	BNE APNL6

.*APNEX1
	LDA #1;Disc address -> store address
	JSR DSCSIN;put disc number & SIN onto NEWARG stack
	INY ;Y:=ARGG
	LDA STRPTR+1
	STA (NEWARG),Y;logical block (lo)
	INY
	LDA #0
	STA (NEWARG),Y;logical block (hi)

	INY
	SEC
	LDA ATINF+INFSZE+1
	SBC STRPTR+1;get number of blocks remaining
	BNE APNL1;detect zero
	LDA #1
	BNE APNL2;skip next bit
.APNL1
	CMP #APWIND
	BCC APNL2;<= window size left
	LDA #APWIND
.APNL2
	STA (NEWARG),Y;no of blocks (lo)

	INY
	LDA #0;no of blocks (hi)
	STA (NEWARG),Y

	JSR STRMAN;do the call

	PHA ;save RC
	CLC
	LDY #ARGB
	LDA (NEWARG),Y;get store address
	STA ATSTRA
	ADC STRPTR
	STA PWFPTR
	INY
	LDA (NEWARG),Y
	STA ATSTRA+1;two bytes
	ADC #0
	STA PWFPTR+1

	INY
	LDA (NEWARG),Y
	STA GENPTR
	INY
	LDA (NEWARG),Y
	STA GENPTR+1;get number of blocks loaded

	LDA STRPTR;look for runnimg over page boundary
	CMP #LO(1-PWENSZ)
	BCC APNL5
	DEC PTREND+1

.APNL5
	LDY #CEBLKS;from cache descriptor
	LDA (GENPTR),Y
	CLC
	ADC PTREND+1
	STA PTREND+1;update PTREND by this value

	PLA
.APNL6
	RTS ;return code from STRMAN
}

	;****   Update counters, test end condition   ****
	;**** Z-> end condition met ****

.APTEST
{
	CLC
	LDA #PWENSZ
	ADC STRPTR
	STA STRPTR;strptr+:= entry size
	BCC APTL1
	INC STRPTR+1

.APTL1
	CLC
	LDA #PWENSZ
	ADC PWFPTR
	STA PWFPTR;curptr+:= entry size
	BCC APTL2
	INC PWFPTR+1

.APTL2
	SEC
	LDA PTREND
	SBC STRPTR
	TAX ;save result
	LDA PTREND+1
	SBC STRPTR+1
	BNE APTL3;not zero condition
	TXA
	BEQ APTL3;start=end
	CMP #PWENSZ
	LDX #LO(-1);unset Z if necessary
	BCS APTL3
	LDA #0
.APTL3
	RTS
}

	;****  test for end of PW file ****

.APEND
{
	LDX #LO(-1)
	LDA PTREND
	CMP ATINF+INFSZE
	BNE APENDX
	LDA PTREND+1
	CMP ATINF+INFSZE+1
	BCC APENDX

	INX
.APENDX
	TXA ;set code
	RTS ;Z shows condition
}


.APENLV
{
	PHA
	LDA #3;STRMAN.unlock window
	BNE APENC;but leave it in cache

.*APENFL
	PHA
	LDA #4;STRMAN.flush old window

.APENC
	JSR SETRTN
	INY ;Y:=ARGG
	LDA ATSTRA;store address
	STA (NEWARG),Y
	INY
	LDA ATSTRA+1
	STA (NEWARG),Y
	INY
	LDA #LO(-1)
	STA (NEWARG),Y;decrement reference count (2)
	JSR STRMAN;make call
	TAX
	PLA
	BNE APENSY
	TXA
.APENSY
	RTS
}

.APENRG
{
	;enlarge the password file
	LDA #14;MAPMAN.changesize without space check
	JSR DSCSIN;put disc number & SIN onto NEWARG stack

	INY ;Y:=ARGG
	LDA #0
	STA (NEWARG),Y;new size (lo)
	CLC
	INY
	LDA ATINF+INFSZE+1
	ADC #1
	STA (NEWARG),Y;new size (mid)
	INY
	LDA ATINF+INFSZE+2
	ADC #0
	STA (NEWARG),Y;new size (hi)

	JSR MAPMAN
	BNE APELRX;skip if some error

	LDA #13;Zero area of disc
	JSR DSCSIN
	INY ;Y:=ARGG
	LDA #0
	STA (NEWARG),Y
	INY
	LDA ATINF+INFSZE+1
	STA (NEWARG),Y
	INY
	LDA ATINF+INFSZE+2
	STA (NEWARG),Y

	JSR MAPMAN
	BNE APELRX

	LDA #6;MAPMAN.ensure
	JSR DSCSIN;discno & sin onto NEWARG stack
	JSR MAPMAN
.APELRX
	RTS ;leave with RC set
}

;.LNK
;UADE10
