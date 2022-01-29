;.OPT
;UADE17;FILE > Uade17
;.TTL
;Fileserver file UADE17

.UADE17

	;INFO/LOGON/PRIV. OPERATIONS ETC.



.CPINFO
{
	;Get info in m/c readable format, depending on
	;argument.

	;1 = creation date
	;2 = load/execution addresses
	;3 = size
	;4 = type/access
	;5 = all above 4 in order load/execute/size/access/date
	;
	;6 = get access, cycle (sequence) number, and last
	;    component of dir. title for dir. name. NOTE
	;    does not use retrieve, and does not trap "NOT
	;    FOUND" error.


	JSR GETUSR
	BNE CPINLX

	LDX BPTR
	LDA MIDRX,X
	BEQ CPINLK;Zero arg => error
	CMP #6
	BEQ CPDINF;=> do directory information
	BCC CPINLA;Otherwise, is valid arg.

.*CPINLK
	LDA #INFERA
	BNE CPINLZ;Abort with err. msg.

.CPINLA
	LDA #0
	STA MIDTX;Set existence flag to "does not exist"
	LDA #INFFTO
	JSR CPRETR;Retrieve details
	BEQ CPINLC;Ok, continue
	CMP #DRERRC;If not found, alter existence flag
	BNE CPINLZ;Not "not found" => error
	LDX #1;Length of reply
	BNE CPINLE;Exit, sending reply

.CPINLC
	INC MIDTX;Set flag to "is a file"
	LDX BPTR
	LDA MIDRX,X;Check arg.
	LDX #1;Offset for COCOPY
	CMP #5;Is "all args" ?
	BNE CPINLB;Nope, continue

	JSR CPYLXS;Yep, copy all args
	BEQ CPINJA;Always jump

.CPINLB
	JSR COCOPY;Copy relevant amount of stuff COWORK->MIDTX

.CPINJA
	LDY #ARGB;check for OWNER/PUBLIC
	LDA (NEWARG),Y
	AND #OWNER
	BEQ CPINJ
	LDA #LO(-1)
.CPINJ
	EOR #LO(-1);invert byte [Owner => 0]
	STA MIDTX,X;put this at the end of the information
	INX

.CPINLD
	LDY #INFACC
	LDA COWORK,Y
	AND #TYPDIR;Check if is a directory
	BEQ CPINLE;Is not -> continue
	INC MIDTX;Is => set flag to 2 => directory

.CPINLE
	TXA;Length of information transfered
	JMP DATOT2;Send reply
.CPINLX
	JMP COMRTS

.*CPINLZ
	JMP ERROR

.CPDINF
	JSR STKUSA;Set user info. on stack
	LDA #3
	STA COZERO;Offset for dir. name result
	LDA #HDRLEN + 1;Offset of dir. name arg.
	LDX #&C1;force directory here
	JSR DIRIN2;Get dir. infomation
	BNE CPINLZ

	LDY #ARGD;Now set access & sequence number
	LDA #0
	STA MIDTX +NAMLNT+ 3;Zero => owner, $FF => public
	STA MIDTX + 1
	LDA #OWNER
	AND (NEWARG),Y;Check access to dir.
	BNE CPINL7;Is owner, continue
	DEC MIDTX +NAMLNT+ 3;Otherwise, give $FF

.CPINL7
	INY
	LDA (NEWARG),Y
	STA MIDTX +NAMLNT+ 4;Set sequence number of dir.
	LDA #NAMLNT
	STA MIDTX + 2;Set length of dir. name
	LDX #NAMLNT+ 5;Length of message - TXHDR
	BNE CPINLD;Exit with reply (Note MIDTX undefined)
}

.CPIXTB
	EQUB 2;Table of length of data
	EQUB 8
	EQUB 3
	EQUB 1

.CPIATB
	EQUB INFDTE;Offset of data in COWORK
	EQUB INFLOA
	EQUB INFSZE
	EQUB INFACC


.CPYLXS

	;Copy load/exec/size/access/date into MIDTX

	LDA #2
	JSR COCOPY;Load/exec
	LDA #3
	JSR COCOPY;Size
	LDA #4
	JSR COCOPY;access
	LDA #1;creation date

	;Fall through into last call of COCOPY

.COCOPY
{
	;Copy some object attribute from COWORK into MIDTX
	;offset by X. Attribute length and value is determined
	;by A on entry which indexes into two tables

	TAY
	LDA CPIXTB - 1,Y
	STA COTEMP;Set length to copy
	LDA CPIATB - 1,Y
	TAY ;Get offset in COWORK
.COCOP1
	LDA COWORK,Y
	STA MIDTX,X
	INY
	INX
	DEC COTEMP
	BNE COCOP1
	RTS ;Note assumed elsewhere always exit EQ
}




.CPSTAT
{
	;Set object attributes according to arg.

	;1 => set load/execute/access
	;2 => set load address
	;3 => set execute address
	;4 => set access byte
	;5 => set date bytes ** 5/9/84 **

	;Basically just a call to DIRMAN.SETATTRIBS


	JSR STKUSE
	BNE ATTLZZ

	LDX BPTR
	LDA MIDRX,X
	BEQ ATTL1
	CMP #6;** 5/9/84 **
	BCC ATTL2;Arg ok, continue

.ATTL1
	LDA #INFERA;Error in arg.
	JMP ERROR

.ATTL2
	TAY
	LDA DRSAT2 - 1,Y;Note uses table in DIPMAN to get data length
	BEQ ATTLXX;If setting zero number of bytes, exit

	CLC
	ADC #HDRLEN + 1;Get offset of file title in RX buffer
	LDY #ARGC
	JSR SETFTP;Set pointer to file title

	INY ;ARGF here
	LDA #&C0
	STA (NEWARG),Y;wild card flag

	LDX BPTR
.ATTL3
	INY
	LDA MIDRX,X;Load arg. to DIRMAN.SETATTRIBS
	STA (NEWARG),Y
	INX
	CPY #ARGH + 9
	BNE ATTL3

	;Stack now all set, call DIRMAN.SETATTRIBS

	LDA #4
	JSR SETRTN
	JSR DIRMAN
.ATTLXX
	JSR RCODE
.ATTLZZ
	JMP COMRTS
}

.CPDEL
{
	;Non-command line call to delete object, and return
	;object information after deletion.

	JSR STKUSE
	BNE CPDELZ
	LDA #HDRLEN;Offset of file title
	JSR CPRETR;Retrieve details of object to delete
	BNE CPDELX;No good, error

	JSR STKUSA;Reset user pointer on stack

	LDA #HDRLEN;File title offset
	JSR SETFTP;Set file title pointer

	LDA #3
	JSR SETRTN

	LDY #ARGG
	LDA #&80
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;** Call DIRMAN.DELETE **
	BNE CPDELX;Error

	LDX #0;Set offset from MIDTX for file details
	JSR CPYLXS;Copy the whole lot over
	CLC
	TXA
	ADC #TXHDR;Length of message
	STA LAST
	JSR OBJCLR;Clear object out of map
	BEQ CPDELY

.CPDELX
	JSR RCODE;Send error return
.*CPDELZ
	JMP COMRTS;And finish
.CPDELY
	LDA LAST
	BNE USRENX
}



.USRENV
{
	;Return user environment information.
	; 1) Call DIRMAN.EXAMINE to get dir. name, and disc
	;    number of CSD.
	; 3) Call MAPMAN.DISCNAME to get name of disc with
	;    given disc number.
	; 4) Call DIRMAN.EXAMINE to get title of LIB.

	JSR GETUSR
	BNE CPDELZ

	LDX BPTR
	LDA #TERMIN
	STA MIDRX,X;Fool DIRINF into getting CSD info.

	LDA #17;Offset from MIDTX for dir. title
	JSR DIRINF;Read dir. information
	BNE USRENZ;Error -> bomb out

	LDA #1;Offset from MIDTX for disc name
	JSR CPDNAM;Read disc name (note assumes disc no. on stack)
	BNE USRENZ

	LDX BPTR
	LDA CPLIB,X;Read library handle
	LDY #UTHSLD
	STA (USTPTR),Y;Set as CSD of user

	LDA #27;Offset from MIDTX of LIB name
	JSR DIRINF;Entry point not changing CSD handle
	BNE USRENZ

	LDA #DNAMLN;Disc name length
	STA MIDTX
	LDA #0
	STA CCODE
	LDA #TXHDR + 37;Message length
.*USRENX
	JSR REPLYC
	JMP COMRTS

.USRENZ
	JMP ERROR
}



	;*************
	;* L O G O N *
	;*************


.LOGON
{
	JSR SPACES
.LGONLP
	LDA MIDRX,Y;Step past station if present
	CMP #'.';Network number terminator - special case
	BEQ LGONL1
	CMP #'0'
	BCC LGONON;On if <0
	CMP #':';On if >9
	BCS LGONON
.LGONL1
	INY
	BNE LGONLP

.LGONON
	JSR BUFTXT;Read User
	BNE LOGRTS
	INX ;Step past CR between userid & pw
	JSR BTXTA
	BNE LOGRTS
	JSR COMEND;Check to end of C.line
	BNE LOGRTS

	LDY #ARGB
	LDX QPTR
	LDA CBSTID,X
	STA (NEWARG),Y
	STA MCNUMB
	INY
	LDA CBSTID+1,X
	STA (NEWARG),Y
	STA MCNUMB+1
	INY

	LDA CBFLG,X
	AND #&78
	STA (NEWARG),Y;pass station id
	STA MCNUMB+2		

	JSR SBUFPT;pointer to TXTBUF

	LDA #1
	JSR SETRTN
	JSR USRMAN;attempt logon
	BNE LOGERR;failed

	LDY #ARGH;test for priv'd logon
	LDA (NEWARG),Y
	BNE LOGJ1;no monitor for priv'd users

	BIT MONFLG
	BPL LOGJ1;skip if monitor off anyway

	LDY QPTR
	JSR PRTMC;output mc number
	JSR VSTRIN
	EQUB ": I am "
	NOP

	LDX #0
.LOGJ2
	LDA TXTBUF,X
	INX
	JSR OSASCI;print the user name
	CMP #CR
	BNE LOGJ2

.LOGJ1

	LDY #ARGG
	LDX #3
.LGLPAA
	LDA (NEWARG),Y;Get UFD/CSD/LIB handles + option byte
	STA MIDTX,X
	DEY
	DEX
	BPL LGLPAA

	LDA #CCLGON
	STA CCODE
	LDA #TXHDR + 4;Message length
	JSR REPLYC;Send handles etc. back

.LOGRTS
	JMP COMRTS;FINISH
.LOGERR
	JMP ERROR
}



	;***************
	;* L O G O F F *
	;***************


.USROFF
{
	;(LABEL USROFF TO AVOID NAME CLASH
	;WITH USRMAN ROUTINE)

	;SET UP ARGS FOR LOGOFF (DELETE USER)
	;IN USERMAN, CALL, AND SEND APPROPRIATE
	;MESSAGE BACK TO CLIENT.

	JSR COMEND
	BNE USROEX

	;nice if this returns no error if not logged on ** 5/9/84 **
.*FCBYE
	JSR GETUSE
	BEQ USROL1;check for logged on user here

	LDA #0
	BEQ USROEZ;exit ok

.USROL1
	JSR STKUSE;Entry by function code
	BNE USROEX

	LDA #2;LOGOFF ROUTINE NUMBER
	JSR SETRTN
	JSR USRMAN
.USROEZ
	JSR RCODE
.USROEX
	JMP COMRTS
}


.CPSOPT
{
	;Set USER OPTION in pw file
	;Just call appropriate AUTMAN routine

	JSR STKUSE
	BNE CPSOEX
	LDA MIDRX,X;Note assumes X is BPTR here
	CMP #&10;Check option <16
	BCS CPSOER
	INY
	STA (NEWARG),Y
	
	LDY #UTPRIV
	LDA (USTPTR),Y
	AND #LOCKPV
	BNE CPSOES;USER NOT ALLOWED TO CHANGE OPTIONS
	
	LDA #7
	JSR SETRTN
	JSR AUTMAN;Call AUTMAN.USEROPTION
.CPSOEZ
	JSR RCODE
.CPSOEX
	JMP COMRTS

.CPSOER
	LDA #INFERA;Bad arg
	BNE CPSOEZ
	
.CPSOES
	LDA #ATERRD
	BNE CPSOEZ
}



;************************
;* S E L E C T  D I S C *
;************************


.SELDSC;SELDSC
{
	;ALL WORK DONE IN USRMAN.

	JSR RDTITL;GET DISC NAME ON STACK
	BNE SELDEX

	LDA #&A
	JSR SETRTN
	JSR SBUFPT
	JSR MAPMAN;*** GET DISC NO FOR NAME **
	BNE SELDER

	LDY #ARGB
	LDA (NEWARG),Y
	PHA
	INY
	LDA (NEWARG),Y
	INY
	INY
	STA (NEWARG),Y;MOVE DISC. NO. UP TO MAKE WAY FOR USRPTR
	DEY
	PLA
	STA (NEWARG),Y
	JSR STKUSE;SET USER POINTER
	BNE SELDEX
	LDY #ARGF
	JSR STKHND;PUT 3 HANDLES ON STACK

	LDA #3
	JSR SETRTN
	JSR USRMAN;*** SELECT DISC **
	BNE SELDER

	LDY #ARGD
	LDX #2
.SDISLP
	LDA (NEWARG),Y
	STA MIDTX,X
	DEY
	DEX
	BPL SDISLP

	LDA #CCSDIS
	STA CCODE
	LDA #TXHDR + 3;MESSAGE LENGTH
	JSR REPLYC
.SELDEX
	JMP COMRTS
.SELDER
	JMP ERROR
}



	;**********************
	;* S E L E C T  D I R *
	;**********************


.SELDIR;SELDIR
{
	;ALL WORK DONE BY USERMAN AGAIN.

	JSR RDTITL
	BNE SDIREX
	JSR STKUSE
	BNE SDIREX

	JSR SBUFPT;** 5/9/84 **
	LDY #ARGF
	JSR STKHND;Handles -> stack
	JSR SELHAN
	BNE SDIRER;error in allocation

	LDA #CCSDIR
	
.*SLIBND	;SHARED WITH SLIB
	STA CCODE
	LDA #TXHDR + 1;Message length
	JSR REPLYC
.SDIREX
.*SLIBEX
	JMP COMRTS
.SDIRER
.*SLIBAB
	JMP ERROR
}

.SELHAN
{
	LDA #5;ROUTINE NUMBER
	JSR SETRTN
	JSR USRMAN;*** SELECT DIR. **
	BNE SELHEX;exit if fail

	LDY #ARGB
	LDA (NEWARG),Y
	STA MIDTX;New s.dir
	LDA #0
.SELHEX
	RTS
}

	;******************************
	;* S E L E C T  L I B R A R Y *
	;******************************


.SLIB;SLIB
{
	;as for DIR but null filename is illegal
	;if RETRIEVE ok then call USRMAN with LIB as CSD

	JSR RDTITL
	BNE SLIBEX
	JSR STKUSE
	BNE SLIBEX
	JSR SBUFPT
	JSR SCOWPT;Set pointer to COWORK on stack
	LDA #2;Dirman retrieve
	JSR SETRTN

	LDY #ARGH
	LDA #&C0
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;*** RETRIEVE **
	BNE SLIBAB

	JSR STKUSE
	BNE SLIBEX
	JSR SBUFPT
	JSR SCOWPT;Set pointer to COWORK on stack

	LDY #ARGF
	JSR STKHND;handles -> stack

	LDY #ARGH
	LDA (NEWARG),Y
	DEY
	STA (NEWARG),Y;LIB->CSD

	JSR SELHAN;** 5/9/84 **
	BNE SLIBAB;error in allocation

	LDA #CCSLIB
	BNE SLIBND
}



	;*********************
	;* S E T A C C E S S *
	;*********************


.SETACC
{
	;1) PUT USER INFO. POINTER AND FILE TITLE
	;POINTER ON STACK.
	;2) INTERPRET STRING SENT FROM CLIENT INTO
	;AN ACCESS BYTE AND STICK ON STACK.
	;3) CALL SETATTRIBS IN DIRMAN
	;4) REPLY TO CLIENT


	JSR BUFTXT;Get file name
	BNE SAEXIT
	STY COWORK;Store cmnd. line ptr.
	JSR STKUSE
	BNE SAEXIT

	JSR SBUFPT

	LDY COWORK;Restore cmnd. line ptr.
	JSR SPACES

	LDA #0
	STA COZERO;Partial result
	DEY
.SALPB
	INY
	LDX #2;Offset into access tables
	JSR CMPACC;Compare letter at Y with access tables
	BEQ SALPB;Access letter found
	CMP #'/';Owner access delimiter
	BEQ SAONF
	JSR SPACER;skip trailing spaces ** 25/5/83 **
	CMP #CR
	BNE SAERR;Invalid character => error
	DEY ;Point to CR so will exit after rotating COZERO

.SAONF
	ASL COZERO;Rotate access bits to owner position
	ASL COZERO

.SALPC
	INY ;Increment past '/' delimiter
	LDX #1
	JSR CMPACC
	BEQ SALPC
	JSR SPACER;skip trailing spaces ** 25/5/83 **
	CMP #CR
	BNE SAERR;If here, CR has terminated string => set access

	LDY #ARGF
	LDA #&C0
	STA (NEWARG),Y;wild cards flag
	LDA #4
	INY ;Set arg to SETATTRIBS => SETACCESS
	STA (NEWARG),Y
	INY
	LDA COZERO;Restore access byte
	STA (NEWARG),Y;SET ACCESS BITS
	LDA #4;SETACC ROUTINE NO.
	JSR SETRTN
	JSR DIRMAN;*** SET ACCESS **
	JSR RCODE;SEND REPLY
.SAEXIT
	JMP COMRTS;FINISH

.SAERR
	LDA #SAERRA;BAD ACCESS STRING ERROR
	JMP ERROR

.OWNTAB
	EQUB "R"
	EQUB "W"
	EQUB "L"

.POWNTB
	EQUB 1;OWNER READ
	EQUB 2;OWNER WRITE
	EQUB 4

.CMPACC
	LDA MIDRX,Y
	EOR OWNTAB,X
	AND #&DF;Compare with cases forced
	BEQ CMPAEX
	DEX
	BPL CMPACC
	LDA MIDRX,Y;Exit, A = char, Z unset
	RTS

.CMPAEX
	LDA POWNTB,X
	ORA COZERO
	STA COZERO;Update partial result
	LDX #0;Set Z flag; note A corrupted
	RTS

}


	;******************
	;* N E W  U S E R *
	;******************


.NEWUSE

	;ALL WORK DONE IN AUTMAN, ROUTINE NO. 1

	JSR RDTITL;Get user name
	BNE NUEXIT
	JSR STKUSE;Stack caller's user info
	BNE NUEXIT
	JSR SBUFPT;Stack new user ptr.

	LDA #1
.DOUSE
	JSR SETRTN;SET ROUTINE NUMBER
	JSR AUTMAN;DO NEW USER
	JSR RCODE;SEND RETURN CODE
.RUEXIT;Shared with REMUSE
.SPWEXT;Shared with USERIN
.NUEXIT
	JMP COMRTS;FINISH



	;************************
	;* R E M O V E  U S E R *
	;************************

.REMUSE

	;REMARKABLY SIMILAR TO NEWUSER

	JSR RDTITL
	BNE RUEXIT
	JSR STKUSE
	BNE RUEXIT
	JSR SBUFPT

	LDA #4
	BNE DOUSE;Always jump


	;**************************
	;* S E T  P A S S W O R D *
	;**************************


.SETPW
{
	;GET USER INFO AND CALL ROUTINE IN AUTMAN
	;TO DO WORK.

	JSR BUFTXT;Buffer old pw
	BNE SPWEXT
	INX ;Ptr. to next free spot in TXTBUF
	JSR BTXTA;Buffer new pw
	BNE SPWEXT
	JSR COMEND
	BNE SPWEXT

	BIT MONFLG;Do monitor message specially to avoid PW
	BPL SETPW1;No monitor -> carry on

	LDY QPTR;Point to RX control block
	JSR PRTMC;Print machine number
	JSR VSTRIN
	EQUB ": Pass",CR
	NOP

.SETPW1
	JSR STKUSE
	BNE SPWEXT
	JSR SBUFPT
	
	LDY #UTPRIV
	LDA (USTPTR),Y
	AND #LOCKPV
	BNE STPWER;USER NOT ALLOWED TO CHANGE PASSWORD

	LDA #3;ROUTINE NUMBER
	BNE DOUSE;Always jump
	
.STPWER
	LDA ATERRD
	JMP ERROR
}


	;**********************
	;* C R E A T E  D I R *
	;**********************


.CDIRFN
{
	JSR STKUSE
	BNE CDIREX

	LDY BPTR
	INY
	JSR RDTITL
	BNE CDIREX
	LDY BPTR
	LDA MIDRX,Y
	BEQ CDIRZ;zero is a silly number
	CMP #HI(MAXDIR)+1
	BCS CDIRZ
	BCC CDIRC;check for overflow

.*CDIR;CDIR

	;PASS USER INFO AND DIRECTORY NAME
	;TO DIRMAN. THEN CLEAN UP IF NECCESSARY.

	JSR RDTITL
	BNE CDIREX
	LDA #2;size of directory

.CDIRC
	CMP CACHSZ+1;** 31/1/85
	BCS CDIRZ
	LDY #ARGF
	STA (NEWARG),Y
	INY
	LDA #&80;** 11/10/84 **
	STA (NEWARG),Y;wild card flag
	JSR STKUSE
	BNE CDIREX

	JSR SBUFPT
	LDA #5;CREATE DIR. FUNCTION CODE
	JSR SETRTN

	JSR DIRMAN;*** DO DIR. CREATE **
	BNE CDIRY
	
	JSR OBJCLR;CLEAR OLD OBJECT
	
.CDIRY
	JSR RCODE;store RC

.*SPEXIT;Shared with SETPRIV
.CDIREX
	JMP COMRTS

.CDIRZ
	LDA #DRERRP
	JMP ERROR
}

	;******************
	;* S E T  P R I V *
	;******************


.STPRIV
{
	;MOST WORK DONE BY AUTMAN

	JSR BUFTXT;MOVE USER ID.
	BNE SPEXIT
	STY COWORK
	JSR STKUSE
	BNE SPEXIT
	JSR SBUFPT

	LDY COWORK
	JSR SPACES
	AND #&DF;Force upper case
	CMP #'S';SYSTEM USER
	BEQ SPRONA
	CMP #'L';LOCK PRIVILEGES
	BEQ SPRONC 
	
	CMP #CR;NON-SYSTEM USER ??
	BNE SPRIER
	LDA #0;NON-SYST. USER
	BEQ SPRONB

.SPRONA
	INY
	JSR COMEND
	BNE SPEXIT
	LDA #1;SYSTEM USER !
.SPRONB
	LDY #ARGF
	STA (NEWARG),Y;Set user's priv. flag

	LDA #5;SET PRIV.
	JMP DOUSE

.SPRIER
	LDA #SPERRA
.SPRIEX
	JMP ERROR
	
.SPRONC
	INY
	JSR COMEND
	BNE SPEXIT
	LDA #2
	BNE SPRONB
}



	;*************
	;* D I S C S *
	;*************

.DISCS;DISCS
{
	;Provide list of discs currently on system.

	;User supplies entry (drive) number, and
	;number of drives. For each drive he is interested in,
	;call MAPMAN once to get disc number
	;and again to get disc name.

	LDA #0
	STA COZERO;No. of drives found
	LDA BBUF
	CLC
	ADC #TXHDR + 1;Result buffer
	STA COZERO + 1
	LDA BBUF + 1
	ADC #0
	STA COZERO + 2
.DSCSLA
	LDX BPTR
	LDA MIDRX,X
	CMP DRIVES
	BCS DSCSLB;User drive >= drives

	LDY #0
	STA (COZERO + 1),Y;Result drive no.
	LDY #ARGB
	STA (NEWARG),Y;Store drive number
	LDA #8;Drive -> disc no.
	JSR SETRTN
	JSR MAPMAN
	BNE DSCSLK;Abort...
	LDA #&B;Disc no. -> name
	JSR SETRTN
	JSR MAPMAN
	BNE DSCSLX
	INC COZERO + 1
	BNE DSCSLC
	INC COZERO + 2
.DSCSLC
	LDA COZERO + 1
	STA MOVTO
	LDA COZERO + 2
	STA MOVTO + 1

	LDY #ARGB
	LDA (NEWARG),Y;Ptr. to name
	STA MOVFRM
	INY
	LDA (NEWARG),Y
	STA MOVFRM + 1
	LDX #DNAMLN
	JSR MOVE;Move name to result
	CLC
	LDA COZERO + 1
	ADC #DNAMLN
	STA COZERO + 1
	BCC DSCSLJ
	INC COZERO + 2
.DSCSLJ
	INC COZERO;No. of drives dealt with
.DSCSLK
	LDX BPTR
	INC MIDRX,X;Next drive to do
	LDA COZERO
	CMP MIDRX+1, X
	BCC DSCSLA

.DSCSLB
	LDY #ARGB
	LDA COZERO;Set args to LSTPRY
	STA (NEWARG),Y
	INY
	LDA COZERO + 1
	STA (NEWARG),Y
	INY
	LDA COZERO + 2
	STA (NEWARG),Y
	LDA #CCDSCS
	JSR LSTRPY;Reply to client
.DSCSLZ
	JMP COMRTS

.DSCSLX
	JSR EXTERR
	JMP COMRTS
}



	;*************
	;* U S E R S *
	;*************

.CPUSRS;CPUSRS
{
	JSR STKLST

	JSR GETUSE; STACK USERS SYSTEM PRIV ARGF
	BNE USRSL2
	
	LDY #UTPRIV; **** LH 14/1/86 ****
	LDA (USTPTR),Y
	AND #SYSTPV
	
.USRSL1
	LDY #ARGF
	STA (NEWARG),Y
	
	LDX BPTR
	LDA FCODE,X
	EOR #&21;FUNCTION &21?
	INY ;Y:=ARGG
	STA (NEWARG),Y

	LDA #7
	JSR SETRTN
	JSR USRMAN
	BNE USRSAB
	LDA #CCUSRS
	JSR LSTRPY
	JMP COMRTS
.USRSAB
	JMP ERROR
.USRSL2
	LDA #0
	BEQ USRSL1
}



	;***********
	;* D A T E *
	;***********

.CPSETD;Set the date ** 20/9/83 **
{
	JSR GETUSR
	BNE DTENEX

	LDY #UTPRIV
	LDA (USTPTR),Y
	AND #SYSTPV
	BEQ DTENAB	;User not allowed to change date/time

	LDY BPTR;check flag
	LDA MIDRX,Y
	LDX MIDRX + 1,Y
	JSR CDTE	
	LDY #(TWORK-DBLK)
	JSR CHKDTE
	BCS DTENGD
	
	LDY BPTR
	LDA MIDRX +2,Y
	CMP #24
	BCS DTENGD
	STA TWORK +4;HOURS
	LDA MIDRX +3,Y
	CMP #60
	BCS DTENGD
	STA TWORK +5;MINS
	LDA MIDRX +4,Y
	CMP #60
	BCS DTENGD
	STA TWORK +6;SECS
	
	;VALID DATE & TIME
	LDX #6
.DTELP
	LDA TWORK,X
	STA DBLK,X
	DEX
	BPL DTELP	
	JSR SETTME
	LDA #TXHDR;Rx header only
	BNE DATOUT
	
	;INVALID DATE OR TIME
.DTENGD
	LDA #DTERR
	JSR RCODE
	
.DTENEX
	JMP COMRTS
.DTENAB
	LDA #ATERRD
	JMP ERROR
}

.CPDATE

	;No user check since doesn't involve
	;files.
	
	LDA DATE;read date
	STA MIDTX
	LDA DATE + 1
	STA MIDTX + 1

	;Now check if real-time clock is available,
	;and if so, supply extra 3 bytes of time

	LDA HRS
	STA MIDTX + 2
	LDA MINS
	STA MIDTX + 3
	LDA SECS
	STA MIDTX + 4;Time = zero if clock not present

	LDA #TXHDR + 5;Message length
	BNE DATOUT



	;*****************
	;* V E R S I O N *
	;*****************


.CPVERN;CPVERN
{
	LDY #0
.CPVLP
	LDA VERSN,Y
	STA MIDTX,Y
	INY
	CMP #6;end of version number character
	BNE CPVLP

	TYA
}

.DATOT2
	CLC
	ADC #TXHDR

.DATOUT
	LDX #0
	STX CCODE;0 command code
	JSR REPLYC;Message length in A here
	
.RNAMRT	;USED BY RENAME
	JMP COMRTS

;.LNK
;UADE18
;
