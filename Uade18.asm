;.OPT
;UADE18;FILE > Uade18
;.TTL
;Fileserver file UADE18

.UADE18

	;Random access and RENAME & User Info



	;***************
	;* R E N A M E *
	;***************


.RENAME
{
	;Rename file <A> as <B>
	;
	;1) Buffer both file names, call DIRMAN.CHECKDISCNOS
	;2) Retrieve A. Check owner access, is a file, is unlocked
	;3) Preserve name B with details of A
	;4) If old SINofB = SINofA, renaming same file -> return
	;5) Else, clear oldB from map, and delete A from dir (not map!)

	STY COTEMP
	JSR GETUSR
	BNE RNAMRT;Exit

	LDY COTEMP;pointer into MIDRX
	JSR BUFTXT;Buffer name A
	BNE RNAMX2;Exit via extra jump
	INX
	STX COTEMP;Store offset of B in TXTBUF
	JSR BTXTA;Buffer name B
	BNE RNAMX2;Exit via extra jump
	JSR COMEND;Check to end of command line
	BNE RNAMX2;Exit via extra jump

	;** 24/9/84 **

	;Substantial rewrite to allow renaming of directories
	;The actual mechanics are identical to renaming files
	;but lots of checking for loops in directories needs to be
	;done, which means that full pathname expansion is called for

	LDA BBUF
	STA PTR
	LDA BBUF+1
	STA PTR+1

	LDA #0;full wild card lookup
	STA REXIST
	JSR RNAMLK;do the checking for existence etc.
	BNE RNAMX2;fail

	STX RTYPE;type of object that we've found
	CLC
	LDA PTR
	STA PTR1
	ADC #LO(INFNXT)
	STA PTR

	LDA PTR+1
	STA PTR1+1
	ADC #HI(INFNXT)
	STA PTR+1;new result area

	;PTR =lastname and info <B>
	;PTR1=lastname and info <A>

	LDA COTEMP;offset of name <B>
	JSR RNAMLK;similar checking as for <A>
	BEQ RNAML1;all succeeded [but may not be ok !]

	LDX #0;no object returned so no type either
	CMP #DRERRC;'not found' in final part of name
.RNAMX2
	BNE RNAMER;failed for some serious reason
	ROR REXIST;object <B> done NOT exist (=>&80)

	;now look for rename across discs
.RNAML1
	LDY #INFDIS
	LDA (PTR),Y
	CMP (PTR1),Y
	BNE RNAMLE;failed here
	INY
	LDA (PTR),Y
	CMP (PTR1),Y
	BNE RNAMLE;failed here too

	LDA RTYPE;look for file/dir
	AND #TYPDIR
	BEQ RNAML4;file so ok

	;now look for loops in directories

	LDY #0;from the beginning
	STY LAST
	STY LAST + 1
	LDX COTEMP;starts with "$" ?
	LDA TXTBUF,Y
	JSR SPECL
	BNE RNAMLA;NOT SPECIAL SYMBOL
	STA LAST + 1
	LDA TXTBUF + 1,Y
	JSR DLIM
	BNE RNAMLA;NOT '.' OR CR
	INC LAST
.RNAMLA
	LDA TXTBUF,X
	JSR SPECL
	BNE RNAMLB;NOT SPECIAL SYMBOL
	
	EOR LAST + 1
	STA LAST + 1
	LDA TXTBUF + 1,X
	JSR DLIM
	BNE RNAMLB
	
	DEC LAST
.RNAMLB;X=0 or fail here
	LDA LAST;test for mismatch
	BNE RNAMLE;bad rename
	
	LDA LAST + 1
	BNE RNAMLE

.RNAML0
	LDA TXTBUF,Y
	JSR ISCHAR
	BCS RNAMCS
	AND #&DF;force upper case
.RNAMCS
	CMP #TERMIN
	BEQ RNAML6;bad, LEFT$<B>=$<A> unless $<A>=$<B>
	STA NAME
	LDA TXTBUF,X;mismatch in names is essential
	JSR ISCHAR;force case on both chars
	BCS RNAMCT
	AND #&DF
.RNAMCT
	CMP NAME;match against other name
	BNE RNAMLD
	INX
	INY
	BNE RNAML0

.RNAMLD
	BIT REXIST;different names so <B> must NOT exist
	BMI RNAML4

.RNAMLE
	LDA #RNAMQQ;bad rename error string

.RNAMER
	JMP ERROR

.RNAML6
	LDA TXTBUF,X;check for subdirectory here
	CMP #SEPART;** 27/3/85 **
	BEQ RNAMLE

.RNAML4
	JSR STKUSE;Set up PRESERVE call for A details, named B
	LDA COTEMP
	JSR SETTXP;put name to stack

	INY
	STY NAME
	LDA #INFLOA
	STA ONAME;counters
	LDX #17

.RNAML5
	LDY ONAME
	LDA (PTR1),Y
	LDY NAME
	STA (NEWARG),Y
	INC ONAME
	INC NAME
	DEX
	BNE RNAML5

	LDA #12
	JSR SETRTN;Do DIRMAN.PRESERVE (without DELCHK)

	LDY #ARGT
	LDA #&80;enable wildcards
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN
	BNE RNAMER

	BIT REXIST;did <B> exist
	BMI RNAML2; .. no ..

	LDX #3
	LDY #INFSIN+2
.RNAML7
	LDA (PTR),Y
	CMP (PTR1),Y
	BNE RNAML8
	DEY
	DEX
	BNE RNAML7

.RNAML8
	TXA
	BEQ RNAML3;if identical then NOP

.RNAML9
	JSR OBJCLR;Clear old B from map (if SIN <> zero)
	BNE RNAMER;Error exit

	;Then finally delete name A from directory

.RNAML2
	JSR STKUSE
	JSR SBUFPT;Set pointer to TXTBUF
	LDA #11;** 24/9/84 **
	JSR SETRTN

	LDY #ARGG
	LDA #&C0
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;Delete A
.RNAML3
	JSR RCODE;Return success/failure and exit
	JMP COMRTS


	;RNAMLK
	
	;A=offset in TXTBUF of pathname for object
	;PTR=results area

	;retrieves object in TXTBUF, expanding pathname into !PTR
	;checks object existence, pathname completeness,
	;object owned, unlocked, not open
.RNAMLK
	PHA
	JSR STKUSE;prepare for retrieve call
	PLA
	JSR SETTXP;point to the name

	LDY #ARGF
	LDA PTR
	STA (NEWARG),Y
	INY
	LDA PTR+1
	STA (NEWARG),Y;pointer to results area
	INY
	LDA #0;no wildcards
	STA (NEWARG),Y

	LDA #2
	JSR SETRTN
	JSR DIRMAN;call retrieve
	BNE RNAMF;DIRMAN call failed

	LDY #ARGB
	LDA (NEWARG),Y
	TAY

	AND #OWNER
	BNE RNAMJ5
	LDA #DRERRE;'insufficient priv'
	RTS

.RNAMJ5
	TYA
	AND #LOCKED
	BEQ RNAMJ6
	LDA #DRERRG;'entry locked'
	RTS

.RNAMJ6
	STY LAST;preserve Type & access

	;now check that it isn't open

	LDA #ARGB
	STA NAME
	LDY #INFDIS
	STY ONAME
	LDX #2
	JSR RNAMJ7;copy disc no.

	LDA #INFSIN
	STA ONAME
	LDX #3
	JSR RNAMJ7;copy SIN

	LDA #2
	JSR SETRTN
	JSR RNDMAN;ask for info
	PHA
	LDX LAST
	PLA
	BNE RNAMJ8;RC=0 if open

	LDA #RDERRH;'already open'
	RTS

.RNAMJ8
	LDA #0

.RNAMF
	RTS ;*** the end **


.RNAMJ7
	LDY ONAME
	LDA (PTR),Y
	LDY NAME
	STA (NEWARG),Y
	INC NAME
	INC ONAME
	DEX
	BNE RNAMJ7
	RTS
	
.SPECL;'$', '^', '&', '%' OR '@'
	CMP #ROOT
	BEQ DLIMX;IS ROOT?
	JMP TSTSYM
.DLIM;'.' OR CR
	CMP #SEPART;IS SEPARATOR?
	BEQ DLIMX
	CMP #CR;IS TERMINATOR?
.DLIMX
	RTS
}


	;**************
	;* USER INFO  *
	;**************


.CPUINF
{
	JSR STKUSE
	BNE USRIXX;Who are you

	LDA #HDRLEN;Offset of user name
	JSR SETFTP;Set ptr. to user name

	LDA #6;USRMAN.USERINFO
	JSR SETRTN

	JSR USRMAN
	BNE USRIYY

	LDY #ARGB
	LDA (NEWARG),Y
	
	and #&40
	STA MIDTX
	INY
	LDA (NEWARG),Y
	STA MIDTX + 1
	INY
	LDA (NEWARG),Y
	STA MIDTX + 2
	
	INY
	LDA (NEWARG),Y
	LSR A
	LSR A
	LSR A
	STA MIDTX + 3
	
	LDX BPTR
	LDA FCODE,X
	CMP #'"'
	LDA #TXHDR + 3;Message length
	BCC CPUINA
	LDA #TXHDR + 4
.CPUINA
	JSR REPLYC;Return message

.*CPSEXT	;Shared with CPSHUT
.*FNDRT2	;Shared with FIND
.USRIXX
	JMP COMRTS

.*CPSIYY
.USRIYY
	JSR RCODE;Send error
	JMP USRIXX
}



	;*************
	;* C L O S E *
	;*************

.CPSHUT
{
	JSR STKUSE
	BNE CPSEXT
	LDX BPTR
	LDA MIDRX,X;Get old handle
	BEQ CPSHTA;Handle=0 => close all files for machine

	INY
	STA (NEWARG),Y
	LDA #3;"Close handle"
	BNE CPSHTB;Always jumps

.CPSHTA
	LDA #&D;"Close all files"

.CPSHTB
	JSR SETRTN
	JSR RNDMAN
	JMP CPSIYY
}

	;********
	;* FIND *
	;********

	;Open a file for reading or update,
	;or open a directory

	;Entry: BPTR is offset of buffer in RXBUF
	;QPTR is offset of control block in RXCBV

.FIND
{
	LDA #0;** 15/11/84 **
	STA COTEMP;COTEMP used to build access byte

	JSR GETUSR;Set mc no. and call FINDMC
	BNE FNDRT2

	;USTPTR now points to user info.
	;Check if need to create a new file
	;or if existing one will do. Then either create or retrieve
	;new or existing file.

	LDX BPTR
	LDA MIDRX + 1,X
	BNE FINDD;Not opening for update
	LDA MIDRX,X
	BNE FINDD;File must exist => don't create

	JSR FICRFL;Otherwise, create file and forge retrieve info.
	BNE FINDC;Error
	
	LDA #FILEJC;Set "just created" flag in mode
	ORA COTEMP
	STA COTEMP
	BNE FINDB;Skip round retrieve since we have all info. already

.FINDD
	JSR FIRETR;Retrieve details of existing file
	BNE FINDC

.FINDB
	LDY #ARGB
	LDA (NEWARG),Y;Get access allowed and type
	TAX
	AND #RDWRAC+TYPE
	BNE Z10;File, no read or write access

	LDA #DRERRE
.FINDC
	JMP ERROR;insufficient access

.Z10
	LDY #ARGE
	TXA
	STA (NEWARG),Y;Set up for RNDMAN call
	PHA ;save access byte

	LDA #1;RNDMAN.OPEN
	JSR SETUSB;Copy USTPTR onto stack

	;Mode so far is in COTEMP - may need write
	;access bit setting.

	PLA ;restore access byte
	LDX BPTR
	LDY MIDRX + 1,X;Get read flag from  client
	BNE FINDF;Jump if set

	AND #3;supply just the public access bits
	JMP FINDF1;fall thru' IS ok here

.FINDF
	LDA #READAC
.FINDF1
	ORA COTEMP
	LDY #ARGD
	STA (NEWARG),Y;Put mode on stack

	INY ;Y := ARGE
	JSR SCOWPT;Put COWORK ptr on stack

	JSR RNDMAN;*** Call RNDMAN.OPEN **
	BNE FINDG;Open failed

	;Open worked - return handle

	LDY #ARGB
	LDA (NEWARG),Y
	STA MIDTX;Store handle in TX buffer

	LDA COTEMP
	AND #FILEJC;DATE ALREADY SET IF JUST CREATED
	BNE FNDNJC
	LDY BPTR
	LDA MIDRX +1,Y;updating file ?
	BNE FNDNJC

	LDA DATE; NO CHANGE TO DIR IF DATE IS SAME *** LH 15/12/85 ***
	CMP COTEMP+INFDTE
	BNE LHDCHG
	LDA DATE+1
	CMP COTEMP+INFDTE+1
	BEQ FNDNJC

.LHDCHG
	LDA #4;DIRMAN.SET ATTRIBUTES
	JSR SETUSB;USER INFO
	LDA #FNDFTO;POINTER TI FILE TITLE
	JSR SETFTP;FILE TITLE POINTER TO STACK
	INY
	LDA #&C0;** 19/11/84 ** only updates one entry
	STA (NEWARG),Y;SET WILD CARD FLAG
	INY
	LDA #5;SET DATE FUNCTION
	STA (NEWARG),Y
	INY
	LDA DATE
	STA (NEWARG),Y;PUT IN CURRENT DATE
	INY
	LDA DATE +1
	STA (NEWARG),Y
	JSR DIRMAN;SET TODAYS DATE ON OBJECT
	BNE FINDG;SHOULDN'T FAIL!!!

.FNDNJC
	LDA #TXHDR + 1;Message length
	JSR REPLYC;Send handle back

.FNDRTS
	JMP COMRTS;Return

.FINDG
	JSR EXTERR
	JMP FNDRTS;exit



	;*** FIRETR ***

	;Called from FIND to retrieve file details.
	;Calls DIRMAN.RETRIEVE

	;Entry: USTPTR points to USERTB entry
	;File title stil in RX buffer

	;Exit:  A: return code
	;COWORK buffer: file details
	;ARGB on NEWARG: max access allowed

.FIRETR
	;Set up stack for DIRMAN call
	LDA #DRRTR;DIRMAN.RETRIEVE
	JSR SETUSB;Put user info on NEWARG stack

	LDA #FNDFTO;File title offset
	JSR SETFTP;Set file title pointer

	JSR SCOWPT

	LDY #ARGH
	LDA #&C0
	STA (NEWARG),Y;wild card flag

	JMP DIRMAN;*** Call DIRMAN.RETRIEVE and return **



	;*** FICRFL ***

	;Called to create new file in FIND.
	;File is created with standard size of FIFLSZ bytes.

	;Entry: USTPTR points to user info
	;File title in request buffer

	;Exit:  A = return code
	;COWORK buffer: file details as if from
	;DIRMAN.RETRIEVE

.FICRFL
	LDA #10;=> get disc number for this file
	JSR SETUSB
	
	LDA #FNDFTO
	JSR SETFTP;Set pointer to file title

	LDY #ARGK
	LDA #&80
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;DIRMAN.FILETITLETODISCNUMBER
	BNE FICRFM

	;Correct disc number now on stack

	LDY #ARGD;Set file size to manifest FIFLSZ
	LDA #LO(FIFLSZ)
	STA (NEWARG),Y;LS byte
	INY
	LDA #HI(FIFLSZ)
	STA (NEWARG),Y;CS byte
	INY
	LDA #0;MS byte zero
	STA (NEWARG),Y

	JSR SETUSE;pass userinfo @ ARGG,H ** 3/10/84 **

	LDA #MAPCS;MAPMAN.CREATESPACE
	JSR SETRTN

	JSR MAPMAN;*** MAPMAN.CREATSPACE **
	BEQ FICRFK
.FICRFM
	RTS ;Exit if failed

	;Disc number and SIN on stack.
	;Save them in COWORK (=DETRAM) buffer in
	;DIRMAN.RETRIEVE format.

.FICRFK
	LDY #ARGB
	LDA (NEWARG),Y;Disc no. (LS)
	STA DETDIS
	INY
	LDA (NEWARG),Y;Disc no. (MS)
	STA DETDIS + 1

	LDY #ARGD
	LDA (NEWARG),Y;SIN (LS)
	STA DETSIN
	INY
	LDA (NEWARG),Y;SIN (CS)
	STA DETSIN + 1
	INY
	LDA (NEWARG),Y;SIN (MS)
	STA DETSIN + 2

	;zero the new space

	LDY #ARGG
	LDA #0;old size
	STA (NEWARG),Y
	INY
	STA (NEWARG),Y
	INY
	STA (NEWARG),Y

	LDA #13
	JSR SETRTN
	JSR MAPMAN
	BNE FICRFM;exit if error

	;Preserve the newly created file

	LDA #DRPRS;DIRMAN.PRESERVE
	JSR SETUSB;Set user info on NEWARG stack

	LDA #FNDFTO;Offset of file title
	JSR SETFTP;Set pointer to title


	LDA #&FF;Clear load & exec addresses to $FF

.FICRFA
	INY
	STA (NEWARG),Y
	CPY #ARGM;ARGM is end of exec address
	BNE FICRFA;Loop

	LDA #(ACCDEF OR TYPFIL);Default access, Type = file
	INY
	STA (NEWARG),Y;Set type & access

	INY ;Set creation date
	LDA DATE
	STA (NEWARG),Y
	INY
	LDA DATE + 1
	STA (NEWARG),Y

	;Copy SIN from COWORK (=DETRAM) to NEWARG stack

	LDA DETSIN;SIN (LS)
	INY
	STA (NEWARG),Y
	LDA DETSIN + 1;SIN (CS)
	INY
	STA (NEWARG),Y
	LDA DETSIN + 2;SIN (MS)
	INY
	STA (NEWARG),Y

	LDY #ARGT
	LDA #&80
	STA (NEWARG),Y;wild card flag

	JSR DIRMAN;*** Call DIRMAN.PRESERVE **
	BNE FICRFB;Failed
	JSR OBJCLR;Clear old object out of map & cache if nesc.
	BNE FICRFB;**** 17/3/83 ****

	;Have created file: fabricate DIRMAN.RETRIEVE info
	;in DETRAM buffer (disc no. and SIN already there)
	;and on NEWARG stack.  Need to include only access
	;info and size.

	LDA #ACCDEF;Default access
	STA DETACC

	LDA #LO(FIFLSZ);Size (LS)
	STA DETSZ
	LDA #HI(FIFLSZ);Size (CS)
	STA DETSZ + 1
	LDA #0;Size (MS)
	STA DETSZ + 2

	;Need to put type of object & max access
	;allowed on NEWARG stack at ARGB (because
	;DIRMAN.RETRIEVE does).
	LDA #(RDWRAC OR TYPFIL);Read/write access, Type = file
	LDY #ARGB
	STA (NEWARG),Y

	LDA #0;return code
	RTS

	;Failed to do preserve - must free space

.FICRFB
	STA COTEMP + 1;Save RC

	JSR FIDSIN;Disc no. & SIN to stack

	LDY #ARGF
	JSR SETUSE;pass pointer to user info ** 3/10/84 **

	LDA #MAPFS;MAPMAN.FREESPACE
	JSR SETRTN

	JSR MAPMAN;*** MAPMAN.FREESPACE **
	BEQ FICRFC;OK
	JSR INTERR;Never happens?

.FICRFC
	LDA COTEMP + 1;Get back RC from Preserve

	RTS
}


	;*** FIDSIN ***

	;Copy disc no. and SIN from DETRAM (=COWORK) buffer
	;to NEWARG stack

.FIDSIN
	LDY #ARGB
	LDA DETDIS;Disc no (LS)
	STA (NEWARG),Y
	LDA DETDIS + 1;Disc no (MS)
	INY
	STA (NEWARG),Y

	LDA DETSIN;SIN (LS)
	INY
	STA (NEWARG),Y
	LDA DETSIN + 1;SIN (CS)
	INY
	STA (NEWARG),Y
	LDA DETSIN + 2;SIN (MS)
	INY
	STA (NEWARG),Y
	RTS



	;***********
	;*  E O F  *
	;***********

.CPEOF
{
	;Calls RDEOF and returns $FF if HWM<=SFP, zero
	;otherwise.

	JSR STKUSE
	BNE CPEOFX;User not valid

	LDX BPTR
	LDA MIDRX,X
	INY
	STA (NEWARG),Y;Set handle of file
	LDA #&F
	JSR SETRTN
	JSR RNDMAN;Do End of File call
	BNE CPEOFZ;Error -> pass on
	LDY #ARGB
	LDA (NEWARG),Y;Get result from randman
	STA MIDTX
	LDA #TXHDR + 1
	JSR REPLYC

.*CPSPX2
.CPEOFX
	JMP COMRTS

.CPEOFZ
	JSR RCODE;Return error
	JMP COMRTS
}

	;*****************************
	;* Return free space on disc *
	;*****************************

.CPSPAC
{
	JSR RDTITL
	BNE CPSPX2;syntax error ?

	LDA #10
	JSR SETRTN
	JSR SBUFPT;put pointer on stack

	JSR MAPMAN;name -> number
	BNE CPSPAX;skip on error

	;returns:
	;ARGB = LS disc number
	;ARGC = MS disc number

	LDA #12;new entry point 21/3/83
	JSR SETRTN
	JSR MAPMAN
	BNE CPSPAX;skip on error

	LDX #5
	LDY #ARGG;ready to copy data back
.CPSPA1
	LDA (NEWARG),Y
	STA MIDTX,X
	DEY
	DEX
	BPL CPSPA1;return three bytes

	LDA #TXHDR + 6;massage length
	JMP DATOUT;send back result

.CPSPAX
	JMP ERROR
}

.RDFREE;return callers free space
{
	JSR STKUSE
	BNE WRFREX

	LDY #ARGC
	LDX BPTR
	LDA MIDRX,X
	CMP #CR
	BNE RDFREJ;no argument

	LDA USTPTR
	CLC
	ADC #UTUSID
	STA USTPTR
	BCC RDFREI
	INC USTPTR+1

.RDFREI
	JSR SETUSE;set the user pointer on stack
	JMP RDFREK

.RDFREJ
	LDA #HDRLEN+0
	JSR SETFTP;point to user name

.RDFREK
	LDA #9
	JSR SETRTN
	JSR AUTMAN
	BNE RDFREX

	LDX #3
	LDY #ARGB+3
.RDFREA
	LDA (NEWARG),Y
	STA MIDTX,X
	DEY
	DEX
	BPL RDFREA

	LDA #HDRLEN+4
	JMP DATOUT

.*RDFREX
	JMP ERROR
}

.WRFREE;set user free space
{
	JSR STKUSE
	BNE WRFREX;common error exit

	LDY #ARGC
	LDA #HDRLEN+4;offset of name
	JSR SETFTP;pointer to name

	LDX BPTR
	LDY #ARGF
.WRFREA
	LDA MIDRX,X
	STA (NEWARG),Y
	INX
	INY
	CPY #ARGF+4
	BNE WRFREA

	LDA #10
	JSR SETRTN
	JSR AUTMAN
	JSR RCODE;reply with zero RC
	
.*WRFREX
	JMP COMRTS;else all ok
}

;.LNK
;UADE19
