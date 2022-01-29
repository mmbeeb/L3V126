;.OPT
;UADE0B;FILE > Uade0B
;.TTL
;Fileserver file UADE0B

.UADE0B

	;**********************************
	;*        STRMAN UTILITIES        *
	;**********************************



.CRDESC
	TYA ;CREATE A NEW DESCRIPTOR
	PHA
	TXA
	PHA
	LDY #CESIZE
	LDX #0
	JSR GETVEC
	STY CRNT
	STY CLRPTR
	STX CRNT + 1;CRNT := ADDR OF NEW DESCRIPTOR
	STX CLRPTR + 1
	LDY #CESIZE
	LDX #0
	JSR CLRSTR;CLEAR DESCRIPTOR TO ZERO
	PLA
	TAX
	PLA
	TAY
	RTS

.MKFREE
{
	TXA
	PHA ;PUSH X
	LDX #CESIZE;CLEAR CRNT DESCRIPTOR
	LDA #0
	TAY
.MKFREB
	STA (CRNT),Y
	INY
	DEX
	BNE MKFREB
	LDA FRECHN;PLACE CRNT DESCRIPTOR ON FREE CHAIN
	LDY #CENEXT
	STA (CRNT),Y
	LDA FRECHN + 1
	INY
	STA (CRNT),Y;NEXT OF CRNT := (FRECHN)
	LDA CRNT
	STA FRECHN
	LDA CRNT + 1
	STA FRECHN + 1;FRECHN := CRNT
	PLA
	TAX ;PULL X
	RTS
}

.FRDESC
{
	LDA FRECHN;REMOVE A DESCRIPTOR FROM FREE CHAIN
	STA CRNT
	LDA FRECHN + 1
	STA CRNT + 1;CRNT := PTR TO A FREE ENTRY
	ORA CRNT
	BNE FRDECA
	LDA #STERRG;NO FREE CACHE DESCRIPTORS
	JSR INTERR
.FRDECA
	LDY #CENEXT
	LDA (CRNT),Y
	STA FRECHN
	INY
	LDA (CRNT),Y
	STA FRECHN + 1;FRECHN:=NEXT OF CRNT
	RTS
}

.STCRNT
	LDA CACHTB;PREV:=CACHTB;CRNT:=NEXT OF PREV
	STA PREV
	LDA CACHTB + 1
	STA PREV + 1
	LDY #CENEXT
	LDA (PREV),Y
	STA CRNT
	INY
	LDA (PREV),Y
	STA CRNT + 1
	RTS


.INSRCH
	JSR STCRNT;INITIALISE CRNT,CMPPTR
	CLC
	LDA ARGPTR
	ADC #ARGB
	STA CMPPTR
	LDA ARGPTR + 1
	ADC #0
	STA CMPPTR + 1
	RTS


.NEXTCE
{
	LDY #CENEXT;PREV:=CRNT;CRNT:=NEXT OF CRNT
	LDA (CRNT),Y
	INY
	ORA (CRNT),Y
	BEQ NEXTCZ;NO MORE ENTRIES (A=0)
	LDA CRNT
	STA PREV
	LDA CRNT + 1
	STA PREV + 1
	DEY ;Y := CENEXT
	LDA (CRNT),Y
	TAX
	INY
	LDA (CRNT),Y
	STA CRNT + 1
	TXA
	STA CRNT
	LDA #&FF;A INDICATES THAT THERE ARE MORE ENTRIES
.NEXTCZ
	RTS
}

.COMP
{
	TXA ;COMPARE CRNT ENTRY WITH X ARGS
	TAY
.COMPA
	DEY
	LDA (CMPPTR),Y
	CMP (CRNT),Y
	BNE COMPZ
	DEX
	BNE COMPA
.COMPZ
	TXA
	RTS
}

.TSTSIN
{
	LDY #ARGD
	LDA (ARGPTR),Y
	INY
	ORA (ARGPTR),Y
	INY
	ORA (ARGPTR),Y
	BNE TSTY
	LDA #STERRB;SIN = 0
	RTS
.TSTY
	LDA #0
	RTS
}

.RETSTA
	LDY #CESTA
	LDA (CRNT),Y
	LDY #ARGB
	STA (ARGPTR),Y;LS(STORE ADDRESS)
	LDY #CESTA + 1
	LDA (CRNT),Y
	LDY #ARGC
	STA (ARGPTR),Y;MS(STORE ADDRESS)
	INY ;Y := ARGD
	LDA CRNT
	STA (ARGPTR),Y
	INY
	LDA CRNT + 1
	STA (ARGPTR),Y;ADDRESS OF CRNT DESCRIPTOR
	RTS


.READ
	JSR MRKCLN;MARK BUFFER CLEAN
	LDA #1;READ OBJECT
	JSR SETRTN;DSCMAN.READ
	JMP TRANSF


.ENSURE
{
	JSR TSTMRK;IS IT DIRTY?
	BEQ ENSURZ
	LDA #2
	JSR SETRTN
	JSR TRANSF;WRITE IT TO DISC

	BNE ENSURZ
	JSR MRKCLN;preserve return code from TRANSF
.ENSURZ
	RTS
}

.TRANSF
	LDA CRNT
	STA MOVFRM
	LDA CRNT + 1
	STA MOVFRM + 1
	CLC
	LDA NEWARG
	ADC #ARGB
	STA MOVTO
	LDA NEWARG + 1
	ADC #0
	STA MOVTO + 1
	LDX #&B;DISC NO -> STORE ADDRESS
	JSR MOVE
	JMP DSCMAN


.REMVIT
{
	PHA ;PUSH A
	JSR TSTCNT
	BEQ REMOV
	LDA #STERRH
	JSR INTERR
.REMOV
	LDY #CENEXT;REMOVE CRNT FROM IN USE CHAIN
	LDA (CRNT),Y
	STA (PREV),Y
	PHA
	INY
	LDA (CRNT),Y
	STA (PREV),Y;NEXT OF PREV := NEXT OF CRNT
	PHA
	JSR MKFREE;PLACE IT ON FREE CHAIN
	PLA
	STA CRNT + 1
	PLA
	STA CRNT;CRNT := NEXT OF PREV
	PLA ;PULL A
	RTS
}

	;EXPELL: ROUTINE TO REMOVE OBJECTS FROM THE CACHE.
	;ENTRY: X := 2 => MATCH ON DISC NUMBER (I.E. ARGB&ARGC)
	;X := 5 => MATCH ON DISC NUMBER + SIN

	;THE ENTRY EXPELA IS USED FROM STRDRT WITH
	;A NON-ZERO TO INDICATE THAT ONLY DIRTY
	;BLOCKS SHOULD BE REMOVED FROM THE CACHE.

.EXPELL
	LDA #0;=> REMOVE ALL BLOCKS REGARDLESS
.EXPELA
{
	PHA
	STX STRTMP;STRTMP := X
	JSR INSRCH
.EXPLLC
	LDX STRTMP
	JSR COMP;COMPARE DISC NUMBER & (POSSIBLY) SIN
	BNE EXPLLF
	JSR TSTCNT
	BEQ EXPLLD
	LDA #STERRH;REF COUNT > 0
	JSR INTERR

	;ENSURE THE OBJECT & THEN REMOVE IT FROM THE CACHE

.EXPLLD
	PLA
	PHA
	BEQ EXPLLG;EQ => REMOVE ALL BLOCKS
	JSR TSTMRK;NZ => REMOVE DIRTY BLOCKS ONLY
	BEQ EXPLLF;NOT DIRTY, GO TO NEXT BLOCK
	
	; Age entries on ensure : remove entries on destroy
	JSR ENSURE; ensure dirty entry
	PHA
	LDY #CEAGE
	LDA #&80
	STA (CRNT),Y; Age entry
	PLA
	BNE EXPLLX; Detect disc errors
	BEQ EXPLLF; Step to next block because non removed.

.EXPLLG
	JSR ENSURE;ENSURE ITS UPTO DATE ON DISC
	JSR REMVIT; Destroy file so remove all.
	BNE EXPLLX; Detect disc errors.

	LDY #CENEXT; REMVIT shuffles blocks down so process new current.
	LDA (CRNT),Y
	INY
	ORA (CRNT),Y
	BNE EXPLLC
.EXPLLF
	JSR NEXTCE;PREV:=CRNT;CRNT:=NEXT OF CRNT
	BNE EXPLLC
	PLA ;RESTORE STACK

	LDA #0;RC:=0
	RTS

.EXPLLX
	TAX ;save A, clean stack
	PLA
	TXA
	RTS
}

.FNDSTA
{
	LDY #ARGB;FIND CACHE ENTRY WITH GIVEN STORE ADDRESS
	LDA (ARGPTR),Y
	STA STRTMP
	INY
	LDA (ARGPTR),Y
	STA STRTMP + 1
	JSR STCRNT
.FNDSAA
	LDY #CESTA
	LDA (CRNT),Y
	CMP STRTMP
	BNE FNDSAC
	INY
	LDA (CRNT),Y
	CMP STRTMP + 1
	BEQ FNDSAZ
.FNDSAC
	JSR NEXTCE
	BNE FNDSAA
	LDA #STERRF;INVALID WINDOW ADDRESS
.FNDSAZ
	RTS
}

.DECGEN
	SEC
	LDY #CESTA
	LDA (CRNT),Y
	SBC #1
	STA GENPTR
	INY
	LDA (CRNT),Y
	SBC #0
	STA GENPTR + 1
	RTS


.MRKCLN
	PHA ;MARK STORE BUFFER CLEAN
	TYA ;SAVE A & Y
	PHA ;PUSH Y
	JSR DECGEN;GENPTR SHOULD CONTAIN ADDRESS OF BUFFER
	LDA #0;A := 0
	TAY ;Y := 0
	STA (GENPTR),Y;[GENPTR] := 0
	PLA
	TAY ;PULL Y
	PLA ;PULL A
	RTS


.TSTMRK
	JSR DECGEN;TEST DIRTY MARKER
	LDY #0;NOTE THIS RTN DESTROYS A & Y
	LDA (GENPTR),Y;A:=[GENPTR]
	RTS


.INCCNT
{
	LDY #CERCNT;INCREMENT REFERENCE COUNT
	LDA (CRNT),Y
	CMP #&FF
	BNE INCCNA
	LDA #STERRK;REFERENCE COUNT = $FF
	JSR INTERR
.INCCNA
	CLC
	ADC #1
	STA (CRNT),Y
	RTS
}

.DECCNT
{
	LDY #CERCNT;DECREMENT REFERENCE COUNT
	LDA (CRNT),Y
	BNE DECCNA
	LDA #STERRD;REFERENCE COUNT = 0
	JSR INTERR
.DECCNA
	SEC
	SBC #1
	STA (CRNT),Y
	RTS
}

.TSTCNT
	LDY #CERCNT;TEST VALUE OF REFERNENCE COUNT
	LDA (CRNT),Y
	RTS


	;FREEST: GET AMTNED WORTH OF FREE STORE.
	;EXIT : A = RC
	;CRNT POINTS TO DESCRIPTOR WITH STORE ADDRESS SET

	;FUNCTION:-
	;1) AGE ALL THE ENTRIES IN THE CACHE
	;2)FIND OUT THE MOST COST EFFECTIVE AREA
	;OF MEMORY TO USE.

	;WHILE crnt <> 0 AND min cost <> 0
	;DO IF amount needed<=(storeaddrOFcrnt-storeaddrOFprev
	;- lengthOFprev-2)
	;THEN IF cost<min cost
	;THEN min cost := cost;
	;best start := prev;
	;best end := crnt
	;FI;
	;prev:=nextOFprev;
	;cost:=cost-costOFprev
	;ELSE IF ref count OF crnt <> 0
	;THEN prev := crnt;
	;cost := 0
	;ELSE cost +:= cost OF crnt
	;FI;
	;crnt := next OF crnt
	;FI
	;OD;

	;WHILE crnt <> best end
	;DO ensure(crnt); #ENSURE WINDOW UPTO DATE ON DISC#
	;remove(crnt) #REMOVE CRNT FROM IN USE CHAIN#
	;OD;

	;crnt := get free descriptor;
	;straddrOFcrnt:=straddrOFprev+lengthOFprev+1




.FREEST
{
	LDA AMTNED
	ORA AMTNED + 1
	BNE FREETA
	LDA #STERRE
	RTS

	;FIRST,AGE EACH OBJECT IN THE CACHE

.FREETA
	JSR STCRNT;PREV:=CACHTB;CRNT:=NEXT OF PREV

.FREETB
	LDY #CEAGE
	LDA (CRNT),Y
	CMP #LRU
	BEQ FREETD
	SEC
	SBC #1
	STA (CRNT),Y;AGE OF CRNT -:= 1
.FREETD
	JSR NXTCRN
	BNE FREETB

	;NOW SEE WHICH IS THE BEST AREA OF MEMORY TO USE

	JSR STCRNT
	LDA #0
	STA BSTSTA
	STA BSTSTA + 1;BSTSTA : = 0
	STA BSTEND
	STA BSTEND + 1;BSTEND := 0
	STA COST
	STA COST + 1;COST := 0
	LDA #&FF
	STA MINCST
	LDA #&7F
	STA MINCST + 1;MINCST := $7FFF

.FREETL
	JSR TSTGAP
	BEQ FREEJC
	JMP FREETQ

	;SEE IF COST < MIN COST

.FREEJC
	LDX #&FF;X<>0 here
	LDA COST
	PHA
	LDA COST + 1
	PHA ;save cost for new algorithm

	LDY #CENEXT
	LDA (PREV),Y
	STA GP1
	INY
	LDA (PREV),Y
	STA GP1 + 1;intermediate pointer

	LDA (GP1),Y
	CMP CRNT + 1
	BNE FREEJA
	DEY
	LDA (GP1),Y
	CMP CRNT
	BNE FREEJA;skip if not single slot

	LDY #CEAGE
	LDA (GP1),Y;get the age
	TAX
	STA COST;make new account
	LDA #0
	STA COST + 1

	LDY AMTNED + 1
	LDA AMTNED
	BEQ FREEJB
	INY

.FREEJB
	DEY
	BEQ FREEJA
	CLC
	TXA
	ADC COST
	STA COST
	BCC FREEJB
	INC COST + 1
	BNE FREEJB

.FREEJA
	LDA COST
	CMP MINCST
	LDA COST + 1
	SBC MINCST + 1
	BCS FREETM

	LDA PREV
	STA BSTSTA
	LDA PREV + 1
	STA BSTSTA + 1;BSTSTA := PREV
	LDA CRNT
	STA BSTEND
	LDA CRNT + 1
	STA BSTEND + 1;BSTEND := CRNT
	LDA COST
	STA MINCST
	LDA COST + 1
	STA MINCST + 1;MINCST := COST
	ORA MINCST
	TAX

.FREETM
	PLA
	STA COST + 1
	PLA
	STA COST;restore old value of COST
	TXA
	BEQ FREETV;I.E. FOUND A GAP
	LDY #CENEXT
	LDA (PREV),Y
	TAX
	INY
	LDA (PREV),Y
	STA PREV + 1
	TXA
	STA PREV;PREV := NEXT OF PREV

	LDY #CEBLKS
	LDA (PREV),Y;get length
	TAX ;count around the loop

.FREEK
	SEC
	LDA COST
	LDY #CEAGE
	SBC (PREV),Y
	STA COST
	BCS FREETJ
	DEC COST + 1;cost -:= cost OF prev*length OF prev
.FREETJ
	DEX
	BNE FREEK;count for length of blocks
	JMP FREETL

.FREETQ
	JSR TSTCNT
	BEQ FREETR

	;COST := 0; PREV := CRNT

	LDA #0
	STA COST
	STA COST + 1
	LDA CRNT
	STA PREV
	LDA CRNT + 1
	STA PREV + 1
	JMP FREETT

.FREETR
	LDY #CEBLKS
	LDA (CRNT),Y;grab length (one byte only)
	TAX ;count around loop

.FREET1
	CLC ;cost +:=(cost OF crnt)*(length OF crnt)
	LDA COST
	LDY #CEAGE
	ADC (CRNT),Y
	STA COST
	BCC FREET2
	INC COST + 1
.FREET2
	DEX
	BNE FREET1;continue until all done

.FREETT
	JSR NXTCRN;CRNT := NEXT OF CRNT
	BEQ FREETS
	JMP FREETL

	;FIRST SEE IF WE FOUND AN AREA OF MEMORY TO USE

.FREETS
	LDA BSTSTA
	ORA BSTSTA + 1
	BNE FREETV
	LDA #STERRL;STORE DEADLOCK !!!!
	RTS

	;NOW REMOVE ALL THE OBJECTS IN THE GAP

.FREETV
	LDA BSTSTA
	STA PREV
	LDA BSTSTA + 1
	STA PREV + 1;PREV := BSTSTA
	LDY #CENEXT
	LDA (PREV),Y
	STA CRNT
	INY
	LDA (PREV),Y
	STA CRNT + 1;CRNT := NEXT OF PREV

	;NOW CREATE THE GAP (IF NECESSARY)

	LDA #0
	PHA ;save RC
.FREETW
	LDA BSTEND;WE MUST DO THIS TEST FIRST
	CMP CRNT
	BNE FREETX
	LDA BSTEND + 1
	CMP CRNT + 1
	BNE FREETX

	JSR NEWDES;GET FREE DESCR,CHAIN IT IN,CALC STORE ADDR
	PLA ;RC here
	RTS

.FREETX
	JSR ENSURE
	BEQ FREETY

	CMP #&C9;look for protected disc
	BEQ FREETU;skip as object is now removed anyway

	JSR INTERR
.FREETY
	JSR REMVIT
	JMP FREETW

.FREETU
	TAX
	PLA ; change old RC
	BNE FREETP
	TXA
.FREETP
	PHA ;change return code
	JMP FREETY


.NEWDES
	JSR FRDESC;CRNT := NEW DESCRIPTOR

	;storeaddrOFcrnt:=storeaddrOFprev+lengthOFprev+1

	JSR LNTPRE
	SEC ;BECAUSE WE WANT TO ADD 1
	LDY #CESTA
	LDA (PREV),Y
	ADC BREGA
	STA (CRNT),Y
	INY
	LDA (PREV),Y
	ADC BREGA + 1
	STA (CRNT),Y

	;NOW CHAIN IT IN

	LDY #CENEXT
	LDA (PREV),Y
	STA (CRNT),Y
	INY
	LDA (PREV),Y
	STA (CRNT),Y;NEXT OF CRNT := NEXT OF PREV
	DEY ;Y:=CENEXT
	LDA CRNT
	STA (PREV),Y
	INY
	LDA CRNT + 1
	STA (PREV),Y;NEXT OF PREV := CRNT
	RTS
}

.LNTPRE
	LDY #CEBLKS;BREGA := LENGTH OF PREV (IN BYTES)
	LDA (PREV),Y
	STA BREGA
	INY
	LDA (PREV),Y
	STA BREGA + 1
	JMP MULTBS


	;TSTGAP: A:= IF amount needed <= storeaddrOFcrnt-
	;storeaddrOFprev-lengthOFprev-2
	;THEN 0
	;ELSE FF
	;FI

.TSTGAP
{
	JSR LNTPRE;BREGA := LENGTH OF PREV(IN BYTES)
	CLC
	LDY #CESTA
	LDA (CRNT),Y
	SBC (PREV),Y
	STA STRTMP
	INY
	LDA (CRNT),Y
	SBC (PREV),Y
	STA STRTMP + 1;STRTMP:=straddrOFcrnt-straddrOFprev-1
	SEC
	LDA BREGA
	ADC AMTNED
	STA BREGA
	LDA BREGA + 1
	ADC AMTNED + 1
	STA BREGA + 1;BREGA:=amountneeded+lengthOFprev+1
	BVC TSTGB
	LDA #STERRM;OVERFLOW!!!
	JSR INTERR
.TSTGB
	SEC
	LDA STRTMP
	SBC BREGA
	LDA STRTMP + 1
	SBC BREGA + 1
	BMI TSTGL
	LDA #0;I.E. GAP BIG ENOUGH
	RTS
.TSTGL
	LDA #&FF;I.E GAP TOO SMALL
	RTS
}

.NXTCRN
	LDY #CENEXT;CRNT := NEXT OF CRNT
	LDA (CRNT),Y
	TAX
	INY
	LDA (CRNT),Y
	STA CRNT + 1
	TXA
	STA CRNT
	ORA CRNT + 1;A:= CRNT ! CRNT +01
	RTS

;.LNK
;UADE0C
;