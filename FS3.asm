	;*******************************************************
	;*                                                     *
	;*  A C O R N   L E V E L  3   F I L E   S E R V E R   *
	;*                                                     *
	;*    REFORMATTED AND UPDATED BY MARTIN MATHER 2022    *
	;*    ASSEMBLED USING BEEBASM 1.09                     *
	;*                                                     *	
	;*******************************************************


	;DEBUG OPTIONS
	DEBUG = FALSE
	DBG1 = DEBUG AND FALSE;SHOW DISC ACCESS
	DBG2 = DEBUG AND TRUE;NET RX
	DBG3 = DEBUG AND FALSE;DONGLE

	;VARIOUS OPTIONS
	LNGDAT = FALSE			;PRINT LONG DATE ON SCREEN
							;SHORT DATE MEANS MORE ROOM FOR CACHE

	;VERSION
	VERLA =  '2'
	VERLB =  '6'
	
	MASK =  &3F				;protection state
	

	INCLUDE "Uade01.asm"	;HEADER FILE 1
	INCLUDE "Uade02.asm"	;HEADER FILE 2


	ORG &0400
.CODESTART
	
	INCLUDE "Uade03.asm"	;UTILITIES
	INCLUDE "Uade04.asm"	;FILE SERVER INITIALISATION
	INCLUDE "Uade05.asm"	;GLORTN AND DOS CONVERSION
	INCLUDE "Uade06.asm"	;USRMAN
	
	INCLUDE "Rman01.asm"	;RNDMAN
	INCLUDE "Rman02.asm"
	INCLUDE "Rman03.asm"
	INCLUDE "Rman04.asm"		
	INCLUDE "Rman05.asm"	


	INCLUDE "Uade0A.asm"	;STRMAN
	INCLUDE "Uade0B.asm"	;STRMAN UTILITIES
	INCLUDE "Uade0C.asm"	;DIRMAN
	INCLUDE "Uade0D.asm"	;DIRMAN UTILITIES
	INCLUDE "Uade0E.asm"	;DIRMAN UTILITIES  2
	INCLUDE "Uade0F.asm"	;AUTMAN
	
	INCLUDE "Uade10.asm"	;MAPMAN
	
	INCLUDE "Uade11.asm"	;MAPMAN UTILITIES
	INCLUDE "Uade12.asm"	;MAPMAN UTILITIES 2
	INCLUDE "Uade13.asm"
	INCLUDE "MBBMCM.asm"	;MAP BLOCK AND BIT MAP MANAGEMENT
	INCLUDE "Uade14.asm"	;DSCMAN
	INCLUDE "Uade15.asm"	;COMMAND PROCESSOR
	INCLUDE "Uade16.asm"	;SAVE, LOAD ETC
	INCLUDE "Uade17.asm"	;Random access and RENAME & User Info
	INCLUDE "Uade18.asm"	;GETBYT
	INCLUDE "Uade19.asm"
	INCLUDE "Uade20.asm"	;COMMAND PROCESSOR UTILITIES

.CODEEND
	
	SAVE "FS3v126", CODESTART, CODEEND
	
	
	