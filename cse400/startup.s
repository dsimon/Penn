; 68000 Minimal System boot PROM
;
; Derron Simon

		ORG	$00000000

VECTOR_INIT:	DC.L	$00008100	; Reset: Initial SSP
		DC.L	STARTUP		; Reset: Initial PC
		DS.L	254		; rest of vectors

		ORG	$00000400

; STARTUP - insert monitor routines here

STARTUP:	MOVE.W		SR,D0		; copy SR to D0	
		BCLR.L		#13,D0		; set to user state
		MOVE.W		D0,SR
LOOP:		BRA.S		LOOP		; LOOP FOREVER!!
		END




