.MODEL SMALL
.STACK 64

.DATA
    deposit LABEL   BYTE
    max_len DB      6
    act_len DB      ?
    kb_data DB      6 DUP(?)
    
    interest_rate EQU 32
    
    year    DB      4           ;max length
            DB      ?           ;actual length
            DB      4 DUP(?)
            
    depositNum  DW ?
    yearNum     DW ?
    resultTotal DW ?
    resultDec   DB ?
    
	;prompts for input
    display DB "Enter deposit amount: RM", "$"
    dispYear DB "How many years of interest would you like to accumulate: ", "$"
    dispFinal DB "Final balance is: RM", "$"
	
	;error message for validation
	err_year_0 DB "Invalid year. Please make sure at least 1 year", "$"
    
.CODE 
MAIN PROC FAR;dummy main procedure for building program

	CALL FD
	MOV AX, 4C00H
    INT 21H
	
MAIN ENDP

;process
FD PROC
;-------FORMAT SCREEN------------
	MOV AH, 06H			;scroll screen
	MOV AL, 00H			;clear whole window
	MOV BH, 71H			;white background, blue text
	MOV CX, 0000H		;top-left
	MOV DX, 184FH		;bottom-right
	INT 10H
	
	MOV AH, 02H			;set cursor
	MOV BH, 00			;page 0
	MOV DX, 0A00H		;row 10 column 0
	INT 10h
;------FORMAT SCREEN END--------

    MOV AX, @DATA
    MOV DS, AX
    
    ; Prompt for deposit
    MOV AH, 09H
    LEA DX, display
    INT 21H
    
	;input for deposit
    MOV AH, 0AH
    LEA DX, deposit
    INT 21H
    
    ; Convert deposit to number
    LEA SI, deposit+2
    CALL ATOI
    MOV depositNum, AX
    
    CALL Newline	;start next message on new line
    
    ; Prompt for year
    MOV AH, 09H
    LEA DX, dispYear
    INT 21H
    
	;input for year
	YearInput:
    MOV AH, 0AH
    LEA DX, year
    INT 21H
    
    ; Convert years to number
    LEA SI, year+2
    CALL ATOI
    MOV yearNum, AX
    
	;check year
	CMP AX, 0
	JE YearInvalid	;jump to YearInvalid if year = 0
	JMP YearValid	;jump to YearValid if year not 0
	
	YearInvalid:
	CALL NEWLINE
	MOV AH, 09H
	LEA DX, err_year_0
	INT 21H
	CALL NEWLINE
	JMP YearInput	;ask again for year
	
    ;Calculate result
	YearValid:
    CALL CALC
    CALL NEWLINE
    
    ;Display final balance message	
    MOV AH, 09H
    LEA DX, dispFinal
    INT 21H
    
	;Display final balance
    MOV AX, resultTotal
    CALL PrintNum
    
    ; Display decimal point for cents
    MOV AH, 02H
    MOV DL, '.'
    INT 21H
    
    MOV AL, resultDec
    CALL Print2Digit
	
	RET
FD ENDP
;----------------------------------------
; String to integer conversion
ATOI PROC	;(ASCII to Integer)
    XOR AX, AX			;clear AX(result)
    XOR CX, CX			;clear CX
ATOI_LOOP:
    MOV CL, [SI]		;get next character
    CMP CL, 13          ;Check for Enter key
    JE ATOI_DONE		;Done if Enter
    SUB CL, '0'         ;Convert ASCII to digit(eg '3' -> 3)
    MOV BX, 10
    MUL BX              ;AX = AX * 10 (shift digits left)
    ADD AX, CX          ;Add new digit
    INC SI				;next character
    JMP ATOI_LOOP
ATOI_DONE:
    RET
ATOI ENDP

;-----------------------------------------
; Print number stored in AX
PrintNum PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    MOV CX, 0			;digit counter
    MOV BX, 10
PN1:
    XOR DX, DX			;clear DX for division
    DIV BX				;AX = AX/10, DX = remainder
    PUSH DX				;save digit
    INC CX				;count digit
    TEST AX, AX			;is AX zero
    JNZ PN1				;if not zero jump back to PN1
PN2:
    POP DX				;get digit
    ADD DL, '0'			;convert to ASCII
    MOV AH, 02H			;print character
    INT 21H			
    LOOP PN2			;repeat for all digits
    POP DX
    POP CX
    POP BX
    POP AX
    RET
PrintNum ENDP

;-----------------------------------------
; Calculate decimal portion (0-99) with rounding
GetDecimal PROC
    ; Input: DX = remainder (0-999)
    ; Output: AL = decimal portion (0-99) with proper rounding
    MOV AX, DX          ; remainder (0-999)
    MOV BX, 100
    MUL BX              ; AX = remainder * 100
    MOV BX, 1000
    DIV BX              ; AX = (remainder*100)/1000
    
    ; Rounding logic: if remainder >= 500, add 1
    CMP DX, 500         ; Compare remainder of division
    JB NoRound          ; If below 500, no rounding
    INC AX              ; Else round up
NoRound:
    CMP AX, 100         ; Ensure we didn't overflow to 100
    JB StoreDecimal
    MOV AX, 99          ; Cap at 99 if rounding caused overflow
StoreDecimal:
    RET
GetDecimal ENDP

;-----------------------------------------
; Print 2-digit number in AL
Print2Digit PROC
    AAM                 ; AH = AL/10, AL = AL%10
    ADD AX, 3030h       ; Convert to ASCII
    PUSH AX
    MOV DL, AH          ; Print tens digit
    MOV AH, 02H
    INT 21H
    POP DX
    MOV AH, 02H         ; Print ones digit
    INT 21H
    RET
Print2Digit ENDP

;-----------------------------------------
Newline PROC
    MOV AH, 02H
    MOV DL, 13
    INT 21H
    MOV DL, 10
    INT 21H
    RET
Newline ENDP

;-----------------------------------------
CALC PROC
    ; Formula: A = P + (P × r × t)/1000
    ; Where r = 32 (3.2%)
    
    ; P × t
    MOV AX, depositNum
    MOV BX, yearNum
    MUL BX              ; DX:AX = P × t
    
    ; × r (32)
    MOV BX, 32
    MUL BX              ; DX:AX = P × t × 32
    
    ; ÷ 1000
    MOV BX, 1000
    DIV BX              ; AX = integer result, DX = remainder (0-999)
    
    ; Get decimal portion
    PUSH AX             ; Save integer result
    CALL GetDecimal     ; Returns decimal in AL
    MOV resultDec, AL   ; Store decimal portion
    POP AX              ; Restore integer result
    
    ; Add original deposit
    ADD AX, depositNum
    MOV resultTotal, AX
    
    RET
CALC ENDP

END MAIN