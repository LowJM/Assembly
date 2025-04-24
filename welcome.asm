.MODEL SMALL
.STACK 100H
.DATA

  welcome_msg DB "Welcome to Public Bank System!$" ;welcome msg
  options_msg DB "Please Choose a Function" ;choose 1 option
  input_char DB ?
  invalid_msg DB 0DH,0AH,"Invalid Option. Please try again.$"
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
    
    display DB "Enter deposit amount: RM", "$"
    dispYear DB "How many years of interest would you like to accumulate: ", "$"
    dispFinal DB "Final balance is: RM", "$"
	
	;--------------------------------------
	;error messages for validation
	;err_min_deposit DB 13, 10, "Error: Minimum deposit is RM 100", 13, 10, "$"
    ;err_max_deposit DB 13, 10, "Error: Maximum deposit is RM 99,999", 13, 10, "$"
    ;err_min_years   DB 13, 10, "Error: Minimum term is 1 year", 13, 10, "$"
    ;err_max_years   DB 13, 10, "Error: Maximum term is 30 years", 13, 10, "$"

.CODE
MAIN PROC FAR
    MOV AX, @DATA
    MOV DS, AX
    
    ; Display welcome message
    MOV AH, 09H
    LEA DX, welcome_msg
    INT 21H
    
    ; Display prompt - MUST include $ terminator!
    MOV AH, 09H
    LEA DX, options_msg
    INT 21H
    
    ; Force buffer flush and wait for input
    MOV AH, 0CH       ; Clear input buffer
    MOV AL, 01H       ; Then do function 01H
    INT 21H           ; Now waits properly in DEBUG
    
    ; AL now contains the character
    CMP AL, '2'
    JE CallFD
    
    ; Invalid option
    MOV AH, 09H
    LEA DX, invalid_msg
    INT 21H
    JMP ExitProgram
    
CallFD:
    CALL FD
    
ExitProgram:
    MOV AX, 4C00h
    INT 21H
MAIN ENDP

	
    
	
;new line
;MOV DL, 0DH
;INT 21H
;MOV DL, 0AH
;INT 21H
;process
FD PROC NEAR
;-------FORMAT SCREEN------------
	MOV AH, 06h
	MOV AL, 00h
	MOV BH, 71h
	MOV CX, 0000h
	MOV DX, 184Fh
	INT 10h
	
	MOV AH, 02h
	MOV BH, 00
	MOV DX, 0A00h
	INT 10h
;------FORMAT SCREEN END--------

    MOV AX, @DATA
    MOV DS, AX
    
    ; Prompt and get deposit
    MOV AH, 09h
    LEA DX, display
    INT 21h
    
    MOV AH, 0Ah
    LEA DX, deposit
    INT 21h
    
    ; Convert deposit to number
    LEA SI, deposit+2
    CALL ATOI
    MOV depositNum, AX
    
    CALL Newline
    
    ; Prompt and get years	
    MOV AH, 09h
    LEA DX, dispYear
    INT 21h
    
    MOV AH, 0Ah
    LEA DX, year
    INT 21h
    
    ; Convert years to number
    LEA SI, year+2
    CALL ATOI
    MOV yearNum, AX
    
    ; Calculate result
    CALL CALC
    CALL NEWLINE
    
    ; Display final balance message	
    MOV AH, 09h
    LEA DX, dispFinal
    INT 21h
    
    MOV AX, resultTotal
    CALL PrintNum
    
    ; Display decimal point and cents
    MOV AH, 02h
    MOV DL, '.'
    INT 21h
    
    MOV AL, resultDec
    CALL Print2Digit
    
    ; Proper exit
    MOV AX, 4C00h
    INT 21h
	RET
FD ENDP
;----------------------------------------
; String to integer conversion
ATOI PROC
    XOR AX, AX
    XOR CX, CX
ATOI_LOOP:
    MOV CL, [SI]
    CMP CL, 13          ; Check for carriage return
    JE ATOI_DONE
    SUB CL, '0'         ; Convert ASCII to digit
    MOV BX, 10
    MUL BX              ; AX = AX * 10
    ADD AX, CX          ; Add new digit
    INC SI
    JMP ATOI_LOOP
ATOI_DONE:
    RET
ATOI ENDP

;-----------------------------------------
; Print number in AX
PrintNum PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    MOV CX, 0
    MOV BX, 10
PN1:
    XOR DX, DX
    DIV BX
    PUSH DX
    INC CX
    TEST AX, AX
    JNZ PN1
PN2:
    POP DX
    ADD DL, '0'
    MOV AH, 02h
    INT 21h
    LOOP PN2
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
    MOV AH, 02h
    INT 21h
    POP DX
    MOV AH, 02h         ; Print ones digit
    INT 21h
    RET
Print2Digit ENDP

;-----------------------------------------
Newline PROC
    MOV AH, 02h
    MOV DL, 13
    INT 21h
    MOV DL, 10
    INT 21h
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