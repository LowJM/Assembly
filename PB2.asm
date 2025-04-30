.MODEL SMALL
.STACK 100H
.DATA

  welcome_msg DB "Welcome to Public Bank System!$" ;welcome msg
  options_msg DB "Please Choose a Function.(1=Deposit,2=Withdraw,3=fixed deposit,4=log out):$" ;choose 1 option
  input_char DB ?
  invalid_msg DB 0DH,0AH,"Invalid Option. Please try again.$"
  ;fong
  transaction_count dw 0  ; Counter variable
  exit_msg       db 13, 10, 'Session ended.', '$'
  ;vincent
  display_interest DB "The current interest rate is 3.2% per annum$"
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
	count_msg      db 13, 10, 'Total transactions: $'
	;error message for validation
	err_year_0 DB "Invalid year. Please make sure at least 1 year", "$"
	
	;yong
	current_balance_msg DB 13,10,"Current Balance: RM$"
    deposit_msg DB 13,10,"Enter deposit amount: RM$"
    withdraw_msg DB 13,10,"Enter withdrawal amount: RM$"
    new_balance_msg DB 13,10,"New Balance: RM$"
    insufficient_msg DB 13,10,"Error: Insufficient funds!$"
    
    balance DW 10000  ; Initial balance (RM100.00 stored as cents)
    amount DW ?       ; Temporary amount storage
	
	;--------------------------------------
	;error messages for validation
	;err_min_deposit DB 13, 10, "Error: Minimum deposit is RM 100", 13, 10, "$"
    ;err_max_deposit DB 13, 10, "Error: Maximum deposit is RM 99,999", 13, 10, "$"
    ;err_min_years   DB 13, 10, "Error: Minimum term is 1 year", 13, 10, "$"
    ;err_max_years   DB 13, 10, "Error: Maximum term is 30 years", 13, 10, "$"
	
	;ian
	; Menu Texts
    menuText    DB 13,10,'1. Register',13,10
                DB '2. Login',13,10
                DB '3. Exit',13,10
                DB 'CHoice: $'
    
    ; System MESsagES
    regText     DB 13,10,'-- Register --',13,10,'$'
    logINText   DB 13,10,'-- Login --',13,10,'$'
    exitText    DB 13,10,'Exiting...',13,10,'$'
    regSuccESsMsg DB 13,10,'Registration complete!',13,10,'$'
    loginSuccESsMsg  DB 13,10,'Login successful!',13,10,'$'
    loginFailMsg     DB 13,10,'Login failed! INCorrect username or password.',13,10,'$'
    inputErrorMsg    DB 13,10,'Error: Input cannot be empty!',13,10,'$'

    ; Input Buffers
    inputUname  DB 20, 0, 20 dup(0)  ; MAX 20 CHars + null
    inputPass   DB 20, 0, 20 dup(0)   
    
    ; STOred CredentiALs
    savedUname  DB 21 dup(0)          ; 20 CHars + null
    savedPass   DB 21 dup(0)          

    ; Prompts
    unamePrompt DB 13,10,'Username: $'
    passPrompt  DB 13,10,'Password: $'

.CODE
MAIN PROC FAR
    CALL start
	
Welcome:

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
	; Display welcome message
    MOV AH, 09H
    LEA DX, welcome_msg
    INT 21H
		
	CALL Functions
	
Functions:

	CALL Newline
	
    ; Display prompt - MUST include $ terminator!
    MOV AH, 09H
    LEA DX, options_msg
    INT 21H
	
	CALL Newline
    
    ; Force buffer flush and wait for input
    MOV AH, 0CH       ; Clear input buffer
    MOV AL, 01H       ; Then do function 01H
    INT 21H           ; Now waits properly in DEBUG
    
    ; AL now contains the character
	CMP AL, '1'
	JE CallDeposit
	
	CMP AL, '2'
	JE CallWithdraw
	
    CMP AL, '3'
    JE CallFD
	
	CMP AL, '4'
    JNE Exception_Handling
	JMP main_menu
	
Exception_Handling:
    
    ; Invalid option
    MOV AH, 09H
    LEA DX, invalid_msg
    INT 21H
    JMP Functions
	
CallDeposit:
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
	CALL Deposit1
   
CallWithdraw:
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
	CALL Withdraw
	
CallFD:
    CALL Newline
    CALL FD
    
ExitProgram:
	;Display for seesion ended
	mov ah, 09h
	lea dx, exit_msg
	int 21h
	
	; Display total transactions
    mov ah, 09h
    lea dx, count_msg
    int 21h
    
    mov ax, transaction_count
    call display_number
	
	MOV AX, 4C00h
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
	
	MOV AH, 09H
    LEA DX, display_interest
    INT 21H
	
	CALL Newline 
    
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
	
	inc transaction_count
	
	JMP Functions
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
    AAM                 ;ascii adjust after multiply(convert binary to digit for display)
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

display_number proc
    push ax
    push bx
    push cx
    push dx
    
    mov cx, 0
    mov bx, 10
    
divide_loop:
    xor dx, dx
    div bx
    push dx
    inc cx
    test ax, ax
    jnz divide_loop
    
print_loop:
    pop dx
    add dl, '0'
    mov ah, 02h
    int 21h
    loop print_loop
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
display_number endp

DisplayCurrentBalance PROC
    MOV AH, 09h
    LEA DX, current_balance_msg
    INT 21h
    
    ; Convert balance to RM display (divide by 100)
    MOV AX, balance
    MOV BX, 100
    XOR DX, DX        ; Clear DX for division
    DIV BX            ; AX = dollars, DX = cents
    
    ; Display dollar amount
    CALL PrintNum
    
    ; Display decimal point and cents
    MOV AH, 02h
    MOV DL, '.'
    INT 21h
    
    MOV AX, DX        ; Move cents to AX
    CALL Print2Digit  ; Your existing procedure
    
    RET
DisplayCurrentBalance ENDP

; Display New Balance Procedure
DisplayNewBalance PROC
    MOV AH, 09h
    LEA DX, new_balance_msg
    INT 21h
    
    ; Convert balance to RM display (divide by 100)
    MOV AX, balance
    MOV BX, 100
    XOR DX, DX        ; Clear DX for division
    DIV BX            ; AX = dollars, DX = cents
    
    ; Display dollar amount
    CALL PrintNum
    
    ; Display decimal point and cents
    MOV AH, 02h
    MOV DL, '.'
    INT 21h
    
    MOV AX, DX        ; Move cents to AX
    CALL Print2Digit  ; Your existing procedure
    
    RET
DisplayNewBalance ENDP

;-----------------------------
; Deposit Procedure
Deposit1 PROC
    CALL DisplayCurrentBalance
    
    ; Prompt for deposit amount
    MOV AH, 09h
    LEA DX, deposit_msg
    INT 21h
    
	; Clear buffer
    LEA DI, deposit+2
    MOV CX, 6
    MOV AL, 0
    REP STOSB
	
    ; Get amount input (using your existing input buffer)
    MOV AH, 0Ah
    LEA DX, deposit
    INT 21h
    
    ; Convert to number
    LEA SI, deposit+2
    CALL ATOI          ; Your existing ATOI procedure
    
    ; Multiply by 100 to convert to cents
    MOV BX, 100
	XOR DX, DX
    MUL BX
    MOV amount, AX
    
    ; Add to balance
	MOV AX, amount
    ADD balance, AX
    
    ; Display new balance
    CALL DisplayNewBalance
    
    inc transaction_count
    
    ; Go back to options
	JMP Functions
	
Deposit1 ENDP

;-----------------------------
; Withdrawal Procedure
Withdraw PROC
    CALL DisplayCurrentBalance
    
    ; Prompt for withdrawal amount
    MOV AH, 09h
    LEA DX, withdraw_msg
    INT 21h
    
	; Clear deposit buffer (REUSE the same buffer safely)
    LEA DI, deposit+2
    MOV CX, 6
    MOV AL, 0
    REP STOSB
	
    ; Get amount input
    MOV AH, 0Ah
    LEA DX, deposit    ; Reusing the same buffer
    INT 21h
    
    ; Convert to number
    LEA SI, deposit+2
    CALL ATOI
    
    ; Multiply by 100 to convert to cents
    MOV BX, 100
	XOR DX, DX
    MUL BX
    MOV amount, AX
    
    ; Check sufficient funds
	MOV AX, amount
    CMP AX, balance
    JA InsufficientFunds
    
    ; Subtract from balance
	MOV AX, amount
    SUB balance, AX
    
    ; Display new balance
    CALL DisplayNewBalance
    JMP WithdrawDone
    
InsufficientFunds:
    MOV AH, 09h
    LEA DX, insufficient_msg
    INT 21h
    
WithdrawDone:
    inc transaction_count
    
    ; Go back to options
	JMP Functions
	
Withdraw ENDP

LOGIN PROC NEAR
start:

    MOV AX, @data
    MOV DS, AX
    MOV ES, AX
    JMP main_menu

main_menu:
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
    LEA DX, menuText
    CALL prINT_string
    CALL read_CHar

    CMP AL, '1'
    JE do_register
    CMP AL, '2'
    JE do_login
    CMP AL, '3'
    JE do_exit
    JMP main_menu

; ========== SYSTEM FUNCTIONS ==========
do_exit:
    LEA DX, exitText
    CALL prINT_string
    CALL ExitProgram

do_register:
    LEA DX, regText
    CALL prINT_string
    
    ; Get username
get_reg_uname:
    LEA DX, inputUname
    CALL get_input
    JC get_reg_uname

    ; Get password
get_reg_pass:
    LEA DX, inputPass
    CALL get_input
    JC get_reg_pass
    
    ; Save credentiALs
    LEA SI, inputUname
    LEA DI, savedUname
    CALL copy_input_to_null_terminated

    LEA SI, inputPass
    LEA DI, savedPass
    CALL copy_input_to_null_terminated
    
    LEA DX, regSuccESsMsg
    CALL prINT_string
	CALL wait3s
    JMP main_menu

do_login:
    LEA DX, logINText
    CALL prINT_string
    
    ; Get username
get_login_uname:
    LEA DX, inputUname
    CALL get_input
    JC get_login_uname

    ; Get password
get_login_pass:
    LEA DX, inputPass
    CALL get_input
    JC get_login_pass
    
    ; Compare credentiALs
    LEA SI, inputUname
    LEA DI, savedUname
    CALL compare_input_vs_saved
    JNZ login_fail

    LEA SI, inputPass
    LEA DI, savedPass
    CALL compare_input_vs_saved
    JNZ login_fail
    
    LEA DX, loginSuccESsMsg
    CALL prINT_string
	CALL wait3s
    JMP Welcome 

login_fail:
    LEA DX, loginFailMsg
    CALL prINT_string
	CALL wait3s
    JMP main_menu

; ========== CORE FUNCTIONS ==========
get_input:
    PUSH DX
    ; Show prompt
    CMP DX, offset inputUname
    JE .uname_prompt
    LEA DX, passPrompt
    JMP .show_prompt
.uname_prompt:
    LEA DX, unamePrompt
.show_prompt:
    CALL prINT_string
    POP DX
    ; CLEAr buffer
    CALL CLEAr_input_buffer
    ; Read input
    CALL read_line
    ; VALidate input
    MOV SI, DX
    CMP byte ptr [SI+1], 0   ; CHeck actuAL length
    JE .empty_input
    ; ADD null terminator safely
    MOV BL, [SI+1]
    XOR BH, BH
    ADD SI, 2
    MOV byte ptr [SI+BX], 0  ; Terminate after input
    CLC
    RET
.empty_input:
    LEA DX, inputErrorMsg
    CALL prINT_string
    STC
    RET

copy_input_to_null_terminated:
    ; SI = input buffer, DI = saved buffer
    MOV CL, [SI+1]      ; ActuAL length
    XOR CH, CH
    ADD SI, 2            ; Start of input data
    JCXz .copy_done      ; Skip if empty
    REP MOVSB            ; Copy CL bytES
.copy_done:
    MOV byte ptr [DI], 0 ; Null-terminate
    RET

compare_input_vs_saved:
    ; SI = input buffer, DI = saved buffer
    MOV CL, [SI+1]      ; Input length
    XOR CH, CH
    ADD SI, 2            ; Input data start
.compare_loop:
    DEC CL
    JS .CHeck_saved_end  ; If input exhausted
    MOV AL, [SI]
    CMP AL, [DI]
    JNE .mismatCH
    INC SI
    INC DI
    JMP .compare_loop
.CHeck_saved_end:
    CMP byte ptr [DI], 0 ; Saved must ALso end here
    JNE .mismatCH
.matCH:
    XOR AX, AX           ; ZF=1 (matCH)
    RET
.mismatCH:
    MOV AX, 1            ; ZF=0 (mismatCH)
    RET

; ========== I/O FUNCTIONS ==========
prINT_string:
    MOV AH, 09h
    INT 21h
    RET

read_CHar:
    MOV AH, 01h
    INT 21h
    RET

read_line:
    MOV AH, 0AH
    INT 21h
    RET

CLEAr_input_buffer:
    ; DX = buffer ADDrESs
    MOV DI, DX
    MOV byte ptr [DI+1], 0 ; RESet actuAL length
    ADD DI, 2
    MOV CX, 20
    MOV AL, 0
    REP STOSB             ; Fill with zeros
    RET
	
wait3s:
    mov ah, 86h      ; BIOS wait function
    mov cx, 0BH   ; High 16 bits of microseconds (~3 sec)
    mov dx, 0B8A0H   ; Low 16 bits of microseconds (~3 sec)
    int 15h          ; Call BIOS
    ret


LOGIN ENDP

END MAIN
