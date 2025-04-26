; Fixed Deposit Calculator (fixed_deposit.asm)
; Assemble with: masm fixed_deposit.asm ; link fixed_deposit.obj

.MODEL SMALL
.STACK 100H

.DATA
    welcome_msg    DB 'Fixed Deposit Calculator', 13, 10, '$'
    prompt_deposit DB 13, 10, 'Enter deposit amount: $'
    prompt_rate    DB 13, 10, 'Enter interest rate (%): $'
    prompt_years   DB 13, 10, 'Enter number of years: $'
    result_msg     DB 13, 10, 'Final balance after $'
    years_text     DB ' years is: $'
    currency       DB '$'
    
    deposit DW ?
    rate    DW ?
    years   DW ?
    result  DW ?

.CODE
main PROC
    ; Initialize DS
    MOV AX, @DATA
    MOV DS, AX

    ; Display welcome message
    MOV AH, 09H
    LEA DX, welcome_msg
    INT 21H

    ; Input deposit amount
    LEA DX, prompt_deposit
    CALL prompt_input
    MOV deposit, BX

    ; Input interest rate
    LEA DX, prompt_rate
    CALL prompt_input
    MOV rate, BX

    ; Input number of years
    LEA DX, prompt_years
    CALL prompt_input
    MOV years, BX

    ; Calculate final balance
    MOV AX, deposit
    MOV CX, AX         ; Save deposit in CX for adding later
    MUL rate           ; AX = deposit * rate
    MUL years          ; AX = deposit * rate * years
    MOV BX, 100
    DIV BX             ; AX = (deposit * rate * years) / 100
    ADD AX, CX         ; Add the original deposit
    MOV result, AX

    ; Display result
    LEA DX, result_msg
    MOV AH, 09H
    INT 21H

    MOV AX, years
    CALL display_num

    LEA DX, years_text
    MOV AH, 09H
    INT 21H

    MOV DL, '$'        ; Display currency symbol
    MOV AH, 02H
    INT 21H

    MOV AX, result
    CALL display_num

    ; Exit program
    MOV AH, 4CH
    INT 21H
main ENDP

; ------------------------------------------------
; Subroutine to prompt and read numeric input
; Output: BX = number entered
; ------------------------------------------------
prompt_input PROC
    MOV AH, 09H
    INT 21H

    XOR BX, BX
    MOV CX, 10

read_loop:
    MOV AH, 01H
    INT 21H
    CMP AL, 13       ; Enter key?
    JE end_input

    SUB AL, '0'
    MOV AH, 0
    XCHG AX, BX
    MUL CX
    ADD BX, AX

    JMP read_loop

end_input:
    RET
prompt_input ENDP

; ------------------------------------------------
; Subroutine to display number from AX
; ------------------------------------------------
display_num PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX

    XOR CX, CX
    MOV BX, 10

div_loop:
    XOR DX, DX
    DIV BX
    PUSH DX
    INC CX
    TEST AX, AX
    JNZ div_loop

print_digits:
    POP DX
    ADD DL, '0'
    MOV AH, 02H
    INT 21H
    LOOP print_digits

    POP DX
    POP CX
    POP BX
    POP AX
    RET
display_num ENDP
END main
