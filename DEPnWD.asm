.MODEL SAMLL
.STACK 100H

.DATA
    ; Add these to your existing data section
    current_balance_msg DB 13,10,"Current Balance: RM$"
    deposit_msg DB 13,10,"Enter deposit amount: RM$"
    withdraw_msg DB 13,10,"Enter withdrawal amount: RM$"
    new_balance_msg DB 13,10,"New Balance: RM$"
    insufficient_msg DB 13,10,"Error: Insufficient funds!$"
    
    balance DW 10000  ; Initial balance (RM100.00 stored as cents)
    amount DW ?       ; Temporary amount storage

.CODE
; Add these procedures to your existing code

;-----------------------------
; Display Balance Procedure
DisplayBalance PROC
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
DisplayBalance ENDP

;-----------------------------
; Deposit Procedure
Deposit PROC
    CALL DisplayBalance
    
    ; Prompt for deposit amount
    MOV AH, 09h
    LEA DX, deposit_msg
    INT 21h
    
    ; Get amount input (using your existing input buffer)
    MOV AH, 0Ah
    LEA DX, deposit
    INT 21h
    
    ; Convert to number
    LEA SI, deposit+2
    CALL ATOI          ; Your existing ATOI procedure
    
    ; Multiply by 100 to convert to cents
    MOV BX, 100
    MUL BX
    MOV amount, AX
    
    ; Add to balance
    ADD balance, AX
    
    ; Display new balance
    MOV AH, 09h
    LEA DX, new_balance_msg
    INT 21h
    CALL DisplayBalance
    
    RET
Deposit ENDP

;-----------------------------
; Withdrawal Procedure
Withdraw PROC
    CALL DisplayBalance
    
    ; Prompt for withdrawal amount
    MOV AH, 09h
    LEA DX, withdraw_msg
    INT 21h
    
    ; Get amount input
    MOV AH, 0Ah
    LEA DX, deposit    ; Reusing the same buffer
    INT 21h
    
    ; Convert to number
    LEA SI, deposit+2
    CALL ATOI
    
    ; Multiply by 100 to convert to cents
    MOV BX, 100
    MUL BX
    MOV amount, AX
    
    ; Check sufficient funds
    CMP AX, balance
    JG InsufficientFunds
    
    ; Subtract from balance
    SUB balance, AX
    
    ; Display new balance
    MOV AH, 09h
    LEA DX, new_balance_msg
    INT 21h
    CALL DisplayBalance
    JMP WithdrawDone
    
InsufficientFunds:
    MOV AH, 09h
    LEA DX, insufficient_msg
    INT 21h
    
WithdrawDone:
    RET
Withdraw ENDP
END