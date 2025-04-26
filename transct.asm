; TRANSACTION COUNTER (TRANSCT.ASM)
; Counts transactions during runtime
; Assemble with: masm transct.asm; link transct.obj;

.model small
.stack 100h

.data
    welcome_msg    db 'Bank Transaction Counter', 13, 10, '-------------------------', 13, 10, '$'
    prompt_trans   db 13, 10, 'Enter transaction type (1=Deposit, 2=Withdraw, 0=Exit): $'
    count_msg      db 13, 10, 'Total transactions: $'
    deposit_msg    db 13, 10, 'Deposit recorded.', '$'
    withdraw_msg   db 13, 10, 'Withdrawal recorded.', '$'
    exit_msg       db 13, 10, 'Session ended.', '$'
    
    transaction_count dw 0  ; Counter variable

.code
main proc
    mov ax, @data
    mov ds, ax
    
    ; Display welcome
    mov ah, 09h
    lea dx, welcome_msg
    int 21h

transaction_loop:
    ; Prompt for transaction type
    mov ah, 09h
    lea dx, prompt_trans
    int 21h
    
    ; Get user input (1/2/0)
    mov ah, 01h
    int 21h
    
    ; Check for exit (0)
    cmp al, '0'
    je exit_program
    
    ; Validate input (must be 1 or 2)
    cmp al, '1'
    je record_deposit
    cmp al, '2'
    je record_withdraw
    jmp transaction_loop  ; Invalid input, reprompt

record_deposit:
    inc transaction_count
    mov ah, 09h
    lea dx, deposit_msg
    int 21h
    jmp transaction_loop

record_withdraw:
    inc transaction_count
    mov ah, 09h
    lea dx, withdraw_msg
    int 21h
    jmp transaction_loop

exit_program:
    ; Display total transactions
    mov ah, 09h
    lea dx, count_msg
    int 21h
    
    mov ax, transaction_count
    call display_number
    
    ; Exit message
    mov ah, 09h
    lea dx, exit_msg
    int 21h
    
    ; Return to DOS
    mov ah, 4ch
    int 21h
main endp

; --------------------------
; Subroutine: Display number in AX
; --------------------------
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

end main