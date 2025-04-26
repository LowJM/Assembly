.MODEL SMALL
.STACK 100h

.DATA
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

.code
start:
    MOV AX, @data
    MOV DS, AX
    MOV ES, AX
    JMP main_menu

; ========== MAIN MENU ==========
main_menu:
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
    MOV AH, 4CH
    INT 21h

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
    JMP main_menu

login_fail:
    LEA DX, loginFailMsg
    CALL prINT_string
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

end start
