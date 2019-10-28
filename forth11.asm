;
;
;
;       FORTH11.ASM          fig-FORTH for the Motorola MC68HC11A1
;
;       October 30, 1990
;
;
;******************************************************************************
;
;      Memory Map
;
;
;           $0000 - $00ff        68hc11 internal ram
;
;           $1000 - $103F        68hc11 internal registers
;
;           $2000 - $7FFF        8k RAM
;
;           $E000 - $FFFF        8k EPROM w/FORTH and 68hc11 vectors
;
; NAM          FORTH
;
;******************************************************************************
;
;                   forth interpreter/compiler
;
;******************************************************************************
;
;
;
MEMTOP              equ       $7FFF
RAMTOP              equ       $7600
OPTION              equ       $1039
;
;       each disk buffer block is one 256 byte sector
;       with a 2 byte block id and a 2 byte null terminator
;
;       8 - 256 byte blocks = 2 - 1024 byte screens
;
NBLK                equ       8
MEMEND              equ       260*NBLK+RAMTOP
;
;
;
REGBS               equ       $1000               ; start of registers
BAUD                equ       REGBS+$2B           ; sci baud reg
SCCR1               equ       REGBS+$2C           ; sci control1 reg
SCCR2               equ       REGBS+$2D           ; sci control2 reg
SCSR                equ       REGBS+$2E           ; sci status reg
SCDAT               equ       REGBS+$2F           ; sci data reg
PORTA               equ       REGBS+$00
PACTL               equ       REGBS+$26
TMSK2               equ       REGBS+$24
TFLG2               equ       REGBS+$25
;
;
;
;******************************************************************************
;
;                   zero page memory
;
;******************************************************************************
;
N                   equ       $0000               ; scratch for (FIND),ENCLOSE,CMOVE
;                                     ; EMIT,KEY,SP@,SWAP,DOES>,COLD
;
;                   registers used by the FORTH virtual machine
;
W                   equ       $0020               ; instruction reg points to 6800 code
IP                  equ       $0022               ; inst. pointer points to pointer to 6800 code
RP                  equ       $0024               ; return stack pointer
UP                  equ       $0026               ; pointer to base of current user's 'USER' table
;                                       ( altered during multitasking )
;
;
;******************************************************************************
;
;                   system parameters initialized by COLD or WARM
;                   names refer to FORTH words of similar ( no X ) name
;
;******************************************************************************
;
                    org       $2000
;
UORIG               rmb       6                   ; user variables
XSPZER              rmb       2                   ; initial top of data stack for user
XRZERO              rmb       2                   ; initial top of return stack
XTIB                rmb       2                   ; start of terminal input buffer
XWIDTH              rmb       2                   ; name field width
XWARN               rmb       2                   ; warning message mode (0 = no disc)
XFENCE              rmb       2                   ; fence for FORGET
XDP                 rmb       2                   ; dictionary pointer
XVOCL               rmb       2                   ; vocabulary linking
XBLK                rmb       2                   ; disk block being accessed
XIN                 rmb       2                   ; scan pointer into the block
XOUT                rmb       2                   ; cursor position
XSCR                rmb       2                   ; disc screen being accessed
XOFSET              rmb       2                   ; disc sector offset for multi disc
XCONT               rmb       2                   ; last word in primary search vocab.
XCURR               rmb       2                   ; last word in extensible vocabulary
XSTATE              rmb       2                   ; interpret/compile mode flag
XBASE               rmb       2                   ; number base for i/o numeric conversion
XDPL                rmb       2                   ; decimal point place
XFLD                rmb       2
XCSP                rmb       2                   ; current stack pos, for compile checks
XRNUM               rmb       2
XHLD                rmb       2
XDELAY              rmb       2                   ; carriage return delay count
XCOLUM              rmb       2                   ; carriage width
IOSTAT              rmb       2                   ; last acia status from read/write
;
;                   end of user table, start of common system variables
;
XUSE                rmb       2
XPREV               rmb       2
XTRACK              rmb       2                   ; ( 4 spares ! )
XSECTOR             rmb       2
XDSTAT              rmb       2
XBLOCK              rmb       2
XBUFFER             rmb       2
;
;                   code here through REND is overwritten at time of cold
;                   load.
;
                    fcb       $C5
                    fcc       'FORT'
                    fcb       $C8
                    fdb       NOOP-7
FORTH               fdb       DODOES,DOVOC,$81A0,TASK-7
                    fdb       0

                    fcc       '(C) Forth Interest Group,  1979'

                    fcb       $C4
                    fcc       'TAS'
                    fcb       $CB
                    fdb       FORTH-8
TASK                fdb       DOCOL,SEMIS
REND                equ       *                   ; first empty location in dictionary
;******************************************************************************
;
;
;
;******************************************************************************
                    org       $E000
;
;       initialize
;
INIT                sei
                    ldaa      #$B3
                    staa      OPTION
;
;       initialize sci for 9600 baud at 8.0 mhz
;
                    ldaa      #$30
                    staa      BAUD                ; baud register
                    ldaa      #$00
                    staa      SCCR1
                    ldaa      #$0C
                    staa      SCCR2               ; enable
;******************************************************************************
;
;                   cold entry
;
;******************************************************************************
ORIG                nop
                    jmp       CENT

;******************************************************************************

;

;                   warm entry

;

;******************************************************************************

                    nop
                    jmp       WENT                ; warm start

;                                     ; keeps current dictionary intact

;******************************************************************************

;

;                   startup parameters

;

;******************************************************************************

                    fdb       $6811,0001          ; cpu and revision
                    fdb       0                   ; topmost word in FORTH vocabulary
BACKSP              fdb       $08                 ; backspace character for editing
UPINIT              fdb       UORIG               ; initial user area
SINIT               fdb       RAMTOP-$100         ; initial top of data stack
RINIT               fdb       RAMTOP-2            ; initial top of return stack
                    fdb       RAMTOP-$D0          ; terminal input buffer
                    fdb       31                  ; initial name field width
                    fdb       0                   ; initial warning mode (0 = no disc)
FENCIN              fdb       REND                ; initial fence
DPINIT              fdb       REND                ; cold start value for DP
VOCINT              fdb       FORTH+8             ; cold start value for VOC-LINK
COLINT              fdb       80                  ; initial terminal carriage width
DELINT              fdb       4                   ; initial carriage return delay
;
;
;
PULABX              pula                          ; get data word
                    pulb

STABX               std       0,X                 ; store at address
                    bra       NEXT

;

GETX                ldd       0,X                 ; get data from address

PUSHBA              pshb                          ; and save on stack
                    psha
;
; *=================== the virtual machine =====================================
;
NEXT                ldx       IP
                    inx                           ; pre-increment mode
                    inx
                    stx       IP

NEXT2               ldx       0,X                 ; get W which points to CFA of word to be done
NEXT3               stx       W

NEXT5               ldx       0,X                 ; get VECT which points to executable code
                    jmp       0,X                 ; and then do it

                    nop
;
; *=============================================================================
;
; *=======>> 1 <<     LIT               ; primitive
;
                    fcb       $83
                    fcc       'LI'
                    fcb       $D4
                    fdb       0                   ; link of zero to terminate dictionary scan
LIT                 fdb       *+2
                    ldx       IP                  ; get instruction pointer
                    inx
                    inx
                    stx       IP                  ; x points to next instruction
                    ldd       0,X                 ; next instruction is 16 bit literal
                    bra       PUSHBA              ; so push it on stack

;

; *=======>> 3 <<     EXECUTE           ; primitive

;

                    fcb       $87
                    fcc       'EXECUT'
                    fcb       $C5
                    fdb       LIT-6
EXEC                fdb       *+2
                    tsx                           ; move stack pointer to x
                    ldx       0,X                 ; get code field address (CFA)
                    ins                           ; pop stack
                    ins
                    bra       NEXT3               ; go execute word that cfa points to

;

; *=======>> 4 <<     BRANCH

;

                    fcb       $86
                    fcc       'BRANC'
                    fcb       $C8
                    fdb       EXEC-10
BRAN                fdb       ZBYES               ; go do unconditional branch
;
; *=======>> 5 <<     0BRANCH           ; primitive
;
                    fcb       $87
                    fcc       '0BRANC'
                    fcb       $C8
                    fdb       BRAN-9
ZBRAN               fdb       *+2
                    pula                          ; get flag
                    pulb
                    aba                           ; add together to see if zero
                    bne       ZBNO                ; a and b are not zero - don't branch
                    bcs       ZBNO                ; if overflow a and b are not zero !
;
;                   flag is false ( zero ) - branch
;
ZBYES               ldx       IP                  ; code is shared with BRANCH, (+LOOP),(LOOP)
                    ldd       2,X                 ; get offset
                    addd      IP                  ; add offset to instruction pointer
                    std       IP                  ; save it
                    bra       NEXT                ; go branch !

;

;                   flag is true ( non-zero ) - don't branch

;

ZBNO                ldx       IP                  ; no branch. this code is shared with (+LOOP),(LOOP)
                    inx                           ; jump over branch delta
                    inx
                    stx       IP                  ; save pointer
                    bra       NEXT                ; go do next instruction

;

; *=======>> 6 <<     (LOOP)            ; primitive

;

                    fcb       $86
                    fcc       '(LOOP'
                    fcb       $A9
                    fdb       ZBRAN-10
XLOOP               fdb       *+2
                    clra                          ; upper 8 is zero
                    ldab      #1                  ; get set to increment counter by 1
                    bra       XPLOP2              ; go steal other guy's code

;

; *=======>> 7 <<     (+LOOP)           ; primitive

;

                    fcb       $87
                    fcc       '(+LOOP'
                    fcb       $A9
                    fdb       XLOOP-9
XPLOOP              fdb       *+2                 ; +LOOP has an unsigned loop counter
                    pula                          ; get increment value
                    pulb
XPLOP2              tsta                           check if forward or backward looping
                    bpl       XPLOF               ; forward looping

                    ldx       RP
                    addd      2,X
                    std       2,X

                    sec
                    sbcb      5,X
                    sbca      4,X
                    bpl       ZBYES
                    bra       XPLONO              ; fall thru

;

XPLOF               nop
                    ldx       RP
                    addd      2,X
                    std       2,X

;       BSR         XPLOPS

                    subd      4,X
                    bmi       ZBYES
XPLONO              inx                           ; done, don't branch back
                    inx
                    inx
                    inx
                    stx       RP
                    bra       ZBNO

;

; *=======>> 8 <<     (DO)              ; primitive

;

                    fcb       $84
                    fcc       '(DO'
                    fcb       $A9
                    fdb       XPLOOP-10
XDO1                fdb       *+2                 ; this is the RUN-TIME DO, not the COMPILING DO
                    ldx       RP                  ; get copy of return pointer
                    dex
                    dex
                    dex
                    dex
                    stx       RP
                    pula                          ; pull data word off stack
                    pulb
                    std       2,X                 ; save on return stack
                    pula                          ; pull ??? off stack
                    pulb
                    std       4,X                 ; save on return stack
                    jmp       NEXT

;

; *=======>> 9 <<     I                 ; primitive

;

                    fcb       $81                 ; I
                    fcb       $C9
                    fdb       XDO1-7
I                   fdb       *+2
                    ldx       RP                  ; looks like I is kept on return stack
                    inx
                    inx
                    jmp       GETX

;

; *=======>> 10 <<    DIGIT

;

                    fcb       $85
                    fcc       'DIGI'
                    fcb       $D4
                    fdb       I-4
DIGIT               fdb       *+2                 ; legal input range is 0-9, A-Z
                    tsx                           ; copy stack pointer to x
                    ldaa      3,X
                    suba      #$30                ; ascii zero
                    bmi       DIGIT2              ; if less than '0', ILLEGAL
                    cmpa      #$0A
                    bmi       DIGIT0              ; if '9' or less
                    cmpa      #$11
                    bmi       DIGIT2              ; if less than "A"
                    cmpa      #$2B
                    bpl       DIGIT2              ; if greater than "Z"
                    suba      #7                  ; translate "A" thru "F"

DIGIT0              cmpa      1,X
                    bpl       DIGIT2              ; if not less than the base
                    ldab      #1                  ; set flag
                    staa      3,X                 ; store digit
DIGIT1              stab      1,X                 ; store the flag
                    jmp       NEXT

DIGIT2              clrb
                    ins
                    ins                           ; pop bottom number
                    tsx
                    stab      0,X                 ; make sure both bytes are 00
                    bra       DIGIT1

;

;                   the word format in the dictionary is :

;

;       NFA         char-count + 80          lowest address

;                   char 1

;                   char 2

;

;                   char n + $80

;

;       LFA         link high byte \___ point to previous word

;                   link low byte  /

;

;       CFA         CFA high byte  \___ point to 6800 code

;                   CFA low byte   /

;

;       PFA         parameter fields

;                      "        "

;                      "        "

;

;

; *=======>> 11 <<    (FIND)

;

                    fcb       $86
                    fcc       '(FIND'
                    fcb       $A9
                    fdb       DIGIT-8
PFIND               fdb       *+2
                    nop
                    nop
PD                  equ       N                   ; pointer to dict word being checked
PA0                 equ       N+2
PA                  equ       N+4
PC                  equ       N+6
                    ldx       #PD
                    ldab      #4

PFIND0              pula                          ; loop to get arguments off stack
                    staa      0,X
                    inx
                    decb
                    bne       PFIND0

                    ldx       PD
PFIND1              ldab      0,X                 ; get count dict count
                    stab      PC
                    andb      #$3F
                    inx
                    stx       PD                  ; update PD
                    ldx       PA0
                    ldaa      0,X                 ; get count from arg
                    inx
                    stx       PA                  ; initialize PA
                    cba                           ; compare lengths
                    bne       PFIND4

PFIND2              ldx       PA
                    ldaa      0,X
                    inx
                    stx       PA
                    ldx       PD
                    ldab      0,X
                    inx
                    stx       PD
                    tstb                           is dict entry neg. ?
                    bpl       PFIND8
                    andb      #$7F                ; clear sign
                    cba
                    beq       FOUND
PFIND3              ldx       0,X                 ; get new link
                    bne       PFIND1              ; continue if link not = 0
;
;                   not found
;
                    clra
                    clrb
                    jmp       PUSHBA

PFIND8              cba
                    beq       PFIND2
PFIND4              ldx       PD
PFIND9              ldab      0,X                 ; scan forward to end of this name
                    inx
                    bpl       PFIND9              ; read until bit 7 is found set
                    bra       PFIND3

;

;

;

FOUND               ldd       PD                  ; compute CFA
                    addd      #4
                    pshb                          ; and push on stack
                    psha
                    ldaa      PC                  ; push dictionary count
                    psha
                    clra
                    psha                          ; with upper 8 bits zero
                    ldab      #1                  ; construct a true flag
                    jmp       PUSHBA              ; and go push on stack

;

; *=======>> 12 <<    ENCLOSE

;

                    fcb       $87
                    fcc       'ENCLOS'
                    fcb       $C5
                    fdb       PFIND-9
;
;                   FC means offset (bytes) to first character of next word
;                   EW   "     "       "    to end of word
;                   NC   "     "       "    to next character to start next enclose at
;
ENCLOS              fdb       *+2
                    ins
                    pulb                          ; now, get low byte, for an 8 bit delimiter
                    tsx                           ; copy stack pointer
                    ldx       0,X                 ; get address to start enclose at
                    clr       N                   ; clear counter
;
;                   wait for a non-delimiter or a NUL
;
ENCL2               ldaa      0,X                 ; get a character
                    beq       ENCL6               ; found null
                    cba                           ; is it the delimiter ?
                    bne       ENCL3               ; yes
                    inx                           ; no
                    inc       N                   ; bump count
                    bra       ENCL2               ; try it again

;

;                   found first character. Push FC

;

ENCL3               ldaa      N                   ; found first character
                    psha                          ; push count
                    clra
                    psha                          ; push $00
;
;                   wait for a delimiter or a NUL
;
ENCL4               ldaa      0,X                 ; get another character
                    beq       ENCL7               ; it's a null
                    cba                           ; check for a delimiter
                    beq       ENCL5               ; yes - it is the delimiter
                    inx                           ; no
                    inc       N                   ; increment count
                    bra       ENCL4               ; see if we can find it somewhere

;

;                   found end of word

;

ENCL5               ldab      N                   ; get count
                    clra                          ; upper 8 = 0
                    pshb                          ; push EW
                    psha
;
;                   advance and push NC
;
                    incb                          ; increment
                    jmp       PUSHBA

;

;                   found NUL before non-delimiter, therefore there is no word

;

ENCL6               ldab      N                   ; found NUL
                    pshb
                    psha
                    incb
                    bra       ENCL7+2

;

;                   found NUL following the word instead of SPACE

;

ENCL7               ldab      N
                    pshb
                    psha
ENCL8               ldab      N
                    jmp       PUSHBA

;

;                   the next 4 words call system dependent I/O subroutines

;                   which are listed after "-->" in the dictionary

;

; *=======>> 13 <<    EMIT

;

                    fcb       $84
                    fcc       'EMI'
                    fcb       $D4
                    fdb       ENCLOS-10
EMIT                fdb       *+2
                    pula                          ; get data
                    pula

                    stab      N                   ; save B
                    stx       N+1                 ; save X
;
EMIT1               ldab      SCSR                ; read status
                    bitb      #$40
                    beq       EMIT1

                    anda      #$7F                ; mask parity
                    staa      SCDAT               ; send character

                    ldab      N                   ; recover B & X
                    ldx       N+1

                    jmp       NEXT

;

; *=======>> 14 <<    KEY

;

                    fcb       $83
                    fcc       'KE'
                    fcb       $D9
                    fdb       EMIT-7
KEY                 fdb       *+2
                    stab      N                   ; save b and x
                    stx       N+1
;
INSCI               ldaa      SCSR                ; read status reg
                    anda      #$20
                    beq       INSCI               ; jump if rdrf=0

                    ldaa      SCDAT               ; read data register
                    anda      #$7F                ; mask parity

                    ldab      N                   ; restore b and x
                    ldx       N+1

                    psha                          ; push data byte
                    clra
                    psha                          ; push a zero byte
                    jmp       NEXT

;

; *=======>> 15 <<    ?TERMINAL

;

                    fcb       $89
                    fcc       '?TERMINA'
                    fcb       $CC
                    fdb       KEY-6
QTERM               fdb       *+2

                    ldaa      SCSR
                    anda      #$20                ; rdrf set ?
                    beq       QTERM1

                    ldaa      SCDAT               ; yes - read data to clear it
                    ldaa      #$01                ; flag = true
                    bra       QTERM2

QTERM1              clra                          ; flag = false

QTERM2              clrb
                    jmp       PUSHBA              ; stack the flag

;

; *=======>> 16 <<    CR

;

                    fcb       $82
                    fcc       'C'
                    fcb       $D2
                    fdb       QTERM-12
CR                  fdb       *+2

                    ldaa      #$D                 ; carriage return
CR1                 ldab      SCSR                ; read status
                    bitb      #$40
                    beq       CR1                 ; loop until tC=1

                    anda      #$7F                ; mask parity
                    staa      SCDAT               ; send character

                    ldaa      #$A                 ; line feed
CR2                 ldab      SCSR                ; read status
                    bitb      #$40
                    beq       CR2                 ; loop until tC=1

                    anda      #$7F                ; mask parity
                    staa      SCDAT               ; send character

                    jmp       NEXT

;

; *=======>> 17 <<    CMOVE             ; source, destination, count

;

                    fcb       $85
                    fcc       'CMOV'
                    fcb       $C5
                    fdb       CR-5
CMOVE               fdb       *+2
                    ldx       #N                  ; find temp storage area
                    ldab      #6                  ; byte count
;
;                   n = count ; n+2 = destination ; n+4 = source
;
CMOV1               pula                          ; pop 6 bytes off stack
                    staa      0,X                 ; move parameters to scratch area
                    inx
                    decb
                    bne       CMOV1

CMOV2               ldd       N                   ; get count
                    subd      #1                  ; subtract one
                    std       N                   ; save count
                    bcs       CMOV3               ; we be done ?

                    ldx       N+4                 ; get source address
                    ldaa      0,X                 ; get source data
                    inx
                    stx       N+4                 ; save source pointer

                    ldx       N+2                 ; get destination pointer
                    staa      0,X                 ; write it to destination
                    inx
                    stx       N+2                 ; save destination pointer

_CMOV2              bra       CMOV2

CMOV3               jmp       NEXT

;

; *=======>> 18 <<    U*

;

                    fcb       $82
                    fcc       'U'
                    fcb       $AA
                    fdb       CMOVE-8
USTAR               fdb       *+2

                    ldaa      #16                 ; bits/word counter
                    psha
                    clra
                    clrb
                    tsx
USTAR2              ror       3,X                 ; shift multiplier
                    ror       4,X
                    dec       0,X                 ; done ?
                    bmi       USTAR4              ; yes

                    bcc       USTAR3
                    addd      1,X
USTAR3              rora
                    rorb                          ; shift result
                    bra       USTAR2

USTAR4              ins                           ; dump counter

                    ins
                    ins
                    jmp       PUSHBA              ; leave high word

;

;

;

; *=======>> 19 <<    U/

;

                    fcb       $82
                    fcc       'U'
                    fcb       $AF
                    fdb       USTAR-5
USLASH              fdb       *+2
                    ldaa      #17
                    psha
                    tsx
                    ldd       3,X
USL1                cmpa      1,X
                    bhi       USL3
                    bcs       USL2
                    cmpb      2,X
                    bcc       USL3
USL2                clc
                    bra       USL4

USL3                subd      1,X
                    sec
USL4                rol       6,X
                    rol       5,X
                    dec       0,X
                    beq       USL5
                    rolb
                    rola
                    bcc       USL1
                    bra       USL3

USL5                ins
                    ins
                    ins
                    ins
                    ins
                    jmp       SWAP+4              ; reverse quotient and remainder

;

; *=======>> 20 <<    AND

;

                    fcb       $83
                    fcc       'AN'
                    fcb       $C4
                    fdb       USLASH-5
AND                 fdb       *+2
                    pula                          ; pop data off stack
                    pulb
                    tsx                           ; copy stack pointer
                    andb      1,X                 ; AND the D acc with data on stack
                    anda      0,X
                    jmp       STABX               ; go save result

;

; *=======>> 21 <<    OR

;

                    fcb       $82
                    fcc       'O'
                    fcb       $D2
                    fdb       AND-6
OR                  fdb       *+2
                    pula                          ; pop data off stack
                    pulb
                    tsx                           ; copy stack pointer
                    orab      1,X                 ; OR the D acc with data on stack
                    oraa      0,X
                    jmp       STABX               ; go save result

;

; *=======>> 22 <<    XOR

;

                    fcb       $83
                    fcc       'XO'
                    fcb       $D2
                    fdb       OR-5
XOR                 fdb       *+2
                    pula                          ; pop data
                    pulb
                    tsx                           ; copy stack pointer
                    eorb      1,X                 ; XOR the D acc with data on stack
                    eora      0,X
                    jmp       STABX               ; go save result

;

;                   SP@

;

                    fcb       $83
                    fcc       'SP'
                    fcb       $C0
                    fdb       XOR-6
SPAT                fdb       *+2
                    tsx                           ; copy stack pointer
                    stx       N                   ; save in scratch area
                    ldx       #N                  ; this doesn't make sense to me !!
                    jmp       GETX

;

; *=======>> 24 <<    SP!

;

                    fcb       $83
                    fcc       'SP'
                    fcb       $A1
                    fdb       SPAT-6
SPSTOR              fdb       *+2
                    ldx       UP                  ; get user pointer
                    ldx       XSPZER-UORIG,X      ; find initialization value for sp
                    txs                           ; watch it! X and S are not equal
                    jmp       NEXT

;

; *=======>> 25 <<    RP!

;

                    fcb       $83
                    fcc       'RP'
                    fcb       $A1
                    fdb       SPSTOR-6
RPSTOR              fdb       *+2
                    ldx       RINIT               ; initialize from rom constant
                    stx       RP                  ; save new return pointer
                    jmp       NEXT

;

; *=======>> 26 <<    ;S

;

                    fcb       $82
                    fcc       ';'
                    fcb       $D3
                    fdb       RPSTOR-6
SEMIS               fdb       *+2
                    ldx       RP                  ; get return pointer
                    inx
                    inx
                    stx       RP
                    ldx       0,X                 ; get address we have just finished
                    jmp       NEXT+2              ; increment the return address & do next word

;

; *=======>> 27 <<    LEAVE

;

                    fcb       $85
                    fcc       'LEAV'
                    fcb       $C5
                    fdb       SEMIS-5
LEAVE               fdb       *+2
                    ldx       RP
                    ldd       2,X
                    std       4,X
                    jmp       NEXT

;

; *=======>> 28 <<    >R

;

                    fcb       $82
                    fcc       '>'
                    fcb       $D2
                    fdb       LEAVE-8
TOR                 fdb       *+2
                    ldx       RP                  ; find return stack
                    dex                           ; make room on return stack
                    dex
                    stx       RP
                    pula                          ; pop data
                    pulb
                    std       2,X                 ; and save on return stack
                    jmp       NEXT

;

; *=======>> 29 <<    R>

;

                    fcb       $82
                    fcc       'R'
                    fcb       $BE
                    fdb       TOR-5
FROMR               fdb       *+2
                    ldx       RP                  ; find return stack
                    ldd       2,X                 ; get data
                    inx                           ; toss out 2 bytes
                    inx
                    stx       RP                  ; save pointer
                    jmp       PUSHBA              ; push data back on stack

;

; *=======>> 30 <<    R

;

                    fcb       $81                 ; R
                    fcb       $D2
                    fdb       FROMR-5
R                   fdb       *+2
                    ldx       RP                  ; find pointer
                    inx
                    inx
                    jmp       GETX                ; copy data and push on data stack

;

; *=======>> 31 <<    0=

;

                    fcb       $82
                    fcc       '0'
                    fcb       $BD
                    fdb       R-4
ZEQU                fdb       *+2
                    tsx                           ; copy stack pointer
                    clra
                    clrb
                    ldx       0,X                 ; now get data off stack
                    bne       ZEQU2               ; not zero so leave false flag
                    incb                          ; it is zero so leave true flag

ZEQU2               tsx
                    jmp       STABX               ; save flag

;

; *=======>> 32 <<    0<

;

                    fcb       $82
                    fcc       '0'
                    fcb       $BC
                    fdb       ZEQU-5
ZLESS               fdb       *+2
                    tsx                           ; copy stack pointer
                    ldaa      #$80                ; check the sign bit
                    anda      0,X
                    beq       ZLESS2
                    clra                          ; if negative
                    ldab      #1                  ; leave true flag as it is less than zero
                    jmp       STABX

ZLESS2              clrb                          ; leave false - it's greater than zero
                    jmp       STABX

;

; *=======>> 33 <<    +

;

                    fcb       $81                 ; +
                    fcb       $AB
                    fdb       ZLESS-5
PLUS                fdb       *+2
                    pula                          ; pop data
                    pulb
                    tsx                           ; copy stack pointer
                    addd      0,X                 ; add two words
                    jmp       STABX               ; and leave result on stack

;

; *=======>> 34 <<    D+

;

                    fcb       $82
                    fcc       'D'
                    fcb       $AB
                    fdb       PLUS-4
DPLUS               fdb       *+2
                    tsx                           ; copy stack pointer
                    clc
                    ldab      #4                  ; double word is 4 bytes
DPLUS2              ldaa      3,X                 ; point to byte of bottom
                    adca      7,X                 ; add to byte of top
                    staa      7,X                 ; save result
                    dex
                    decb                          ; knock down count
                    bne       DPLUS2              ; do until 4 bytes complete
                    ins                           ; toss 2 words
                    ins
                    ins
                    ins
                    jmp       NEXT

;

; *=======>> 35 <<    MINUS             ; change sign of word on stack

;

                    fcb       $85
                    fcc       'MINU'
                    fcb       $D3
                    fdb       DPLUS-5
MINUS               fdb       *+2
                    tsx                           ; copy stack pointer
                    neg       1,X                 ; negate bottom byte
                    bcs       MINUS2
                    neg       0,X                 ; negate upper byte
                    bra       MINUS3

MINUS2              com       0,X
MINUS3              jmp       NEXT

;

; *=======>> 36 <<    DMINUS            ; change sign of double word on stack

;

                    fcb       $86
                    fcc       'DMINU'
                    fcb       $D3
                    fdb       MINUS-8
DMINUS              fdb       *+2
                    tsx                           ; copy stack pointer
                    com       0,X
                    com       1,X
                    com       2,X
                    neg       3,X

                    bne       DMINX               ; figure this out later
                    inc       2,X
                    bne       DMINX
                    inc       1,X
                    bne       DMINX
                    inc       0,X
DMINX               jmp       NEXT

;

; *=======>> 37 <<    OVER

;

                    fcb       $84
                    fcc       'OVE'
                    fcb       $D2
                    fdb       DMINUS-9
OVER                fdb       *+2
                    tsx                           ; copy stack pointer
                    ldd       2,X                 ; get second word on stack
                    jmp       PUSHBA              ; and copy it to top

;

; *=======>> 38 <<    DROP

;

                    fcb       $84
                    fcc       'DRO'
                    fcb       $D0
                    fdb       OVER-7
DROP                fdb       *+2
                    ins                           ; knock sp twice
                    ins                           ; to remove top item froom stack
                    jmp       NEXT

;

; *=======>> 39 <<    SWAP

;

                    fcb       $84
                    fcc       'SWA'
                    fcb       $D0
                    fdb       DROP-7
SWAP                fdb       *+2
                    pula                          ; get top item of stack
                    pulb
                    tsx                           ; copy sp
                    ldx       0,X                 ; copy second item
                    ins
                    ins
                    pshb                          ; save top item as second item
                    psha
                    stx       N                   ; now go save second as top
                    ldx       #N
                    jmp       GETX

;

; *=======>> 40 <<    DUP

;

                    fcb       $83
                    fcc       'DU'
                    fcb       $D0
                    fdb       SWAP-7
DUP                 fdb       *+2
                    pula                          ; get data
                    pulb
                    pshb                          ; push data
                    psha
                    jmp       PUSHBA              ; push it again to duplicate

;

; *=======>> 41 <<    +!

;

                    fcb       $82
                    fcc       '+'
                    fcb       $A1
                    fdb       DUP-6
PSTORE              fdb       *+2
                    tsx                           ; copy stack pointer
                    ldx       0,X                 ; get address
                    ins
                    ins
                    pula                          ; get data from stack
                    pulb
                    addb      1,X                 ; add and store low byte
                    stab      1,X
                    adca      0,X                 ; add and store high byte
                    staa      0,X
                    jmp       NEXT

;

; *=======>> 42 <<    TOGGLE

;

                    fcb       $86
                    fcc       'TOGGL'
                    fcb       $C5
                    fdb       PSTORE-5
TOGGLE              fdb       DOCOL,OVER,CAT,XOR,SWAP,CSTORE
                    fdb       SEMIS
;
; *=======>> 43 <<    @
;
                    fcb       $81                 ; @
                    fcb       $C0
                    fdb       TOGGLE-9
AT                  fdb       *+2
                    tsx                           ; copy sp
                    ldx       0,X                 ; get address
                    ins
                    ins
                    jmp       GETX                ; get 16 bit data from address

;

; *=======>> 44 <<    C@

;

                    fcb       $82
                    fcc       'C'
                    fcb       $C0
                    fdb       AT-4
CAT                 fdb       *+2
                    tsx                           ; copy sp
                    ldx       0,X                 ; get address
                    clra                          ; make upper byte zero
                    ldab      0,X                 ; get 8 bit data from address
                    ins
                    ins
                    jmp       PUSHBA              ; and save on stack

;

; *=======>> 45 <<    !

;

                    fcb       $81                 ; !
                    fcb       $A1
                    fdb       CAT-5
STORE               fdb       *+2
                    tsx
                    ldx       0,X                 ; get address
                    ins
                    ins
                    jmp       PULABX              ; then get data and store at addr

;

; *=======>> 46 <<    C!

;

                    fcb       $82
                    fcc       'C'
                    fcb       $A1
                    fdb       STORE-4
CSTORE              fdb       *+2
                    tsx                           ; copy stack pointer
                    ldx       0,X                 ; get address
                    ins
                    ins
                    ins
                    pulb                          ; get 8 bit data
                    stab      0,X                 ; and store it
                    jmp       NEXT

;

; *=======>> 47 <<    :

;

                    fcb       $C1
                    fcb       $BA
                    fdb       CSTORE-5
COLON               fdb       DOCOL,QEXEC,SCSP,CURENT,AT,CONTXT,STORE
                    fdb       CREATE,RBRAK
                    fdb       PSCODE
;
;                   here is the IP pusher for allowing nested words
;                   in the virtual machine
;                   ( ;S is the equivalent un-nester)
;
;
DOCOL               ldx       RP                  ; make room in the stack
                    dex
                    dex
                    stx       RP
                    ldd       IP                  ; get instruction pointer
                    std       2,X                 ; store address of the high level word
                    ldx       W                   ; get first sub-word of that definition
                    jmp       NEXT+2              ; and execute it

;

;       >> 48 <<    ;

;

                    fcb       $C1                 ; immediate code
                    fcb       $BB
                    fdb       COLON-4
SEMI                fdb       DOCOL,QCSP,COMPIL,SEMIS,SMUDGE,LBRAK
                    fdb       SEMIS
;
; *=======>> 49 <<    CONSTANT
;
                    fcb       $88
                    fcc       'CONSTAN'
                    fcb       $D4
                    fdb       SEMI-4
CON                 fdb       DOCOL,CREATE,SMUDGE,COMMA,PSCODE
DOCON               ldx       W                   ; pointer
                    ldd       2,X                 ; get constant data
                    jmp       PUSHBA              ; and save it

;

; *=======>> 50 <<    VARIABLE

;

                    fcb       $88
                    fcc       'VARIABL'
                    fcb       $C5
                    fdb       CON-11
VAR                 fdb       DOCOL,CON,PSCODE
DOVAR               ldd       W                   ; pointer to parameter field
                    addd      #2                  ; A:B now contain the address of the variable
                    jmp       PUSHBA

;

; *=======>> 51 <<    USER

;

                    fcb       $84
                    fcc       'USE'
                    fcb       $D2
                    fdb       VAR-11
USER                fdb       DOCOL,CON,PSCODE
DOUSER              ldx       W                   ; get offset into user's table
                    ldd       2,X
                    addd      UP
                    jmp       PUSHBA              ; push address of user's variable

;

; *=======>> 52 <<    0

;

                    fcb       $81                 ; 0
                    fcb       $B0
                    fdb       USER-7
ZERO                fdb       DOCON
                    fdb       0000
;
; *=======>> 53 <<    1
;
                    fcb       $81                 ; 1
                    fcb       $B1
                    fdb       ZERO-4
ONE                 fdb       DOCON
                    fdb       1
;
; *=======>> 54 <<    2
;
                    fcb       $81                 ; 2
                    fcb       $B2
                    fdb       ONE-4
TWO                 fdb       DOCON
                    fdb       2
;
; *=======>> 55 <<    3
;
                    fcb       $81                 ; 3
                    fcb       $B3
                    fdb       TWO-4
THREE               fdb       DOCON
                    fdb       3
;
; *=======>> 56 <<    BL
;
                    fcb       $82
                    fcc       'B'
                    fcb       $CC
                    fdb       THREE-4
BL                  fdb       DOCON               ; ascii blank
                    fdb       $20

;
; *=======>> 57 <<    FIRST
;
                    fcb       $85
                    fcc       'FIRS'
                    fcb       $D4
                    fdb       BL-5
FIRST               fdb       DOCON
                    fdb       RAMTOP
;
; *=======>> 58 <<    LIMIT             ; the end of memory +1
;
                    fcb       $85
                    fcc       'LIMI'
                    fcb       $D4
                    fdb       FIRST-8
LIMIT               fdb       DOCON
                    fdb       MEMEND
;
; *=======>> 59 <<    B/BUF             ; 256 bytes/buffer
;
                    fcb       $85
                    fcc       'B/BU'
                    fcb       $C6
                    fdb       LIMIT-8
BBUF                fdb       DOCON
                    fdb       256
;
; *=======>> 60 <<    B/SCR             ; blocks/screen = 1024/(B/BUF) = 4
;                                     ;
                    fcb       $85
                    fcc       'B/SC'
                    fcb       $D2
                    fdb       BBUF-8
BSCR                fdb       DOCON
                    fdb       4
;
; *=======>> 61 <<    +ORIGIN
;
                    fcb       $87
                    fcc       '+ORIGI'
                    fcb       $CE
                    fdb       BSCR-8
PORIG               fdb       DOCOL,LIT,ORIG,PLUS
                    fdb       SEMIS
;
; *=======>> 62 <<    S0
;
                    fcb       $82
                    fcc       'S'
                    fcb       $B0
                    fdb       PORIG-10
SZERO               fdb       DOUSER
                    fdb       XSPZER-UORIG
;
; *=======>> 63 <<    R0
;
                    fcb       $82
                    fcc       'R'
                    fcb       $B0
                    fdb       SZERO-5
RZERO               fdb       DOUSER
                    fdb       XRZERO-UORIG
;
; *=======>> 64 <<    TIB
;
                    fcb       $83
                    fcc       'TI'
                    fcb       $C2
                    fdb       RZERO-5
TIB                 fdb       DOUSER
                    fdb       XTIB-UORIG
;
; *=======>> 65 <<    WIDTH
;
                    fcb       $85
                    fcc       'WIDT'
                    fcb       $C8
                    fdb       TIB-6
WIDTH               fdb       DOUSER
                    fdb       XWIDTH-UORIG
;
; *=======>> 66 <<    WARNING
;
                    fcb       $87
                    fcc       'WARNIN'
                    fcb       $C7
                    fdb       WIDTH-8
WARN                fdb       DOUSER
                    fdb       XWARN-UORIG
;
; *=======>> 67 <<    FENCE
;
                    fcb       $85
                    fcc       'FENC'
                    fcb       $C5
                    fdb       WARN-10
FENCE               fdb       DOUSER
                    fdb       XFENCE-UORIG
;
; *=======>> 68       DP                pointer to first free
;                                     byte at end of dictionary
;
                    fcb       $82
                    fcc       'D'
                    fcb       $D0
                    fdb       FENCE-8
DP                  fdb       DOUSER
                    fdb       XDP-UORIG
;
; *=======>> 68.5 <<  VOC-LINK
;
                    fcb       $88
                    fcc       'VOC-LIN'
                    fcb       $CB
                    fdb       DP-5
VOCLIN              fdb       DOUSER
                    fdb       XVOCL-UORIG
;
; *=======>> 69 <<    BLK
;
                    fcb       $83
                    fcc       'BL'
                    fcb       $CB
                    fdb       VOCLIN-11
BLK                 fdb       DOUSER
                    fdb       XBLK-UORIG
;
; *=======>> 70 <<    IN                ; scan pointer for input line buffer
;
                    fcb       $82
                    fcc       'I'
                    fcb       $CE
                    fdb       BLK-6
IN                  fdb       DOUSER
                    fdb       XIN-UORIG
;
; *=======>> 71 <<    OUT
;
                    fcb       $83
                    fcc       'OU'
                    fcb       $D4
                    fdb       IN-5
OUT                 fdb       DOUSER
                    fdb       XOUT-UORIG
;
; *=======>> 72 <<    SCR
;
                    fcb       $83
                    fcc       'SC'
                    fcb       $D2
                    fdb       OUT-6
SCR                 fdb       DOUSER
                    fdb       XSCR-UORIG
;
; *=======>> 73 <<    OFFSET
;
                    fcb       $86
                    fcc       'OFFSE'
                    fcb       $D4
                    fdb       SCR-6
OFSET               fdb       DOUSER
                    fdb       XOFSET-UORIG
;
; *=======>> 74 <<    CONTEXT           ; points to pointer to
;                                     ; vocabulary to search first
                    fcb       $87
                    fcc       'CONTEX'
                    fcb       $D4
                    fdb       OFSET-9
CONTXT              fdb       DOUSER
                    fdb       XCONT-UORIG
;
; *=======>> 75 <<    CURRENT           ; points to pointer to
;                                     ; vocabulary being extended
                    fcb       $87
                    fcc       'CURREN'
                    fcb       $D4
                    fdb       CONTXT-10
CURENT              fdb       DOUSER
                    fdb       XCURR-UORIG
;
; *=======>> 76 <<    STATE             ; 1 if compiling, 0 if not
;
                    fcb       $85
                    fcc       'STAT'
                    fcb       $C5
                    fdb       CURENT-10
STATE               fdb       DOUSER
                    fdb       XSTATE-UORIG
;
; *=======>> 77 <<    BASE                ; number base for all input and output
;
                    fcb       $84
                    fcc       'BAS'
                    fcb       $C5
                    fdb       STATE-8
BASE                fdb       DOUSER
                    fdb       XBASE-UORIG
;
; *=======>> 78 <<    DPL
;
                    fcb       $83
                    fcc       'DP'
                    fcb       $CC
                    fdb       BASE-7
DPL                 fdb       DOUSER
                    fdb       XDPL-UORIG
;
; *=======>> 79 <<    FLD
;
                    fcb       $83
                    fcc       'FL'
                    fcb       $C4
                    fdb       DPL-6
FLD                 fdb       DOUSER
                    fdb       XFLD-UORIG
;
; *=======>> 80 <<    CSP
;
                    fcb       $83
                    fcc       'CS'
                    fcb       $D0
                    fdb       FLD-6
CSP                 fdb       DOUSER
                    fdb       XCSP-UORIG
;
; *=======>> 81 <<    R#
;
                    fcb       $82
                    fcc       'R'
                    fcb       $A3
                    fdb       CSP-6
RNUM                fdb       DOUSER
                    fdb       XRNUM-UORIG
;
; *=======>> 82 <<    HLD
;
                    fcb       $83
                    fcc       'HL'
                    fcb       $C4
                    fdb       RNUM-5
HLD                 fdb       DOCON
                    fdb       XHLD
;
; *=======>> 82.5 <<  COLUMNS             ; line width of terminal
;
                    fcb       $87
                    fcc       'COLUMN'
                    fcb       $D3
                    fdb       HLD-6
COLUMS              fdb       DOUSER
                    fdb       XCOLUM-UORIG
;
; *=======>> 83 <<    1+
;
                    fcb       $82
                    fcc       '1'
                    fcb       $AB
                    fdb       COLUMS-10
ONEP                fdb       DOCOL,ONE,PLUS
                    fdb       SEMIS
;
; *=======>> 84 <<    2+
;
                    fcb       $82
                    fcc       '2'
                    fcb       $AB
                    fdb       ONEP-5
TWOP                fdb       DOCOL,TWO,PLUS
                    fdb       SEMIS
;
; *=======>> 85 <<    HERE
;
                    fcb       $84
                    fcc       'HER'
                    fcb       $C5
                    fdb       TWOP-5
HERE                fdb       DOCOL,DP,AT
                    fdb       SEMIS
;
; *=======>> 86 <<    ALLOT
;
                    fcb       $85
                    fcc       'ALLO'
                    fcb       $D4
                    fdb       HERE-7
ALLOT               fdb       DOCOL,DP,PSTORE
                    fdb       SEMIS
;
; *=======>> 87 <<    ,                   ( this is a comma )
;
                    fcb       $81                 ; , (comma)
                    fcb       $AC
                    fdb       ALLOT-8
COMMA               fdb       DOCOL,HERE,STORE,TWO,ALLOT
                    fdb       SEMIS
;
; *=======>> 88 <<    C,
;
                    fcb       $82
                    fcc       'C'
                    fcb       $AC
                    fdb       COMMA-4
CCOMM               fdb       DOCOL,HERE,CSTORE,ONE,ALLOT
                    fdb       SEMIS
;
; *=======>> 89 <<    -                 ( minus sign )
;
                    fcb       $81                 ; -
                    fcb       $AD
                    fdb       CCOMM-5
SUB                 fdb       DOCOL,MINUS,PLUS
                    fdb       SEMIS
;
; *=======>> 90 <<    =                 ( equals sign )
;
                    fcb       $81                 ; =
                    fcb       $BD
                    fdb       SUB-4
EQUAL               fdb       DOCOL,SUB,ZEQU
                    fdb       SEMIS
;
; *=======>> 91 <<    <                 ( left arrow )
;
                    fcb       $81                 ; <
                    fcb       $BC
                    fdb       EQUAL-4
LESS                fdb       *+2
                    pula                          ; pop data
                    pulb
                    tsx                           ; copy stack pointer
                    cmpa      0,X                 ; compare upper bytes
                    ins
                    bgt       LESST
                    bne       LESSF
                    cmpb      1,X
                    bhi       LESST
LESSF               clrb                          ; set flag false
                    bra       LESSX

LESST               ldab      #1                  ; set flag true
LESSX               clra
                    ins
                    jmp       PUSHBA

;

; *=======>> 92 <<    >                 ( right arrow )

;

                    fcb       $81                 ; >
                    fcb       $BE
                    fdb       LESS-4
GREAT               fdb       DOCOL,SWAP,LESS
                    fdb       SEMIS
;
; *=======>> 93 <<    ROT
;
                    fcb       $83
                    fcc       'RO'
                    fcb       $D4
                    fdb       GREAT-4
ROT                 fdb       DOCOL,TOR,SWAP,FROMR,SWAP
                    fdb       SEMIS
;
; *=======>> 94 <<    SPACE
;
                    fcb       $85
                    fcc       'SPAC'
                    fcb       $C5
                    fdb       ROT-6
SPACE               fdb       DOCOL,BL,EMIT
                    fdb       SEMIS
;
; *=======>> 95 <<    MIN
;
                    fcb       $83
                    fcc       'MI'
                    fcb       $CE
                    fdb       SPACE-8
MIN                 fdb       DOCOL,OVER,OVER,GREAT,ZBRAN
                    fdb       MIN2-$
                    fdb       SWAP
MIN2                fdb       DROP
                    fdb       SEMIS
;
; *=======>> 96 <<    MAX
;
                    fcb       $83
                    fcc       'MA'
                    fcb       $D8
                    fdb       MIN-6
MAX                 fdb       DOCOL,OVER,OVER,LESS,ZBRAN
                    fdb       MAX2-$
                    fdb       SWAP
MAX2                fdb       DROP
                    fdb       SEMIS
;
; *=======>> 97 <<    -DUP
;
                    fcb       $84
                    fcc       '-DU'
                    fcb       $D0
                    fdb       MAX-6
DDUP                fdb       DOCOL,DUP,ZBRAN
                    fdb       DDUP2-$
                    fdb       DUP
DDUP2               fdb       SEMIS
;
; *=======>> 98 <<    TRAVERSE
;
                    fcb       $88
                    fcc       'TRAVERS'
                    fcb       $C5
                    fdb       DDUP-7
TRAV                fdb       DOCOL,SWAP
TRAV2               fdb       OVER,PLUS,LIT
                    fdb       $7F
                    fdb       OVER,CAT,LESS,ZBRAN
                    fdb       TRAV2-$
                    fdb       SWAP,DROP
                    fdb       SEMIS
;
; *=======>> 99 <<    LATEST
;
                    fcb       $86
                    fcc       'LATES'
                    fcb       $D4
                    fdb       TRAV-11
LATEST              fdb       DOCOL,CURENT,AT,AT
                    fdb       SEMIS
;
; *=======>> 100 <<   LFA
;
                    fcb       $83
                    fcc       'LF'
                    fcb       $C1
                    fdb       LATEST-9
LFA                 fdb       DOCOL,LIT
                    fdb       4
                    fdb       SUB
                    fdb       SEMIS
;
; *=======>> 101 <<   CFA
;
                    fcb       $83
                    fcc       'CF'
                    fcb       $C1
                    fdb       LFA-6
CFA                 fdb       DOCOL,TWO,SUB
                    fdb       SEMIS
;
; *=======>> 102 <<   NFA
;
                    fcb       $83
                    fcc       'NF'
                    fcb       $C1
                    fdb       CFA-6
NFA                 fdb       DOCOL,LIT
                    fdb       5
                    fdb       SUB,ONE,MINUS,TRAV
                    fdb       SEMIS
;
; *=======>> 103 <<   PFA
;
                    fcb       $83
                    fcc       'PF'
                    fcb       $C1
                    fdb       NFA-6
PFA                 fdb       DOCOL,ONE,TRAV,LIT
                    fdb       5
                    fdb       PLUS
                    fdb       SEMIS
;
; *=======>> 104 <<   !CSP
;
                    fcb       $84
                    fcc       '!CS'
                    fcb       $D0
                    fdb       PFA-6
SCSP                fdb       DOCOL,SPAT,CSP,STORE
                    fdb       SEMIS
;
; *=======>> 105 <<   ?ERROR
;
                    fcb       $86
                    fcc       '?ERRO'
                    fcb       $D2
                    fdb       SCSP-7
QERR                fdb       DOCOL,SWAP,ZBRAN
                    fdb       QERR2-$
                    fdb       ERROR,BRAN
                    fdb       QERR3-$
QERR2               fdb       DROP
QERR3               fdb       SEMIS
;
; *=======>> 106 <<   ?COMP
;
                    fcb       $85
                    fcc       '?COM'
                    fcb       $D0
                    fdb       QERR-9
QCOMP               fdb       DOCOL,STATE,AT,ZEQU,LIT
                    fdb       $11
                    fdb       QERR
                    fdb       SEMIS
;
; *=======>> 107 <<   ?EXEC
;
                    fcb       $85
                    fcc       '?EXE'
                    fcb       $C3
                    fdb       QCOMP-8
QEXEC               fdb       DOCOL,STATE,AT,LIT
                    fdb       $12
                    fdb       QERR
                    fdb       SEMIS
;
; *=======>> 108 <<   ?PAIRS
;
                    fcb       $86
                    fcc       '?PAIR'
                    fcb       $D3
                    fdb       QEXEC-8
QPAIRS              fdb       DOCOL,SUB,LIT
                    fdb       $13
                    fdb       QERR
                    fdb       SEMIS
;
; *=======>> 109 <<   ?CSP
;
                    fcb       $84
                    fcc       '?CS'
                    fcb       $D0
                    fdb       QPAIRS-9
QCSP                fdb       DOCOL,SPAT,CSP,AT,SUB,LIT
                    fdb       $14
                    fdb       QERR
                    fdb       SEMIS
;
; *=======>> 110 <<   ?LOADING
;
                    fcb       $88
                    fcc       '?LOADIN'
                    fcb       $C7
                    fdb       QCSP-7
QLOAD               fdb       DOCOL,BLK,AT,ZEQU,LIT
                    fdb       $16
                    fdb       QERR
                    fdb       SEMIS
;
; *=======>> 111 <<   COMPILE
;
                    fcb       $87
                    fcc       'COMPIL'
                    fcb       $C5
                    fdb       QLOAD-11
COMPIL              fdb       DOCOL,QCOMP,FROMR,TWOP,DUP,TOR,AT,COMMA
                    fdb       SEMIS
;
; *=======>> 112 <<   [
;
                    fcb       $C1                 ; [ immediate
                    fcb       $DB
                    fdb       COMPIL-10
LBRAK               fdb       DOCOL,ZERO,STATE,STORE
                    fdb       SEMIS
;
; *=======>> 113 <<    ]
;
                    fcb       $81                 ; ]
                    fcb       $DD
                    fdb       LBRAK-4
RBRAK               fdb       DOCOL,LIT
                    fdb       $C0
                    fdb       STATE,STORE
                    fdb       SEMIS
;
; *=======>> 114 <<   SMUDGE
;
                    fcb       $86
                    fcc       'SMUDG'
                    fcb       $C5
                    fdb       RBRAK-4
SMUDGE              fdb       DOCOL,LATEST,LIT
                    fdb       $20
                    fdb       TOGGLE
                    fdb       SEMIS
;
; *=======>> 115 <<   HEX
;
                    fcb       $83
                    fcc       'HE'
                    fcb       $D8
                    fdb       SMUDGE-9
HEX                 fdb       DOCOL
                    fdb       LIT
                    fdb       16
                    fdb       BASE,STORE
                    fdb       SEMIS
;
; *=======>> 116 <<   DECIMAL
;
                    fcb       $87
                    fcc       'DECIMA'
                    fcb       $CC
                    fdb       HEX-6
DEC                 fdb       DOCOL
                    fdb       LIT
                    fdb       10
                    fdb       BASE,STORE
                    fdb       SEMIS
;
; *=======>> 117 <<   (;CODE)
;
                    fcb       $87
                    fcc       '(;CODE'
                    fcb       $A9
                    fdb       DEC-10
PSCODE              fdb       DOCOL,FROMR,TWOP,LATEST,PFA,CFA,STORE
                    fdb       SEMIS
;
; *=======>> 118 <<   ;CODE
;
                    fcb       $C5
                    fcc       ';COD'
                    fcb       $C5
                    fdb       PSCODE-10
SEMIC               fdb       DOCOL,QCSP,COMPIL,PSCODE,SMUDGE,LBRAK,QSTACK
                    fdb       SEMIS
;
;                   note : `QSTACK` will be replaced by `ASSEMBLER` later
;
; *=======>> 119 <<   <BUILDS
;
                    fcb       $87
                    fcc       '<BUILD'
                    fcb       $D3
                    fdb       SEMIC-8
BUILDS              fdb       DOCOL,ZERO,CON
                    fdb       SEMIS
;
; *=======>> 120 <<   DOES>
;
                    fcb       $85
                    fcc       'DOES'
                    fcb       $BE
                    fdb       BUILDS-10
DOES                fdb       DOCOL,FROMR,TWOP,LATEST,PFA,STORE
                    fdb       PSCODE
;
DODOES              ldd       IP                  ; get instruction pointer
                    ldx       RP                  ; get return pointer
                    dex
                    dex
                    stx       RP                  ; save rp
                    std       2,X
                    ldx       W
                    inx
                    inx
                    stx       N
                    ldx       0,X
                    stx       IP
                    clra
                    ldab      #2
                    addd      N
                    pshb
                    psha
                    jmp       NEXT2

;

; *=======>> 121 <<   COUNT

;

                    fcb       $85
                    fcc       'COUN'
                    fcb       $D4
                    fdb       DOES-8
COUNT               fdb       DOCOL,DUP,ONEP,SWAP,CAT
                    fdb       SEMIS
;
; *=======>> 122 <<   TYPE
;
                    fcb       $84
                    fcc       'TYP'
                    fcb       $C5
                    fdb       COUNT-8
TYPE                fdb       DOCOL,DDUP,ZBRAN
                    fdb       TYPE3-$
                    fdb       OVER,PLUS,SWAP,XDO1
TYPE2               fdb       I,CAT,EMIT,XLOOP
                    fdb       TYPE2-$
                    fdb       BRAN
                    fdb       TYPE4-$
TYPE3               fdb       DROP
TYPE4               fdb       SEMIS
;
; *=======>> 123 <<   -TRAILING
;
                    fcb       $89
                    fcc       '-TRAILIN'
                    fcb       $C7
                    fdb       TYPE-7
DTRAIL              fdb       DOCOL,DUP,ZERO,XDO1
DTRAL2              fdb       OVER,OVER,PLUS,ONE,SUB,CAT,BL
                    fdb       SUB,ZBRAN
                    fdb       DTRAL3-$
                    fdb       LEAVE,BRAN
                    fdb       DTRAL4-$
DTRAL3              fdb       ONE,SUB
DTRAL4              fdb       XLOOP
                    fdb       DTRAL2-$
                    fdb       SEMIS
;
; *=======>> 124 <<   (.")
;
                    fcb       $84
                    fcc       '(."'
                    fcb       $A9
                    fdb       DTRAIL-12
PDOTQ               fdb       DOCOL,R,TWOP,COUNT,DUP,ONEP
                    fdb       FROMR,PLUS,TOR,TYPE
                    fdb       SEMIS
;
; *=======>> 125 <<   ."
;
                    fcb       $C2
                    fcc       '.'
                    fcb       $A2
                    fdb       PDOTQ-7
DOTQ                fdb       DOCOL
                    fdb       LIT
                    fdb       $22
                    fdb       STATE,AT,ZBRAN
                    fdb       DOTQ1-$
                    fdb       COMPIL,PDOTQ,WORD
                    fdb       HERE,CAT,ONEP,ALLOT,BRAN
                    fdb       DOTQ2-$
DOTQ1               fdb       WORD,HERE,COUNT,TYPE
DOTQ2               fdb       SEMIS
;
; *=======>> 126 <<   ?STACK            MACHINE DEPENDENT
;
                    fcb       $86
                    fcc       '?STAC'
                    fcb       $CB
                    fdb       DOTQ-5
QSTACK              fdb       DOCOL,LIT
                    fdb       $12
                    fdb       PORIG,AT,TWO,SUB,SPAT,LESS,ONE
                    fdb       QERR
;
;                   prints 'empty stack'
;
QSTAC2              fdb       SPAT
;
;                   here we compare with a value at least 128
;                   higher than dict. ptr.  (DP)
;
                    fdb       HERE,LIT
                    fdb       $80
                    fdb       PLUS,LESS,ZBRAN
                    fdb       QSTAC3-$
                    fdb       TWO
                    fdb       QERR
;
;                   prints 'full stack'
;
QSTAC3              fdb       SEMIS
;
; *=======>> 128 <<   EXPECT
;
                    fcb       $86
                    fcc       'EXPEC'
                    fcb       $D4
                    fdb       QSTACK-9
EXPECT              fdb       DOCOL,OVER,PLUS,OVER,XDO1
EXPEC2              fdb       KEY,DUP,LIT
                    fdb       $0E
                    fdb       PORIG,AT,EQUAL,ZBRAN
                    fdb       EXPEC3-$
                    fdb       DROP,LIT
                    fdb       8
                    fdb       OVER,I,EQUAL,DUP,FROMR,TWO,SUB,PLUS
                    fdb       TOR,SUB,BRAN
                    fdb       EXPEC6-$
EXPEC3              fdb       DUP,LIT
                    fdb       $D
                    fdb       EQUAL,ZBRAN
                    fdb       EXPEC4-$
                    fdb       LEAVE,DROP,BL,ZERO,BRAN
                    fdb       EXPEC5-$
EXPEC4              fdb       DUP
EXPEC5              fdb       I,CSTORE,ZERO,I,ONEP,STORE
EXPEC6              fdb       EMIT,XLOOP
                    fdb       EXPEC2-$
                    fdb       DROP
                    fdb       SEMIS
;
; *=======>> 129 <<   QUERY
;
                    fcb       $85
                    fcc       'QUER'
                    fcb       $D9
                    fdb       EXPECT-9
QUERY               fdb       DOCOL
                    fdb       TIB,AT,COLUMS
                    fdb       AT,EXPECT,ZERO,IN,STORE
                    fdb       SEMIS
;
; *=======>> 130 <<                     ( null - as in 00 hex )
;
                    fcb       $C1
                    fcb       $80
                    fdb       QUERY-8
NULL                fdb       DOCOL,BLK,AT,ZBRAN
                    fdb       NULL2-$
                    fdb       ONE,BLK,PSTORE
                    fdb       ZERO,IN,STORE,BLK,AT,BSCR,MOD
                    fdb       ZEQU
;                   check for end of screen
                    fdb       ZBRAN
                    fdb       NULL1-$
                    fdb       QEXEC,FROMR,DROP
NULL1               fdb       BRAN
                    fdb       NULL3-$
NULL2               fdb       FROMR,DROP
NULL3               fdb       SEMIS
;
; *=======>> 133 <<   FILL
;
                    fcb       $84
                    fcc       'FIL'
                    fcb       $CC
                    fdb       NULL-4
FILL                fdb       DOCOL,SWAP,TOR,OVER,CSTORE,DUP,ONEP
                    fdb       FROMR,ONE,SUB,CMOVE
                    fdb       SEMIS
;
; *=======>> 134 <<   ERASE
;
                    fcb       $85
                    fcc       'ERAS'
                    fcb       $C5
                    fdb       FILL-7
ERASE               fdb       DOCOL,ZERO,FILL
                    fdb       SEMIS
;
; *=======>> 135 <<   BLANKS
;
                    fcb       $86
                    fcc       'BLANK'
                    fcb       $D3
                    fdb       ERASE-8
BLANKS              fdb       DOCOL,BL,FILL
                    fdb       SEMIS
;
; *=======>> 136 <<   HOLD
;
                    fcb       $84
                    fcc       'HOL'
                    fcb       $C4
                    fdb       BLANKS-9
HOLD                fdb       DOCOL,LIT,$FFFF,HLD,PSTORE,HLD,AT,CSTORE
                    fdb       SEMIS
;
; *=======>> 137 <<   PAD
;
                    fcb       $83
                    fcc       'PA'
                    fcb       $C4
                    fdb       HOLD-7
PAD                 fdb       DOCOL,HERE,LIT
                    fdb       $44
                    fdb       PLUS
                    fdb       SEMIS
;
; *=======>> 138 <<   WORD
;
                    fcb       $84
                    fcc       'WOR'
                    fcb       $C4
                    fdb       PAD-6
WORD                fdb       DOCOL,BLK,AT,ZBRAN
                    fdb       WORD2-$
                    fdb       BLK,AT,BLOCK,BRAN
                    fdb       WORD3-$
WORD2               fdb       TIB,AT
WORD3               fdb       IN,AT,PLUS,SWAP,ENCLOS,HERE,LIT
                    fdb       34
                    fdb       BLANKS,IN,PSTORE,OVER,SUB,TOR,R,HERE
                    fdb       CSTORE,PLUS,HERE,ONEP,FROMR,CMOVE
                    fdb       SEMIS
;
; *=======>> 139 <<   (NUMBER)
;
                    fcb       $88
                    fcc       '(NUMBER'
                    fcb       $A9
                    fdb       WORD-7
PNUMB               fdb       DOCOL
PNUMB2              fdb       ONEP,DUP,TOR,CAT,BASE,AT,DIGIT,ZBRAN
                    fdb       PNUMB4-$
                    fdb       SWAP,BASE,AT,USTAR,DROP,ROT,BASE
                    fdb       AT,USTAR,DPLUS,DPL,AT,ONEP,ZBRAN
                    fdb       PNUMB3-$
                    fdb       ONE,DPL,PSTORE
PNUMB3              fdb       FROMR,BRAN
                    fdb       PNUMB2-$
PNUMB4              fdb       FROMR
                    fdb       SEMIS
;
; *=======>> 140 <<   NUMBER
;
                    fcb       $86
                    fcc       'NUMBE'
                    fcb       $D2
                    fdb       PNUMB-11
NUMB                fdb       DOCOL,ZERO,ZERO,ROT,DUP,ONEP,CAT,LIT
                    fcb       0
                    fcc       "-"
                    fdb       EQUAL,DUP,TOR,PLUS,LIT,$FFFF
NUMB1               fdb       DPL,STORE,PNUMB,DUP,CAT,BL,SUB
                    fdb       ZBRAN
                    fdb       NUMB2-$
                    fdb       DUP,CAT,LIT
                    fcb       0
                    fcc       "."
                    fdb       SUB,ZERO,QERR,ZERO,BRAN
                    fdb       NUMB1-$
NUMB2               fdb       DROP,FROMR,ZBRAN
                    fdb       NUMB3-$
                    fdb       DMINUS
NUMB3               fdb       SEMIS
;
; *=======>> 141 <<   -FIND
;
                    fcb       $85
                    fcc       '-FIN'
                    fcb       $C4
                    fdb       NUMB-9
DFIND               fdb       DOCOL,BL,WORD,HERE,CONTXT,AT,AT
                    fdb       PFIND,DUP,ZEQU,ZBRAN
                    fdb       DFIND2-$
                    fdb       DROP,HERE,LATEST,PFIND
DFIND2              fdb       SEMIS
;
; *=======>> 142 <<   (ABORT)
;
                    fcb       $87
                    fcc       '(ABORT'
                    fcb       $A9
                    fdb       DFIND-8
PABORT              fdb       DOCOL,ABORT
                    fdb       SEMIS
;
; *=======>> 143 <<   ERROR
;
                    fcb       $85
                    fcc       'ERRO'
                    fcb       $D2
                    fdb       PABORT-10
ERROR               fdb       DOCOL,WARN,AT,ZLESS
                    fdb       ZBRAN
;
;                   WARNING is -1 to abort, 0 to print error number
;                   and 1 to print error message from disc
;
                    fdb       ERROR2-$
                    fdb       PABORT
ERROR2              fdb       HERE,COUNT,TYPE,PDOTQ
                    fcb       4
                    fcc       "  ? "
                    fdb       MESS,SPSTOR,IN,AT,BLK,AT,QUIT
                    fdb       SEMIS
;
; *=======>> 144 <<   ID.
;
                    fcb       $83
                    fcc       'ID'
                    fcb       $AE
                    fdb       ERROR-8
IDDOT               fdb       DOCOL,PAD,LIT
                    fdb       32
                    fdb       LIT
                    fdb       $5F                 ; ( underline )
                    fdb       FILL,DUP,PFA,LFA,OVER,SUB,PAD
                    fdb       SWAP,CMOVE,PAD,COUNT,LIT
                    fdb       31
                    fdb       AND,TYPE,SPACE
                    fdb       SEMIS
;
; *=======>> 145 <<   CREATE
;
                    fcb       $86
                    fcc       'CREAT'
                    fcb       $C5
                    fdb       IDDOT-6
CREATE              fdb       DOCOL,DFIND,ZBRAN
                    fdb       CREAT2-$
                    fdb       DROP,CR,PDOTQ
                    fcb       8
                    fcc       " redef: "
                    fdb       NFA,IDDOT,LIT
                    fdb       4
                    fdb       MESS,SPACE
CREAT2              fdb       HERE,DUP,CAT,WIDTH,AT,MIN
                    fdb       ONEP,ALLOT,DUP,LIT
                    fdb       $A0
                    fdb       TOGGLE,HERE,ONE,SUB,LIT
                    fdb       $80
                    fdb       TOGGLE,LATEST,COMMA,CURENT,AT,STORE
                    fdb       HERE,TWOP,COMMA
                    fdb       SEMIS
;
; *=======>> 146 <<   [COMPILE]
;
                    fcb       $C9                 ; immediate
                    fcc       '[COMPILE'
                    fcb       $DD
                    fdb       CREATE-9
BCOMP               fdb       DOCOL,DFIND,ZEQU,ZERO,QERR,DROP,CFA,COMMA
                    fdb       SEMIS
;
; *=======>> 147 <<   LITERAL
;
                    fcb       $C7                 ; immediate
                    fcc       'LITERA'
                    fcb       $CC
                    fdb       BCOMP-12
LITER               fdb       DOCOL,STATE,AT,ZBRAN
                    fdb       LITER2-$
                    fdb       COMPIL,LIT,COMMA
LITER2              fdb       SEMIS

;
; *=======>> 148 <<   DLITERAL
;
                    fcb       $C8                 ; immediate
                    fcc       'DLITERA'
                    fcb       $CC
                    fdb       LITER-10
DLITER              fdb       DOCOL,STATE,AT,ZBRAN
                    fdb       DLITE2-$
                    fdb       SWAP,LITER,LITER
DLITE2              fdb       SEMIS
;
; *=======>> 149 <<   INTERPRET
;
                    fcb       $89
                    fcc       'INTERPRE'
                    fcb       $D4
                    fdb       DLITER-11
INTERP              fdb       DOCOL

INTER2              fdb       DFIND,ZBRAN
                    fdb       INTER5-$
                    fdb       STATE,AT,LESS
                    fdb       ZBRAN

                    fdb       INTER3-$
                    fdb       CFA,COMMA,BRAN
                    fdb       INTER4-$

INTER3              fdb       CFA,EXEC
INTER4              fdb       BRAN
                    fdb       INTER7-$

INTER5              fdb       HERE,NUMB,DPL,AT,ONEP,ZBRAN
                    fdb       INTER6-$
                    fdb       DLITER,BRAN
                    fdb       INTER7-$
INTER6              fdb       DROP,LITER

INTER7              fdb       QSTACK,BRAN
                    fdb       INTER2-$            ; branch always
;
; *=======>> 150 <<   IMMEDIATE
;
                    fcb       $89
                    fcc       'IMMEDIAT'
                    fcb       $C5
                    fdb       INTERP-12
IMMED               fdb       DOCOL,LATEST,LIT
                    fdb       $40
                    fdb       TOGGLE
                    fdb       SEMIS
;
; *=======>> 151 <<   VOCABULARY
;
                    fcb       $8A
                    fcc       'VOCABULAR'
                    fcb       $D9
                    fdb       IMMED-12
VOCAB               fdb       DOCOL,BUILDS,LIT,$81A0,COMMA,CURENT,AT,CFA
                    fdb       COMMA,HERE,VOCLIN,AT,COMMA,VOCLIN,STORE,DOES
DOVOC               fdb       TWOP,CONTXT,STORE
                    fdb       SEMIS
;
; *=======>> 153 <<   DEFINITIONS
;
                    fcb       $8B
                    fcc       'DEFINITION'
                    fcb       $D3
                    fdb       VOCAB-13
DEFIN               fdb       DOCOL,CONTXT,AT,CURENT,STORE
                    fdb       SEMIS
;
; *=======>> 154 <<   (
;
                    fcb       $C1                 ; immediate (
                    fcb       $A8
                    fdb       DEFIN-14
PAREN               fdb       DOCOL,LIT
                    fcb       0
                    fcc       ")"
                    fdb       WORD
                    fdb       SEMIS
;
; *=======>> 155 <<   QUIT
;
                    fcb       $84
                    fcc       'QUI'
                    fcb       $D4
                    fdb       PAREN-4
QUIT                fdb       DOCOL
                    fdb       ZERO,BLK,STORE,LBRAK
;
;                   Here is the outer interpreter
;                   which gets a line of input, does it, prints " OK"
;                   then repeats :
;
QUIT2               fdb       RPSTOR,CR,QUERY,INTERP,STATE,AT,ZEQU
                    fdb       ZBRAN
                    fdb       QUIT3-$
                    fdb       PDOTQ
                    fcb       3
                    fcc       ' OK'
QUIT3               fdb       BRAN
                    fdb       QUIT2-$             ; branch always
;
; *=======>> 156 <<   ABORT
;
                    fcb       $85
                    fcc       'ABOR'
                    fcb       $D4
                    fdb       QUIT-7
ABORT               fdb       DOCOL,SPSTOR,DEC,QSTACK,CR,MTBUF
                    fdb       RESTR
                    fdb       FIRST,DUP,USE,STORE,PREV,STORE  ; added 2/7/90
                    fdb       PDOTQ
                    fcb       15
                    fcc       " HCforth  v2.0 "
                    fdb       FORTH,DEFIN,CR
                    fdb       QUIT                ; branch always
;
; *=======>> 157 <<   COLD
;
;                   bootstrap code - move rom contents to ram
;
                    fcb       $84
                    fcc       'COL'
                    fcb       $C4
                    fdb       ABORT-8
COLD                fdb       *+2
CENT                lds       #REND-1             ; top of destination
                    ldx       #ERAM               ; top of stuff to move
COLD2               dex
                    ldaa      0,X
                    psha                          ; move TASK and FORTH to ram
                    cpx       #RAM
                    bne       COLD2
;
                    lds       #XFENCE-1           ; put stack at a safe place for now
                    ldx       COLINT
                    stx       XCOLUM              ; columns
                    ldx       DELINT
                    stx       XDELAY              ; delay
                    ldx       VOCINT
                    stx       XVOCL               ; vocabulary link
                    ldx       DPINIT
                    stx       XDP                 ; dictionary pointer
                    ldx       FENCIN
                    stx       XFENCE              ; fence
;
WENT                lds       #XFENCE-1           ; top of destination
                    ldx       #FENCIN             ; top of stuff to move
WARM2               dex
                    ldaa      0,X                 ; get byte
                    psha                          ; save byte
                    cpx       #SINIT              ; done ?
                    bne       WARM2               ; no
;
                    lds       SINIT               ; load stack pointer
                    ldx       UPINIT
                    stx       UP                  ; init user ram pointer

                    ldx       #ABORT              ; get cfa of abort

                    stx       IP                  ; and save as first instruction
;
;                   start the virtual machine running
;
                    jmp       RPSTOR+2

;

;                   here is the stuff that gets copied to ram

;

RAM                 fdb       RAMTOP,RAMTOP,0,0
;
; *=======>> 152 <<
;
                    fcb       $C5                 ; immediate
                    fcc       'FORT'
                    fcb       $C8
                    fdb       NOOP-7
RFORTH              fdb       DODOES,DOVOC,$81A0,TASK-7
                    fdb       0
                    fcc       "(C) Forth Interest Group,  1979"
;
                    fcb       $84
                    fcc       'TAS'
                    fcb       $CB
                    fdb       FORTH-8
RTASK               fdb       DOCOL,SEMIS
ERAM                equ       *
;
; *=======>> 158 <<   S->D
;
                    fcb       $84                 ; sign extend word to double
                    fcc       'S->'
                    fcb       $C4
                    fdb       COLD-7
STOD                fdb       DOCOL,DUP,ZLESS,MINUS
                    fdb       SEMIS
;
; *=======>> 159 <<   *
;
                    fcb       $81                 ; multiply two words
                    fcb       $AA
                    fdb       STOD-7
STAR                fdb       *+2

                    ldaa      #16                 ; bits/word counter
                    psha

                    clra
                    clrb
                    tsx
STAR2               ror       3,X                 ; shift multiplier
                    ror       4,X
                    dec       0,X                 ; done ?
                    bmi       STAR4               ; yes

                    bcc       STAR3
                    addd      1,X
STAR3               rora
                    rorb                          ; shift result
                    bra       STAR2

STAR4               ins                           ; dump counter

                    ins
                    ins
                    jmp       NEXT

;

; *=======>> 160 <<   /MOD

;

                    fcb       $84
                    fcc       '/MO'
                    fcb       $C4
                    fdb       STAR-4
SLMOD               fdb       DOCOL,TOR,STOD,FROMR,USLASH
                    fdb       SEMIS
;
; *=======>> 161 <<   /
;
                    fcb       $81                 ; /
                    fcb       $AF
                    fdb       SLMOD-7
SLASH               fdb       DOCOL,SLMOD,SWAP,DROP
                    fdb       SEMIS
;
; *=======>> 162 <<   MOD
;
                    fcb       $83
                    fcc       'MO'
                    fcb       $C4
                    fdb       SLASH-4
MOD                 fdb       DOCOL,SLMOD,DROP
                    fdb       SEMIS
;
; *=======>> 163 <<   */MOD
;
                    fcb       $85
                    fcc       '*/MO'
                    fcb       $C4
                    fdb       MOD-6
SSMOD               fdb       DOCOL,TOR,USTAR,FROMR,USLASH
                    fdb       SEMIS
;
; *=======>> 164 <<   */
;
                    fcb       $82
                    fcc       '*'
                    fcb       $AF
                    fdb       SSMOD-8
SSLASH              fdb       DOCOL,SSMOD,SWAP,DROP
                    fdb       SEMIS
;
; *=======>> 165 <<   M/MOD
;
                    fcb       $85
                    fcc       'M/MO'
                    fcb       $C4
                    fdb       SSLASH-5
MSMOD               fdb       DOCOL,TOR,ZERO,R,USLASH
                    fdb       FROMR,SWAP,TOR,USLASH,FROMR
                    fdb       SEMIS
;
; *=======>> 166 <<   ABS
;
                    fcb       $83
                    fcc       'AB'
                    fcb       $D3
                    fdb       MSMOD-8
ABS                 fdb       DOCOL,DUP,ZLESS,ZBRAN
                    fdb       ABS2-$
                    fdb       MINUS
ABS2                fdb       SEMIS
;
; *=======>> 167 <<   DABS
;
                    fcb       $84
                    fcc       'DAB'
                    fcb       $D3
                    fdb       ABS-6
DABS                fdb       DOCOL,DUP,ZLESS,ZBRAN
                    fdb       DABS2-$
                    fdb       DMINUS
DABS2               fdb       SEMIS
;
;                   disc primitives
;
; *=======>> 168 <<   USE
;
                    fcb       $83
                    fcc       'US'
                    fcb       $C5
                    fdb       DABS-7
USE                 fdb       DOCON
                    fdb       XUSE
;
; *=======>> 169 <<   PREV
;
                    fcb       $84
                    fcc       'PRE'
                    fcb       $D6
                    fdb       USE-6
PREV                fdb       DOCON
                    fdb       XPREV
;
; *=======>> 170 <<   +BUF
;
                    fcb       $84
                    fcc       '+BU'
                    fcb       $C6
                    fdb       PREV-7
PBUF                fdb       DOCOL,LIT
                    fdb       260
                    fdb       PLUS,DUP,LIMIT
                    fdb       EQUAL,ZBRAN
                    fdb       PBUF2-$
                    fdb       DROP,FIRST
PBUF2               fdb       DUP,PREV,AT,SUB
                    fdb       SEMIS
;
; *=======>> 171 <<   UPDATE
;
                    fcb       $86
                    fcc       'UPDAT'
                    fcb       $C5
                    fdb       PBUF-7
UPDATE              fdb       DOCOL,PREV,AT,AT,LIT,$8000,OR,PREV,AT,STORE
                    fdb       SEMIS
;
; *=======>> 172 <<   EMPTY-BUFFERS
;
                    fcb       $8D
                    fcc       'EMPTY-BUFFER'
                    fcb       $D3
                    fdb       UPDATE-9
MTBUF               fdb       DOCOL,FIRST,LIMIT,OVER,SUB,ERASE
                    fdb       SEMIS
;
; *=======>> 175 <<   BUFFER
;
                    fcb       $86
                    fcc       'BUFFE'
                    fcb       $D2
                    fdb       MTBUF-16
BUFFER              fdb       DOCOL,USE,AT,DUP,TOR
BUFFR2              fdb       PBUF,ZBRAN
                    fdb       BUFFR2-$
                    fdb       USE,STORE,R,AT,ZLESS
                    fdb       ZBRAN
                    fdb       BUFFR3-$
                    fdb       R,TWOP,R,AT,LIT,$7FFF,AND,ZERO,RW
BUFFR3              fdb       R,STORE,R,PREV,STORE,FROMR,TWOP
                    fdb       SEMIS
;
; *=======>> 176 <<   BLOCK
;
                    fcb       $85
                    fcc       'BLOC'
                    fcb       $CB
                    fdb       BUFFER-9
BLOCK               fdb       DOCOL,OFSET,AT,PLUS,TOR
                    fdb       PREV,AT,DUP,AT,R,SUB,DUP,PLUS,ZBRAN
                    fdb       BLOCK5-$
BLOCK3              fdb       PBUF,ZEQU,ZBRAN
                    fdb       BLOCK4-$
                    fdb       DROP,R,BUFFER,DUP,R,ONE,RW,TWO,SUB
BLOCK4              fdb       DUP,AT,R,SUB,DUP,PLUS,ZEQU,ZBRAN
                    fdb       BLOCK3-$
                    fdb       DUP,PREV,STORE
BLOCK5              fdb       FROMR,DROP,TWOP
                    fdb       SEMIS
;
; *=======>> 177 <<   (LINE)
;
                    fcb       $86
                    fcc       '(LINE'
                    fcb       $A9
                    fdb       BLOCK-8
PLINE               fdb       DOCOL,TOR,LIT
                    fdb       $40
                    fdb       BBUF,SSMOD,FROMR,BSCR,STAR,PLUS,BLOCK,PLUS,LIT
                    fdb       $40
                    fdb       SEMIS
;
; *=======>> 178 <<   .LINE
;
                    fcb       $85
                    fcc       '.LIN'
                    fcb       $C5
                    fdb       PLINE-9
DLINE               fdb       DOCOL,PLINE,DTRAIL,TYPE
                    fdb       SEMIS
;
; *=======>> 179 <<   MESSAGE
;
                    fcb       $87
                    fcc       'MESSAG'
                    fcb       $C5
                    fdb       DLINE-8
MESS                fdb       DOCOL,WARN,AT,ZBRAN
                    fdb       MESS3-$
                    fdb       DDUP,ZBRAN
                    fdb       MESS3-$
                    fdb       LIT,4
                    fdb       OFSET,AT,BSCR,SLASH,SUB,DLINE,BRAN
                    fdb       MESS4-$

MESS3               fdb       PDOTQ               ; print message
                    fcb       6
                    fcc       'err # '            ; error number
                    fdb       DOT                 ; print top of stack
MESS4               fdb       SEMIS
;
; *=======>> 180 <<   LOAD              ; input scr #
;
                    fcb       $84
                    fcc       'LOA'
                    fcb       $C4
                    fdb       MESS-10
LOAD                fdb       DOCOL,BLK,AT,TOR,IN,AT,TOR,ZERO,IN,STORE
                    fdb       BSCR,STAR,BLK,STORE
                    fdb       INTERP,FROMR,IN,STORE,FROMR,BLK,STORE
                    fdb       SEMIS
;
; *=======>> 181 <<   -->
;
                    fcb       $C3
                    fcc       '--'
                    fcb       $BE
                    fdb       LOAD-7
ARROW               fdb       DOCOL,QLOAD,ZERO,IN,STORE,BSCR
                    fdb       BLK,AT,OVER,MOD,SUB,BLK,PSTORE
                    fdb       SEMIS
;
;
;
; *=======>> 182 <<   code for EMIT
;
;
; *=======>> 183 <<   code for key
;
;
; *=======>> 184 <<   code for ?TERMINAL
;
;
; *=======>> 185 <<   code for CR
;
;
; *=======>> 189 <<   BLOCK-WRITE       ; write block to disk
;
                    fcb       $8B
                    fcc       'BLOCK-WRIT'
                    fcb       $C5
                    fdb       ARROW-6
BWRITE              fdb       *+2
;
;
;
                    jmp       NEXT

;

; *=======>> 190 <<   BLOCK-READ        ; read block from disk

;

                    fcb       $8A
                    fcc       'BLOCK-REA'
                    fcb       $C4
                    fdb       BWRITE-14
BREAD               fdb       *+2
;
;
;
                    jmp       NEXT

;

; *=======>> 191 <<   R/W

;

                    fcb       $83
                    fcc       'R/'
                    fcb       $D7
                    fdb       BREAD-13
RW                  fdb       DOCOL
                    fdb       ZBRAN               ; branch if zero
                    fdb       RW3-$

                    fdb       BREAD               ; read
                    fdb       BRAN
                    fdb       RW4-$

RW3                 fdb       BWRITE              ; write

RW4                 fdb       SEMIS
;
; *=======>> 192 <<   '                 ( an apostrophe )
;
                    fcb       $C1
                    fcb       $A7
                    fdb       RW-6
TICK                fdb       DOCOL,DFIND,ZEQU,ZERO,QERR,DROP,LITER
                    fdb       SEMIS
;
; *=======>> 193 <<   FORGET
;
                    fcb       $86
                    fcc       'FORGE'
                    fcb       $D4
                    fdb       TICK-4
;
;
;
FORGET              fdb       DOCOL,CURENT,AT,CONTXT,AT,SUB,LIT
                    fdb       $18
                    fdb       QERR,TICK,DUP,FENCE,AT,LESS,LIT
                    fdb       $15
                    fdb       QERR,DUP,NFA,DP,STORE,LFA,AT,CONTXT,AT,STORE
                    fdb       SEMIS
;
; *=======>> 194 <<   BACK
;
                    fcb       $84
                    fcc       'BAC'
                    fcb       $CB
                    fdb       FORGET-9
BACK                fdb       DOCOL,HERE,SUB,COMMA
                    fdb       SEMIS
;
; *=======>> 195 <<   BEGIN
;
                    fcb       $C5
                    fcc       'BEGI'
                    fcb       $CE
                    fdb       BACK-7
BEGIN               fdb       DOCOL,QCOMP,HERE,ONE
                    fdb       SEMIS
;
; *=======>> 196 <<   ENDIF
;
                    fcb       $C5
                    fcc       'ENDI'
                    fcb       $C6
                    fdb       BEGIN-8
ENDIF               fdb       DOCOL,QCOMP,TWO,QPAIRS,HERE
                    fdb       OVER,SUB,SWAP,STORE
                    fdb       SEMIS
;
; *=======>> 197 <<   THEN
;
                    fcb       $C4
                    fcc       'THE'
                    fcb       $CE
                    fdb       ENDIF-8
THEN                fdb       DOCOL,ENDIF
                    fdb       SEMIS
;
; *=======>> 198 <<   DO
;
                    fcb       $C2
                    fcc       'D'
                    fcb       $CF
                    fdb       THEN-7
DO                  fdb       DOCOL,COMPIL,XDO1,HERE,THREE
                    fdb       SEMIS
;
; *=======>> 199 <<   LOOP
;
                    fcb       $C4
                    fcc       'LOO'
                    fcb       $D0
                    fdb       DO-5
LOOP                fdb       DOCOL,THREE,QPAIRS,COMPIL,XLOOP,BACK
                    fdb       SEMIS
;
; *=======>> 200 <<   +LOOP
;
                    fcb       $C5
                    fcc       '+LOO'
                    fcb       $D0
                    fdb       LOOP-7
PLOOP               fdb       DOCOL,THREE,QPAIRS,COMPIL,XPLOOP,BACK
                    fdb       SEMIS
;
; *=======>> 201 <<   UNTIL
;
                    fcb       $C5
                    fcc       'UNTI'              ; ( same as end )
                    fcb       $CC
                    fdb       PLOOP-8
UNTIL               fdb       DOCOL,ONE,QPAIRS,COMPIL,ZBRAN,BACK
                    fdb       SEMIS
;
; *=======>> 202 <<   END
;
                    fcb       $C3
                    fcc       'EN'
                    fcb       $C4
                    fdb       UNTIL-8
END                 fdb       DOCOL,UNTIL
                    fdb       SEMIS
;
; *=======>> 203 <<   AGAIN
;
                    fcb       $C5
                    fcc       'AGAI'
                    fcb       $CE
                    fdb       END-6
AGAIN               fdb       DOCOL,ONE,QPAIRS,COMPIL,BRAN,BACK
                    fdb       SEMIS
;
; *=======>> 204 <<   REPEAT
;
                    fcb       $C6
                    fcc       'REPEA'
                    fcb       $D4
                    fdb       AGAIN-8
REPEAT              fdb       DOCOL,TOR,TOR,AGAIN,FROMR,FROMR
                    fdb       TWO,SUB,ENDIF
                    fdb       SEMIS
;
; *=======>> 205 <<   IF
;
                    fcb       $C2
                    fcc       'I'
                    fcb       $C6
                    fdb       REPEAT-9
IF                  fdb       DOCOL,COMPIL,ZBRAN,HERE,ZERO,COMMA,TWO
                    fdb       SEMIS
;
; *=======>> 206 <<   ELSE
;
                    fcb       $C4
                    fcc       'ELS'
                    fcb       $C5
                    fdb       IF-5
ELSE                fdb       DOCOL,TWO,QPAIRS,COMPIL,BRAN,HERE
                    fdb       ZERO,COMMA,SWAP,TWO,ENDIF,TWO
                    fdb       SEMIS
;
; *=======>> 207 <<   WHILE
;
                    fcb       $C5
                    fcc       'WHIL'
                    fcb       $C5
                    fdb       ELSE-7
WHILE               fdb       DOCOL,IF,TWOP
                    fdb       SEMIS
;
; *=======>> 208 <<   SPACES
;
                    fcb       $86
                    fcc       'SPACE'
                    fcb       $D3
                    fdb       WHILE-8
SPACES              fdb       DOCOL,ZERO,MAX,DDUP,ZBRAN
                    fdb       SPACE3-$
                    fdb       ZERO,XDO1
SPACE2              fdb       SPACE,XLOOP
                    fdb       SPACE2-$
SPACE3              fdb       SEMIS
;
; *=======>> 209 <<   <#
;
                    fcb       $82
                    fcc       '<'
                    fcb       $A3
                    fdb       SPACES-9
BDIGS               fdb       DOCOL,PAD,HLD,STORE
                    fdb       SEMIS
;
; *=======>> 210 <<   #>
;
                    fcb       $82
                    fcc       '#'
                    fcb       $BE
                    fdb       BDIGS-5
EDIGS               fdb       DOCOL,DROP,DROP,HLD,AT,PAD,OVER,SUB
                    fdb       SEMIS
;
; *=======>> 211 <<   SIGN
;
                    fcb       $84
                    fcc       'SIG'
                    fcb       $CE
                    fdb       EDIGS-5
SIGN                fdb       DOCOL,ROT,ZLESS,ZBRAN
                    fdb       SIGN2-$
                    fdb       LIT
                    fcb       0
                    fcc       "-"
                    fdb       HOLD
SIGN2               fdb       SEMIS
;
; *=======>> 212 <<   #             ( octothorpe )
;
                    fcb       $81
                    fcb       $A3
                    fdb       SIGN-7
DIG                 fdb       DOCOL,BASE,AT,MSMOD,ROT,LIT
                    fdb       9
                    fdb       OVER,LESS,ZBRAN
                    fdb       DIG2-$
                    fdb       LIT
                    fdb       7
                    fdb       PLUS
DIG2                fdb       LIT
                    fcb       0
                    fcc       "0"                 ; ascii zero
                    fdb       PLUS,HOLD
                    fdb       SEMIS
;
; *=======>> 213 <<   #S
;
                    fcb       $82
                    fcc       '#'
                    fcb       $D3
                    fdb       DIG-4
DIGS                fdb       DOCOL
DIGS2               fdb       DIG,OVER,OVER,OR,ZEQU,ZBRAN
                    fdb       DIGS2-$
                    fdb       SEMIS
;
; *=======>> 214 <<   .R
;
                    fcb       $82
                    fcc       '.'
                    fcb       $D2
                    fdb       DIGS-5
DOTR                fdb       DOCOL,TOR,STOD,FROMR,DDOTR
                    fdb       SEMIS
;
; *=======>> 215 <<   D.R
;
                    fcb       $83
                    fcc       'D.'
                    fcb       $D2
                    fdb       DOTR-5
DDOTR               fdb       DOCOL,TOR,SWAP,OVER,DABS,BDIGS,DIGS,SIGN
                    fdb       EDIGS,FROMR,OVER,SUB,SPACES,TYPE
                    fdb       SEMIS
;
; *=======>> 216 <<   D.
;
                    fcb       $82
                    fcc       'D'
                    fcb       $AE
                    fdb       DDOTR-6
DDOT                fdb       DOCOL,ZERO,DDOTR,SPACE
                    fdb       SEMIS
;
; *=======>> 217 <<   .             ( period )
;
                    fcb       $81
                    fcb       $AE
                    fdb       DDOT-5
DOT                 fdb       DOCOL,STOD,DDOT
                    fdb       SEMIS
;
; *=======>> 218 <<   ?             ( question mark )
;
                    fcb       $81
                    fcb       $BF
                    fdb       DOT-4
QUEST               fdb       DOCOL,AT,DOT
                    fdb       SEMIS
;
; *=======>> 219 <<   LIST
;
                    fcb       $84
                    fcc       'LIS'
                    fcb       $D4
                    fdb       QUEST-4
LIST                fdb       DOCOL,DEC,CR,DUP,SCR,STORE,PDOTQ
                    fcb       6
                    fcc       "SCR # "
                    fdb       DOT,LIT
                    fdb       $10
                    fdb       ZERO,XDO1
LIST2               fdb       CR,I,THREE
                    fdb       DOTR,SPACE,I,SCR,AT,DLINE,XLOOP
                    fdb       LIST2-$
                    fdb       CR
                    fdb       SEMIS
;
; *=======>> 220 <<   INDEX
;
                    fcb       $85
                    fcc       'INDE'
                    fcb       $D8
                    fdb       LIST-7
INDEX               fdb       DOCOL,CR,ONEP,SWAP,XDO1
INDEX2              fdb       CR,I,THREE
                    fdb       DOTR,SPACE,ZERO,I,DLINE
                    fdb       QTERM,ZBRAN
                    fdb       INDEX3-$
                    fdb       LEAVE
INDEX3              fdb       XLOOP
                    fdb       INDEX2-$
                    fdb       CR
                    fdb       SEMIS
;
; *=======>> 221 <<   TRIAD
;
                    fcb       $85
                    fcc       'TRIA'
                    fcb       $C4
                    fdb       INDEX-8
TRIAD               fdb       DOCOL,THREE,SLASH,THREE,STAR
                    fdb       THREE,OVER,PLUS,SWAP,XDO1
TRIAD2              fdb       CR,I
                    fdb       LIST,QTERM,ZBRAN
                    fdb       TRIAD3-$
                    fdb       LEAVE
TRIAD3              fdb       XLOOP
                    fdb       TRIAD2-$
                    fdb       CR,LIT
                    fdb       $0F
                    fdb       MESS,CR
                    fdb       SEMIS
;
; *=======>> 222 <<   VLIST
;
                    fcb       $85
                    fcc       'VLIS'
                    fcb       $D4
                    fdb       TRIAD-8
VLIST               fdb       DOCOL,LIT
                    fdb       $80
                    fdb       OUT,STORE,CONTXT,AT,AT
VLIST1              fdb       OUT,AT,COLUMS,AT,LIT
                    fdb       32
                    fdb       SUB,GREAT,ZBRAN
                    fdb       VLIST2-$
                    fdb       CR,ZERO,OUT,STORE
VLIST2              fdb       DUP,IDDOT,SPACE,SPACE,PFA,LFA,AT
                    fdb       DUP,ZEQU,QTERM,OR,ZBRAN
                    fdb       VLIST1-$
                    fdb       DROP
                    fdb       CR
                    fdb       SEMIS
;
; *=======>> 223 <<   PAUSE             ; one second pause
;
                    fcb       $85
                    fcc       'PAUS'
                    fcb       $C5
                    fdb       VLIST-8
PAUSE               fdb       *+2
                    ldx       #1000               ; 1000 milliseconds
PAUS1               ldaa      #$C8                ; 1 msec @ 4.00 mhz
PAUS2               deca
                    bne       PAUS2
                    dex
                    bne       PAUS1
                    jmp       NEXT

;

; *=======>> 224 <<   C/L               ; characters/line

;

                    fcb       $83
                    fcc       'C/'
                    fcb       $CC
                    fdb       PAUSE-8
CL                  fdb       DOCON               ; 64 characters per line
                    fdb       64
;
; *=======>> 225 <<   DEPTH             ; no operation
;
                    fcb       $85
                    fcc       'DEPT'
                    fcb       $C8
                    fdb       CL-6
DEPTH               fdb       DOCOL,SZERO,AT,SPAT
                    fdb       SUB,TWO,SLASH,ONE,SUB
                    fdb       SEMIS
;
; *=======>> 226 <<   .S                  ; print out contents of stack
;
                    fcb       $82
                    fcc       '.'
                    fcb       $D3
                    fdb       DEPTH-8
DOTS                fdb       DOCOL,DEPTH,ZBRAN   ; if zero, print empty message
                    fdb       DOTS2-$
                    fdb       CR,SPAT,TWO,SUB
                    fdb       SZERO,AT,TWO,SUB
                    fdb       XDO1
DOTS1               fdb       I,AT,DOT,LIT,$FFFE,XPLOOP
                    fdb       DOTS1-$
                    fdb       BRAN
                    fdb       DOTS3-$             ; skip over message
DOTS2               fdb       PDOTQ
                    fcb       14
                    fcc       ' stack empty! '
DOTS3               fdb       QUIT,SEMIS
;
; *=======>> 227 <<   DUMP
;
                    fcb       $84
                    fcc       'DUM'
                    fcb       $D0
                    fdb       DOTS-5
DUMP                fdb       DOCOL,HEX,CR,CR,LIT,5,SPACES
                    fdb       LIT,16,ZERO,XDO1
DUMP1               fdb       I,LIT,3,DOTR,XLOOP
                    fdb       DUMP1-$

                    fdb       TWO,SPACES,LIT,16,ZERO,XDO1
DUMP2               fdb       I,ZERO,BDIGS,DIG,EDIGS,TYPE,XLOOP
                    fdb       DUMP2-$

                    fdb       CR,OVER,PLUS,SWAP,DUP,LIT,$F
                    fdb       AND,XOR,XDO1
DUMP3               fdb       CR,I,ZERO,LIT,4,DDOTR,ONE
                    fdb       SPACES,I,LIT,16,PLUS,I

                    fdb       OVER,OVER,XDO1
DUMP4               fdb       I,CAT,SPACE,ZERO,BDIGS,DIG,DIG
                    fdb       EDIGS,TYPE,XLOOP
                    fdb       DUMP4-$

                    fdb       TWO,SPACES,XDO1
DUMP5               fdb       I,CAT,DUP,LIT,32,LESS,ZBRAN
                    fdb       DUMP6-$
                    fdb       DROP,LIT,46
DUMP6               fdb       DUP,LIT,126,GREAT,ZBRAN
                    fdb       DUMP7-$
                    fdb       DROP,LIT,46
DUMP7               fdb       EMIT,XLOOP
                    fdb       DUMP5-$

                    fdb       LIT,16,XPLOOP
                    fdb       DUMP3-$
                    fdb       CR,SEMIS
;
; *=======>> 228 <<   ROOM                ; number of bytes available
;
                    fcb       $84
                    fcc       'ROO'
                    fcb       $CD
                    fdb       DUMP-7
ROOM                fdb       DOCOL,SZERO,AT,DP,AT
                    fdb       SUB,CR,DOT
                    fdb       PDOTQ
                    fcb       16
                    fcc       ' bytes available'
                    fdb       CR,SEMIS
;
; *=======>> 229 <<   U.                  ; print unsigned double number
;
                    fcb       $82
                    fcc       'U'
                    fcb       $AE
                    fdb       ROOM-7
UDOT                fdb       DOCOL,ZERO
                    fdb       DDOT,SEMIS
;
; *=======>> 230 <<   NEXT-LINK           ; address of NEXT
;
                    fcb       $89
                    fcc       'NEXT-LIN'
                    fcb       $CB
                    fdb       UDOT-5
NEXTLNK             fdb       DOCON
                    fdb       NEXT
;
; *=======>> 231 <<   W                   ; address of W
;
                    fcb       $81
                    fcb       $D7
                    fdb       NEXTLNK-12
WREG                fdb       DOCON
                    fdb       W
;
; *=======>> 232 <<   IP                  ; address of IP
;
                    fcb       $82
                    fcc       'I'
                    fcb       $D0
                    fdb       WREG-4
IPREG               fdb       DOCON
                    fdb       IP
;
; *=======>> 235 <<   FLUSH             ; flush updated buffers to disk
;
                    fcb       $85
                    fcc       'FLUS'
                    fcb       $C8
                    fdb       IPREG-5
FLUSH               fdb       DOCOL
                    fdb       LIT,8,ZERO,XDO1
FLUSH1              fdb       LIT,$7FFF,BUFFER,DROP,XLOOP
                    fdb       FLUSH1-$
                    fdb       SEMIS
;
; *=======>>   <<     -ROT
;
                    fcb       $84
                    fcc       '-RO'
                    fcb       $D4
                    fdb       FLUSH-8
DROT                fdb       DOCOL,SWAP,TOR
                    fdb       SWAP,FROMR,SEMIS
;
; *=======>>   <<     PICK
;
                    fcb       $84
                    fcc       'PIC'
                    fcb       $CB
                    fdb       DROT-7
PICK                fdb       DOCOL,DUP,PLUS,SPAT
                    fdb       PLUS,AT,SEMIS
;
; *=======>>   <<     MYSELF
;
                    fcb       $C6
                    fcc       'MYSEL'
                    fcb       $C6
                    fdb       PICK-7
MSELF               fdb       DOCOL,LATEST,PFA,CFA,COMMA,SEMIS
;
; *=======>>   <<     ROLL
;
                    fcb       $84
                    fcc       'ROL'
                    fcb       $CC
                    fdb       MSELF-9
ROLL                fdb       DOCOL,DUP,TWO,LESS,ZBRAN
                    fdb       ROL1-$
                    fdb       DROP,BRAN
                    fdb       ROL2-$
ROL1                fdb       SWAP,TOR,ONE,SUB
                    fdb       ROLL,FROMR,SWAP
ROL2                fdb       SEMIS
;
; *=======>>   <<     2SWAP
;
                    fcb       $85
                    fcc       '2SWA'
                    fcb       $D0
                    fdb       ROLL-7
TSWAP               fdb       DOCOL,ROT,TOR
                    fdb       ROT,FROMR,SEMIS
;
; *=======>>   <<     2ROLL
;
                    fcb       $84
                    fcc       '2RO'
                    fcb       $D4
                    fdb       TSWAP-8
TROT                fdb       TOR,TOR,TSWAP
                    fdb       FROMR,FROMR,TSWAP,SEMIS
;
; *=======>>   <<     2DROP
;
                    fcb       $85
                    fcc       '2DRO'
                    fcb       $D0
                    fdb       TROT-7
TDROP               fdb       DOCOL,DROP,DROP,SEMIS
;
; *=======>>   <<     2DUP
;
                    fcb       $84
                    fcc       '2DU'
                    fcb       $D0
                    fdb       TDROP-8
TDUP                fdb       DOCOL,OVER,OVER,SEMIS
;
; *=======>>   <<     2OVER
;
                    fcb       $85
                    fcc       '2OVE'
                    fcb       $D2
                    fdb       TDUP-7
TOVER               fdb       DOCOL,LIT,4,PICK
                    fdb       LIT,4,PICK,SEMIS
;
; *=======>>   <<     D-
;
                    fcb       $82
                    fcc       'D'
                    fcb       $AD
                    fdb       TOVER-8
DSUB                fdb       DOCOL,DMINUS,DPLUS,SEMIS
;
; *=======>>   <<     D0=
;
                    fcb       $83
                    fcc       'D0'
                    fcb       $BD
                    fdb       DSUB-5
DZEQ                fdb       DOCOL,OR,ZEQU,SEMIS
;
; *=======>>   <<     D0<
;
                    fcb       $83
                    fcc       'D0'
                    fcb       $BC
                    fdb       DZEQ-6
DZLS                fdb       DOCOL,SWAP,DROP
                    fdb       ZLESS,SEMIS
;
; *=======>>   <<     D=
;
                    fcb       $82
                    fcc       'D'
                    fcb       $BD
                    fdb       DZLS-6
DEQ                 fdb       DOCOL,DSUB
                    fdb       DZEQ,SEMIS
;
; *=======>>   <<     D<
;
                    fcb       $82
                    fcc       'D'
                    fcb       $BC
                    fdb       DEQ-5
DLS                 fdb       DOCOL,DSUB,DZLS,SEMIS
;
; *=======>>   <<     D>
;
                    fcb       $82
                    fcc       'D'
                    fcb       $BE
                    fdb       DLS-5
DGT                 fdb       DOCOL,TSWAP,DLS,SEMIS
;
; *=======>>   <<     D+-
;
                    fcb       $83
                    fcc       'D+'
                    fcb       $AD
                    fdb       DGT-5
DPLM                fdb       DOCOL,ZLESS,ZBRAN
                    fdb       DPLM1-$
                    fdb       MINUS
DPLM1               fdb       SEMIS
;
; *=======>>   <<     D*
;
                    fcb       $82
                    fcc       'D'
                    fcb       $AA
                    fdb       DPLM-6
DSTAR               fdb       DOCOL,OVER,LIT,5,PICK
                    fdb       USTAR,LIT,6,ROLL
                    fdb       LIT,4,ROLL,STAR,PLUS,TSWAP
                    fdb       STAR,PLUS,SEMIS
;
; *=======>>   <<     UM*
;
                    fcb       $83
                    fcc       'UM'
                    fcb       $AA
                    fdb       DSTAR-5
UMSTR               fdb       DOCOL,TOR,OVER,USTAR
                    fdb       ROT,FROMR,STAR,PLUS,SEMIS
;
; *=======>>   <<     UM/
;
                    fcb       $83
                    fcc       'UM'
                    fcb       $AF
                    fdb       UMSTR-6
UMSLSH              fdb       DOCOL,SWAP,OVER
                    fdb       SLMOD,TOR,SWAP
                    fdb       USLASH,SWAP,DROP
                    fdb       FROMR,SEMIS
;
; *=======>>   <<     RESTORE             ; restore disk head to track 0
;
                    fcb       $87
                    fcc       'RESTOR'
                    fcb       $C5
                    fdb       UMSLSH-6
RESTR               fdb       *+2
;
;
;
                    jmp       NEXT

;

; *=======>>   <<     SEEK                ; disk head to track

;

                    fcb       $84
                    fcc       'SEE'
                    fcb       $CB
                    fdb       RESTR-10
DSEEK               fdb       *+2
;
;
;
                    jmp       NEXT

;

; *=======>>   <<     TRACK               ; disk head to track

;

                    fcb       $85
                    fcc       'TRAC'
                    fcb       $CB
                    fdb       DSEEK-7
DTRACK              fdb       DOUSER
                    fdb       XTRACK-UORIG
;
; *=======>>   <<     SECTOR              ;
;
                    fcb       $86
                    fcc       'SECTO'
                    fcb       $D2
                    fdb       DTRACK-8
DSECTOR             fdb       DOUSER
                    fdb       XSECTOR-UORIG
;
; *=======>>   <<                         ;
;
                    fcb       $87
                    fcc       'DSTATU'
                    fcb       $D3
                    fdb       DSECTOR-9
DSTAT               fdb       DOUSER
                    fdb       XDSTAT-UORIG

;
; *=======>> XX <<    NOOP                ; no operation
;
                    fcb       $84
                    fcc       'NOO'
                    fcb       $D0
                    fdb       DSTAT-10
NOOP                fdb       NEXT                ; a useful no-op
;
;                   end of  forth
;
;
;       reset vectors for rom
;
                    org       $FFD6

SCI                 fdb       INIT
SPI                 fdb       INIT
PAIE                fdb       INIT
PAO                 fdb       INIT
TOF                 fdb       INIT
TOC5                fdb       INIT
TOC4                fdb       INIT
TOC3                fdb       INIT
TOC2                fdb       INIT
TOC1                fdb       INIT
TIC3                fdb       INIT
TIC2                fdb       INIT
TIC1                fdb       INIT
RTI                 fdb       INIT
IRQ                 fdb       INIT
XIRQ                fdb       INIT
SWI                 fdb       INIT
ILLOP               fdb       INIT
COP                 fdb       INIT
CLM                 fdb       INIT
RST                 fdb       INIT
;
;
;
                    end
