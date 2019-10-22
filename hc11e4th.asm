;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;
;          68HC11 eForth for the Motorola FREEWARE assembler
;
;  This version of eForth runs on the Motorola 68HC11 microcontrollers.
;  The source code itself is derived from the original 8051 eForth as
;  developed by Dr. C. H. Ting.  I have rewritten the source to use the
;  Motorola FREEWARE assembler, and have added features of interest
;  to users of the 'HC11 family:
;
;  1.  This code creates separate name and code areas, per conventional
;  eForth.  The two areas, however, are intertwined throughout the
;  object file, rather than growing inward from opposite ends of the
;  memory space during assembly as done in the original 8051 code.
;
;  At execution time, the name and code pointers are aimed at addresses
;  selected by the user at assembly time.  From then on, the name and
;  code areas expand as normal (code area grows toward higher memory,
;  name area grows toward lower memory).
;
;  2.  The object created by this source code is fully ROMable and
;  fits in about 7K of EPROM.  Upon execution, a word called TASK is
;  moved into the RAM area and the name and code pointers are initialized.
;
;  3.  Like the original eForth, this version is word-aligned in all
;  respects.  This imposes a tiny size penalty and a minor speed penalty
;  over a typical byte-aligned 68HC11 Forth.
;
;  4.  I have not added any code for interrupt vectoring.  You should bring
;  this up initially by burning it into EPROM and running it by a G command
;  in BUFFALO.  (Alternatively, you could download the S19 file via serial
;  port into RAM.)  Later, you can add your own interrupt vectoring, move
;  the code into the $ffff area, and disable BUFFALO.  This would let you
;  execute eForth directly on power-up.
;
;  5.  I have added the word NOOP (for no-operation).  It is an empty word
;  that simply serves as the execution code for TASK.
;
;  This is a straight-up, no-frills porting of 8051 eForth to the 68HC11.
;  I have not added words you will likely miss, such as CONSTANT and C,.
;  Nor have I rewritten any high-level words in assembly language; since
;  FIND and NUMBER are high-level, you can expect slow compilation.
;
;  But eForth is intended to be a starting point; feel free to extend and
;  modify as you like.
;
;  I place this source code in the public domain.  I cannot guarantee
;  to support or maintain it, and I certainly do not take any responsibility
;  for whatever you do with or to this code, nor for any effects this code
;  generates, either directly or indirectly.  I wrote this code primarily
;  for pleasure, and I hope you enjoy playing with it and learning from it.
;
;  If you have any questions or comments, you can reach me at:
;
;                   Karl Lunt
;                   2133 186th Pl., SE
;                   Bothell, WA   98012
;                   (206) 483-0447
;                   Internet:  karl@mav.com
;*******************************************************************************

                    #ListOff
                    #Uses     exp-f1.inc
                    #ListOn

;*******************************************************************************
; Macros
;*******************************************************************************

token               macro     label,'string'[,c|i]
                    mset      #','
                    mreq      1,2:label,'string'
                    mstr      2
                    align     2
                    #temp     :index
                    fdb       ~1~
          #ifz :temp-1
                    fdb       0                   ; link of 0 means first entry
          #else
                    fdb       _link{:temp-1}
          #endif
                    #temp1
          #ifparm ~3~ = c
                    #temp1    compo
          #else ifparm ~3~ = i
                    #temp1    immed
          #else ifparm ~3~ = ic
                    #temp1    immed+compo
          #else ifparm ~3~ = ci
                    #temp1    immed+compo
          #else ifnb ~3~
                    merror    Unexpected: ~3~
          #endif
_link{:temp}        fcb       :temp1+:2-2         ;length of literal without quotes
                    fcc       ~2~
          #ifz :2\2
                    fcb       0                   ; null-fill cell
          #endif
~1~                 def       *
                    endm

;*******************************************************************************

compo               equ       $40                 ; lexicon compile only bit
immed               equ       $80                 ; lexicon immediate bit
maskk               equ       $1f7f               ; lexicon bit mask

celll               equ       2                   ; size of a cell
basee               equ       10                  ; default radix
vocss               equ       8                   ; depth of vocabulary stack

ver                 equ       1                   ; version number
ext                 equ       0                   ; extension number

err                 equ       27                  ; error escape
tic                 equ       39                  ; tick
TRUE                equ       -1                  ; eForth true flag
FALSE               equ       0                   ; eForth false flag
nopjmp              equ       $017E               ; nop-jmp opcodes

;===============================================================================
;  RAM allocation:  +-------------------------------+ top of RAM (RAMEND)
;                   |   return stack grows down     |
;                   |            \/                 |
;                   |            \/                 |
;                   +-------------------------------+
;                   |      text input buffer        |
;                   +-------------------------------+
;                   |    data stack grows down      |
;                   |            \/                 |
;                   |            \/                 |
;                   +-------------------------------+
;                   |            /\                 |
;                   |            /\                 |
;                   |    user area grows up         |
;                   +-------------------------------|
;                   |  name dictionary grows down   |
;                   |            \/                 |
;                   |            \/                 |
;                   +-------------------------------+
;                   |            /\                 |
;                   |            /\                 |
;                   |   code dictionary grows up    |
;                   +-------------------------------+ bottom of RAM (rambeg)
;===============================================================================

;  You can customize the memory usage of this eForth by changing the
;  values assigned to the following equates.  All sizes are in bytes
;  and must be even numbers.

RAMBEG              equ       $a000               ; bottom of ram memory
RAMEND              equ       $bfff               ; top of ram memory
ROMBEG              equ       $c000               ; bottom of rom memory

us                  equ       $100                ; user area size in bytes
rts                 equ       $100                ; return stack/tib size
dts                 equ       $100                ; data stack size

;  These equates fix the relative positions of eForth's buffers and
;  stack areas.

rpp                 equ       RAMEND&$fffe        ; start of return stack (rp0)
tibb                equ       rpp-rts             ; start of tib, body of rtn stack
spp                 equ       tibb-2              ; start of data stack (sp0)
upp                 equ       spp-dts-us-$10      ; start of user area (up)

namee               equ       upp-$10&$fffe       ; initial name dictionary (word align)
codee               equ       RAMBEG              ; initial code dictionary

;  Allocation of 68HC11 working registers.  These registers should stay
;  in the zero-page area for faster execution speed.

ip                  equ       $0022               ; 2 bytes for IP

;*******************************************************************************
                    #ROM                          ; Start of 'hc11 eForth code
;*******************************************************************************

;  ROM-based default data values.  These are automatically
;  preloaded into the user area upon reset.

uzero               fdb:4     0                   ; reserved
                    fdb       spp                 ; sp0
                    fdb       rpp                 ; rp0
                    fdb       qrx                 ; '?key
                    fdb       txsto               ; 'emit
                    fdb       accep               ; 'expect
                    fdb       ktap                ; 'tap
                    fdb       txsto               ; 'echo
                    fdb       dotok               ; 'prompt
                    fdb       basee               ; base
                    fdb       0                   ; tmp
                    fdb       0                   ; span
                    fdb       0                   ; >in
                    fdb       0                   ; #tib
                    fdb       tibb                ; tib
                    fdb       0                   ; csp
                    fdb       inter               ; 'eval
                    fdb       numbq               ; 'number
                    fdb       0                   ; hld
                    fdb       0                   ; handler
                    fdb       0                   ; context pointer
                    fdb:8     0                   ; vocabulary stack (vocss deep)
                    fdb       0                   ; current pointer
                    fdb       0                   ; vocabulary link pointer
                    fdb       codee               ; cp
                    fdb       0                   ; np (overwritten at powerup)
                    fdb       0                   ; last (overwritten at powerup)
                    fdb       0                   ; forth (overwritten at powerup)
                    fdb       0                   ; vocabulary link
ulast

;  The following code is copied to the start of the RAM name
;  dictionary space on powerup.

ntask               fdb       noop                ; 'code' for task
                    fdb       coldlink            ; link 'back' to cold
                    fcs       4,'TASK'
ntaskl              equ       *-ntask

;*******************************************************************************
;  Start of the compiler; jump here on power-up.
;  Note that serial port initialization can occur here and at !IO; if
;  you change any initialization code here, be sure to duplicate the
;  changes in !IO.

Start               proc
                    sei
                    lda       #%10110011          ; turn on a/d, use edge irq, enable
                                                  ; delay after stop, set slow clk to
                                                  ; watchdog
                    sta       OPTION

                    lda       #$30                ; 9600 BAUD @ 2MHz bus
                    sta       BAUD
                    clr       SCCR1               ; 8-bit xfers
                    lda       #$0c
                    sta       SCCR2               ; no interrupts, enable r & t
                    lds       #spp-2              ; temp start of data stack
                    ldy       #rpp                ; temp start of return stack
                    ldx       #cold1              ; point to first instruction
                    bra       ?next2              ; and start it up

;*******************************************************************************
;  The forth inner interpreter
;  Entry can occur at one of three locations, depending on the action
;  needed.
;  Entry at pushd pushes the word in A:B onto the data stack, then
;  falls into ?next.  Use this entry point when leaving a low-level
;  definition that must also push a result (in D) onto the data
;  stack.
;  Entry at ?next simply moves to the next low-level definition in
;  the thread.  Use this entry point when leaving a low-level
;  definition that does not need to save any results onto the data
;  stack.
;  Entry at ?next2 uses the value in X as the current IP (instruction
;  pointer) and calculates the next executable address from it.  Use
;  this entry point when changing execution threads; for example, see
;  the code at ?BRANCH.

pushd               proc
                    pshb                          ; save D as word on stack
                    psha                          ; and fall into ?next

?next               ldx       ip                  ; get current ip
?next2              ldb       #2                  ; ip = ip + 2
                    abx
                    stx       ip
                    dex:2                         ; x = ip - 2
                    ldx       ,x                  ; x = (x)
                    jmp       ,x                  ; go to (x)

dolst               ldb       #4                  ; x = x + 4
                    abx                           ; (to jump around NOP JMP DOLST)
                    ldd       ip                  ; get old IP
                    std       ,y                  ; save on return stack
                    dey                           ; make room on return stack
                    dey
                    bra       ?next2              ; and go to next level

exit                ldb       #2                  ; y = y + 2
                    aby
                    ldx       ,y                  ; pull old IP from return stack
                    bra       ?next2              ; and go to previous level

;*******************************************************************************
;  Each eForth word contains a header of the following format:
;    fdb  label               points to executable code
;    fdb  prev_name      points back to previous name
; this_name:
;    fcb  options+n      n = length of name in bytes
;    fcc  'XXXX'              string containing word's name
;    fcb  0              word-alignment IF n IS EVEN!
; label:
;    -----                    start of assembly code for XXXX
;  where options is immed if this is an IMMEDIATE word and compo if
;  this is a COMPILE-ONLY word.  Note that the line containing fcb 0 is
;  optional; it should only appear if the word's name contains an even
;  number of characters.  Look over several definitions below
;  for examples of building headers.
;  Note that all low-level (assembly language) definitions begin with the
;  phrase:
;                   align     2                   ;WAS: org *+1&$fffe
;  This forces the definition to be word-aligned.  This org statement is
;  not necessary for high-level definitions.
;  Note also that all embedded strings (usually following an 'fdb  dotqp')
;  must contain an even number of bytes INCLUDING THE LENGTH BYTE.  If
;  necessary, use a 'fcb  0' to pad out to the next word address.  Check
;  the definitions below for examples.
;  If you add customized low-level definitions, observe the following
;  register usage:
;  The hardware stack (S-register) serves as eForth's data stack.  You must
;  always push and pull data in word-wide sequences.  Never leave the data
;  stack having pushed or pulled an odd number of bytes.
;  The Y-register defines eForth's return stack.  All accesses must be
;  word-wide.  See the code at >R and R> for examples of using the Y-register
;  as a stack pointer.
;  The X-register may be freely trashed in your code, with one possible
;  exception.  If you leave your new definition with a jump to ?next2,
;  X must point to the next threaded address to execute.  See the code in
;  BRANCH for an example.
;  The A- and B-registers may also be used freely, again with one possible
;  exception.  If you leave your new definition with a jump to pushd,
;  A:B will be pushed onto the data stack prior to executing the next
;  word in the thread.

;*******************************************************************************
;  The kernel
;*******************************************************************************

;*******************************************************************************
;   dolit     ( -- w )
;    push an inline literal.

                    @token    dolit,'doLIT',c
                    ldx       ip                  ; get addr of next word
                    ldd       ,x                  ; get data at that addr
                    inx:2                         ; now bump IP
                    stx       ip
                    bra       pushd               ; and save on stack

;*******************************************************************************
;   dolist    ( a -- )
;    process colon list.

                    @token    dolst,'doLIST',c    ; points back into inner interpreter

;*******************************************************************************
;   next ( -- )
;    run time code for the single index loop.
;    : next ( -- ) \ hilevel model
;    r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;

                    @token    donxt,'next',c
                    ldd       2,y                 ; get counter on return stack
                    beq       donxt1              ; branch if loop is done
                    subd      #1                  ; no, bump the counter
                    std       2,y                 ; and replace on stack
                    bra       bran                ; and branch back to top
donxt1              iny:2                         ; done, burn counter from stack
                    ldx       ip                  ; get the IP
                    inx:2                         ; and get addr past branch target
                    bra       ?next2              ; and go do next word

;*******************************************************************************
;   ?branch   ( f -- )
;    branch if flag is zero.

                    @token    qbran,'?branch',c
                    puld                          ; get TOS to D
                    cmpd      #0                  ; did we get a 0?
                    bne       qbran1              ; branch if not
bran                ldx       ip                  ; yes, get next word as addr
                    ldx       ,x                  ; now get contents as new IP
                    bra       ?next2              ; and jump there
qbran1              ldx       ip                  ; get old ip
                    inx:2                         ; and move past branch addr
                    jmp       ?next2              ; and jump there

;*******************************************************************************
;   branch    ( -- )
;    branch to an inline address.

                    @token    bran,'branch',c     ; use code inside ?BRANCH

;*******************************************************************************
;   execute   ( ca -- )
;    execute the word at ca.

                    @token    execu,'EXECUTE'
                    pulx                          ; get ca from TOS
                    jmp       ,x                  ; and go do it

;*******************************************************************************
;   exit ( -- )
;    terminate a colon definition.

                    @token    exit,'EXIT'         ; points back into inner interpreter

;*******************************************************************************
;   !     ( u a -- )
;    store u into address a.

                    @token    store,'!'
                    pulx                          ; get the addr
                    puld                          ; get the word
                    std       ,x                  ; and save to addr
                    jmp       ?next


;*******************************************************************************
;   @     ( a -- w )
;    push memory location to the data stack.

                    @token    at,'@'
                    pulx                          ; get the addr
                    ldd       ,x                  ; get the data there
                    jmp       pushd               ; and save it


;*******************************************************************************
;   c!    ( c b -- )
;    pop the data stack to byte memory.

                    @token    cstor,'C!'
                    pulx                          ; get the addr
                    puld                          ; get the data (only low byte counts)
                    stb       ,x                  ; save to addr
                    jmp       ?next

;*******************************************************************************
;   c@    ( b -- c )
;    push byte memory location to the data stack.

                    @token    cat,'C@'
                    pulx                          ; get the addr
                    ldb       ,x                  ; get the data
                    clra                          ; make MSB = 0
                    jmp       pushd               ; and save it

;*******************************************************************************
;   >r    ( w -- )
;    push the data stack to the return stack.

                    @token    tor,'>R',c
                    puld                          ; get the data at TOS
                    std       ,y                  ; save on return stack
                    dey:2                         ; and make room
                    jmp       ?next

;*******************************************************************************
;   r@    ( -- w )
;    copy top of return stack to the data stack.

                    @token    rat,'R@'
                    ldd       2,y                 ; get top value on return stack
                    jmp       pushd               ; and save to data stack

;*******************************************************************************
;   r>    ( -- w )
;    pop the return stack to the data stack.

                    @token    rfrom,'R>'
                    iny:2                         ; count this value
                    ldd       ,y                  ; get top value on return stack
                    jmp       pushd               ; now save it

;*******************************************************************************
;   rp@       ( -- a )
;    push the current rp to the data stack.

                    @token    rpat,'RP@'
                    pshy                          ; save return pointer
                    jmp       ?next

;*******************************************************************************
;   rp!       ( a -- )
;    set the return stack pointer.

                    @token    rpsto,'RP!',c
                    puly                          ; use new return pointer
                    jmp       ?next

;*******************************************************************************
;   sp@       ( -- a )
;    push the current data stack pointer.

                    @token    spat,'SP@'
                    tsx                           ; get current stack pointer
                    dex                           ; adjust for tsx instr
                    pshx                          ; and save on data stack
                    jmp       ?next

;*******************************************************************************
;   sp!       ( a -- )
;    set the data stack pointer.

                    @token    spsto,'SP!'
                    pulx                          ; get new stack pointer
                    inx                           ; prepare for txs
                    txs                           ; and make it count
                    jmp       ?next

;*******************************************************************************
;   dup       ( w -- w w )
;    duplicate the top stack item.

                    @token    dup,'DUP'
                    pulx                          ; get TOS
                    pshx:2                        ; save it back, and a copy
                    jmp       ?next

;*******************************************************************************
;   drop ( w -- )
;    discard top stack item.

                    @token    drop,'DROP'
                    pulx                          ; burn a data item
                    jmp       ?next

;*******************************************************************************
;   swap ( w1 w2 -- w2 w1 )
;    exchange top two stack items.

                    @token    swap,'SWAP'
                    pulx                          ; get top item
                    puld                          ; get second item
                    pshx                          ; save top item
                    jmp       pushd               ; and save second item

;*******************************************************************************
;   over ( w1 w2 -- w1 w2 w1 )
;    copy second stack item to top.

                    @token    over,'OVER'
                    tsx                           ; get the stack pointer
                    ldd       2,x                 ; get second item
                    jmp       pushd               ; and push onto stack

;*******************************************************************************
;   0<    ( n -- t )
;    return true if n is negative.

                    @token    zless,'0<'
                    puld                          ; get TOS
                    tsta                          ; check high bit
                    bmi       zless1              ; branch if negative
                    ldd       #FALSE              ; get the flag
                    jmp       pushd               ; and set it
zless1              ldd       #TRUE               ; get the flag
                    jmp       pushd               ; and set it

;*******************************************************************************
;   and       ( w w -- w )
;    bitwise and.

                    @token    and,'AND'
                    puld                          ; get TOS
                    tsx                           ; get stack pointer
                    anda      ,x                  ; and do the and
                    andb      1,x
                    std       ,x                  ; save back to stack
                    jmp       ?next

;*******************************************************************************
;   or    ( w w -- w )
;    bitwise inclusive or.

                    @token    or,'OR'
                    puld                          ; get TOS
                    tsx                           ; get stack pointer
                    ora       ,x                  ; and do the and
                    orb       1,x
                    std       ,x                  ; save back to stack
                    jmp       ?next

;*******************************************************************************
;   xor       ( w w -- w )
;    bitwise exclusive or.

                    @token    xor,'XOR'
                    puld                          ; get TOS
                    tsx                           ; get stack pointer
                    eora      ,x                  ; and do the and
                    eorb      1,x
                    std       ,x                  ; save back to stack
                    jmp       ?next

;*******************************************************************************
;   um+       ( w w -- w cy )
;    add two numbers, return the sum and carry flag.

                    @token    uplus,'UM+'
                    puld                          ; get TOS
                    tsx                           ; get the stack pointer
                    addd      ,x                  ; and add second item
                    std       ,x                  ; put back on stack
                    ldd       #0                  ; presume false
                    rolb                          ; move carry into word
                    jmp       pushd               ; and save on stack

;*******************************************************************************
;  device dependent i/o
;*******************************************************************************

;*******************************************************************************
;   !io       ( -- )
;    initialize the serial i/o devices.

                    @token    stoio,'!IO'
                    lda       #$30                ; 9600 BAUD
                    sta       BAUD
                    lda       #$00                ; 8-bit xfers
                    sta       SCCR1
                    lda       #$0c
                    sta       SCCR2               ; no interrupts, enable r & t
                    jmp       ?next

;*******************************************************************************
;   ?rx       ( -- c t | f )
;    return input character and true, or a false if no input.

                    @token    qrx,'?RX'
                    clra                          ; assume no char available
                    ldb       SCSR                ; get serial status reg
                    andb      #%00100000          ; check RDRF bit
                    beq       qrx1                ; branch if nothing there
                    ldb       SCDR                ; char; move into B
                    pshb                          ; save char to stack
                    psha                          ; as word
                    ldd       #TRUE               ; get the flag
qrx1                jmp       pushd               ; and save flag

;*******************************************************************************
;   tx!       ( c -- )
;    send character c to the output device.

                    @token    txsto,'TX!'
                    puld                          ; get char from TOS, char is in B
txsto1              lda       SCSR                ; time to send?
                    bpl       txsto1              ; loop until time
                    stb       SCDR                ; write char to SCI
                    jmp       ?next

;*******************************************************************************
;  system and user variables
;*******************************************************************************

;*******************************************************************************
;   dovar     ( -- a )
;    run time routine for variable and create.

                    @token    dovar,'doVAR',c
                    nop
                    jmp       dolst

                    fdb       rfrom,exit

;*******************************************************************************
;   up    ( -- a )
;    pointer to the user area.

                    @token    up,'UP'
                    nop
                    jmp       dolst

                    fdb       dovar
                    fdb       upp

;*******************************************************************************
;   douser    ( -- a )
;    run time routine for user variables.

                    @token    douse,'doUSER',c
                    nop
                    jmp       dolst

                    fdb       rfrom,at,up,at,plus,exit

;*******************************************************************************
;   sp0       ( -- a )
;    pointer to bottom of the data stack.

                    @token    szero,'SP0'
                    nop
                    jmp       dolst

                    fdb       douse,8,exit

;*******************************************************************************
;   rp0       ( -- a )
;    pointer to bottom of the return stack.

                    @token    rzero,'RP0'
                    nop
                    jmp       dolst

                    fdb       douse,10,exit

;*******************************************************************************
;   '?key     ( -- a )
;    execution vector of ?key.

                    @token    tqkey,"'?key"
                    nop
                    jmp       dolst

                    fdb       douse,12,exit

;*******************************************************************************
;   'emit     ( -- a )
;    execution vector of emit.

                    @token    temit,"'emit"
                    nop
                    jmp       dolst

                    fdb       douse,14,exit

;*******************************************************************************
;   'expect   ( -- a )
;    execution vector of expect.

                    @token    texpe,"'expect"
                    nop
                    jmp       dolst

                    fdb       douse,16,exit

;*******************************************************************************
;   'tap ( -- a )
;    execution vector of tap.

                    @token    ttap,"'tap"
                    nop
                    jmp       dolst

                    fdb       douse,18,exit

;*******************************************************************************
;   'echo     ( -- a )
;    execution vector of echo.

                    @token    techo,"'echo"
                    nop
                    jmp       dolst

                    fdb       douse,20,exit

;*******************************************************************************
;   'prompt   ( -- a )
;    execution vector of prompt.

                    @token    tprom,"'prompt"
                    nop
                    jmp       dolst

                    fdb       douse,22,exit

;*******************************************************************************
;   base ( -- a )
;    storage of the radix base for numeric i/o.

                    @token    base,'BASE'
                    nop
                    jmp       dolst

                    fdb       douse,24,exit

;*******************************************************************************
;   tmp       ( -- a )
;    a temporary storage location used in parse and find.

                    @token    temp,'tmp',c
                    nop
                    jmp       dolst

                    fdb       douse,26,exit

;*******************************************************************************
;   span ( -- a )
;    hold character count received by expect.

                    @token    span,'SPAN'
                    nop
                    jmp       dolst

                    fdb       douse,28,exit

;*******************************************************************************
;   >in       ( -- a )
;    hold the character pointer while parsing input stream.

                    @token    inn,'>IN'
                    nop
                    jmp       dolst

                    fdb       douse,30,exit

;*******************************************************************************
;   #tib ( -- a )
;    hold the current count and address of the terminal input buffer.

                    @token    ntib,'#TIB'
                    nop
                    jmp       dolst

                    fdb       douse,32,exit

;*******************************************************************************
;   csp       ( -- a )
;    hold the stack pointer for error checking.

                    @token    csp,'CSP'
                    nop
                    jmp       dolst

                    fdb       douse,36,exit

;*******************************************************************************
;   'eval     ( -- a )
;    execution vector of eval.

                    @token    teval,"'eval"
                    nop
                    jmp       dolst

                    fdb       douse,38,exit

;*******************************************************************************
;   'number   ( -- a )
;    execution vector of number?.

                    @token    tnumb,"'number"
                    nop
                    jmp       dolst

                    fdb       douse,40,exit

;*******************************************************************************
;   hld       ( -- a )
;    hold a pointer in building a numeric output string.

                    @token    hld,'HLD'
                    nop
                    jmp       dolst

                    fdb       douse,42,exit

;*******************************************************************************
;   handler   ( -- a )
;    hold the return stack pointer for error handling.

                    @token    handl,'HANDLER'
                    nop
                    jmp       dolst

                    fdb       douse,44,exit

;*******************************************************************************
;   context   ( -- a )
;    a area to specify vocabulary search order.

                    @token    cntxt,'CONTEXT'
                    nop
                    jmp       dolst

                    fdb       douse,46,exit

;*******************************************************************************
;   current   ( -- a )
;    point to the vocabulary to be extended.

                    @token    crrnt,'CURRENT'
                    nop
                    jmp       dolst

                    fdb       douse,64,exit

;*******************************************************************************
;   cp    ( -- a )
;    point to the top of the code dictionary.

                    @token    cp,'CP'
                    nop
                    jmp       dolst

                    fdb       douse,68,exit

;*******************************************************************************
;   np    ( -- a )
;    point to the bottom of the name dictionary.

                    @token    np,'NP'
                    nop
                    jmp       dolst

                    fdb       douse,70,exit

;*******************************************************************************
;   last ( -- a )
;    point to the last name in the name dictionary.

                    @token    last,'LAST'
                    nop
                    jmp       dolst

                    fdb       douse,72,exit

;*******************************************************************************
;   forth     ( -- a )
;    point to the last name in the name dictionary.

                    @token    vfrth,'forth'
                    nop
                    jmp       dolst

                    fdb       douse,74,exit

;*******************************************************************************
;  WARNING: Next available user area offset is 78.
;*******************************************************************************

;*******************************************************************************
;  common functions
;*******************************************************************************

;*******************************************************************************
;   forth     ( -- )
;    make forth the context vocabulary.

                    @token    forth,'FORTH'
                    nop
                    jmp       dolst

                    fdb       vfrth,cntxt,store,exit

;*******************************************************************************
;   ?dup ( w -- w w | 0 )
;    dup tos if its is not zero.

                    @token    qdup,'?DUP'
                    nop
                    jmp       dolst

                    fdb       dup
                    fdb       qbran,qdup1
                    fdb       dup
qdup1               fdb       exit

;*******************************************************************************
;   rot       ( w1 w2 w3 -- w2 w3 w1 )
;    rot 3rd item to top.

                    @token    rot,'ROT'
                    nop
                    jmp       dolst

                    fdb       tor,swap,rfrom,swap,exit

;*******************************************************************************
;   2drop     ( w w -- )
;    discard two items on stack.

                    @token    ddrop,'2DROP'
                    nop
                    jmp       dolst

                    fdb       drop,drop,exit

;*******************************************************************************
;   2dup ( w1 w2 -- w1 w2 w1 w2 )
;    duplicate top two items.

                    @token    ddup,'2DUP'
                    nop
                    jmp       dolst

                    fdb       over,over,exit

;*******************************************************************************
;   +     ( w w -- sum )
;    add top two items.

                    @token    plus,'+'
                    nop
                    jmp       dolst

                    fdb       uplus,drop,exit

;*******************************************************************************
;   d+    ( d d -- d )
;    double addition, as an example using um+.

                    @token    dplus,'D+'
                    nop
                    jmp       dolst

                    fdb       tor,swap,tor,uplus
                    fdb       rfrom,rfrom,plus,plus,exit

;*******************************************************************************
;   not       ( w -- w )
;    one's complement of tos.

                    @token    inver,'NOT'
                    nop
                    jmp       dolst

                    fdb       dolit,-1,xor,exit

;*******************************************************************************
;   negate    ( n -- -n )
;    two's complement of tos.

                    @token    negat,'NEGATE'
                    nop
                    jmp       dolst

                    fdb       inver,dolit,1,plus,exit

;*******************************************************************************
;   dnegate   ( d -- -d )
;    two's complement of top double.

                    @token    dnega,'DNEGATE'
                    nop
                    jmp       dolst

                    fdb       inver,tor,inver
                    fdb       dolit,1,uplus
                    fdb       rfrom,plus,exit

;*******************************************************************************
;   -     ( n1 n2 -- n1-n2 )
;    subtraction.

                    @token    sub,'-'
                    nop
                    jmp       dolst

                    fdb       negat,plus,exit

;*******************************************************************************
;   abs       ( n -- n )
;    return the absolute value of n.

                    @token    abs,'ABS'
                    nop
                    jmp       dolst

                    fdb       dup,zless
                    fdb       qbran,abs1
                    fdb       negat
abs1:
                    fdb       exit

;*******************************************************************************
;   =     ( w w -- t )
;    return true if top two are equal.

                    @token    equal,'='
                    nop
                    jmp       dolst

                    fdb       xor
                    fdb       qbran,equ1
                    fdb       dolit,FALSE,exit    ; false flag
equ1:
                    fdb       dolit,TRUE,exit     ; true flag

;*******************************************************************************
;   u<    ( u u -- t )
;    unsigned compare of top two items.

                    @token    uless,'U<'
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless
                    fdb       qbran,ules1
                    fdb       swap,drop,zless,exit
ules1               fdb       sub,zless,exit

;*******************************************************************************
;   <     ( n1 n2 -- t )
;    signed compare of top two items.

                    @token    less,'<'
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless
                    fdb       qbran,less1
                    fdb       drop,zless,exit
less1               fdb       sub,zless,exit

;*******************************************************************************
;   max       ( n n -- n )
;    return the greater of two top stack items.

                    @token    max,'MAX'
                    nop
                    jmp       dolst

                    fdb       ddup,less
                    fdb       qbran,max1
                    fdb       swap
max1                fdb       drop,exit

;*******************************************************************************
;   min       ( n n -- n )
;    return the smaller of top two stack items.

                    @token    min,'MIN'
                    nop
                    jmp       dolst

                    fdb       ddup,swap,less
                    fdb       qbran,min1
                    fdb       swap
min1                fdb       drop,exit

;*******************************************************************************
;   within    ( u ul uh -- t )
;    return true if u is within the range of ul and uh.

                    @token    withi,'WITHIN'
                    nop
                    jmp       dolst

                    fdb       over,sub,tor        ; ul <= u < uh
                    fdb       sub,rfrom,uless,exit

;*******************************************************************************
;  divide
;*******************************************************************************

;*******************************************************************************
;   um/mod    ( udl udh u -- ur uq )
;    unsigned divide of a double by a single. return mod and quotient.

                    @token    ummod,'UM/MOD'
                    nop
                    jmp       dolst

                    fdb       ddup,uless
                    fdb       qbran,umm4
                    fdb       negat,dolit,15,tor
umm1                fdb       tor,dup,uplus
                    fdb       tor,tor,dup,uplus
                    fdb       rfrom,plus,dup
                    fdb       rfrom,rat,swap,tor
                    fdb       uplus,rfrom,or
                    fdb       qbran,umm2
                    fdb       tor,drop,dolit,1,plus,rfrom
                    fdb       bran,umm3
umm2                fdb       drop
umm3                fdb       rfrom
                    fdb       donxt,umm1
                    fdb       drop,swap,exit
umm4                fdb       drop,ddrop
                    fdb       dolit,-1,dup,exit   ; overflow, return max

;*******************************************************************************
;   m/mod     ( d n -- r q )
;    signed floored divide of double by single. return mod and quotient.

                    @token    msmod,'M/MOD'
                    nop
                    jmp       dolst

                    fdb       dup,zless,dup,tor
                    fdb       qbran,mmod1
                    fdb       negat,tor,dnega,rfrom
mmod1               fdb       tor,dup,zless
                    fdb       qbran,mmod2
                    fdb       rat,plus
mmod2               fdb       rfrom,ummod,rfrom
                    fdb       qbran,mmod3
                    fdb       swap,negat,swap
mmod3               fdb       exit

;*******************************************************************************
;   /mod ( n n -- r q )
;    signed divide. return mod and quotient.

                    @token    slmod,'/MOD'
                    nop
                    jmp       dolst

                    fdb       over,zless,swap,msmod,exit

;*******************************************************************************
;   mod       ( n n -- r )
;    signed divide. return mod only.

                    @token    mod,'MOD'
                    nop
                    jmp       dolst

                    fdb       slmod,drop,exit

;*******************************************************************************
;   /     ( n n -- q )
;    signed divide. return quotient only.

                    @token    slash,'/'
                    nop
                    jmp       dolst

                    fdb       slmod,swap,drop,exit

;*******************************************************************************
;  multiply
;*******************************************************************************

;*******************************************************************************
;   um*       ( u u -- ud )
;    unsigned multiply. return double product.

                    @token    umsta,'UM*'
                    nop
                    jmp       dolst

                    fdb       dolit,0,swap,dolit,15,tor
umst1               fdb       dup,uplus,tor,tor
                    fdb       dup,uplus,rfrom,plus,rfrom
                    fdb       qbran,umst2
                    fdb       tor,over,uplus,rfrom,plus
umst2               fdb       donxt,umst1
                    fdb       rot,drop,exit

;*******************************************************************************
;   *     ( n n -- n )
;    signed multiply. return single product.

                    @token    star,'*'
                    nop
                    jmp       dolst

                    fdb       umsta,drop,exit

;*******************************************************************************
;   m*    ( n n -- d )
;    signed multiply. return double product.

                    @token    mstar,'M*'
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless,tor
                    fdb       abs,swap,abs,umsta
                    fdb       rfrom
                    fdb       qbran,msta1
                    fdb       dnega
msta1               fdb       exit

;*******************************************************************************
;   */mod     ( n1 n2 n3 -- r q )
;    multiply n1 and n2, then divide by n3. return mod and quotient.

                    @token    ssmod,'*/MOD'
                    nop
                    jmp       dolst

                    fdb       tor,mstar,rfrom,msmod,exit

;*******************************************************************************
;   */    ( n1 n2 n3 -- q )
;    multiply n1 by n2, then divide by n3. return quotient only.

                    @token    stasl,'*/'
                    nop
                    jmp       dolst

                    fdb       ssmod,swap,drop,exit

;*******************************************************************************
;  miscellaneous
;*******************************************************************************

;*******************************************************************************
;   cell+     ( a -- a )
;    add cell size in byte to address.

                    @token    cellp,'CELL+'
                    nop
                    jmp       dolst

                    fdb       dolit,celll,plus,exit

;*******************************************************************************
;   cell-     ( a -- a )
;    subtract cell size in byte from address.

                    @token    cellm,'CELL-'
                    nop
                    jmp       dolst

                    fdb       dolit,0-celll,plus,exit

;*******************************************************************************
;   cells     ( n -- n )
;    multiply tos by cell size in bytes.

                    @token    cells,'CELLS'
                    nop
                    jmp       dolst

                    fdb       dolit,celll,star,exit

;*******************************************************************************
;   aligned   ( b -- a )
;    align address to the cell boundary.

                    @token    algnd,'ALIGNED'
                    nop
                    jmp       dolst

                    fdb       dup,dolit,0,dolit,celll
                    fdb       ummod,drop,dup
                    fdb       qbran,algn1
                    fdb       dolit,celll,swap,sub
algn1               fdb       plus,exit

;*******************************************************************************
;   bl    ( -- 32 )
;    return 32, the blank character.

                    @token    blank,'BL'
                    nop
                    jmp       dolst

                    fdb       dolit,$20,exit      ; literal space

;*******************************************************************************
;   >char     ( c -- c )
;    filter non-printing characters.

                    @token    tchar,'>CHAR'
                    nop
                    jmp       dolst

                    fdb       dolit,$7F,and,dup   ; mask msb
                    fdb       dolit,127,blank,withi  ; check for printable
                    fdb       qbran,tcha1         ; branch if printable
                    fdb       drop,dolit,$5f      ; literal underscore
tcha1               fdb       exit

;*******************************************************************************
;   depth     ( -- n )
;    return the depth of the data stack.

                    @token    depth,'DEPTH'
                    nop
                    jmp       dolst

                    fdb       spat,szero,at,swap,sub
                    fdb       dolit,celll,slash,exit

;*******************************************************************************
;   pick ( ... +n -- ... w )
;    copy the nth stack item to tos.

                    @token    pick,'PICK'
                    nop
                    jmp       dolst

                    fdb       dolit,1,plus,cells
                    fdb       dolit,1,plus
                    fdb       spat,plus,at,exit

;*******************************************************************************
;  memory access
;*******************************************************************************

;*******************************************************************************
;   +!    ( n a -- )
;    add n to the contents at address a.

                    @token    pstor,'+!'
                    nop
                    jmp       dolst

                    fdb       swap,over,at,plus
                    fdb       swap,store,exit

;*******************************************************************************
;   2!    ( d a -- )
;    store the double integer to address a.

                    @token    dstor,'2!'
                    nop
                    jmp       dolst

                    fdb       swap,over,store
                    fdb       cellp,store,exit

;*******************************************************************************
;   2@    ( a -- d )
;    fetch double integer from address a.

                    @token    dat,'2@'
                    nop
                    jmp       dolst

                    fdb       dup,cellp,at
                    fdb       swap,at,exit

;*******************************************************************************
;   count     ( b -- b +n )
;    return count byte of a string and add 1 to byte address.

                    @token    count,'COUNT'
                    nop
                    jmp       dolst

                    fdb       dup,dolit,1,plus
                    fdb       swap,cat,exit

;*******************************************************************************
;   here ( -- a )
;    return the top of the code dictionary.

                    @token    here,'HERE'
                    nop
                    jmp       dolst

                    fdb       cp,at,exit

;*******************************************************************************
;   pad       ( -- a )
;    return the address of a temporary buffer.

                    @token    pad,'PAD'
                    nop
                    jmp       dolst

                    fdb       here,dolit,80,plus,exit

;*******************************************************************************
;   tib       ( -- a )
;    return the address of the terminal input buffer.

                    @token    tib,'TIB'
                    nop
                    jmp       dolst

                    fdb       ntib,cellp,at,exit

;*******************************************************************************
;   @execute  ( a -- )
;    execute vector stored in address a.

                    @token    atexe,'@EXECUTE'
                    nop
                    jmp       dolst

                    fdb       at,qdup             ; ?address or zero
                    fdb       qbran,exe1
                    fdb       execu               ; execute if non-zero
exe1                fdb       exit                ; do nothing if zero

;*******************************************************************************
;   cmove     ( b1 b2 u -- )
;    copy u bytes from b1 to b2.

                    @token    cmove,'CMOVE'
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,cmov2
cmov1               fdb       tor,dup,cat
                    fdb       rat,cstor
                    fdb       dolit,1,plus
                    fdb       rfrom,dolit,1,plus
cmov2               fdb       donxt,cmov1
                    fdb       ddrop,exit

;*******************************************************************************
;   fill ( b u c -- )
;    fill u bytes of character c to area beginning at b.

                    @token    fill,'FILL'
                    nop
                    jmp       dolst

                    fdb       swap,tor,swap
                    fdb       bran,fill2
fill1               fdb       ddup,cstor,dolit,1,plus
fill2               fdb       donxt,fill1
                    fdb       ddrop,exit

;*******************************************************************************
;   -trailing ( b u -- b u )
;    adjust the count to eliminate trailing white space.

                    @token    dtrai,'-TRAILING'
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,dtra2
dtra1               fdb       blank,over,rat,plus,cat,less
                    fdb       qbran,dtra2
                    fdb       rfrom,dolit,1,plus,exit  ; adjusted count
dtra2               fdb       donxt,dtra1
                    fdb       dolit,0,exit        ; count=0

;*******************************************************************************
;   pack$     ( b u a -- a )
;    build a counted string with u characters from b. null fill.

                    @token    packs,'PACK$'
                    nop
                    jmp       dolst

                    fdb       algnd,dup,tor       ; strings only on cell boundary
                    fdb       over,dup,dolit,0
                    fdb       dolit,celll,ummod,drop  ; count mod cell
                    fdb       sub,over,plus
                    fdb       dolit,0,swap,store  ; null fill cell
                    fdb       ddup,cstor,dolit,1,plus  ; save count
                    fdb       swap,cmove,rfrom,exit  ; move string

;*******************************************************************************
;  numeric output, single precision
;*******************************************************************************

;*******************************************************************************
;   digit     ( u -- c )
;    convert digit u to a character.

                    @token    digit,'DIGIT'
                    nop
                    jmp       dolst

                    fdb       dolit,9,over,less
                    fdb       dolit,7,and,plus
                    fdb       dolit,$30,plus,exit  ; literal 0

;*******************************************************************************
;   extract   ( n base -- n c )
;    extract the least significant digit from n.

                    @token    extrc,'EXTRACT'
                    nop
                    jmp       dolst

                    fdb       dolit,0,swap,ummod
                    fdb       swap,digit,exit

;*******************************************************************************
;   <#    ( -- )
;    initiate the numeric output process.

                    @token    bdigs,'<#'
                    nop
                    jmp       dolst

                    fdb       pad,hld,store,exit

;*******************************************************************************
;   hold ( c -- )
;    insert a character into the numeric output string.

                    @token    hold,'HOLD'
                    nop
                    jmp       dolst

                    fdb       hld,at,dolit,1,sub
                    fdb       dup,hld,store,cstor,exit

;*******************************************************************************
;   #     ( u -- u )
;    extract one digit from u and append the digit to output string.

                    @token    dig,'#'
                    nop
                    jmp       dolst

                    fdb       base,at,extrc,hold,exit

;*******************************************************************************
;   #s    ( u -- 0 )
;    convert u until all digits are added to the output string.

                    @token    digs,'#S'
                    nop
                    jmp       dolst

digs1               fdb       dig,dup
                    fdb       qbran,digs2
                    fdb       bran,digs1
digs2               fdb       exit

;*******************************************************************************
;   sign ( n -- )
;    add a minus sign to the numeric output string.

                    @token    sign,'SIGN'
                    nop
                    jmp       dolst

                    fdb       zless
                    fdb       qbran,sign1
                    fdb       dolit,$2d,hold      ; literal minus sign
sign1               fdb       exit

;*******************************************************************************
;   #>    ( w -- b u )
;    prepare the output string to be type'd.

                    @token    edigs,'#>'
                    nop
                    jmp       dolst

                    fdb       drop,hld,at
                    fdb       pad,over,sub,exit

;*******************************************************************************
;   str       ( n -- b u )
;    convert a signed integer to a numeric string.

                    @token    str,'str'
                    nop
                    jmp       dolst

                    fdb       dup,tor,abs
                    fdb       bdigs,digs,rfrom
                    fdb       sign,edigs,exit

;*******************************************************************************
;   hex       ( -- )
;    use radix 16 as base for numeric conversions.

                    @token    hex,'HEX'
                    nop
                    jmp       dolst

                    fdb       dolit,16,base,store,exit

;*******************************************************************************
;   decimal   ( -- )
;    use radix 10 as base for numeric conversions.

                    @token    decim,'DECIMAL'
                    nop
                    jmp       dolst

                    fdb       dolit,10,base,store,exit

;*******************************************************************************
;  numeric input, single precision
;*******************************************************************************

;*******************************************************************************
;   digit?    ( c base -- u t )
;    convert a character to its numeric value. a flag indicates success.

                    @token    digtq,'DIGIT?'
                    nop
                    jmp       dolst

                    fdb       tor,dolit,$30,sub   ; literal 0
                    fdb       dolit,9,over,less
                    fdb       qbran,dgtq1
                    fdb       dolit,7,sub
                    fdb       dup,dolit,10,less,or
dgtq1:
                    fdb       dup,rfrom,uless,exit

;*******************************************************************************
;   number?   ( a -- n t | a f )
;    convert a number string to integer. push a flag on tos.

                    @token    numbq,'NUMBER?'
                    nop
                    jmp       dolst

                    fdb       base,at,tor,dolit,0,over,count
                    fdb       over,cat,dolit,$24,equal  ; literal $
                    fdb       qbran,numq1
                    fdb       hex,swap,dolit,1,plus
                    fdb       swap,dolit,1,sub
numq1               fdb       over,cat,dolit,$2d,equal,tor  ; literal minus sign
                    fdb       swap,rat,sub,swap,rat,plus,qdup
                    fdb       qbran,numq6
                    fdb       dolit,1,sub,tor
numq2               fdb       dup,tor,cat,base,at,digtq
                    fdb       qbran,numq4
                    fdb       swap,base,at,star,plus,rfrom
                    fdb       dolit,1,plus
                    fdb       donxt,numq2
                    fdb       rat,swap,drop
                    fdb       qbran,numq3
                    fdb       negat
numq3               fdb       swap
                    fdb       bran,numq5
numq4               fdb       rfrom,rfrom,ddrop,ddrop,dolit,0
numq5               fdb       dup
numq6               fdb       rfrom,ddrop
                    fdb       rfrom,base,store,exit

;*******************************************************************************
;  basic i/o
;*******************************************************************************

;*******************************************************************************
;   ?key ( -- c t | f )
;    return input character and true, or a false if no input.

                    @token    qkey,'?KEY'
                    nop
                    jmp       dolst

                    fdb       tqkey,atexe,exit

;*******************************************************************************
;   key       ( -- c )
;    wait for and return an input character.

                    @token    key,'KEY'
                    nop
                    jmp       dolst

key1                fdb       qkey
                    fdb       qbran,key1
                    fdb       exit

;*******************************************************************************
;   emit ( c -- )
;    send a character to the output device.

                    @token    emit,'EMIT'
                    nop
                    jmp       dolst

                    fdb       temit,atexe,exit

;*******************************************************************************
;   nuf? ( -- t )
;    return false if no input, else pause and if cr return true.

                    @token    nufq,'NUF?'
                    nop
                    jmp       dolst

                    fdb       qkey,dup
                    fdb       qbran,nufq1
                    fdb       ddrop,key,dolit,CR,equal
nufq1               fdb       exit

;*******************************************************************************
;   pace ( -- )
;    send a pace character for the file downloading process.

                    @token    pace,'PACE'
                    nop
                    jmp       dolst

                    fdb       dolit,11,emit,exit

;*******************************************************************************
;   space     ( -- )
;    send the blank character to the output device.

                    @token    space,'SPACE'
                    nop
                    jmp       dolst

                    fdb       blank,emit,exit

;*******************************************************************************
;   spaces    ( +n -- )
;    send n spaces to the output device.

                    @token    spacs,'SPACES'
                    nop
                    jmp       dolst

                    fdb       dolit,0,max,tor
                    fdb       bran,char2
char1               fdb       space
char2               fdb       donxt,char1
                    fdb       exit

;*******************************************************************************
;   type ( b u -- )
;    output u characters from b.

                    @token    typee,'TYPE'
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,type2
type1               fdb       dup,cat,emit
                    fdb       dolit,1,plus
type2               fdb       donxt,type1
                    fdb       drop,exit

;*******************************************************************************
;   cr    ( -- )
;    output a carriage return and a line feed.

                    @token    crr,'CR'
                    nop
                    jmp       dolst

                    fdb       dolit,CR,emit
                    fdb       dolit,LF,emit,exit

;*******************************************************************************
;   do$       ( -- a )
;    return the address of a compiled string.

                    @token    dostr,'do$',c
                    nop
                    jmp       dolst

                    fdb       rfrom,rat,rfrom,count,plus
                    fdb       algnd,tor,swap,tor,exit

;*******************************************************************************
;   $"|       ( -- a )
;    run time routine compiled by $". return address of a compiled string.

                    @token    strqp,'$"|',c
                    nop
                    jmp       dolst

                    fdb       dostr,exit          ; force a call to do$

;*******************************************************************************
;   ."|       ( -- )
;    run time routine of ." . output a compiled string.

                    @token    dotqp,'."|',c
                    nop
                    jmp       dolst

                    fdb       dostr,count,typee,exit

;*******************************************************************************
;   .r    ( n +n -- )
;    display an integer in a field of n columns, right justified.

                    @token    dotr,'.R'
                    nop
                    jmp       dolst

                    fdb       tor,str,rfrom,over,sub
                    fdb       spacs,typee,exit

;*******************************************************************************
;   u.r       ( u +n -- )
;    display an unsigned integer in n column, right justified.

                    @token    udotr,'U.R'
                    nop
                    jmp       dolst

                    fdb       tor,bdigs,digs,edigs
                    fdb       rfrom,over,sub
                    fdb       spacs,typee,exit

;*******************************************************************************
;   u.    ( u -- )
;    display an unsigned integer in free format.

                    @token    udot,'U.'
                    nop
                    jmp       dolst

                    fdb       bdigs,digs,edigs
                    fdb       space,typee,exit

;*******************************************************************************
;   .     ( w -- )
;    display an integer in free format, preceeded by a space.

                    @token    dot,'.'
                    nop
                    jmp       dolst

                    fdb       base,at,dolit,10,xor ; ?decimal
                    fdb       qbran,dot1
                    fdb       udot,exit           ; no, display unsigned
dot1                fdb       str,space,typee,exit  ; yes, display signed

;*******************************************************************************
;   ?     ( a -- )
;    display the contents in a memory cell.

                    @token    quest,'?'
                    nop
                    jmp       dolst

                    fdb       at,dot,exit

;*******************************************************************************
;  parsing
;*******************************************************************************

;*******************************************************************************
;   parse     ( b u c -- b u delta ; <string> )
;    scan string delimited by c. return found string and its offset.

                    @token    pars,'parse'
                    nop
                    jmp       dolst

                    fdb       temp,store,over,tor,dup
                    fdb       qbran,pars8
                    fdb       dolit,1,sub,temp,at,blank,equal
                    fdb       qbran,pars3
                    fdb       tor
pars1               fdb       blank,over,cat      ; skip leading blanks only
                    fdb       sub,zless,inver
                    fdb       qbran,pars2
                    fdb       dolit,1,plus
                    fdb       donxt,pars1
                    fdb       rfrom,drop,dolit,0,dup,exit
pars2               fdb       rfrom
pars3               fdb       over,swap
                    fdb       tor
pars4               fdb       temp,at,over,cat,sub  ; scan for delimiter
                    fdb       temp,at,blank,equal
                    fdb       qbran,pars5
                    fdb       zless
pars5               fdb       qbran,pars6
                    fdb       dolit,1,plus
                    fdb       donxt,pars4
                    fdb       dup,tor
                    fdb       bran,pars7
pars6               fdb       rfrom,drop,dup
                    fdb       dolit,1,plus,tor
pars7               fdb       over,sub
                    fdb       rfrom,rfrom,sub,exit
pars8               fdb       over,rfrom,sub,exit

;*******************************************************************************
;   parse     ( c -- b u ; <string> )
;    scan input stream and return counted string delimited by c.

                    @token    parse,'PARSE'
                    nop
                    jmp       dolst

                    fdb       tor,tib,inn,at,plus  ; current input buffer pointer
                    fdb       ntib,at,inn,at,sub  ; remaining count
                    fdb       rfrom,pars,inn,pstor,exit

;*******************************************************************************
;   .(    ( -- )
;    output following string up to next ) .

                    @token    dotpr,'.(',i
                    nop
                    jmp       dolst

                    fdb       dolit,$29,parse,typee,exit  ; literal )

;*******************************************************************************
;   (     ( -- )
;    ignore following string up to next ) . a comment.

                    @token    paren,'(',i
                    nop
                    jmp       dolst

                    fdb       dolit,$29,parse,ddrop,exit  ; literal )

;*******************************************************************************
;   \     ( -- )
;    ignore following text till the end of line.

                    @token    bksla,'\',i
                    nop
                    jmp       dolst

                    fdb       ntib,at,inn,store,exit

;*******************************************************************************
;   char ( -- c )
;    parse next word and return its first character.

                    @token    char,'CHAR'
                    nop
                    jmp       dolst

                    fdb       blank,parse,drop,cat,exit

;*******************************************************************************
;   token     ( -- a ; <string> )
;    parse a word from input stream and copy it to name dictionary.

                    @token    token,'TOKEN'
                    nop
                    jmp       dolst

                    fdb       blank,parse,dolit,31,min
                    fdb       np,at,over,sub,cellm
                    fdb       packs,exit

;*******************************************************************************
;   word ( c -- a ; <string> )
;    parse a word from input stream and copy it to code dictionary.

                    @token    word,'WORD'
                    nop
                    jmp       dolst

                    fdb       parse,here,packs,exit

;*******************************************************************************
;  dictionary search
;*******************************************************************************

;*******************************************************************************
;   name>     ( na -- ca )
;    return a code address given a name address.

                    @token    namet,'NAME>'
                    nop
                    jmp       dolst

                    fdb       cellm,cellm,at,exit

;*******************************************************************************
;   same?     ( a a u -- a a f \ -0+ )
;    compare u cells in two strings. return 0 if identical.

                    @token    sameq,'SAME?'
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,same2
same1               fdb       over,rat,cells,plus,at
                    fdb       over,rat,cells,plus,at
                    fdb       sub,qdup
                    fdb       qbran,same2
                    fdb       rfrom,drop,exit     ; strings not equal
same2               fdb       donxt,same1
                    fdb       dolit,0,exit        ; strings equal

;*******************************************************************************
;   find ( a va -- ca na | a f )
;    search a vocabulary for a string. return ca and na if succeeded.

                    @token    find,'find'
                    nop
                    jmp       dolst

                    fdb       swap,dup,cat
                    fdb       dolit,celll,slash,temp,store
                    fdb       dup,at,tor,cellp,swap
find1               fdb       at,dup
                    fdb       qbran,find6
                    fdb       dup,at,dolit,maskk,and,rat,xor
                    fdb       qbran,find2
                    fdb       cellp,dolit,TRUE    ; true flag
                    fdb       bran,find3
find2               fdb       cellp,temp,at,sameq
find3               fdb       bran,find4
find6               fdb       rfrom,drop
                    fdb       swap,cellm,swap,exit
find4               fdb       qbran,find5
                    fdb       cellm,cellm
                    fdb       bran,find1
find5               fdb       rfrom,drop,swap,drop
                    fdb       cellm
                    fdb       dup,namet,swap,exit

;*******************************************************************************
;   name?     ( a -- ca na | a f )
;    search all context vocabularies for a string.

                    @token    nameq,'NAME?'
                    nop
                    jmp       dolst

                    fdb       cntxt,dup,dat,xor   ; ?context=also
                    fdb       qbran,namq1
                    fdb       cellm               ; no, start with context
namq1               fdb       tor
namq2               fdb       rfrom,cellp,dup,tor  ; next in search order
                    fdb       at,qdup
                    fdb       qbran,namq3
                    fdb       find,qdup           ; search vocabulary
                    fdb       qbran,namq2
                    fdb       rfrom,drop,exit     ; found name
namq3               fdb       rfrom,drop          ; name not found
                    fdb       dolit,FALSE,exit    ; false flag

;*******************************************************************************
;  terminal response
;*******************************************************************************

;*******************************************************************************
;   ^h    ( bot eot cur -- bot eot cur )
;    backup the cursor by one character.

                    @token    bksp,'^H'
                    nop
                    jmp       dolst

                    fdb       tor,over,rfrom,swap,over,xor
                    fdb       qbran,back1
                    fdb       dolit,BS,techo,atexe,dolit,1,sub
                    fdb       blank,techo,atexe
                    fdb       dolit,BS,techo,atexe
back1               fdb       exit

;*******************************************************************************
;   tap       ( bot eot cur c -- bot eot cur )
;    accept and echo the key stroke and bump the cursor.

                    @token    tap,'TAP'
                    nop
                    jmp       dolst

                    fdb       dup,techo,atexe
                    fdb       over,cstor,dolit,1,plus,exit

;*******************************************************************************
;   ktap ( bot eot cur c -- bot eot cur )
;    process a key stroke, CR or backspace.

                    @token    ktap,'kTAP'
                    nop
                    jmp       dolst

                    fdb       dup,dolit,CR,xor
                    fdb       qbran,ktap2
                    fdb       dolit,BS,xor
                    fdb       qbran,ktap1
                    fdb       blank,tap,exit
ktap1               fdb       bksp,exit
ktap2               fdb       drop,swap,drop,dup,exit

;*******************************************************************************
;   accept    ( b u -- b u )
;    accept characters to input buffer. return with actual count.

                    @token    accep,'accept'
                    nop
                    jmp       dolst

                    fdb       over,plus,over
accp1               fdb       ddup,xor
                    fdb       qbran,accp4
                    fdb       key,dup
;                   fdb       blank,sub,dolit,95,uless
                    fdb       blank,dolit,127,withi
                    fdb       qbran,accp2
                    fdb       tap
                    fdb       bran,accp3
accp2               fdb       ttap,atexe
accp3               fdb       bran,accp1
accp4               fdb       drop,over,sub,exit

;*******************************************************************************
;   expect    ( b u -- )
;    accept input stream and store count in span.

                    @token    expec,'EXPECT'
                    nop
                    jmp       dolst

                    fdb       texpe,atexe,span,store,drop,exit

;*******************************************************************************
;   query     ( -- )
;    accept input stream to terminal input buffer.

                    @token    query,'QUERY'
                    nop
                    jmp       dolst

                    fdb       tib,dolit,80,texpe,atexe,ntib,store
                    fdb       drop,dolit,0,inn,store,exit

;*******************************************************************************
;  error handling
;*******************************************************************************

;*******************************************************************************
;   catch     ( ca -- 0 | err# )
;    execute word at ca and set up an error frame for it.

                    @token    catch,'CATCH'
                    nop
                    jmp       dolst

                    fdb       spat,tor,handl,at,tor  ; save error frame
                    fdb       rpat,handl,store,execu  ; execute
                    fdb       rfrom,handl,store   ; restore error frame
                    fdb       rfrom,drop,dolit,0,exit  ; no error

;*******************************************************************************
;   throw     ( err# -- err# )
;    reset system to current local error frame an update error flag.

                    @token    throw,'THROW'
                    nop
                    jmp       dolst

                    fdb       handl,at,rpsto      ; restore return stack
                    fdb       rfrom,handl,store   ; restore handler frame
                    fdb       rfrom,swap,tor,spsto  ; restore data stack
                    fdb       drop,rfrom,exit

;*******************************************************************************
;   null$     ( -- a )
;    return address of a null string with zero count.

                    @token    nulls,'NULL$'
                    nop
                    jmp       dolst

                    fdb       dovar               ; emulate create
                    fdb       0
                    fcb       99,111,121,111,116,101

;*******************************************************************************
;   abort     ( -- )
;    reset data stack and jump to quit.

                    @token    abort,'ABORT'
                    nop
                    jmp       dolst

                    fdb       nulls,throw

;*******************************************************************************
;   abort"    ( f -- )
;    run time routine of abort" . abort with a message.

                    @token    aborq,'abort"',c
                    nop
                    jmp       dolst

                    fdb       qbran,abor1         ; text flag
                    fdb       dostr,throw         ; pass error string
abor1               fdb       dostr,drop,exit     ; drop error

;*******************************************************************************
;  the text interpreter
;*******************************************************************************

;*******************************************************************************
;   $interpret ( a -- )
;    interpret a word. if failed, try to convert it to an integer.

                    @token    inter,'$INTERPRET'
                    nop
                    jmp       dolst

                    fdb       nameq,qdup          ; ?defined
                    fdb       qbran,inte1
;                   fdb       at,dolit,compo,and  ; ?compile only lexicon bits
                    fdb       cat,dolit,compo,and ; ?compile only lexicon bits
                    fdb       aborq
                    fcb       13
                    fcc       ' compile only'
                    fdb       execu,exit          ; execute defined word
inte1               fdb       tnumb,atexe         ; convert a number
                    fdb       qbran,inte2
                    fdb       exit
inte2               fdb       throw               ; error

;*******************************************************************************
;   [     ( -- )
;    start the text interpreter.

                    @token    lbrac,'[',i
                    nop
                    jmp       dolst

                    fdb       dolit,inter,teval,store,exit

;*******************************************************************************
;   .ok       ( -- )
;    display 'ok' only while interpreting.

                    @token    dotok,'.OK'
                    nop
                    jmp       dolst

                    fdb       dolit,inter,teval,at,equal
                    fdb       qbran,doto1
                    fdb       dotqp
                    fcb       3
                    fcc       ' ok'
doto1               fdb       crr,exit

;*******************************************************************************
;   ?stack    ( -- )
;    abort if the data stack underflows.

                    @token    qstac,'?STACK'
                    nop
                    jmp       dolst

                    fdb       depth,zless         ; check only for underflow
                    fdb       aborq
                    fcb       10
                    fcc       ' underflow '       ; extra space for align
                    fdb       exit

;*******************************************************************************
;   eval ( -- )
;    interpret the input stream.

                    @token    eval,'EVAL'
                    nop
                    jmp       dolst

eval1               fdb       token,dup,cat       ; ?input stream empty
                    fdb       qbran,eval2
                    fdb       teval,atexe,qstac   ; evaluate input, check stack
                    fdb       bran,eval1
eval2               fdb       drop,tprom,atexe,exit  ; prompt

;*******************************************************************************
;  shell
;*******************************************************************************

;*******************************************************************************
;   preset    ( -- )
;    reset data stack pointer and the terminal input buffer.

                    @token    prese,'PRESET'
                    nop
                    jmp       dolst

                    fdb       szero,at,spsto
                    fdb       dolit,tibb,ntib,cellp,store,exit

;*******************************************************************************
;   xio       ( a a a -- )
;    reset the i/o vectors 'expect, 'tap, 'echo and 'prompt.

                    @token    xio,'xio',c
                    nop
                    jmp       dolst

                    fdb       dolit,accep,texpe,dstor
                    fdb       techo,dstor,exit

;*******************************************************************************
;   file ( -- )
;    select i/o vectors for file download.

                    @token    file,'FILE'
                    nop
                    jmp       dolst

                    fdb       dolit,pace,dolit,drop
                    fdb       dolit,ktap,xio,exit

;*******************************************************************************
;   hand ( -- )
;    select i/o vectors for terminal interface.

                    @token    hand,'HAND'
                    nop
                    jmp       dolst

                    fdb       dolit,dotok,dolit,emit
                    fdb       dolit,ktap,xio,exit

;*******************************************************************************
;   i/o       ( -- a )
;    array to store default i/o vectors.

                    @token    islo,'I/O'
                    nop
                    jmp       dolst

                    fdb       dovar               ; emulate create
                    fdb       qrx,txsto           ; default i/o vectors

;*******************************************************************************
;   console   ( -- )
;    initiate terminal interface.

                    @token    conso,'CONSOLE'
                    nop
                    jmp       dolst

                    fdb       islo,dat,tqkey,dstor  ; restore default i/o device
                    fdb       hand,exit           ; keyboard input

;*******************************************************************************
;   quit ( -- )
;    reset return stack pointer and start text interpreter.

                    @token    quit,'QUIT'
                    nop
                    jmp       dolst

                    fdb       rzero,at,rpsto      ; reset return stack pointer
quit1               fdb       lbrac               ; start interpretation
quit2               fdb       query               ; get input
                    fdb       dolit,eval,catch,qdup  ; evaluate input
                    fdb       qbran,quit2         ; continue till error
                    fdb       tprom,at,swap       ; save input device
                    fdb       conso,nulls,over,xor  ; ?display error message
                    fdb       qbran,quit3
                    fdb       space,count,typee   ; error message
                    fdb       dotqp
                    fcb       3
                    fcc       ' ? '               ; error prompt
quit3               fdb       dolit,dotok,xor     ; ?file input
                    fdb       qbran,quit4
                    fdb       dolit,err,emit      ; file error, tell host
quit4               fdb       prese               ; some cleanup
                    fdb       bran,quit1

;*******************************************************************************
; The compiler
;*******************************************************************************

;*******************************************************************************
;   '     ( -- ca )
;    search context vocabularies for the next word in input stream.

                    @token    tick,"'"
                    nop
                    jmp       dolst

                    fdb       token,nameq         ; ?defined
                    fdb       qbran,tick1
                    fdb       exit                ; yes, push code address
tick1               fdb       throw               ; no, error

;*******************************************************************************
;   allot     ( n -- )
;    allocate n bytes to the code dictionary.

                    @token    allot,'ALLOT'
                    nop
                    jmp       dolst

                    fdb       cp,pstor,exit       ; adjust code pointer

;*******************************************************************************
;   ,     ( w -- )
;    compile an integer into the code dictionary.

                    @token    comma,','
                    nop
                    jmp       dolst

                    fdb       here,dup,cellp      ; cell boundary
                    fdb       cp,store,store,exit  ; adjust code pointer, compile

;*******************************************************************************
;   [compile] ( -- ; <string> )
;    compile the next immediate word into code dictionary.

                    @token    bcomp,'[COMPILE]',i
                    nop
                    jmp       dolst

                    fdb       tick,comma,exit

;*******************************************************************************
;   compile   ( -- )
;    compile the next address in colon list to code dictionary.

                    @token    compi,'COMPILE',c
                    nop
                    jmp       dolst

                    fdb       rfrom,dup,at,comma  ; compile address
                    fdb       cellp,tor,exit      ; adjust return address

;*******************************************************************************
;   literal   ( w -- )
;    compile tos to code dictionary as an integer literal.

                    @token    liter,'LITERAL',i
                    nop
                    jmp       dolst

                    fdb       compi,dolit,comma,exit

;*******************************************************************************
;   $,"       ( -- )
;    compile a literal string up to next " .

                    @token    scomq,'$,"'
                    nop
                    jmp       dolst

                    fdb       dolit,$22,word      ; literal " (move word to dictionary)
                    fdb       count,plus,algnd    ; calculate aligned end of string
                    fdb       cp,store,exit       ; adjust the code pointer

;*******************************************************************************
;   recurse   ( -- )
;    make the current word available for compilation.

                    @token    recur,'RECURSE',i
                    nop
                    jmp       dolst

                    fdb       last,at,namet,comma,exit

;*******************************************************************************
;  structures
;*******************************************************************************

;*******************************************************************************
;   for       ( -- a )
;    start a for-next loop structure in a colon definition.

                    @token    for,'FOR',i
                    nop
                    jmp       dolst

                    fdb       compi,tor,here,exit

;*******************************************************************************
;   begin     ( -- a )
;    start an infinite or indefinite loop structure.

                    @token    begin,'BEGIN',i
                    nop
                    jmp       dolst

                    fdb       here,exit

;*******************************************************************************
;   next ( a -- )
;    terminate a for-next loop structure.

                    @token    next,'NEXT',i
                    nop
                    jmp       dolst

                    fdb       compi,donxt,comma,exit

;*******************************************************************************
;   until     ( a -- )
;    terminate a begin-until indefinite loop structure.

                    @token    until,'UNTIL',i
                    nop
                    jmp       dolst

                    fdb       compi,qbran,comma,exit

;*******************************************************************************
;   again     ( a -- )
;    terminate a begin-again infinite loop structure.

                    @token    again,'AGAIN',i
                    nop
                    jmp       dolst

                    fdb       compi,bran,comma,exit

;*******************************************************************************
;   if    ( -- a )
;    begin a conditional branch structure.

                    @token    if,'IF',i
                    nop
                    jmp       dolst

                    fdb       compi,qbran,here
                    fdb       dolit,0,comma,exit

;*******************************************************************************
;   ahead     ( -- a )
;    compile a forward branch instruction.

                    @token    ahead,'AHEAD',i
                    nop
                    jmp       dolst

                    fdb       compi,bran,here,dolit,0,comma,exit

;*******************************************************************************
;   repeat    ( a a -- )
;    terminate a begin-while-repeat indefinite loop.

                    @token    repea,'REPEAT',i
                    nop
                    jmp       dolst

                    fdb       again,here,swap,store,exit

;*******************************************************************************
;   then ( a -- )
;    terminate a conditional branch structure.

                    @token    then,'THEN',i
                    nop
                    jmp       dolst

                    fdb       here,swap,store,exit

;*******************************************************************************
;   aft       ( a -- a a )
;    jump to then in a for-aft-then-next loop the first time through.

                    @token    aft,'AFT',i
                    nop
                    jmp       dolst

                    fdb       drop,ahead,begin,swap,exit

;*******************************************************************************
;   else ( a -- a )
;    start the false clause in an if-else-then structure.

                    @token    else,'ELSE',i
                    nop
                    jmp       dolst

                    fdb       ahead,swap,then,exit

;*******************************************************************************
;   while     ( a -- a a )
;    conditional branch out of a begin-while-repeat loop.

                    @token    while,'WHILE',i
                    nop
                    jmp       dolst

                    fdb       if,swap,exit

;*******************************************************************************
;   abort"    ( -- ; <string> )
;    conditional abort with an error message.

                    @token    abrtq,'ABORT"',i
                    nop
                    jmp       dolst

                    fdb       compi,aborq,scomq,exit

;*******************************************************************************
;   $"    ( -- ; <string> )
;    compile an inline string literal.

                    @token    strq,'$"',i
                    nop
                    jmp       dolst

                    fdb       compi,strqp,scomq,exit

;*******************************************************************************
;   ."    ( -- ; <string> )
;    compile an inline string literal to be typed out at run time.

                    @token    dotq,'."',i
                    nop
                    jmp       dolst

                    fdb       compi,dotqp,scomq,exit

;*******************************************************************************
;  name compiler
;*******************************************************************************

;*******************************************************************************
;   ?unique   ( a -- a )
;    display a warning message if the word already exists.

                    @token    uniqu,'?UNIQUE'
                    nop
                    jmp       dolst

                    fdb       dup,nameq           ; ?name exists
                    fdb       qbran,uniq1         ; redefinitions are ok
                    fdb       dotqp
                    fcb       7
                    fcc       ' redef '           ; but warn the user
                    fdb       over,count,typee    ; just in case its not planned
uniq1               fdb       drop,exit

;*******************************************************************************
;   $,n       ( na -- )
;    build a new dictionary name using the string at na.

                    @token    sname,'$,n'
                    nop
                    jmp       dolst

                    fdb       dup,cat             ; ?null input
                    fdb       qbran,snam1
                    fdb       uniqu               ; ?redefinition
                    fdb       dup,last,store      ; save na for vocab link
                    fdb       here,algnd,swap     ; align code address
                    fdb       cellm               ; link address
                    fdb       crrnt,at,at,over,store
                    fdb       cellm,dup,np,store  ; adjust name pointer
                    fdb       store,exit          ; save code pointer
snam1               fdb       strqp
                    fcb       5
                    fcc       ' name'             ; null input
                    fdb       throw

;*******************************************************************************
;  forth compiler
;*******************************************************************************

;*******************************************************************************
;   $compile  ( a -- )
;    compile next word to code dictionary as a token or literal.

                    @token    scomp,'$COMPILE'
                    nop
                    jmp       dolst

                    fdb       nameq,qdup          ; ?defined
                    fdb       qbran,scom2
;                   fdb       at,dolit,immed,and  ; ?immediate
                    fdb       cat,dolit,immed,and ; ?immediate
                    fdb       qbran,scom1
                    fdb       execu,exit          ; its immediate, execute
scom1               fdb       comma,exit          ; its not immediate, compile
scom2               fdb       tnumb,atexe         ; try to convert to number
                    fdb       qbran,scom3
                    fdb       liter,exit          ; compile number as integer
scom3               fdb       throw               ; error

;*******************************************************************************
;   overt     ( -- )
;    link a new word into the current vocabulary.

                    @token    overt,'OVERT'
                    nop
                    jmp       dolst

                    fdb       last,at,crrnt,at,store,exit

;*******************************************************************************
;   ;     ( -- )
;    terminate a colon definition.

                    @token    semis,';',ic
                    nop
                    jmp       dolst

                    fdb       compi,exit,lbrac,overt,exit

;*******************************************************************************
;   ]     ( -- )
;    start compiling the words in the input stream.

                    @token    rbrac,']'
                    nop
                    jmp       dolst

                    fdb       dolit,scomp,teval,store,exit

;*******************************************************************************
;   call,     ( ca -- )
;    assemble a call instruction to ca.

                    @token    callc,'call,'
                    nop
                    jmp       dolst

                    fdb       dolit,nopjmp,comma  ; insert NOP and JMP
                    fdb       comma,exit          ; insert address
;                   fdb       dolit,calll,comma   ; direct threaded code
;                   fdb       comma,exit          ; dtc 8086 relative call

;*******************************************************************************
;   :     ( -- ; <string> )
;    start a new colon definition using next word as its name.

                    @token    colon,':'
                    nop
                    jmp       dolst

                    fdb       token,sname,dolit,dolst  ; add call to list proc
                    fdb       callc,rbrac,exit

;*******************************************************************************
;   immediate ( -- )
;    make the last compiled word an immediate word.

                    @token    immedi,'IMMEDIATE'
                    nop
                    jmp       dolst

                    fdb       dolit,immed,last,at,cat,or
                    fdb       last,at,cstor,exit
;                   fdb       dolit,immed,last,at,at,or
;                   fdb       last,at,store,exit

;*******************************************************************************
;  defining words
;*******************************************************************************

;*******************************************************************************
;   user ( u -- ; <string> )
;    compile a new user variable.

                    @token    user,'USER'
                    nop
                    jmp       dolst

                    fdb       token,sname,overt
                    fdb       dolit,dolst,callc   ; add call to list proc
                    fdb       compi,douse,comma,exit

;*******************************************************************************
;   create    ( -- ; <string> )
;    compile a new array entry without allocating code space.

                    @token    creat,'CREATE'
                    nop
                    jmp       dolst

                    fdb       token,sname,overt
                    fdb       dolit,dolst,callc
                    fdb       compi,dovar,exit

;*******************************************************************************
;   variable  ( -- ; <string> )
;    compile a new variable initialized to 0.

                    @token    varia,'VARIABLE'
                    nop
                    jmp       dolst

                    fdb       creat,dolit,0,comma,exit

;*******************************************************************************
;  tools
;*******************************************************************************

;*******************************************************************************
;   _type     ( b u -- )
;    display a string. filter non-printing characters.

                    @token    utype,'_TYPE'
                    nop
                    jmp       dolst

                    fdb       tor                 ; start count down loop
                    fdb       bran,utyp2          ; skip first pass
utyp1               fdb       dup,cat,tchar,emit  ; display only printable
                    fdb       dolit,1,plus        ; increment address
utyp2               fdb       donxt,utyp1         ; loop till done
                    fdb       drop,exit

;*******************************************************************************
;   dm+       ( a u -- a )
;    dump u bytes from , leaving a+u on the stack.

                    @token    dmp,'dm+'
                    nop
                    jmp       dolst

                    fdb       over,dolit,4,udotr  ; display address
                    fdb       space,tor           ; start count down loop
                    fdb       bran,pdum2          ; skip first pass
pdum1               fdb       dup,cat,dolit,3,udotr  ; display numeric data
                    fdb       dolit,1,plus        ; increment address
pdum2               fdb       donxt,pdum1         ; loop till done
                    fdb       exit

;*******************************************************************************
;   dump ( a u -- )
;    dump u bytes from a, in a formatted manner.

                    @token    dump,'DUMP'
                    nop
                    jmp       dolst

                    fdb       base,at,tor,hex     ; save radix, set hex
                    fdb       dolit,16,slash      ; change count to lines
                    fdb       tor                 ; start count down loop
dump1               fdb       crr,dolit,16,ddup,dmp  ; display numeric
                    fdb       rot,rot
                    fdb       space,space,utype   ; display printable characters
                    fdb       nufq,inver          ; user control
                    fdb       qbran,dump2
                    fdb       donxt,dump1         ; loop till done
                    fdb       bran,dump3
dump2               fdb       rfrom,drop          ; cleanup loop stack, early exit
dump3               fdb       drop,rfrom,base,store  ; restore radix
                    fdb       exit

;*******************************************************************************
;   .s    ( ... -- ... )
;    display the contents of the data stack.

                    @token    dots,'.S'
                    nop
                    jmp       dolst

                    fdb       crr,depth           ; stack depth
                    fdb       tor                 ; start count down loop
                    fdb       bran,dots2          ; skip first pass
dots1               fdb       rat,pick,dot        ; index stack, display contents
dots2               fdb       donxt,dots1         ; loop till done
                    fdb       dotqp
                    fcb       4
                    fcc       ' <sp '             ; extra space for align
                    fdb       exit

;*******************************************************************************
;   !csp ( -- )
;    save stack pointer in csp for error checking.

                    @token    stcsp,'!CSP'
                    nop
                    jmp       dolst

                    fdb       spat,csp,store,exit  ; save pointer

;*******************************************************************************
;   ?csp ( -- )
;    abort if stack pointer differs from that saved in csp.

                    @token    qcsp,'?CSP'
                    nop
                    jmp       dolst

                    fdb       spat,csp,at,xor     ; compare pointers
                    fdb       aborq               ; abort if different
                    fcb       6
                    fcc       'stacks '           ; extra space for align
                    fdb       exit

;*******************************************************************************
;   >name     ( ca -- na | f )
;    convert code address to a name address.

                    @token    tname,'>NAME'
                    nop
                    jmp       dolst

                    fdb       crrnt               ; vocabulary link
tnam1               fdb       cellp,at,qdup       ; check all vocabularies
                    fdb       qbran,tnam4
                    fdb       ddup
tnam2               fdb       at,dup              ; ?last word in a vocabulary
                    fdb       qbran,tnam3
                    fdb       ddup,namet,xor      ; compare
                    fdb       qbran,tnam3
                    fdb       cellm               ; continue with next word
                    fdb       bran,tnam2
tnam3               fdb       swap,drop,qdup
                    fdb       qbran,tnam1
                    fdb       swap,drop,swap,drop,exit
tnam4               fdb       drop,dolit,FALSE,exit  ; false flag

;*******************************************************************************
;   .id       ( na -- )
;    display the name at address.

                    @token    dotid,'.ID'
                    nop
                    jmp       dolst

                    fdb       qdup                ; if zero no name
                    fdb       qbran,doti1
                    fdb       count,dolit,$1F,and  ; mask lexicon bits
                    fdb       utype,exit          ; display name string
doti1               fdb       dotqp
                    fcb       9
                    fcc       ' {','noname}'
                    fdb       exit

;*******************************************************************************
;   see       ( -- ; <string> )
;    a simple decompiler.

                    @token    see,'SEE'
                    nop
                    jmp       dolst

                    fdb       tick                ; starting address
                    fdb       crr,cellp
see1                fdb       cellp,dup,at,dup    ; ?does it contain a zero
                    fdb       qbran,see2
                    fdb       tname               ; ?is it a name
see2                fdb       qdup                ; name address or zero
                    fdb       qbran,see3
                    fdb       space,dotid         ; display name
                    fdb       bran,see4
see3                fdb       dup,at,udot         ; display number
see4                fdb       nufq                ; user control
                    fdb       qbran,see1
                    fdb       drop,exit

;*******************************************************************************
;   words     ( -- )
;    display the names in the context vocabulary.

                    @token    words,'WORDS'
                    nop
                    jmp       dolst

                    fdb       crr,cntxt,at        ; only in context
wors1               fdb       at,qdup             ; ?at end of list
                    fdb       qbran,wors2
                    fdb       dup,space,dotid     ; display a name
                    fdb       cellm,nufq          ; user control
                    fdb       qbran,wors1
                    fdb       drop
wors2               fdb       exit

;*******************************************************************************
;  hardware reset
;*******************************************************************************

;*******************************************************************************
;   ver       ( -- n )
;    return the version number of this implementation.

                    @token    versn,'VER'
                    nop
                    jmp       dolst

                    fdb       dolit,ver*256+ext,exit

;*******************************************************************************
;   hi    ( -- )
;    display the sign-on message of eforth.

                    @token    hi,'hi'
                    nop
                    jmp       dolst

                    fdb       stoio,crr           ; initialize i/o
                    fdb       dotqp
                    fcb       17
                    fcc       ' 68hc11 eforth v ' ; model
                    fdb       base,at,hex         ; save radix
                    fdb       versn,bdigs,dig,dig
                    fdb       dolit,$2e,hold      ; literal .
                    fdb       digs,edigs,typee    ; format version number
                    fdb       base,store,crr,exit ; restore radix

;*******************************************************************************
;   'boot     ( -- a )
;    the application startup vector.

                    @token    tboot,"'BOOT"
                    nop
                    jmp       dolst

                    fdb       dovar
                    fdb       hi                  ; application to boot

;*******************************************************************************
;   noop  ( -- )
;    no-operation (runtime code for TASK)

                    @token    noop,'NOOP'
                    nop
                    jmp       dolst

                    fdb       exit

;*******************************************************************************
;   cold ( -- )
;    the hilevel cold start sequence.
;  NOTE:  cold must be the last word in the assembly language source
;  file.  TASK connects into the ROM-based dictionary by referencing
;  COLDLINK in the following definition.

coldlink            equ       *+4
                    @token    cold,'COLD'
                    nop
                    jmp       dolst

cold1               fdb       dolit,uzero,dolit,upp
                    fdb       dolit,ulast-uzero   ; initialize user area
                    fdb       cmove,prese         ; initialize stack and tib
                    fdb       dolit,ntask         ; initialize RAM name dictionary
                    fdb       dolit,namee-ntaskl
                    fdb       dolit,ntaskl
                    fdb       cmove
                    fdb       dolit,namee-ntaskl  ; initialize name pointer
                    fdb       np,store
                    fdb       dolit,namee-ntaskl+4 ; calc start of name in TASK
                    fdb       dup,last,store      ; initialize LAST
                    fdb       vfrth,store         ; initialize forth vocabulary ptr
                    fdb       tboot,atexe         ; application boot
                    fdb       forth,cntxt,at,dup  ; initialize search order
                    fdb       crrnt,dstor,overt
                    fdb       quit                ; start interpretation
                    fdb       bran,cold1          ; just in case

                    @vector   Vreset,Start
