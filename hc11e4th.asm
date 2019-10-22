;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;
;          68hc11 eForth for the Motorola FREEWARE assembler
;
;  This version of eForth runs on the Motorola 68HC11 microcontrollers.
;  The source code itself is derived from the original 8051 eForth as
;  developed by Dr. C. H. Ting.  I have rewritten the source to use the
;  Motorola FREEWARE assembler, and have added features of interest
;  to users of the 'hc11 family:
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
;  over a typical byte-aligned 68hc11 Forth.
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
;
;  This is a straight-up, no-frills porting of 8051 eForth to the 68hc11.
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
;
;*******************************************************************************

                    #ListOff
                    #Include  exp-f1.inc
                    #ListOn

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

;
;  RAM allocation:
;
;    top of RAM (ramend) ---------------------------------
;                   |   return stack grows down   |
;                   |            \/               |
;                   |            \/               |
;                   +-------------------------------+
;                   |      text input buffer |
;                   +-------------------------------+
;                   |    data stack grows down    |
;                   |            \/               |
;                   |            \/               |
;                   +-------------------------------+
;                   |            /\                 |
;                   |            /\               |
;                   |    user area grows up       |
;                   +-------------------------------|
;                   |  name dictionary grows down |
;                   |            \/               |
;                   |            \/               |
;                   +-------------------------------+
;                   |            /\                 |
;                   |            /\               |
;                   |   code dictionary grows up  |
;    bottom of RAM (rambeg)   +-------------------------------+
;

;
;  You can customize the memory usage of this eForth by changing the
;  values assigned to the following equates.  All sizes are in bytes
;  and must be even numbers.
;

rambeg              equ       $a000               ; bottom of ram memory
ramend              equ       $bfff               ; top of ram memory
rombeg              equ       $c000               ; bottom of rom memory

us                  equ       $100                ; user area size in bytes
rts                 equ       $100                ; return stack/tib size
dts                 equ       $100                ; data stack size

;
;  These equates fix the relative positions of eForth's buffers and
;  stack areas.
;

rpp                 equ       ramend&$fffe        ; start of return stack (rp0)
tibb                equ       rpp-rts             ; start of tib, body of rtn stack
spp                 equ       tibb-2              ; start of data stack (sp0)
upp                 equ       spp-dts-us-$10      ; start of user area (up)

namee               equ       upp-$10&$fffe       ; initial name dictionary (word align)
codee               equ       rambeg              ; initial code dictionary

;
;  Allocation of 68hc11 working registers.  These registers should stay
;  in the zero-page area for faster execution speed.
;

ip                  equ       $0022               ; 2 bytes for IP

;
;  ================  Start of 'hc11 eForth code  ===================
;

                    #ROM
;
;  ROM-based default data values.  These are automatically
;  preloaded into the user area upon reset.
;

uzero               fdb       0,0,0,0             ; reserved
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
                    fdb       0,0,0,0,0,0,0,0     ; vocabulary stack (vocss deep)
                    fdb       0                   ; current pointer
                    fdb       0                   ; vocabulary link pointer
                    fdb       codee               ; cp
                    fdb       0                   ; np (overwritten at powerup)
                    fdb       0                   ; last (overwritten at powerup)
                    fdb       0                   ; forth (overwritten at powerup)
                    fdb       0                   ; vocabulary link
ulast:


;
;  The following code is copied to the start of the RAM name
;  dictionary space on powerup.
;

ntask:
                    fdb       noop                ; 'code' for task
                    fdb       coldlink            ; link 'back' to cold
                    fcb       4
                    fcc       'TASK'
                    fcb       0                   ; null-fill cell
ntaskl              equ       *-ntask

;  Start of the compiler; jump here on power-up.
;
;  Note that serial port initialization can occur here and at !IO; if
;  you change any initialization code here, be sure to duplicate the
;  changes in !IO.

Start               sei
                    ldaa      #%10110011          ; turn on a/d, use edge irq, enable
                                                  ; delay after stop, set slow clk to
                                                  ; watchdog
                    staa      OPTION

                    ldaa      #$30                ; 9600 BAUD @ 2MHz bus
                    staa      BAUD
                    clr       SCCR1               ; 8-bit xfers
                    ldaa      #$0c
                    staa      SCCR2               ; no interrupts, enable r & t
                    lds       #spp-2              ; temp start of data stack
                    ldy       #rpp                ; temp start of return stack
                    ldx       #cold1              ; point to first instruction
                    bra       _next2              ; and start it up

;  The forth inner interpreter
;
;  Entry can occur at one of three locations, depending on the action
;  needed.
;
;  Entry at pushd pushes the word in A:B onto the data stack, then
;  falls into _next.  Use this entry point when leaving a low-level
;  definition that must also push a result (in D) onto the data
;  stack.
;
;  Entry at _next simply moves to the next low-level definition in
;  the thread.  Use this entry point when leaving a low-level
;  definition that does not need to save any results onto the data
;  stack.
;
;  Entry at _next2 uses the value in X as the current IP (instruction
;  pointer) and calculates the next executable address from it.  Use
;  this entry point when changing execution threads; for example, see
;  the code at ?BRANCH.
;

pushd:
                    pshb                          ; save D as word on stack
                    psha                          ; and fall into _next

_next:
                    ldx       ip                  ; get current ip
_next2:
                    ldab      #2                  ; ip = ip + 2
                    abx
                    stx       ip
                    dex                           ; x = ip - 2
                    dex
                    ldx       0,x                 ; x = (x)
                    jmp       0,x                 ; go to (x)

dolst:
                    ldab      #4                  ; x = x + 4
                    abx                           ; (to jump around NOP JMP DOLST)
                    ldd       ip                  ; get old IP
                    std       0,y                 ; save on return stack
                    dey                           ; make room on return stack
                    dey
                    bra       _next2              ; and go to next level

exit:
                    ldab      #2                  ; y = y + 2
                    aby
                    ldx       0,y                 ; pull old IP from return stack
                    bra       _next2              ; and go to previous level


;
;  Each eForth word contains a header of the following format:
;
;    fdb  label               points to executable code
;    fdb  prev_name      points back to previous name
; this_name:
;    fcb  options+n      n = length of name in bytes
;    fcc  'XXXX'              string containing word's name
;    fcb  0              word-alignment IF n IS EVEN!
; label:
;    -----                    start of assembly code for XXXX
;
;  where options is immed if this is an IMMEDIATE word and compo if
;  this is a COMPILE-ONLY word.  Note that the line containing fcb 0 is
;  optional; it should only appear if the word's name contains an even
;  number of characters.  Look over several definitions below
;  for examples of building headers.
;
;  Note that all low-level (assembly language) definitions begin with the
;  phrase:
;
;    org  *+1&$fffe
;
;  This forces the definition to be word-aligned.  This org statement is
;  not necessary for high-level definitions.
;
;  Note also that all embedded strings (usually following an 'fdb  dotqp')
;  must contain an even number of bytes INCLUDING THE LENGTH BYTE.  If
;  necessary, use a 'fcb  0' to pad out to the next word address.  Check
;  the definitions below for examples.
;

;
;  If you add customized low-level definitions, observe the following
;  register usage:
;
;  The hardware stack (S-register) serves as eForth's data stack.  You must
;  always push and pull data in word-wide sequences.  Never leave the data
;  stack having pushed or pulled an odd number of bytes.
;
;  The Y-register defines eForth's return stack.  All accesses must be
;  word-wide.  See the code at >R and R> for examples of using the Y-register
;  as a stack pointer.
;
;  The X-register may be freely trashed in your code, with one possible
;  exception.  If you leave your new definition with a jump to _next2,
;  X must point to the next threaded address to execute.  See the code in
;  BRANCH for an example.
;
;
;  The A- and B-registers may also be used freely, again with one possible
;  exception.  If you leave your new definition with a jump to pushd,
;  A:B will be pushed onto the data stack prior to executing the next
;  word in the thread.
;


;
;  the kernel
;

;   dolit     ( -- w )
;    push an inline literal.

                    org       *+1&$fffe

                    fdb       dolit
                    fdb       0                   ; link of 0 means first entry
_link1:
                    fcb       compo+5
                    fcc       'doLIT'
dolit:
                    ldx       ip                  ; get addr of next word
                    ldd       0,x                 ; get data at that addr
                    inx                           ; now bump IP
                    inx
                    stx       ip
                    bra       pushd               ; and save on stack


;   dolist    ( a -- )
;    process colon list.

                    org       *+1&$fffe

                    fdb       dolst               ; points back into inner interpreter
                    fdb       _link1
_link2:
                    fcb       compo+6
                    fcc       'doLIST'
                    fcb       0                   ; null-fill cell


;   next ( -- )
;    run time code for the single index loop.
;    : next ( -- ) \ hilevel model
;    r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;


                    org       *+1&$fffe

                    fdb       donxt
                    fdb       _link2
_link3:
                    fcb       compo+4
                    fcc       'next'
                    fcb       0                   ; null-fill cell
donxt:
                    ldd       2,y                 ; get counter on return stack
                    beq       donxt1              ; branch if loop is done
                    subd      #1                  ; no, bump the counter
                    std       2,y                 ; and replace on stack
                    bra       bran                ; and branch back to top

donxt1:
                    iny                           ; done, burn counter from stack
                    iny
                    ldx       ip                  ; get the IP
                    inx                           ; and get addr past branch target
                    inx
                    bra       _next2              ; and go do next word


;   ?branch   ( f -- )
;    branch if flag is zero.

                    org       *+1&$fffe

                    fdb       qbran
                    fdb       _link3
_link4:
                    fcb       compo+7
                    fcc       '?branch'
qbran:
                    pula                          ; get TOS to D
                    pulb
                    cmpd      #0                  ; did we get a 0?
                    bne       qbran1              ; branch if not
bran:
                    ldx       ip                  ; yes, get next word as addr
                    ldx       0,x                 ; now get contents as new IP
                    bra       _next2              ; and jump there

qbran1:
                    ldx       ip                  ; get old ip
                    inx                           ; and move past branch addr
                    inx
                    jmp       _next2              ; and jump there


;   branch    ( -- )
;    branch to an inline address.

                    org       *+1&$fffe

                    fdb       bran                ; use code inside ?BRANCH
                    fdb       _link4
_link5:
                    fcb       compo+6
                    fcc       'branch'
                    fcb       0                   ; null-fill cell


;   execute   ( ca -- )
;    execute the word at ca.

                    org       *+1&$fffe

                    fdb       execu
                    fdb       _link5
_link6:
                    fcb       7
                    fcc       'EXECUTE'
execu:
                    pulx                          ; get ca from TOS
                    jmp       0,x                 ; and go do it


;   exit ( -- )
;    terminate a colon definition.

                    org       *+1&$fffe

                    fdb       exit                ; points back into inner interpreter
                    fdb       _link6
_link7:
                    fcb       4
                    fcc       'EXIT'
                    fcb       0                   ; null-fill cell

;   !     ( u a -- )
;    store u into address a.

                    org       *+1&$fffe

                    fdb       store
                    fdb       _link7
_link8:
                    fcb       1
                    fcc       '!'
store:
                    pulx                          ; get the addr
                    pula                          ; get the word
                    pulb
                    std       0,x                 ; and save to addr
                    jmp       _next


;   @     ( a -- w )
;    push memory location to the data stack.

                    org       *+1&$fffe

                    fdb       at
                    fdb       _link8
_link9:
                    fcb       1
                    fcc       '@'
at:
                    pulx                          ; get the addr
                    ldd       0,x                 ; get the data there
                    jmp       pushd               ; and save it


;   c!    ( c b -- )
;    pop the data stack to byte memory.

                    org       *+1&$fffe

                    fdb       cstor
                    fdb       _link9
_link10:
                    fcb       2
                    fcc       'C!'
                    fcb       0                   ; null-fill cell

cstor:
                    pulx                          ; get the addr
                    pula                          ; get the data
                    pulb                          ; only low byte counts
                    stb       0,x                 ; save to addr
                    jmp       _next


;   c@    ( b -- c )
;    push byte memory location to the data stack.

                    org       *+1&$fffe

                    fdb       cat
                    fdb       _link10
_link11:
                    fcb       2
                    fcc       'C@'
                    fcb       0                   ; null-fill cell
cat:
                    pulx                          ; get the addr
                    ldb       0,x                 ; get the data
                    clra                          ; make MSB = 0
                    jmp       pushd               ; and save it


;   >r    ( w -- )
;    push the data stack to the return stack.

                    org       *+1&$fffe

                    fdb       tor
                    fdb       _link11
_link12:
                    fcb       compo+2
                    fcc       '>R'
                    fcb       0                   ; null-fill cell
tor:
                    pula                          ; get the data at TOS
                    pulb
                    std       0,y                 ; save on return stack
                    dey                           ; and make room
                    dey
                    jmp       _next


;   r@    ( -- w )
;    copy top of return stack to the data stack.

                    org       *+1&$fffe

                    fdb       rat
                    fdb       _link12
_link13:
                    fcb       2
                    fcc       'R@'
                    fcb       0                   ; null-fill cell
rat:
                    ldd       2,y                 ; get top value on return stack
                    jmp       pushd               ; and save to data stack


;   r>    ( -- w )
;    pop the return stack to the data stack.

                    org       *+1&$fffe

                    fdb       rfrom
                    fdb       _link13
_link14:
                    fcb       2
                    fcc       'R>'
                    fcb       0                   ; null-fill cell
rfrom:
                    iny                           ; count this value
                    iny
                    ldd       0,y                 ; get top value on return stack
                    jmp       pushd               ; now save it


;   rp@       ( -- a )
;    push the current rp to the data stack.

                    org       *+1&$fffe

                    fdb       rpat
                    fdb       _link14
_link15:
                    fcb       3
                    fcc       'RP@'
rpat:
                    pshy                          ; save return pointer
                    jmp       _next


;   rp!       ( a -- )
;    set the return stack pointer.

                    org       *+1&$fffe

                    fdb       rpsto
                    fdb       _link15
_link16:
                    fcb       compo+3
                    fcc       'RP!'
rpsto:
                    puly                          ; use new return pointer
                    jmp       _next


;   sp@       ( -- a )
;    push the current data stack pointer.

                    org       *+1&$fffe

                    fdb       spat
                    fdb       _link16
_link17:
                    fcb       3
                    fcc       'SP@'
spat:
                    tsx                           ; get current stack pointer
                    dex                           ; adjust for tsx instr
                    pshx                          ; and save on data stack
                    jmp       _next


;   sp!       ( a -- )
;    set the data stack pointer.

                    org       *+1&$fffe

                    fdb       spsto
                    fdb       _link17
_link18:
                    fcb       3
                    fcc       'SP!'
spsto:
                    pulx                          ; get new stack pointer
                    inx                           ; prepare for txs
                    txs                           ; and make it count
                    jmp       _next


;   dup       ( w -- w w )
;    duplicate the top stack item.

                    org       *+1&$fffe

                    fdb       dup
                    fdb       _link18
_link19:
                    fcb       3
                    fcc       'DUP'
dup:
                    pulx                          ; get TOS
                    pshx                          ; save it back
                    pshx                          ; and a copy
                    jmp       _next


;   drop ( w -- )
;    discard top stack item.

                    org       *+1&$fffe

                    fdb       drop
                    fdb       _link19
_link20:
                    fcb       4
                    fcc       'DROP'
                    fcb       0                   ; null-fill cell
drop:
                    pulx                          ; burn a data item
                    jmp       _next


;   swap ( w1 w2 -- w2 w1 )
;    exchange top two stack items.

                    org       *+1&$fffe

                    fdb       swap
                    fdb       _link20
_link21:
                    fcb       4
                    fcc       'SWAP'
                    fcb       0                   ; null-fill cell
swap:
                    pulx                          ; get top item
                    pula                          ; get second item
                    pulb
                    pshx                          ; save top item
                    jmp       pushd               ; and save second item


;   over ( w1 w2 -- w1 w2 w1 )
;    copy second stack item to top.

                    org       *+1&$fffe

                    fdb       over
                    fdb       _link21
_link22:
                    fcb       4
                    fcc       'OVER'
                    fcb       0                   ; null-fill cell
over:
                    tsx                           ; get the stack pointer
                    ldd       2,x                 ; get second item
                    jmp       pushd               ; and push onto stack


;   0<    ( n -- t )
;    return true if n is negative.

                    org       *+1&$fffe

                    fdb       zless
                    fdb       _link22
_link23:
                    fcb       2
                    fcc       '0<'
                    fcb       0                   ; null-fill cell
zless:
                    pula                          ; get TOS
                    pulb
                    tsta      check               ; high bit
                    bmi       zless1              ; branch if negative
                    ldd       #FALSE              ; get the flag
                    jmp       pushd               ; and set it

zless1:
                    ldd       #TRUE               ; get the flag
                    jmp       pushd               ; and set it


;   and       ( w w -- w )
;    bitwise and.

                    org       *+1&$fffe

                    fdb       and
                    fdb       _link23
_link24:
                    fcb       3
                    fcc       'AND'
and:
                    pula                          ; get TOS
                    pulb
                    tsx                           ; get stack pointer
                    anda      0,x                 ; and do the and
                    andb      1,x
                    std       0,x                 ; save back to stack
                    jmp       _next


;   or    ( w w -- w )
;    bitwise inclusive or.

                    org       *+1&$fffe

                    fdb       or
                    fdb       _link24
_link25:
                    fcb       2
                    fcc       'OR'
                    fcb       0                   ; null-fill cell
or:
                    pula                          ; get TOS
                    pulb
                    tsx                           ; get stack pointer
                    oraa      0,x                 ; and do the and
                    orab      1,x
                    std       0,x                 ; save back to stack
                    jmp       _next


;   xor       ( w w -- w )
;    bitwise exclusive or.

                    org       *+1&$fffe

                    fdb       xor
                    fdb       _link25
_link26:
                    fcb       3
                    fcc       'XOR'
xor:
                    pula                          ; get TOS
                    pulb
                    tsx                           ; get stack pointer
                    eora      0,x                 ; and do the and
                    eorb      1,x
                    std       0,x                 ; save back to stack
                    jmp       _next


;   um+       ( w w -- w cy )
;    add two numbers, return the sum and carry flag.

                    org       *+1&$fffe

                    fdb       uplus
                    fdb       _link26
_link27:
                    fcb       3
                    fcc       'UM+'
uplus:
                    pula                          ; get TOS
                    pulb
                    tsx                           ; get the stack pointer
                    addd      0,x                 ; and add second item
                    std       0,x                 ; put back on stack
                    ldd       #0                  ; presume false
                    rolb                          ; move carry into word
                    jmp       pushd               ; and save on stack

;
;  device dependent i/o
;

;   !io       ( -- )
;    initialize the serial i/o devices.

                    org       *+1&$fffe

                    fdb       stoio
                    fdb       _link27
_link28:
                    fcb       3
                    fcc       '!IO'
stoio:
                    ldaa      #$30                ; 9600 BAUD
                    staa      BAUD
                    ldaa      #$00                ; 8-bit xfers
                    staa      SCCR1
                    ldaa      #$0c
                    staa      SCCR2               ; no interrupts, enable r & t
                    jmp       _next


;   ?rx       ( -- c t | f )
;    return input character and true, or a false if no input.

                    org       *+1&$fffe

                    fdb       qrx
                    fdb       _link28
_link29:
                    fcb       3
                    fcc       '?RX'
qrx:
                    clra                          ; assume no char available
                    ldab      SCSR                ; get serial status reg
                    andb      #%00100000          ; check RDRF bit
                    beq       qrx1                ; branch if nothing there
                    ldab      SCDR                ; char; move into B
                    pshb                          ; save char to stack
                    psha                          ; as word
                    ldd       #TRUE               ; get the flag
qrx1:
                    jmp       pushd               ; and save flag


;   tx!       ( c -- )
;    send character c to the output device.

                    org       *+1&$fffe

                    fdb       txsto
                    fdb       _link29
_link30:
                    fcb       3
                    fcc       'TX!'
txsto:
                    pula                          ; get char from TOS
                    pulb                          ; char is in B
txsto1:
                    ldaa      SCSR                ; time to send?
                    bpl       txsto1              ; loop until time
                    stab      SCDR                ; write char to SCI
                    jmp       _next

;
;  system and user variables
;

;   dovar     ( -- a )
;    run time routine for variable and create.

                    org       *+1&$fffe

                    fdb       dovar
                    fdb       _link30
_link31:
                    fcb       compo+5
                    fcc       'doVAR'
dovar:
                    nop
                    jmp       dolst

                    fdb       rfrom,exit


;   up    ( -- a )
;    pointer to the user area.

                    fdb       up
                    fdb       _link31
_link32:
                    fcb       2
                    fcc       'UP'
                    fcb       0                   ; null-fill cell
up:
                    nop
                    jmp       dolst

                    fdb       dovar
                    fdb       upp

;   douser    ( -- a )
;    run time routine for user variables.

                    org       *+1&$fffe

                    fdb       douse
                    fdb       _link32
_link33:
                    fcb       compo+6
                    fcc       'doUSER'
                    fcb       0                   ; null-fill cell
douse:
                    nop
                    jmp       dolst

                    fdb       rfrom,at,up,at,plus,exit



;   sp0       ( -- a )
;    pointer to bottom of the data stack.

                    fdb       szero
                    fdb       _link33

_link33A            fcb       3
                    fcc       'SP0'

szero:
                    nop
                    jmp       dolst

                    fdb       douse,8,exit

;   rp0       ( -- a )
;    pointer to bottom of the return stack.

                    fdb       rzero
                    fdb       _link33A

_link33a            fcb       3
                    fcc       'RP0'

rzero:
                    nop
                    jmp       dolst

                    fdb       douse,10,exit

;   '?key     ( -- a )
;    execution vector of ?key.

                    fdb       tqkey
                    fdb       _link33a
_link33b:
                    fcb       5
                    fcc       "'?key"

tqkey:
                    nop
                    jmp       dolst

                    fdb       douse,12,exit

;   'emit     ( -- a )
;    execution vector of emit.

                    fdb       temit
                    fdb       _link33b
_link33c:
                    fcb       5
                    fcc       "'emit"

temit:
                    nop
                    jmp       dolst

                    fdb       douse,14,exit

;   'expect   ( -- a )
;    execution vector of expect.

                    fdb       texpe
                    fdb       _link33c
_link33d:
                    fcb       7
                    fcc       "'expect"

texpe:
                    nop
                    jmp       dolst

                    fdb       douse,16,exit

;   'tap ( -- a )
;    execution vector of tap.

                    fdb       ttap
                    fdb       _link33d
_link33e:
                    fcb       4
                    fcc       "'tap"
                    fcb       0                   ; null-fill cell
ttap:
                    nop
                    jmp       dolst

                    fdb       douse,18,exit

;   'echo     ( -- a )
;    execution vector of echo.

                    fdb       techo
                    fdb       _link33e
_link33f:
                    fcb       5
                    fcc       "'echo"

techo:
                    nop
                    jmp       dolst

                    fdb       douse,20,exit

;   'prompt   ( -- a )
;    execution vector of prompt.

                    fdb       tprom
                    fdb       _link33f
_link33g:
                    fcb       7
                    fcc       "'prompt"

tprom:
                    nop
                    jmp       dolst

                    fdb       douse,22,exit

;   base ( -- a )
;    storage of the radix base for numeric i/o.

                    fdb       base
                    fdb       _link33g
_link33h:
                    fcb       4
                    fcc       'BASE'
                    fcb       0                   ; null-fill cell
base:
                    nop
                    jmp       dolst

                    fdb       douse,24,exit

;   tmp       ( -- a )
;    a temporary storage location used in parse and find.

                    fdb       temp
                    fdb       _link33h
_link33i:
                    fcb       compo+3
                    fcc       'tmp'

temp:
                    nop
                    jmp       dolst

                    fdb       douse,26,exit

;   span ( -- a )
;    hold character count received by expect.

                    fdb       span
                    fdb       _link33i
_link33j:
                    fcb       4
                    fcc       'SPAN'
                    fcb       0                   ; null-fill cell
span:
                    nop
                    jmp       dolst

                    fdb       douse,28,exit

;   >in       ( -- a )
;    hold the character pointer while parsing input stream.

                    fdb       inn
                    fdb       _link33j
_link33k:
                    fcb       3
                    fcc       '>IN'

inn:
                    nop
                    jmp       dolst

                    fdb       douse,30,exit

;   #tib ( -- a )
;    hold the current count and address of the terminal input buffer.

                    fdb       ntib
                    fdb       _link33k
_link33l:
                    fcb       4
                    fcc       '#TIB'
                    fcb       0                   ; null-fill cell

ntib:
                    nop
                    jmp       dolst

                    fdb       douse,32,exit

;   csp       ( -- a )
;    hold the stack pointer for error checking.

                    fdb       csp
                    fdb       _link33l
_link33m:
                    fcb       3
                    fcc       'CSP'

csp:
                    nop
                    jmp       dolst

                    fdb       douse,36,exit

;   'eval     ( -- a )
;    execution vector of eval.

                    fdb       teval
                    fdb       _link33m
_link33n:
                    fcb       5
                    fcc       "'eval"

teval:
                    nop
                    jmp       dolst

                    fdb       douse,38,exit

;   'number   ( -- a )
;    execution vector of number?.

                    fdb       tnumb
                    fdb       _link33n
_link33o:
                    fcb       7
                    fcc       "'number"

tnumb:
                    nop
                    jmp       dolst

                    fdb       douse,40,exit

;   hld       ( -- a )
;    hold a pointer in building a numeric output string.

                    fdb       hld
                    fdb       _link33o
_link33p:
                    fcb       3
                    fcc       'HLD'

hld:
                    nop
                    jmp       dolst

                    fdb       douse,42,exit

;   handler   ( -- a )
;    hold the return stack pointer for error handling.

                    fdb       handl
                    fdb       _link33p
_link33q:
                    fcb       7
                    fcc       'HANDLER'

handl:
                    nop
                    jmp       dolst

                    fdb       douse,44,exit

;   context   ( -- a )
;    a area to specify vocabulary search order.

                    fdb       cntxt
                    fdb       _link33q
_link33r:
                    fcb       7
                    fcc       'CONTEXT'

cntxt:
                    nop
                    jmp       dolst

                    fdb       douse,46,exit

;   current   ( -- a )
;    point to the vocabulary to be extended.

                    fdb       crrnt
                    fdb       _link33r
_link33s:
                    fcb       7
                    fcc       'CURRENT'

crrnt:
                    nop
                    jmp       dolst

                    fdb       douse,64,exit

;   cp    ( -- a )
;    point to the top of the code dictionary.

                    fdb       cp
                    fdb       _link33s
_link33t:
                    fcb       2
                    fcc       'CP'
                    fcb       0                   ; null-fill cell
cp:
                    nop
                    jmp       dolst

                    fdb       douse,68,exit

;   np    ( -- a )
;    point to the bottom of the name dictionary.

                    fdb       np
                    fdb       _link33t
_link33u:
                    fcb       2
                    fcc       'NP'
                    fcb       0                   ; null-fill cell
np:
                    nop
                    jmp       dolst

                    fdb       douse,70,exit

;   last ( -- a )
;    point to the last name in the name dictionary.

                    fdb       last
                    fdb       _link33u
_link33v:
                    fcb       4
                    fcc       'LAST'
                    fcb       0                   ; null-fill cell
last:
                    nop
                    jmp       dolst

                    fdb       douse,72,exit

;   forth     ( -- a )
;    point to the last name in the name dictionary.

                    fdb       vfrth
                    fdb       _link33v
_link33x:
                    fcb       5
                    fcc       'forth'

vfrth:
                    nop
                    jmp       dolst

                    fdb       douse,74,exit

;
;  WARNING: Next available user area offset is 78.
;



;
;  common functions
;

;   forth     ( -- )
;    make forth the context vocabulary.

                    fdb       forth
                    fdb       _link33x
_link34:
                    fcb       5
                    fcc       'FORTH'
forth:
                    nop
                    jmp       dolst

                    fdb       vfrth,cntxt,store,exit

;   ?dup ( w -- w w | 0 )
;    dup tos if its is not zero.

                    fdb       qdup
                    fdb       _link34
_link35:
                    fcb       4
                    fcc       '?DUP'
                    fcb       0                   ; null-fill cell
qdup:
                    nop
                    jmp       dolst

                    fdb       dup
                    fdb       qbran,qdup1
                    fdb       dup
qdup1:
                    fdb       exit

;   rot       ( w1 w2 w3 -- w2 w3 w1 )
;    rot 3rd item to top.

                    fdb       rot
                    fdb       _link35
_link36:
                    fcb       3
                    fcc       'ROT'
rot:
                    nop
                    jmp       dolst

                    fdb       tor,swap,rfrom,swap,exit

;   2drop     ( w w -- )
;    discard two items on stack.

                    fdb       ddrop
                    fdb       _link36
_link37:
                    fcb       5
                    fcc       '2DROP'
ddrop:
                    nop
                    jmp       dolst

                    fdb       drop,drop,exit

;   2dup ( w1 w2 -- w1 w2 w1 w2 )
;    duplicate top two items.

                    fdb       ddup
                    fdb       _link37
_link38:
                    fcb       4
                    fcc       '2DUP'
                    fcb       0                   ; null-fill cell
ddup:
                    nop
                    jmp       dolst

                    fdb       over,over,exit

;   +     ( w w -- sum )
;    add top two items.

                    fdb       plus
                    fdb       _link38
_link39:
                    fcb       1
                    fcc       '+'
plus:
                    nop
                    jmp       dolst

                    fdb       uplus,drop,exit

;   d+    ( d d -- d )
;    double addition, as an example using um+.

                    fdb       dplus
                    fdb       _link39
_link39a:
                    fcb       2
                    fcc       'D+'
                    fcb       0
dplus:
                    nop
                    jmp       dolst

                    fdb       tor,swap,tor,uplus
                    fdb       rfrom,rfrom,plus,plus,exit


;   not       ( w -- w )
;    one's complement of tos.

                    fdb       inver
                    fdb       _link39a
_link40:
                    fcb       3
                    fcc       'NOT'
inver:
                    nop
                    jmp       dolst

                    fdb       dolit,-1,xor,exit

;   negate    ( n -- -n )
;    two's complement of tos.

                    fdb       negat
                    fdb       _link40
_link41:
                    fcb       6
                    fcc       'NEGATE'
                    fcb       0                   ; null-fill cell
negat:
                    nop
                    jmp       dolst

                    fdb       inver,dolit,1,plus,exit

;   dnegate   ( d -- -d )
;    two's complement of top double.

                    fdb       dnega
                    fdb       _link41
_link42:
                    fcb       7
                    fcc       'DNEGATE'
dnega:
                    nop
                    jmp       dolst

                    fdb       inver,tor,inver
                    fdb       dolit,1,uplus
                    fdb       rfrom,plus,exit

;   -     ( n1 n2 -- n1-n2 )
;    subtraction.

                    fdb       sub
                    fdb       _link42
_link43:
                    fcb       1
                    fcc       '-'
sub:
                    nop
                    jmp       dolst

                    fdb       negat,plus,exit

;   abs       ( n -- n )
;    return the absolute value of n.

                    fdb       abs
                    fdb       _link43
_link44:
                    fcb       3
                    fcc       'ABS'
abs:
                    nop
                    jmp       dolst

                    fdb       dup,zless
                    fdb       qbran,abs1
                    fdb       negat
abs1:
                    fdb       exit

;   =     ( w w -- t )
;    return true if top two are equal.

                    fdb       equal
                    fdb       _link44
_link45:
                    fcb       1
                    fcc       '='
equal:
                    nop
                    jmp       dolst

                    fdb       xor
                    fdb       qbran,equ1
                    fdb       dolit,FALSE,exit    ; false flag
equ1:
                    fdb       dolit,TRUE,exit     ; true flag

;   u<    ( u u -- t )
;    unsigned compare of top two items.

                    fdb       uless
                    fdb       _link45
_link46:
                    fcb       2
                    fcc       'U<'
                    fcb       0                   ; null-fill cell
uless:
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless
                    fdb       qbran,ules1
                    fdb       swap,drop,zless,exit
ules1:
                    fdb       sub,zless,exit

;   <     ( n1 n2 -- t )
;    signed compare of top two items.

                    fdb       less
                    fdb       _link46
_link47:
                    fcb       1
                    fcc       '<'
less:
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless
                    fdb       qbran,less1
                    fdb       drop,zless,exit
less1:
                    fdb       sub,zless,exit

;   max       ( n n -- n )
;    return the greater of two top stack items.

                    fdb       max
                    fdb       _link47
_link48:
                    fcb       3
                    fcc       'MAX'
max:
                    nop
                    jmp       dolst

                    fdb       ddup,less
                    fdb       qbran,max1
                    fdb       swap
max1:
                    fdb       drop,exit

;   min       ( n n -- n )
;    return the smaller of top two stack items.

                    fdb       min
                    fdb       _link48
_link49:
                    fcb       3
                    fcc       'MIN'
min:
                    nop
                    jmp       dolst

                    fdb       ddup,swap,less
                    fdb       qbran,min1
                    fdb       swap
min1:
                    fdb       drop,exit

;   within    ( u ul uh -- t )
;    return true if u is within the range of ul and uh.

                    fdb       withi
                    fdb       _link49
_link50:
                    fcb       6
                    fcc       'WITHIN'
                    fcb       0                   ; null-fill cell
withi:
                    nop
                    jmp       dolst

                    fdb       over,sub,tor        ; ul <= u < uh
                    fdb       sub,rfrom,uless,exit

;
;  divide
;

;   um/mod    ( udl udh u -- ur uq )
;    unsigned divide of a double by a single. return mod and quotient.

                    fdb       ummod
                    fdb       _link50
_link51:
                    fcb       6
                    fcc       'UM/MOD'
                    fcb       0                   ; null-fill cell
ummod:
                    nop
                    jmp       dolst

                    fdb       ddup,uless
                    fdb       qbran,umm4
                    fdb       negat,dolit,15,tor
umm1:
                    fdb       tor,dup,uplus
                    fdb       tor,tor,dup,uplus
                    fdb       rfrom,plus,dup
                    fdb       rfrom,rat,swap,tor
                    fdb       uplus,rfrom,or
                    fdb       qbran,umm2
                    fdb       tor,drop,dolit,1,plus,rfrom
                    fdb       bran,umm3
umm2:
                    fdb       drop
umm3:
                    fdb       rfrom
                    fdb       donxt,umm1
                    fdb       drop,swap,exit
umm4:
                    fdb       drop,ddrop
                    fdb       dolit,-1,dup,exit   ; overflow, return max

;   m/mod     ( d n -- r q )
;    signed floored divide of double by single. return mod and quotient.

                    fdb       msmod
                    fdb       _link51
_link52:
                    fcb       5
                    fcc       'M/MOD'
msmod:
                    nop
                    jmp       dolst

                    fdb       dup,zless,dup,tor
                    fdb       qbran,mmod1
                    fdb       negat,tor,dnega,rfrom
mmod1:
                    fdb       tor,dup,zless
                    fdb       qbran,mmod2
                    fdb       rat,plus
mmod2:
                    fdb       rfrom,ummod,rfrom
                    fdb       qbran,mmod3
                    fdb       swap,negat,swap
mmod3:
                    fdb       exit

;   /mod ( n n -- r q )
;    signed divide. return mod and quotient.

                    fdb       slmod
                    fdb       _link52
_link53:
                    fcb       4
                    fcc       '/MOD'
                    fcb       0                   ; null-fill cell
slmod:
                    nop
                    jmp       dolst

                    fdb       over,zless,swap,msmod,exit

;   mod       ( n n -- r )
;    signed divide. return mod only.

                    fdb       mod
                    fdb       _link53
_link54:
                    fcb       3
                    fcc       'MOD'
mod:
                    nop
                    jmp       dolst

                    fdb       slmod,drop,exit

;   /     ( n n -- q )
;    signed divide. return quotient only.

                    fdb       slash
                    fdb       _link54
_link55:
                    fcb       1
                    fcc       '/'
slash:
                    nop
                    jmp       dolst

                    fdb       slmod,swap,drop,exit

;
;  multiply
;

;   um*       ( u u -- ud )
;    unsigned multiply. return double product.

                    fdb       umsta
                    fdb       _link55
_link56:
                    fcb       3
                    fcc       'UM*'
umsta:
                    nop
                    jmp       dolst

                    fdb       dolit,0,swap,dolit,15,tor
umst1:
                    fdb       dup,uplus,tor,tor
                    fdb       dup,uplus,rfrom,plus,rfrom
                    fdb       qbran,umst2
                    fdb       tor,over,uplus,rfrom,plus
umst2:
                    fdb       donxt,umst1
                    fdb       rot,drop,exit

;   *     ( n n -- n )
;    signed multiply. return single product.

                    fdb       star
                    fdb       _link56
_link57:
                    fcb       1
                    fcc       '*'
star:
                    nop
                    jmp       dolst

                    fdb       umsta,drop,exit

;   m*    ( n n -- d )
;    signed multiply. return double product.

                    fdb       mstar
                    fdb       _link57
_link58:
                    fcb       2
                    fcc       'M*'
                    fcb       0                   ; null-fill cell
mstar:
                    nop
                    jmp       dolst

                    fdb       ddup,xor,zless,tor
                    fdb       abs,swap,abs,umsta
                    fdb       rfrom
                    fdb       qbran,msta1
                    fdb       dnega
msta1:
                    fdb       exit

;   */mod     ( n1 n2 n3 -- r q )
;    multiply n1 and n2, then divide by n3. return mod and quotient.

                    fdb       ssmod
                    fdb       _link58
_link59:
                    fcb       5
                    fcc       '*/MOD'
ssmod:
                    nop
                    jmp       dolst

                    fdb       tor,mstar,rfrom,msmod,exit

;   */    ( n1 n2 n3 -- q )
;    multiply n1 by n2, then divide by n3. return quotient only.

                    fdb       stasl
                    fdb       _link59
_link60:
                    fcb       2
                    fcc       '*/'
                    fcb       0                   ; null-fill cell
stasl:
                    nop
                    jmp       dolst

                    fdb       ssmod,swap,drop,exit

;
;  miscellaneous
;

;   cell+     ( a -- a )
;    add cell size in byte to address.

                    fdb       cellp
                    fdb       _link60
_link61:
                    fcb       5
                    fcc       'CELL+'
cellp:
                    nop
                    jmp       dolst

                    fdb       dolit,celll,plus,exit

;   cell-     ( a -- a )
;    subtract cell size in byte from address.

                    fdb       cellm
                    fdb       _link61
_link62:
                    fcb       5
                    fcc       'CELL-'
cellm:
                    nop
                    jmp       dolst

                    fdb       dolit,0-celll,plus,exit

;   cells     ( n -- n )
;    multiply tos by cell size in bytes.

                    fdb       cells
                    fdb       _link62
_link63:
                    fcb       5
                    fcc       'CELLS'
cells:
                    nop
                    jmp       dolst

                    fdb       dolit,celll,star,exit

;   aligned   ( b -- a )
;    align address to the cell boundary.

                    fdb       algnd
                    fdb       _link63
_link64:
                    fcb       7
                    fcc       'ALIGNED'
algnd:
                    nop
                    jmp       dolst

                    fdb       dup,dolit,0,dolit,celll
                    fdb       ummod,drop,dup
                    fdb       qbran,algn1
                    fdb       dolit,celll,swap,sub
algn1:
                    fdb       plus,exit

;   bl    ( -- 32 )
;    return 32, the blank character.

                    fdb       blank
                    fdb       _link64
_link65:
                    fcb       2
                    fcc       'BL'
                    fcb       0                   ; null-fill cell
blank:
                    nop
                    jmp       dolst

                    fdb       dolit,$20,exit      ; literal space

;   >char     ( c -- c )
;    filter non-printing characters.

                    fdb       tchar
                    fdb       _link65
_link66:
                    fcb       5
                    fcc       '>CHAR'
tchar:
                    nop
                    jmp       dolst

                    fdb       dolit,$7F,and,dup   ; mask msb
                    fdb       dolit,127,blank,withi  ; check for printable
                    fdb       qbran,tcha1         ; branch if printable
                    fdb       drop,dolit,$5f      ; literal underscore

tcha1:
                    fdb       exit

;   depth     ( -- n )
;    return the depth of the data stack.

                    fdb       depth
                    fdb       _link66
_link67:
                    fcb       5
                    fcc       'DEPTH'
depth:
                    nop
                    jmp       dolst

                    fdb       spat,szero,at,swap,sub
                    fdb       dolit,celll,slash,exit

;   pick ( ... +n -- ... w )
;    copy the nth stack item to tos.

                    fdb       pick
                    fdb       _link67
_link68:
                    fcb       4
                    fcc       'PICK'
                    fcb       0                   ; null-fill cell
pick:
                    nop
                    jmp       dolst

                    fdb       dolit,1,plus,cells
                    fdb       dolit,1,plus
                    fdb       spat,plus,at,exit

;
;  memory access
;

;   +!    ( n a -- )
;    add n to the contents at address a.

                    fdb       pstor
                    fdb       _link68
_link69:
                    fcb       2
                    fcc       '+!'
                    fcb       0                   ; null-fill cell
pstor:
                    nop
                    jmp       dolst

                    fdb       swap,over,at,plus
                    fdb       swap,store,exit

;   2!    ( d a -- )
;    store the double integer to address a.

                    fdb       dstor
                    fdb       _link69
_link70:
                    fcb       2
                    fcc       '2!'
                    fcb       0                   ; null-fill cell
dstor:
                    nop
                    jmp       dolst

                    fdb       swap,over,store
                    fdb       cellp,store,exit

;   2@    ( a -- d )
;    fetch double integer from address a.

                    fdb       dat
                    fdb       _link70
_link71:
                    fcb       2
                    fcc       '2@'
                    fcb       0                   ; null-fill cell
dat:
                    nop
                    jmp       dolst

                    fdb       dup,cellp,at
                    fdb       swap,at,exit

;   count     ( b -- b +n )
;    return count byte of a string and add 1 to byte address.

                    fdb       count
                    fdb       _link71
_link72:
                    fcb       5
                    fcc       'COUNT'
count:
                    nop
                    jmp       dolst

                    fdb       dup,dolit,1,plus
                    fdb       swap,cat,exit

;   here ( -- a )
;    return the top of the code dictionary.

                    fdb       here
                    fdb       _link72
_link73:
                    fcb       4
                    fcc       'HERE'
                    fcb       0                   ; null-fill cell
here:
                    nop
                    jmp       dolst

                    fdb       cp,at,exit

;   pad       ( -- a )
;    return the address of a temporary buffer.

                    fdb       pad
                    fdb       _link73
_link74:
                    fcb       3
                    fcc       'PAD'
pad:
                    nop
                    jmp       dolst

                    fdb       here,dolit,80,plus,exit

;   tib       ( -- a )
;    return the address of the terminal input buffer.

                    fdb       tib
                    fdb       _link74
_link75:
                    fcb       3
                    fcc       'TIB'
tib:
                    nop
                    jmp       dolst

                    fdb       ntib,cellp,at,exit

;   @execute  ( a -- )
;    execute vector stored in address a.

                    fdb       atexe
                    fdb       _link75
_link76:
                    fcb       8
                    fcc       '@EXECUTE'
                    fcb       0                   ; null-fill cell
atexe:
                    nop
                    jmp       dolst

                    fdb       at,qdup             ; ?address or zero
                    fdb       qbran,exe1
                    fdb       execu               ; execute if non-zero
exe1:
                    fdb       exit                ; do nothing if zero

;   cmove     ( b1 b2 u -- )
;    copy u bytes from b1 to b2.

                    fdb       cmove
                    fdb       _link76
_link77:
                    fcb       5
                    fcc       'CMOVE'
cmove:
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,cmov2
cmov1:
                    fdb       tor,dup,cat
                    fdb       rat,cstor
                    fdb       dolit,1,plus
                    fdb       rfrom,dolit,1,plus
cmov2:
                    fdb       donxt,cmov1
                    fdb       ddrop,exit

;   fill ( b u c -- )
;    fill u bytes of character c to area beginning at b.

                    fdb       fill
                    fdb       _link77
_link78:
                    fcb       4
                    fcc       'FILL'
                    fcb       0                   ; null-fill cell
fill:
                    nop
                    jmp       dolst

                    fdb       swap,tor,swap
                    fdb       bran,fill2
fill1:
                    fdb       ddup,cstor,dolit,1,plus
fill2:
                    fdb       donxt,fill1
                    fdb       ddrop,exit

;   -trailing ( b u -- b u )
;    adjust the count to eliminate trailing white space.

                    fdb       dtrai
                    fdb       _link78
_link79:
                    fcb       9
                    fcc       '-TRAILING'
dtrai:
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,dtra2
dtra1:
                    fdb       blank,over,rat,plus,cat,less
                    fdb       qbran,dtra2
                    fdb       rfrom,dolit,1,plus,exit  ; adjusted count
dtra2:
                    fdb       donxt,dtra1
                    fdb       dolit,0,exit        ; count=0

;   pack$     ( b u a -- a )
;    build a counted string with u characters from b. null fill.

                    fdb       packs
                    fdb       _link79
_link80:
                    fcb       5
                    fcc       'PACK$'
packs:
                    nop
                    jmp       dolst

                    fdb       algnd,dup,tor       ; strings only on cell boundary
                    fdb       over,dup,dolit,0
                    fdb       dolit,celll,ummod,drop  ; count mod cell
                    fdb       sub,over,plus
                    fdb       dolit,0,swap,store  ; null fill cell
                    fdb       ddup,cstor,dolit,1,plus  ; save count
                    fdb       swap,cmove,rfrom,exit  ; move string

;
;  numeric output, single precision
;

;   digit     ( u -- c )
;    convert digit u to a character.

                    fdb       digit
                    fdb       _link80
_link81:
                    fcb       5
                    fcc       'DIGIT'
digit:
                    nop
                    jmp       dolst

                    fdb       dolit,9,over,less
                    fdb       dolit,7,and,plus
                    fdb       dolit,$30,plus,exit  ; literal 0

;   extract   ( n base -- n c )
;    extract the least significant digit from n.

                    fdb       extrc
                    fdb       _link81
_link82:
                    fcb       7
                    fcc       'EXTRACT'
extrc:
                    nop
                    jmp       dolst

                    fdb       dolit,0,swap,ummod
                    fdb       swap,digit,exit

;   <#    ( -- )
;    initiate the numeric output process.

                    fdb       bdigs
                    fdb       _link82
_link83:
                    fcb       2
                    fcc       '<#'
                    fcb       0                   ; null-fill cell
bdigs:
                    nop
                    jmp       dolst

                    fdb       pad,hld,store,exit

;   hold ( c -- )
;    insert a character into the numeric output string.

                    fdb       hold
                    fdb       _link83
_link84:
                    fcb       4
                    fcc       'HOLD'
                    fcb       0                   ; null-fill cell
hold:
                    nop
                    jmp       dolst

                    fdb       hld,at,dolit,1,sub
                    fdb       dup,hld,store,cstor,exit

;   #     ( u -- u )
;    extract one digit from u and append the digit to output string.

                    fdb       dig
                    fdb       _link84
_link85:
                    fcb       1
                    fcc       '#'
dig:
                    nop
                    jmp       dolst

                    fdb       base,at,extrc,hold,exit

;   #s    ( u -- 0 )
;    convert u until all digits are added to the output string.

                    fdb       digs
                    fdb       _link85
_link86:
                    fcb       2
                    fcc       '#S'
                    fcb       0                   ; null-fill cell
digs:
                    nop
                    jmp       dolst

digs1:
                    fdb       dig,dup
                    fdb       qbran,digs2
                    fdb       bran,digs1
digs2:
                    fdb       exit

;   sign ( n -- )
;    add a minus sign to the numeric output string.

                    fdb       sign
                    fdb       _link86
_link87:
                    fcb       4
                    fcc       'SIGN'
                    fcb       0                   ; null-fill cell
sign:
                    nop
                    jmp       dolst

                    fdb       zless
                    fdb       qbran,sign1
                    fdb       dolit,$2d,hold      ; literal minus sign
sign1:
                    fdb       exit

;   #>    ( w -- b u )
;    prepare the output string to be type'd.

                    fdb       edigs
                    fdb       _link87
_link88:
                    fcb       2
                    fcc       '#>'
                    fcb       0                   ; null-fill cell
edigs:
                    nop
                    jmp       dolst

                    fdb       drop,hld,at
                    fdb       pad,over,sub,exit

;   str       ( n -- b u )
;    convert a signed integer to a numeric string.

                    fdb       str
                    fdb       _link88
_link89:
                    fcb       3
                    fcc       'str'
str:
                    nop
                    jmp       dolst

                    fdb       dup,tor,abs
                    fdb       bdigs,digs,rfrom
                    fdb       sign,edigs,exit

;   hex       ( -- )
;    use radix 16 as base for numeric conversions.

                    fdb       hex
                    fdb       _link89
_link90:
                    fcb       3
                    fcc       'HEX'
hex:
                    nop
                    jmp       dolst

                    fdb       dolit,16,base,store,exit

;   decimal   ( -- )
;    use radix 10 as base for numeric conversions.

                    fdb       decim
                    fdb       _link90
_link91:
                    fcb       7
                    fcc       'DECIMAL'
decim:
                    nop
                    jmp       dolst

                    fdb       dolit,10,base,store,exit

;
;  numeric input, single precision
;

;   digit?    ( c base -- u t )
;    convert a character to its numeric value. a flag indicates success.

                    fdb       digtq
                    fdb       _link91
_link92:
                    fcb       6
                    fcc       'DIGIT?'
                    fcb       0                   ; null-fill cell
digtq:
                    nop
                    jmp       dolst

                    fdb       tor,dolit,$30,sub   ; literal 0
                    fdb       dolit,9,over,less
                    fdb       qbran,dgtq1
                    fdb       dolit,7,sub
                    fdb       dup,dolit,10,less,or
dgtq1:
                    fdb       dup,rfrom,uless,exit

;   number?   ( a -- n t | a f )
;    convert a number string to integer. push a flag on tos.

                    fdb       numbq
                    fdb       _link92
_link93             fcb       7
                    fcc       'NUMBER?'
numbq               nop
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

;
;  basic i/o
;

;   ?key ( -- c t | f )
;    return input character and true, or a false if no input.

                    fdb       qkey
                    fdb       _link93

_link94             fcb       4
                    fcc       '?KEY'
                    fcb       0                   ; null-fill cell

qkey                nop
                    jmp       dolst

                    fdb       tqkey,atexe,exit

;   key       ( -- c )
;    wait for and return an input character.

                    fdb       key
                    fdb       _link94

_link95             fcb       3
                    fcc       'KEY'
key                 nop
                    jmp       dolst

key1                fdb       qkey
                    fdb       qbran,key1
                    fdb       exit

;   emit ( c -- )
;    send a character to the output device.

                    fdb       emit
                    fdb       _link95
_link96             fcb       4
                    fcc       'EMIT'
                    fcb       0                   ; null-fill cell
emit                nop
                    jmp       dolst

                    fdb       temit,atexe,exit

;   nuf? ( -- t )
;    return false if no input, else pause and if cr return true.

                    fdb       nufq
                    fdb       _link96
_link97             fcb       4
                    fcc       'NUF?'
                    fcb       0                   ; null-fill cell
nufq                nop
                    jmp       dolst

                    fdb       qkey,dup
                    fdb       qbran,nufq1
                    fdb       ddrop,key,dolit,CR,equal
nufq1               fdb       exit

;   pace ( -- )
;    send a pace character for the file downloading process.

                    fdb       pace
                    fdb       _link97
_link98             fcb       4
                    fcc       'PACE'
                    fcb       0                   ; null-fill cell
pace                nop
                    jmp       dolst

                    fdb       dolit,11,emit,exit

;   space     ( -- )
;    send the blank character to the output device.

                    fdb       space
                    fdb       _link98
_link99             fcb       5
                    fcc       'SPACE'
space               nop
                    jmp       dolst

                    fdb       blank,emit,exit

;   spaces    ( +n -- )
;    send n spaces to the output device.

                    fdb       spacs
                    fdb       _link99
_link100            fcb       6
                    fcc       'SPACES'
                    fcb       0                   ; null-fill cell
spacs               nop
                    jmp       dolst

                    fdb       dolit,0,max,tor
                    fdb       bran,char2
char1               fdb       space
char2               fdb       donxt,char1
                    fdb       exit

;   type ( b u -- )
;    output u characters from b.

                    fdb       typee
                    fdb       _link100
_link101            fcb       4
                    fcc       'TYPE'
                    fcb       0                   ; null-fill cell
typee               nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,type2
type1:
                    fdb       dup,cat,emit
                    fdb       dolit,1,plus
type2:
                    fdb       donxt,type1
                    fdb       drop,exit

;   cr    ( -- )
;    output a carriage return and a line feed.

                    fdb       crr
                    fdb       _link101
_link102            fcb       2
                    fcc       'CR'
                    fcb       0                   ; null-fill cell
crr                 nop
                    jmp       dolst

                    fdb       dolit,CR,emit
                    fdb       dolit,LF,emit,exit

;   do$       ( -- a )
;    return the address of a compiled string.

                    fdb       dostr
                    fdb       _link102
_link103            fcb       compo+3
                    fcc       'do$'
dostr               nop
                    jmp       dolst

                    fdb       rfrom,rat,rfrom,count,plus
                    fdb       algnd,tor,swap,tor,exit

;   $"|       ( -- a )
;    run time routine compiled by $". return address of a compiled string.

                    fdb       strqp
                    fdb       _link103
_link104:
                    fcb       compo+3
                    fcc       '$"|'
strqp:
                    nop
                    jmp       dolst

                    fdb       dostr,exit          ; force a call to do$

;   ."|       ( -- )
;    run time routine of ." . output a compiled string.

                    fdb       dotqp
                    fdb       _link104
_link105:
                    fcb       compo+3
                    fcc       '."|'
dotqp:
                    nop
                    jmp       dolst

                    fdb       dostr,count,typee,exit

;   .r    ( n +n -- )
;    display an integer in a field of n columns, right justified.

                    fdb       dotr
                    fdb       _link105
_link106:
                    fcb       2
                    fcc       '.R'
                    fcb       0                   ; null-fill cell
dotr:
                    nop
                    jmp       dolst

                    fdb       tor,str,rfrom,over,sub
                    fdb       spacs,typee,exit

;   u.r       ( u +n -- )
;    display an unsigned integer in n column, right justified.

                    fdb       udotr
                    fdb       _link106
_link107:
                    fcb       3
                    fcc       'U.R'
udotr:
                    nop
                    jmp       dolst

                    fdb       tor,bdigs,digs,edigs
                    fdb       rfrom,over,sub
                    fdb       spacs,typee,exit

;   u.    ( u -- )
;    display an unsigned integer in free format.

                    fdb       udot
                    fdb       _link107
_link108:
                    fcb       2
                    fcc       'U.'
                    fcb       0                   ; null-fill cell
udot:
                    nop
                    jmp       dolst

                    fdb       bdigs,digs,edigs
                    fdb       space,typee,exit

;   .     ( w -- )
;    display an integer in free format, preceeded by a space.

                    fdb       dot
                    fdb       _link108
_link109:
                    fcb       1
                    fcc       '.'
dot:
                    nop
                    jmp       dolst

                    fdb       base,at,dolit,10,xor  ; ?decimal
                    fdb       qbran,dot1
                    fdb       udot,exit           ; no, display unsigned
dot1:
                    fdb       str,space,typee,exit  ; yes, display signed

;   ?     ( a -- )
;    display the contents in a memory cell.

                    fdb       quest
                    fdb       _link109
_link110:
                    fcb       1
                    fcc       '?'
quest:
                    nop
                    jmp       dolst

                    fdb       at,dot,exit

;
;  parsing
;

;   parse     ( b u c -- b u delta ; <string> )
;    scan string delimited by c. return found string and its offset.

                    fdb       pars
                    fdb       _link110
_link111:
                    fcb       5
                    fcc       'parse'
pars:
                    nop
                    jmp       dolst

                    fdb       temp,store,over,tor,dup
                    fdb       qbran,pars8
                    fdb       dolit,1,sub,temp,at,blank,equal
                    fdb       qbran,pars3
                    fdb       tor
pars1:
                    fdb       blank,over,cat      ; skip leading blanks only
                    fdb       sub,zless,inver
                    fdb       qbran,pars2
                    fdb       dolit,1,plus
                    fdb       donxt,pars1
                    fdb       rfrom,drop,dolit,0,dup,exit
pars2:
                    fdb       rfrom
pars3:
                    fdb       over,swap
                    fdb       tor
pars4:
                    fdb       temp,at,over,cat,sub  ; scan for delimiter
                    fdb       temp,at,blank,equal
                    fdb       qbran,pars5
                    fdb       zless
pars5:
                    fdb       qbran,pars6
                    fdb       dolit,1,plus
                    fdb       donxt,pars4
                    fdb       dup,tor
                    fdb       bran,pars7
pars6:
                    fdb       rfrom,drop,dup
                    fdb       dolit,1,plus,tor
pars7:
                    fdb       over,sub
                    fdb       rfrom,rfrom,sub,exit
pars8:
                    fdb       over,rfrom,sub,exit

;   parse     ( c -- b u ; <string> )
;    scan input stream and return counted string delimited by c.

                    fdb       parse
                    fdb       _link111
_link112:
                    fcb       5
                    fcc       'PARSE'
parse:
                    nop
                    jmp       dolst

                    fdb       tor,tib,inn,at,plus  ; current input buffer pointer
                    fdb       ntib,at,inn,at,sub  ; remaining count
                    fdb       rfrom,pars,inn,pstor,exit

;   .(    ( -- )
;    output following string up to next ) .

                    fdb       dotpr
                    fdb       _link112
_link113:
                    fcb       immed+2
                    fcc       '.('
                    fcb       0                   ; null-fill cell
dotpr:
                    nop
                    jmp       dolst

                    fdb       dolit,$29,parse,typee,exit  ; literal )

;   (     ( -- )
;    ignore following string up to next ) . a comment.

                    fdb       paren
                    fdb       _link113
_link114:
                    fcb       immed+1
                    fcc       '('
paren:
                    nop
                    jmp       dolst

                    fdb       dolit,$29,parse,ddrop,exit  ; literal )

;   \     ( -- )
;    ignore following text till the end of line.

                    fdb       bksla
                    fdb       _link114
_link115:
                    fcb       immed+1
                    fcc       '\'
bksla:
                    nop
                    jmp       dolst

                    fdb       ntib,at,inn,store,exit

;   char ( -- c )
;    parse next word and return its first character.

                    fdb       char
                    fdb       _link115
_link116:
                    fcb       4
                    fcc       'CHAR'
                    fcb       0                   ; null-fill cell
char:
                    nop
                    jmp       dolst

                    fdb       blank,parse,drop,cat,exit

;   token     ( -- a ; <string> )
;    parse a word from input stream and copy it to name dictionary.

                    fdb       token
                    fdb       _link116
_link117:
                    fcb       5
                    fcc       'TOKEN'
token:
                    nop
                    jmp       dolst

                    fdb       blank,parse,dolit,31,min
                    fdb       np,at,over,sub,cellm
                    fdb       packs,exit

;   word ( c -- a ; <string> )
;    parse a word from input stream and copy it to code dictionary.

                    fdb       word
                    fdb       _link117
_link118:
                    fcb       4
                    fcc       'WORD'
                    fcb       0                   ; null-fill cell
word:
                    nop
                    jmp       dolst

                    fdb       parse,here,packs,exit

;
;  dictionary search
;

;   name>     ( na -- ca )
;    return a code address given a name address.

                    fdb       namet
                    fdb       _link118
_link119:
                    fcb       5
                    fcc       'NAME>'
namet:
                    nop
                    jmp       dolst

                    fdb       cellm,cellm,at,exit

;   same?     ( a a u -- a a f \ -0+ )
;    compare u cells in two strings. return 0 if identical.

                    fdb       sameq
                    fdb       _link119
_link120:
                    fcb       5
                    fcc       'SAME?'
sameq:
                    nop
                    jmp       dolst

                    fdb       tor
                    fdb       bran,same2
same1:
                    fdb       over,rat,cells,plus,at
                    fdb       over,rat,cells,plus,at
                    fdb       sub,qdup
                    fdb       qbran,same2
                    fdb       rfrom,drop,exit     ; strings not equal
same2:
                    fdb       donxt,same1
                    fdb       dolit,0,exit        ; strings equal

;   find ( a va -- ca na | a f )
;    search a vocabulary for a string. return ca and na if succeeded.

                    fdb       find
                    fdb       _link120
_link121:
                    fcb       4
                    fcc       'find'
                    fcb       0                   ; null-fill cell
find:
                    nop
                    jmp       dolst

                    fdb       swap,dup,cat
                    fdb       dolit,celll,slash,temp,store
                    fdb       dup,at,tor,cellp,swap
find1:
                    fdb       at,dup
                    fdb       qbran,find6
                    fdb       dup,at,dolit,maskk,and,rat,xor
                    fdb       qbran,find2
                    fdb       cellp,dolit,TRUE    ; true flag
                    fdb       bran,find3
find2:
                    fdb       cellp,temp,at,sameq
find3:
                    fdb       bran,find4
find6:
                    fdb       rfrom,drop
                    fdb       swap,cellm,swap,exit
find4:
                    fdb       qbran,find5
                    fdb       cellm,cellm
                    fdb       bran,find1
find5:
                    fdb       rfrom,drop,swap,drop
                    fdb       cellm
                    fdb       dup,namet,swap,exit

;   name?     ( a -- ca na | a f )
;    search all context vocabularies for a string.

                    fdb       nameq
                    fdb       _link121
_link122:
                    fcb       5
                    fcc       'NAME?'
nameq:
                    nop
                    jmp       dolst

                    fdb       cntxt,dup,dat,xor   ; ?context=also
                    fdb       qbran,namq1
                    fdb       cellm               ; no, start with context
namq1:
                    fdb       tor
namq2:
                    fdb       rfrom,cellp,dup,tor  ; next in search order
                    fdb       at,qdup
                    fdb       qbran,namq3
                    fdb       find,qdup           ; search vocabulary
                    fdb       qbran,namq2
                    fdb       rfrom,drop,exit     ; found name
namq3:
                    fdb       rfrom,drop          ; name not found
                    fdb       dolit,FALSE,exit    ; false flag

;
;  terminal response
;

;   ^h    ( bot eot cur -- bot eot cur )
;    backup the cursor by one character.

                    fdb       bksp
                    fdb       _link122
_link123:
                    fcb       2
                    fcc       '^H'
                    fcb       0                   ; null-fill cell
bksp:
                    nop
                    jmp       dolst

                    fdb       tor,over,rfrom,swap,over,xor
                    fdb       qbran,back1
                    fdb       dolit,BS,techo,atexe,dolit,1,sub
                    fdb       blank,techo,atexe
                    fdb       dolit,BS,techo,atexe
back1:
                    fdb       exit

;   tap       ( bot eot cur c -- bot eot cur )
;    accept and echo the key stroke and bump the cursor.

                    fdb       tap
                    fdb       _link123
_link124:
                    fcb       3
                    fcc       'TAP'
tap:
                    nop
                    jmp       dolst

                    fdb       dup,techo,atexe
                    fdb       over,cstor,dolit,1,plus,exit

;   ktap ( bot eot cur c -- bot eot cur )
;    process a key stroke, CR or backspace.

                    fdb       ktap
                    fdb       _link124
_link125:
                    fcb       4
                    fcc       'kTAP'
                    fcb       0                   ; null-fill cell
ktap:
                    nop
                    jmp       dolst

                    fdb       dup,dolit,CR,xor
                    fdb       qbran,ktap2
                    fdb       dolit,BS,xor
                    fdb       qbran,ktap1
                    fdb       blank,tap,exit
ktap1:
                    fdb       bksp,exit
ktap2:
                    fdb       drop,swap,drop,dup,exit

;   accept    ( b u -- b u )
;    accept characters to input buffer. return with actual count.

                    fdb       accep
                    fdb       _link125
_link126:
                    fcb       6
                    fcc       'accept'
                    fcb       0                   ; null-fill cell
accep:
                    nop
                    jmp       dolst

                    fdb       over,plus,over
accp1:
                    fdb       ddup,xor
                    fdb       qbran,accp4
                    fdb       key,dup
;    fdb  blank,sub,dolit,95,uless
                    fdb       blank,dolit,127,withi
                    fdb       qbran,accp2
                    fdb       tap
                    fdb       bran,accp3
accp2:
                    fdb       ttap,atexe
accp3:
                    fdb       bran,accp1
accp4:
                    fdb       drop,over,sub,exit

;   expect    ( b u -- )
;    accept input stream and store count in span.

                    fdb       expec
                    fdb       _link126
_link127:
                    fcb       6
                    fcc       'EXPECT'
                    fcb       0                   ; null-fill cell
expec:
                    nop
                    jmp       dolst

                    fdb       texpe,atexe,span,store,drop,exit

;   query     ( -- )
;    accept input stream to terminal input buffer.

                    fdb       query
                    fdb       _link127
_link128:
                    fcb       5
                    fcc       'QUERY'
query:
                    nop
                    jmp       dolst

                    fdb       tib,dolit,80,texpe,atexe,ntib,store
                    fdb       drop,dolit,0,inn,store,exit

;
;  error handling
;

;   catch     ( ca -- 0 | err# )
;    execute word at ca and set up an error frame for it.

                    fdb       catch
                    fdb       _link128
_link129:
                    fcb       5
                    fcc       'CATCH'
catch:
                    nop
                    jmp       dolst

                    fdb       spat,tor,handl,at,tor  ; save error frame
                    fdb       rpat,handl,store,execu  ; execute
                    fdb       rfrom,handl,store   ; restore error frame
                    fdb       rfrom,drop,dolit,0,exit  ; no error

;   throw     ( err# -- err# )
;    reset system to current local error frame an update error flag.

                    fdb       throw
                    fdb       _link129
_link130:
                    fcb       5
                    fcc       'THROW'
throw:
                    nop
                    jmp       dolst

                    fdb       handl,at,rpsto      ; restore return stack
                    fdb       rfrom,handl,store   ; restore handler frame
                    fdb       rfrom,swap,tor,spsto  ; restore data stack
                    fdb       drop,rfrom,exit

;   null$     ( -- a )
;    return address of a null string with zero count.

                    fdb       nulls
                    fdb       _link130
_link131:
                    fcb       5
                    fcc       'NULL$'
nulls:
                    nop
                    jmp       dolst

                    fdb       dovar               ; emulate create
                    fdb       0
                    fcb       99,111,121,111,116,101
;    $align

;   abort     ( -- )
;    reset data stack and jump to quit.

                    fdb       abort
                    fdb       _link131
_link132:
                    fcb       5
                    fcc       'ABORT'
abort:
                    nop
                    jmp       dolst

                    fdb       nulls,throw

;   abort"    ( f -- )
;    run time routine of abort" . abort with a message.

                    fdb       aborq
                    fdb       _link132
_link133:
                    fcb       compo+6
                    fcc       'abort"'
                    fcb       0                   ; null-fill cell
aborq:
                    nop
                    jmp       dolst

                    fdb       qbran,abor1         ; text flag
                    fdb       dostr,throw         ; pass error string
abor1:
                    fdb       dostr,drop,exit     ; drop error

;
;  the text interpreter
;

;   $interpret ( a -- )
;    interpret a word. if failed, try to convert it to an integer.

                    fdb       inter
                    fdb       _link133
_link134:
                    fcb       10
                    fcc       '$INTERPRET'
                    fcb       0                   ; null-fill cell
inter:
                    nop
                    jmp       dolst

                    fdb       nameq,qdup          ; ?defined
                    fdb       qbran,inte1
;    fdb  at,dolit,compo,and       ?compile only lexicon bits
                    fdb       cat,dolit,compo,and  ; ?compile only lexicon bits
                    fdb       aborq
                    fcb       13
                    fcc       ' compile only'
                    fdb       execu,exit          ; execute defined word
inte1:
                    fdb       tnumb,atexe         ; convert a number
                    fdb       qbran,inte2
                    fdb       exit
inte2:
                    fdb       throw               ; error

;   [     ( -- )
;    start the text interpreter.

                    fdb       lbrac
                    fdb       _link134
_link135:
                    fcb       immed+1
                    fcc       '['
lbrac:
                    nop
                    jmp       dolst

                    fdb       dolit,inter,teval,store,exit

;   .ok       ( -- )
;    display 'ok' only while interpreting.

                    fdb       dotok
                    fdb       _link135
_link136:
                    fcb       3
                    fcc       '.OK'
dotok:
                    nop
                    jmp       dolst

                    fdb       dolit,inter,teval,at,equal
                    fdb       qbran,doto1
                    fdb       dotqp
                    fcb       3
                    fcc       ' ok'
doto1:
                    fdb       crr,exit

;   ?stack    ( -- )
;    abort if the data stack underflows.

                    fdb       qstac
                    fdb       _link136
_link137:
                    fcb       6
                    fcc       '?STACK'
                    fcb       0                   ; null-fill cell
qstac:
                    nop
                    jmp       dolst

                    fdb       depth,zless         ; check only for underflow
                    fdb       aborq
                    fcb       10
                    fcc       ' underflow '       ; extra space for align
                    fdb       exit

;   eval ( -- )
;    interpret the input stream.

                    fdb       eval
                    fdb       _link137
_link138:
                    fcb       4
                    fcc       'EVAL'
                    fcb       0                   ; null-fill cell
eval:
                    nop
                    jmp       dolst

eval1:
                    fdb       token,dup,cat       ; ?input stream empty
                    fdb       qbran,eval2
                    fdb       teval,atexe,qstac   ; evaluate input, check stack
                    fdb       bran,eval1
eval2:
                    fdb       drop,tprom,atexe,exit  ; prompt

;
;  shell
;

;   preset    ( -- )
;    reset data stack pointer and the terminal input buffer.

                    fdb       prese
                    fdb       _link138
_link139:
                    fcb       6
                    fcc       'PRESET'
                    fcb       0                   ; null-fill cell
prese:
                    nop
                    jmp       dolst

                    fdb       szero,at,spsto
                    fdb       dolit,tibb,ntib,cellp,store,exit

;   xio       ( a a a -- )
;    reset the i/o vectors 'expect, 'tap, 'echo and 'prompt.

                    fdb       xio
                    fdb       _link139
_link140:
                    fcb       compo+3
                    fcc       'xio'
xio:
                    nop
                    jmp       dolst

                    fdb       dolit,accep,texpe,dstor
                    fdb       techo,dstor,exit

;   file ( -- )
;    select i/o vectors for file download.

                    fdb       file
                    fdb       _link140
_link141:
                    fcb       4
                    fcc       'FILE'
                    fcb       0                   ; null-fill cell
file:
                    nop
                    jmp       dolst

                    fdb       dolit,pace,dolit,drop
                    fdb       dolit,ktap,xio,exit

;   hand ( -- )
;    select i/o vectors for terminal interface.

                    fdb       hand
                    fdb       _link141
_link142:
                    fcb       4
                    fcc       'HAND'
                    fcb       0                   ; null-fill cell
hand:
                    nop
                    jmp       dolst

                    fdb       dolit,dotok,dolit,emit
                    fdb       dolit,ktap,xio,exit

;   i/o       ( -- a )
;    array to store default i/o vectors.

                    fdb       islo
                    fdb       _link142
_link143:
                    fcb       3
                    fcc       'I/O'
islo:
                    nop
                    jmp       dolst

                    fdb       dovar               ; emulate create
                    fdb       qrx,txsto           ; default i/o vectors

;   console   ( -- )
;    initiate terminal interface.

                    fdb       conso
                    fdb       _link143
_link144:
                    fcb       7
                    fcc       'CONSOLE'
conso:
                    nop
                    jmp       dolst

                    fdb       islo,dat,tqkey,dstor  ; restore default i/o device
                    fdb       hand,exit           ; keyboard input

;   quit ( -- )
;    reset return stack pointer and start text interpreter.

                    fdb       quit
                    fdb       _link144
_link145:
                    fcb       4
                    fcc       'QUIT'
                    fcb       0                   ; null-fill cell
quit:
                    nop
                    jmp       dolst

                    fdb       rzero,at,rpsto      ; reset return stack pointer
quit1:
                    fdb       lbrac               ; start interpretation
quit2:
                    fdb       query               ; get input
                    fdb       dolit,eval,catch,qdup  ; evaluate input
                    fdb       qbran,quit2         ; continue till error
                    fdb       tprom,at,swap       ; save input device
                    fdb       conso,nulls,over,xor  ; ?display error message
                    fdb       qbran,quit3
                    fdb       space,count,typee   ; error message
                    fdb       dotqp
                    fcb       3
                    fcc       ' ? '               ; error prompt
quit3:
                    fdb       dolit,dotok,xor     ; ?file input
                    fdb       qbran,quit4
                    fdb       dolit,err,emit      ; file error, tell host
quit4:
                    fdb       prese               ; some cleanup
                    fdb       bran,quit1

;
;  the compiler
;

;   '     ( -- ca )
;    search context vocabularies for the next word in input stream.

                    fdb       tick
                    fdb       _link145
_link146:
                    fcb       1
                    fcc       "'"
tick:
                    nop
                    jmp       dolst

                    fdb       token,nameq         ; ?defined
                    fdb       qbran,tick1
                    fdb       exit                ; yes, push code address
tick1:
                    fdb       throw               ; no, error

;   allot     ( n -- )
;    allocate n bytes to the code dictionary.

                    fdb       allot
                    fdb       _link146
_link147:
                    fcb       5
                    fcc       'ALLOT'
allot:
                    nop
                    jmp       dolst

                    fdb       cp,pstor,exit       ; adjust code pointer

;   ,     ( w -- )
;    compile an integer into the code dictionary.

                    fdb       comma
                    fdb       _link147
_link148:
                    fcb       1
                    fcc       ','
comma:
                    nop
                    jmp       dolst

                    fdb       here,dup,cellp      ; cell boundary
                    fdb       cp,store,store,exit  ; adjust code pointer, compile

;   [compile] ( -- ; <string> )
;    compile the next immediate word into code dictionary.

                    fdb       bcomp
                    fdb       _link148
_link149:
                    fcb       immed+9
                    fcc       '[COMPILE]'
bcomp:
                    nop
                    jmp       dolst

                    fdb       tick,comma,exit

;   compile   ( -- )
;    compile the next address in colon list to code dictionary.

                    fdb       compi
                    fdb       _link149
_link150:
                    fcb       compo+7
                    fcc       'COMPILE'
compi:
                    nop
                    jmp       dolst

                    fdb       rfrom,dup,at,comma  ; compile address
                    fdb       cellp,tor,exit      ; adjust return address

;   literal   ( w -- )
;    compile tos to code dictionary as an integer literal.

                    fdb       liter
                    fdb       _link150
_link151:
                    fcb       immed+7
                    fcc       'LITERAL'
liter:
                    nop
                    jmp       dolst

                    fdb       compi,dolit,comma,exit

;   $,"       ( -- )
;    compile a literal string up to next " .

                    fdb       scomq
                    fdb       _link151
_link152:
                    fcb       3
                    fcc       '$,"'
scomq:
                    nop
                    jmp       dolst

                    fdb       dolit,$22,word      ; literal " (move word to dictionary)
                    fdb       count,plus,algnd    ; calculate aligned end of string
                    fdb       cp,store,exit       ; adjust the code pointer

;   recurse   ( -- )
;    make the current word available for compilation.

                    fdb       recur
                    fdb       _link152
_link153:
                    fcb       immed+7
                    fcc       'RECURSE'
recur:
                    nop
                    jmp       dolst

                    fdb       last,at,namet,comma,exit

;
;  structures
;

;   for       ( -- a )
;    start a for-next loop structure in a colon definition.

                    fdb       for
                    fdb       _link153
_link154:
                    fcb       immed+3
                    fcc       'FOR'
for:
                    nop
                    jmp       dolst

                    fdb       compi,tor,here,exit

;   begin     ( -- a )
;    start an infinite or indefinite loop structure.

                    fdb       begin
                    fdb       _link154
_link155:
                    fcb       immed+5
                    fcc       'BEGIN'
begin:
                    nop
                    jmp       dolst

                    fdb       here,exit

;   next ( a -- )
;    terminate a for-next loop structure.

                    fdb       next
                    fdb       _link155
_link156:
                    fcb       immed+4
                    fcc       'NEXT'
                    fcb       0                   ; null-fill cell
next:
                    nop
                    jmp       dolst

                    fdb       compi,donxt,comma,exit

;   until     ( a -- )
;    terminate a begin-until indefinite loop structure.

                    fdb       until
                    fdb       _link156
_link157:
                    fcb       immed+5
                    fcc       'UNTIL'
until:
                    nop
                    jmp       dolst

                    fdb       compi,qbran,comma,exit

;   again     ( a -- )
;    terminate a begin-again infinite loop structure.

                    fdb       again
                    fdb       _link157
_link158:
                    fcb       immed+5
                    fcc       'AGAIN'
again:
                    nop
                    jmp       dolst

                    fdb       compi,bran,comma,exit

;   if    ( -- a )
;    begin a conditional branch structure.

                    fdb       if
                    fdb       _link158
_link159:
                    fcb       immed+2
                    fcc       'IF'
                    fcb       0                   ; null-fill cell
if:
                    nop
                    jmp       dolst

                    fdb       compi,qbran,here
                    fdb       dolit,0,comma,exit

;   ahead     ( -- a )
;    compile a forward branch instruction.

                    fdb       ahead
                    fdb       _link159
_link160:
                    fcb       immed+5
                    fcc       'AHEAD'
ahead:
                    nop
                    jmp       dolst

                    fdb       compi,bran,here,dolit,0,comma,exit

;   repeat    ( a a -- )
;    terminate a begin-while-repeat indefinite loop.

                    fdb       repea
                    fdb       _link160
_link161:
                    fcb       immed+6
                    fcc       'REPEAT'
                    fcb       0                   ; null-fill cell
repea:
                    nop
                    jmp       dolst

                    fdb       again,here,swap,store,exit

;   then ( a -- )
;    terminate a conditional branch structure.

                    fdb       then
                    fdb       _link161
_link162:
                    fcb       immed+4
                    fcc       'THEN'
                    fcb       0                   ; null-fill cell
then:
                    nop
                    jmp       dolst

                    fdb       here,swap,store,exit

;   aft       ( a -- a a )
;    jump to then in a for-aft-then-next loop the first time through.

                    fdb       aft
                    fdb       _link162
_link163:
                    fcb       immed+3
                    fcc       'AFT'
aft:
                    nop
                    jmp       dolst

                    fdb       drop,ahead,begin,swap,exit

;   else ( a -- a )
;    start the false clause in an if-else-then structure.

                    fdb       else
                    fdb       _link163
_link164:
                    fcb       immed+4
                    fcc       'ELSE'
                    fcb       0                   ; null-fill cell
else:
                    nop
                    jmp       dolst

                    fdb       ahead,swap,then,exit

;   while     ( a -- a a )
;    conditional branch out of a begin-while-repeat loop.

                    fdb       while
                    fdb       _link164
_link165:
                    fcb       immed+5
                    fcc       'WHILE'
while:
                    nop
                    jmp       dolst

                    fdb       if,swap,exit

;   abort"    ( -- ; <string> )
;    conditional abort with an error message.

                    fdb       abrtq
                    fdb       _link165
_link166:
                    fcb       immed+6
                    fcc       'ABORT"'
                    fcb       0                   ; null-fill cell
abrtq:
                    nop
                    jmp       dolst

                    fdb       compi,aborq,scomq,exit

;   $"    ( -- ; <string> )
;    compile an inline string literal.

                    fdb       strq
                    fdb       _link166
_link167:
                    fcb       immed+2
                    fcc       '$"'
                    fcb       0                   ; null-fill cell
strq:
                    nop
                    jmp       dolst

                    fdb       compi,strqp,scomq,exit

;   ."    ( -- ; <string> )
;    compile an inline string literal to be typed out at run time.

                    fdb       dotq
                    fdb       _link167
_link168:
                    fcb       immed+2
                    fcc       '."'
                    fcb       0                   ; null-fill cell
dotq:
                    nop
                    jmp       dolst

                    fdb       compi,dotqp,scomq,exit

;
;  name compiler
;

;   ?unique   ( a -- a )
;    display a warning message if the word already exists.

                    fdb       uniqu
                    fdb       _link168
_link169:
                    fcb       7
                    fcc       '?UNIQUE'
uniqu:
                    nop
                    jmp       dolst

                    fdb       dup,nameq           ; ?name exists
                    fdb       qbran,uniq1         ; redefinitions are ok
                    fdb       dotqp
                    fcb       7
                    fcc       ' redef '           ; but warn the user
                    fdb       over,count,typee    ; just in case its not planned
uniq1:
                    fdb       drop,exit

;   $,n       ( na -- )
;    build a new dictionary name using the string at na.

                    fdb       sname               ; was scn ??
                    fdb       _link169
_link170:
                    fcb       3
                    fcc       '$,n'
sname:
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
snam1:
                    fdb       strqp
                    fcb       5
                    fcc       ' name'             ; null input
                    fdb       throw

;
;  forth compiler
;

;   $compile  ( a -- )
;    compile next word to code dictionary as a token or literal.

                    fdb       scomp
                    fdb       _link170
_link171:
                    fcb       8
                    fcc       '$COMPILE'
                    fcb       0                   ; null cell to align
scomp:
                    nop
                    jmp       dolst

                    fdb       nameq,qdup          ; ?defined
                    fdb       qbran,scom2
;    fdb  at,dolit,immed,and       ?immediate
                    fdb       cat,dolit,immed,and  ; ?immediate
                    fdb       qbran,scom1
                    fdb       execu,exit          ; its immediate, execute
scom1:
                    fdb       comma,exit          ; its not immediate, compile
scom2:
                    fdb       tnumb,atexe         ; try to convert to number
                    fdb       qbran,scom3
                    fdb       liter,exit          ; compile number as integer
scom3:
                    fdb       throw               ; error

;   overt     ( -- )
;    link a new word into the current vocabulary.

                    fdb       overt
                    fdb       _link171
_link172:
                    fcb       5
                    fcc       'OVERT'
overt:
                    nop
                    jmp       dolst

                    fdb       last,at,crrnt,at,store,exit

;   ;     ( -- )
;    terminate a colon definition.

                    fdb       semis
                    fdb       _link172
_link173:
                    fcb       immed+compo+1
                    fcc       ';'
semis:
                    nop
                    jmp       dolst

                    fdb       compi,exit,lbrac,overt,exit

;   ]     ( -- )
;    start compiling the words in the input stream.

                    fdb       rbrac
                    fdb       _link173
_link174:
                    fcb       1
                    fcc       ']'
rbrac:
                    nop
                    jmp       dolst

                    fdb       dolit,scomp,teval,store,exit

;   call,     ( ca -- )
;    assemble a call instruction to ca.

                    fdb       callc
                    fdb       _link174
_link175:
                    fcb       5
                    fcc       'call,'
callc:
                    nop
                    jmp       dolst

                    fdb       dolit,nopjmp,comma  ; insert NOP and JMP
                    fdb       comma,exit          ; insert address
;    fdb  dolit,calll,comma   ;direct threaded code
;    fdb  comma,exit     ;dtc 8086 relative call

;   :     ( -- ; <string> )
;    start a new colon definition using next word as its name.

                    fdb       colon
                    fdb       _link175
_link176:
                    fcb       1
                    fcc       ':'
colon:
                    nop
                    jmp       dolst

                    fdb       token,sname,dolit,dolst  ; add call to list proc
                    fdb       callc,rbrac,exit

;   immediate ( -- )
;    make the last compiled word an immediate word.

                    fdb       immedi
                    fdb       _link176
_link177:
                    fcb       9
                    fcc       'IMMEDIATE'
immedi:
                    nop
                    jmp       dolst

                    fdb       dolit,immed,last,at,cat,or
                    fdb       last,at,cstor,exit
;    fdb  dolit,immed,last,at,at,or
;    fdb  last,at,store,exit

;
;  defining words
;

;   user ( u -- ; <string> )
;    compile a new user variable.

                    fdb       user
                    fdb       _link177
_link178:
                    fcb       4
                    fcc       'USER'
                    fcb       0                   ; null-fill cell
user:
                    nop
                    jmp       dolst

                    fdb       token,sname,overt
                    fdb       dolit,dolst,callc   ; add call to list proc
                    fdb       compi,douse,comma,exit

;   create    ( -- ; <string> )
;    compile a new array entry without allocating code space.

                    fdb       creat
                    fdb       _link178
_link179:
                    fcb       6
                    fcc       'CREATE'
                    fcb       0                   ; null-fill cell
creat:
                    nop
                    jmp       dolst

                    fdb       token,sname,overt
                    fdb       dolit,dolst,callc
                    fdb       compi,dovar,exit

;   variable  ( -- ; <string> )
;    compile a new variable initialized to 0.

                    fdb       varia
                    fdb       _link179
_link180:
                    fcb       8
                    fcc       'VARIABLE'
                    fcb       0                   ; null-fill cell
varia:
                    nop
                    jmp       dolst

                    fdb       creat,dolit,0,comma,exit

;
;  tools
;

;   _type     ( b u -- )
;    display a string. filter non-printing characters.

                    fdb       utype
                    fdb       _link180
_link181:
                    fcb       5
                    fcc       '_TYPE'
utype:
                    nop
                    jmp       dolst

                    fdb       tor                 ; start count down loop
                    fdb       bran,utyp2          ; skip first pass
utyp1:
                    fdb       dup,cat,tchar,emit  ; display only printable
                    fdb       dolit,1,plus        ; increment address
utyp2:
                    fdb       donxt,utyp1         ; loop till done
                    fdb       drop,exit

;   dm+       ( a u -- a )
;    dump u bytes from , leaving a+u on the stack.

                    fdb       dmp
                    fdb       _link181
_link182:
                    fcb       3
                    fcc       'dm+'
dmp:
                    nop
                    jmp       dolst

                    fdb       over,dolit,4,udotr  ; display address
                    fdb       space,tor           ; start count down loop
                    fdb       bran,pdum2          ; skip first pass
pdum1:
                    fdb       dup,cat,dolit,3,udotr  ; display numeric data
                    fdb       dolit,1,plus        ; increment address
pdum2:
                    fdb       donxt,pdum1         ; loop till done
                    fdb       exit

;   dump ( a u -- )
;    dump u bytes from a, in a formatted manner.

                    fdb       dump
                    fdb       _link182
_link183:
                    fcb       4
                    fcc       'DUMP'
                    fcb       0                   ; null-fill cell
dump:
                    nop
                    jmp       dolst

                    fdb       base,at,tor,hex     ; save radix, set hex
                    fdb       dolit,16,slash      ; change count to lines
                    fdb       tor                 ; start count down loop
dump1:
                    fdb       crr,dolit,16,ddup,dmp  ; display numeric
                    fdb       rot,rot
                    fdb       space,space,utype   ; display printable characters
                    fdb       nufq,inver          ; user control
                    fdb       qbran,dump2
                    fdb       donxt,dump1         ; loop till done
                    fdb       bran,dump3
dump2:
                    fdb       rfrom,drop          ; cleanup loop stack, early exit
dump3:
                    fdb       drop,rfrom,base,store  ; restore radix
                    fdb       exit

;   .s    ( ... -- ... )
;    display the contents of the data stack.

                    fdb       dots
                    fdb       _link183
_link184:
                    fcb       2
                    fcc       '.S'
                    fcb       0                   ; null-fill cell
dots:
                    nop
                    jmp       dolst

                    fdb       crr,depth           ; stack depth
                    fdb       tor                 ; start count down loop
                    fdb       bran,dots2          ; skip first pass
dots1:
                    fdb       rat,pick,dot        ; index stack, display contents
dots2:
                    fdb       donxt,dots1         ; loop till done
                    fdb       dotqp
                    fcb       4
                    fcc       ' <sp '             ; extra space for align
                    fdb       exit

;   !csp ( -- )
;    save stack pointer in csp for error checking.

                    fdb       stcsp
                    fdb       _link184
_link185:
                    fcb       4
                    fcc       '!CSP'
                    fcb       0                   ; null-fill cell
stcsp:
                    nop
                    jmp       dolst

                    fdb       spat,csp,store,exit  ; save pointer

;   ?csp ( -- )
;    abort if stack pointer differs from that saved in csp.

                    fdb       qcsp
                    fdb       _link185
_link186:
                    fcb       4
                    fcc       '?CSP'
                    fcb       0                   ; null-fill cell
qcsp:
                    nop
                    jmp       dolst

                    fdb       spat,csp,at,xor     ; compare pointers
                    fdb       aborq               ; abort if different
                    fcb       6
                    fcc       'stacks '           ; extra space for align
                    fdb       exit

;   >name     ( ca -- na | f )
;    convert code address to a name address.

                    fdb       tname
                    fdb       _link186
_link187:
                    fcb       5
                    fcc       '>NAME'
tname:
                    nop
                    jmp       dolst

                    fdb       crrnt               ; vocabulary link
tnam1:
                    fdb       cellp,at,qdup       ; check all vocabularies
                    fdb       qbran,tnam4
                    fdb       ddup
tnam2:
                    fdb       at,dup              ; ?last word in a vocabulary
                    fdb       qbran,tnam3
                    fdb       ddup,namet,xor      ; compare
                    fdb       qbran,tnam3
                    fdb       cellm               ; continue with next word
                    fdb       bran,tnam2
tnam3:
                    fdb       swap,drop,qdup
                    fdb       qbran,tnam1
                    fdb       swap,drop,swap,drop,exit
tnam4:
                    fdb       drop,dolit,FALSE,exit  ; false flag

;   .id       ( na -- )
;    display the name at address.

                    fdb       dotid
                    fdb       _link187
_link188:
                    fcb       3
                    fcc       '.ID'
dotid:
                    nop
                    jmp       dolst

                    fdb       qdup                ; if zero no name
                    fdb       qbran,doti1
                    fdb       count,dolit,$1F,and  ; mask lexicon bits
                    fdb       utype,exit          ; display name string
doti1:
                    fdb       dotqp
                    fcb       9
                    fcc       ' {','noname}'
                    fdb       exit

;   see       ( -- ; <string> )
;    a simple decompiler.

                    fdb       see
                    fdb       _link188
_link189:
                    fcb       3
                    fcc       'SEE'
see:
                    nop
                    jmp       dolst

                    fdb       tick                ; starting address
                    fdb       crr,cellp
see1:
                    fdb       cellp,dup,at,dup    ; ?does it contain a zero
                    fdb       qbran,see2
                    fdb       tname               ; ?is it a name
see2:
                    fdb       qdup                ; name address or zero
                    fdb       qbran,see3
                    fdb       space,dotid         ; display name
                    fdb       bran,see4
see3:
                    fdb       dup,at,udot         ; display number
see4:
                    fdb       nufq                ; user control
                    fdb       qbran,see1
                    fdb       drop,exit

;   words     ( -- )
;    display the names in the context vocabulary.

                    fdb       words
                    fdb       _link189
_link190:
                    fcb       5
                    fcc       'WORDS'
words:
                    nop
                    jmp       dolst

                    fdb       crr,cntxt,at        ; only in context
wors1:
                    fdb       at,qdup             ; ?at end of list
                    fdb       qbran,wors2
                    fdb       dup,space,dotid     ; display a name
                    fdb       cellm,nufq          ; user control
                    fdb       qbran,wors1
                    fdb       drop
wors2:
                    fdb       exit

;
;  hardware reset
;

;   ver       ( -- n )
;    return the version number of this implementation.

                    fdb       versn
                    fdb       _link190
_link191:
                    fcb       3
                    fcc       'VER'

versn               nop
                    jmp       dolst

                    fdb       dolit,ver*256+ext,exit

;   hi    ( -- )
;    display the sign-on message of eforth.

                    fdb       hi
                    fdb       _link191

_link192            fcb       2
                    fcc       'hi'
                    fcb       0                   ; null-fill cell

hi                  nop
                    jmp       dolst

                    fdb       stoio,crr           ; initialize i/o
                    fdb       dotqp
                    fcb       17
                    fcc       ' 68hc11 eforth v '  ; model
                    fdb       base,at,hex         ; save radix
                    fdb       versn,bdigs,dig,dig
                    fdb       dolit,$2e,hold      ; literal .
                    fdb       digs,edigs,typee    ; format version number
                    fdb       base,store,crr,exit  ; restore radix

;   'boot     ( -- a )
;    the application startup vector.

                    fdb       tboot
                    fdb       _link192

_link193            fcb       5
                    fcc       "'BOOT"

tboot               nop
                    jmp       dolst

                    fdb       dovar
                    fdb       hi                  ; application to boot

;   noop  ( -- )
;    no-operation (runtime code for TASK)
;

                    fdb       noop
                    fdb       _link193

_link193a           fcb       4
                    fcc       'NOOP'
                    fcb       0                   ; null-fill cell

noop                nop
                    jmp       dolst

                    fdb       exit


;   cold ( -- )
;    the hilevel cold start sequence.
;
;  NOTE:  cold must be the last word in the assembly language source
;  file.  TASK connects into the ROM-based dictionary by referencing
;  COLDLINK in the following definition.
;

                    fdb       cold
                    fdb       _link193a
coldlink:
                    fcb       4
                    fcc       'COLD'
                    fcb       0                   ; null-fill cell

cold                nop
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
                    fdb       dolit,namee-ntaskl+4  ; calc start of name in TASK
                    fdb       dup,last,store      ; initialize LAST
                    fdb       vfrth,store         ; initialize forth vocabulary ptr
                    fdb       tboot,atexe         ; application boot
                    fdb       forth,cntxt,at,dup  ; initialize search order
                    fdb       crrnt,dstor,overt
                    fdb       quit                ; start interpretation
                    fdb       bran,cold1          ; just in case

                    @vector   Vreset,Start
