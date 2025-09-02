'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
'=====================================================================
'
' Device & clock (adjust as required for your board)
Device = 18F2525

Config_Start
  OSC = INTIO67
  FCMEN = OFF
  IESO = OFF
  PWRT = OFF
  BOREN = SBORDIS
  BORV = 3
  WDT = OFF
  WDTPS = 32768
  CCP2MX = PORTC
  PBADEN = OFF
  LPT1OSC = OFF
  MCLRE = On
  STVREN = On
  LVP = OFF
  XINST = OFF
  Debug = OFF
  Cp0 = OFF
  CP1 = OFF
  CP2 = OFF
  CPB = OFF
  CPD = OFF
  WRT0 = OFF
  WRT1 = OFF
  WRT2 = OFF
  WRTC = OFF
  WRTB = OFF
  WRTD = OFF
  EBTR0 = OFF
  EBTR1 = OFF
  EBTR2 = OFF
  EBTRB = OFF
Config_End

OSCCON    = %01110000        ' 8 MHz
OSCTUNE.6 = 1                ' PLL x4 -> 32 MHz
ADCON1    = $0F              ' All digital

All_Digital = True
Declare Xtal = 32
Declare PORTB_Pullups = On

'---------------------------------------------------------------------
' LCD (HD44780-compatible) 20x4 in 4-bit mode
Declare LCD_Type = 0
Declare LCD_DTPin = PORTA.0
Declare LCD_ENPin = PORTA.7
Declare LCD_RSPin = PORTA.6
Declare LCD_Interface = 4
Declare LCD_Lines = 4

'---------------------------------------------------------------------
' UART (TX on RC6 @ 115200)
Declare Hserial_Baud  = 115200         ' baud rate
Declare Hserial_RCSTA = %10010000      ' SPEN=1 (enable serial), CREN=1 (RX enable ok even if unused)
Declare Hserial_TXSTA = %00100100      ' BRGH=1 (high speed), TXEN=1 (enable TX)


'---------------------------------------------------------------------
' Encoder & button pins (adjust to your board)
Symbol _ENC_A = PORTB.1
Symbol _ENC_B = PORTB.2
Symbol _BTN   = PORTB.6                         ' Active-low push button
Symbol _BUZZER = PORTC.2

'---------------------------------------------------------------------
' UART (optional debug; keep disabled by default to save flash)
' Declare Hserial_Baud = 115200
' Declare Hserial_RCSTA = %10010000
' Declare Hserial_TXSTA = %00100100

'---------------------------------------------------------------------
' Timing & UI constants
Symbol TICK_MS          = 1                     ' ISR tick period
Symbol BTN_SHORT_MS     = 200                   ' short press threshold
Symbol BTN_LONG_MS      = 900                   ' long press threshold
Symbol BTN_VLONG_MS     = 1800                  ' very long press threshold
Symbol UI_TIMEOUT_MS    = 30000                 ' inactivity timeout to unwind

'---------------------------------------------------------------------
' Enums and modes
Symbol MODE_NO    = 0
Symbol MODE_PULSE = 1
Symbol MODE_LATCH = 2

Symbol YES = 1
Symbol NO  = 0

Symbol SENSOR_FLOW = 0
Symbol SENSOR_PRES = 1
Symbol SENSOR_TEMP = 2

'---------------------------------------------------------------------
'--- Map prior LONG/VERY_LONG constants to your existing ones
Symbol LONG_MS       = BTN_LONG_MS
Symbol VERY_LONG_MS  = BTN_VLONG_MS


'---------------------------------------------------------------------
' Global state
Dim b_ReInitLCD  As Bit                       ' cleared at each menu entry
Dim b_ScrDirty   As Bit                       ' request redraw of current view
Dim b_Escape     As Bit                       ' shared escape flag for unwind

Dim L_Millis     As Dword                     ' millisecond counter
Dim L_LastInput  As Dword                     ' last input ms (activity)

' Encoder state
Dim B_EncPrev    As Byte
Dim B_EncNow     As Byte
Dim B_EncDelta   As SByte                     ' -1, 0, +1 per poll

' Button state
Dim b_BtnLast As Bit      ' 0=up, 1=down
Dim L_BtnDownMs  As Dword
Dim B_KeyEvent   As Byte                      ' 0=none 1=short 2=long 3=very long

' Temp string helper
Dim S_T As String * 18

Dim W_Beep As Word        ' ms remaining for buzzer (0 = off)

'--- Encoder/Button debounce & tracking
Dim B_RE_Count     As Byte
Dim B_AState       As Byte
Dim B_BState       As Byte
Dim B_ButtonState  As Byte
Dim B_DebA         As Byte
Dim B_DebB         As Byte
Dim B_DebBtn       As Byte
Dim B_LastState    As Byte
Dim S_Qacc         As SByte          ' signed accumulator (-/+)
Dim W_EncoderPos   As Word           ' debounced position
Dim W_EncReadPos   As Word           ' last readback for delta
Dim W_BtnHoldMS    As Word           ' debounced hold time
Dim b_Long         As Bit
Dim b_VLong        As Bit


'---------------------------------------------------------------------
' EEPROM map (Input1 only for this pass; extend as needed)
' Offsets kept symbolic for clarity and easy expansion
Symbol EE_VER           = 0                     ' config version
Symbol EE_I1_BASE       = 10
Symbol EE_I1_ENABLED    = EE_I1_BASE + 0        ' Byte: 0/1
Symbol EE_I1_SENSORT    = EE_I1_BASE + 1        ' Byte: SENSOR_*
Symbol EE_I1_FLOWMODE   = EE_I1_BASE + 2        ' Byte: 0=meter 1=switch
Symbol EE_I1_SCALE4     = EE_I1_BASE + 4        ' Word
Symbol EE_I1_SCALE20    = EE_I1_BASE + 6        ' Word
Symbol EE_I1_BP_HIGH    = EE_I1_BASE + 8        ' Word seconds (mm:ss)
Symbol EE_I1_BP_PLP     = EE_I1_BASE + 10       ' Word seconds (mm:ss)
Symbol EE_I1_BP_SLP     = EE_I1_BASE + 12       ' Word seconds (mm:ss)
Symbol EE_I1_RLY_HIGH   = EE_I1_BASE + 14       ' Byte: MODE_*
Symbol EE_I1_RLY_PLP    = EE_I1_BASE + 15       ' Byte: MODE_*
Symbol EE_I1_RLY_SLP    = EE_I1_BASE + 16       ' Byte: MODE_*
Symbol EE_I1_DISPLAY    = EE_I1_BASE + 17       ' Byte: YES/NO

' RAM mirrors for Input1 (this pass implements Pressure)
Dim B_I1_Enabled  As Byte
Dim B_I1_SensorT  As Byte
Dim B_I1_FlowMode As Byte
Dim W_I1_Scale4   As Word
Dim W_I1_Scale20  As Word
Dim W_I1_BP_High  As Word
Dim W_I1_BP_PLP   As Word
Dim W_I1_BP_SLP   As Word
Dim B_I1_RlyHigh  As Byte
Dim B_I1_RlyPLP   As Byte
Dim B_I1_RlySLP   As Byte
Dim B_I1_Display  As Byte


' 32MHz => Fosc/4 = 8MHz => 0.125us per tick
' 8-bit, prescaler 1:32 => 4us per tick. Need ~1ms => 250 ticks.
' Preload so overflow in ~1ms: 256 - 250 = 6

'--- Configure Timer0 for ~1ms tick @ 32MHz (adjust as required)
INTCONbits_T0IF = 0
INTCONbits_T0IE = 1
INTCONbits_GIE  = 1
T0CONbits_TMR0ON = 1



Symbol RBIF = INTCON.0   ' RB Port Interrupt Flag
Symbol INT0IF = INTCON.1 ' INT0 External Interrupt Flag
Symbol TMR0IF = INTCON.2 ' TMR0 Overflow Interrupt Flag
Symbol RBIE = INTCON.3   ' RB Port Change Interrupt Enable
Symbol INT0IE = INTCON.4 ' INT0 External Interrupt Enable
Symbol TMR0IE = INTCON.5 ' TMR0 Overflow Interrupt Enable
Symbol PEIE = INTCON.6   ' Peripheral Interrupt Enable
Symbol GIEL = INTCON.6   ' Peripheral Interrupt Enable
Symbol GIE = INTCON.7    ' Global Interrupt Enable
Symbol GIEH = INTCON.7   ' Global Interrupt Enable

T0CON = %11000100          ' TMR0 ON, 8-bit, prescale 1:32 @ 32MHz
TMR0L = 6
Clear TMR0IF
TMR0IE = 1
GIE = 1

Clear INTCONbits_T0IF


'=====================================================================
'                TIMER0 TICK (1ms) AND INITIALISATION
'=====================================================================


'--- ISR
On_Hardware_Interrupt GoTo Isr



GoTo Over_Interrupt


Isr:
    Context Save

    If TMR0IF = 1 Then
        TMR0L = 6
        Clear TMR0IF
        Inc L_Millis

        '--- 10ms debounce/sample bucket
        Inc B_RE_Count
        If B_RE_Count > 9 Then
            Dim B_NewA As Byte
            Dim B_NewB As Byte
            Dim B_NewBtn As Byte

            B_NewA   = _ENC_A
            B_NewB   = _ENC_B
            B_NewBtn = _ENC_SW

            ' Debounce A
            If B_NewA <> B_AState Then
                Inc B_DebA
                If B_DebA >= 2 Then
                    B_AState = B_NewA
                    B_DebA = 0
                EndIf
            Else
                B_DebA = 0
            EndIf

            ' Debounce B
            If B_NewB <> B_BState Then
                Inc B_DebB
                If B_DebB >= 2 Then
                    B_BState = B_NewB
                    B_DebB = 0
                EndIf
            Else
                B_DebB = 0
            EndIf

            ' Debounce Button
            If B_NewBtn <> B_ButtonState Then
                Inc B_DebBtn
                If B_DebBtn >= 2 Then
                    B_ButtonState = B_NewBtn
                    B_DebBtn = 0
                    L_LastInput = L_Millis
                EndIf
            Else
                B_DebBtn = 0
            EndIf

            ' Quadrature accumulate
            Dim B_Curr As Byte
            B_Curr = (B_AState * 2) + B_BState

            Dim B_Combined As Byte
            B_Combined = (B_LastState * 4) + B_Curr

            Select B_Combined
                Case %0001, %0111, %1110, %1000
                    Dec S_Qacc
                Case %0010, %1011, %1101, %0100
                    Inc S_Qacc
            EndSelect

            ' Commit on detent (00) with threshold
            If B_Curr = 0 Then
                If S_Qacc >= 2 Then
                    Inc W_EncoderPos
                    L_LastInput = L_Millis
                ElseIf S_Qacc <= -2 Then
                    Dec W_EncoderPos
                    L_LastInput = L_Millis
                EndIf
                S_Qacc = 0
            EndIf

            B_LastState = B_Curr
            Clear B_RE_Count
        EndIf

        '--- Beeper (uses your W_Beep contract)
        If W_Beep > 0 Then
            _BUZZER = 1
            Dec W_Beep
        Else
            _BUZZER = 0
        EndIf

        '--- Button hold time & key event on release
        If B_ButtonState = 0 Then                     ' pressed (active-low)
            If W_BtnHoldMS < 65535 Then Inc W_BtnHoldMS
        Else                                          ' released: classify
            If W_BtnHoldMS > 0 Then
                If W_BtnHoldMS >= VERY_LONG_MS Then
                    B_KeyEvent = 3
                ElseIf W_BtnHoldMS >= LONG_MS Then
                    B_KeyEvent = 2
                ElseIf W_BtnHoldMS >= BTN_SHORT_MS Then
                    B_KeyEvent = 1
                EndIf
                L_LastInput = L_Millis
                W_BtnHoldMS = 0
            EndIf
        EndIf
    EndIf

    Context Restore

'=====================================================================
Over_Interrupt:

Cls
Print At 1,1,"Startup OK"
HRSOut "Startup OK",13
DelayMS 1000


'--- GPIO directions
Proc P_PinInit()
    TRISA = %00010000        ' RA4 input, others outputs for LCD
    TRISB = %01000110        ' RB6,RB2,RB1 inputs (SW,B,A)
    TRISC = %00000000        ' RC2 buzzer output; others as required
    ' Enable weak pullups if needed for BTN/ENC via WPUx (device-dependent)
EndProc

Proc P_InputInit()
    B_AState      = _ENC_A
    B_BState      = _ENC_B
    B_ButtonState = _ENC_SW
    B_LastState   = (B_AState * 2) + B_BState
    B_DebA = 0 : B_DebB = 0 : B_DebBtn = 0
    B_RE_Count = 0
    S_Qacc = 0
    W_EncoderPos = 0
    W_EncReadPos = 0
    W_BtnHoldMS = 0
    b_Long = 0 : b_VLong = 0
EndProc
'---------------------------------------------------------------------

'=====================================================================
'                          UTILS / HELPERS
'=====================================================================
'--- Start a beep for W_Dur ms (0..65535)
Proc P_BeepMs(W_Dur As Word)
    GIE = 0
    W_Beep = W_Dur
    GIE = 1
EndProc

'--- Stop the beep immediately
Proc P_BeepOff()
    GIE = 0
    W_Beep = 0
    GIE = 1
EndProc
'---------------------------------------------------------------------
'--- Beep presets and simple patterns
Proc P_Beeps(B_Type As Byte)
    Select B_Type
        Case 1
            ' Very short chirp (audibility depends on transducer; 2ms is often too short)
            P_BeepMs(20)

        Case 2
            ' Short confirm
            P_BeepMs(50)

        Case 3
            ' Medium beep (~200ms)
            P_BeepMs(200)

        Case 4
            ' Long beep (1 second)
            P_BeepMs(1000)

        Case 5
            ' Double short beep (blocking sequence)
            P_BeepMs(60)
            While W_Beep > 0 : Wend
            DelayMS 80
            P_BeepMs(60)

        Case 6
            ' Triple quick beep (blocking sequence)
            P_BeepMs(40) : While W_Beep > 0 : Wend
            DelayMS 60
            P_BeepMs(40) : While W_Beep > 0 : Wend
            DelayMS 60
            P_BeepMs(40)

        Case Else
            ' Default: tiny chirp to indicate “unknown type”
            P_BeepMs(20)
    EndSelect
EndProc
'---------------------------------------------------------------------
Proc P_Startup()
    Dim B_Beepcount As Byte
    For B_Beepcount=0 To 5
        P_Beeps(2)
        DelayMS 100    
    Next B_Beepcount
EndProc
'---------------------------------------------------------------------
'--- Delay helper (ms)
Proc DelayMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
EndProc
'---------------------------------------------------------------------
'--- Clear a LCD line (1..4)
Proc P_ClearLine(B_Row As Byte)
    Print At B_Row,1, "                    "   ' 20 spaces
EndProc
'---------------------------------------------------------------------
'--- Draw fixed title on line 1 (exactly 20 cols)
Proc P_DrawTitle(S_Title As String)
    Print At 1,1,"                    "    
    Print At 1,1,S_Title
EndProc
'---------------------------------------------------------------------
'--- Print a bracketed row at given LCD line (2..4). If active, prefix "[" else space.
Proc P_PrintRow(B_Row As Byte, S_Text As String, B_Active As Byte)
    Print At B_Row,1,"                    "
    If B_Active=1 Then
        Print At B_Row,1,"[", S_Text, "]"   
    Else
        Print At B_Row,2,S_Text    
    EndIf
EndProc
'---------------------------------------------------------------------
'--- Format mm:ss from seconds (0..5999 => 99:59)
Proc P_PrintMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word, B_Sec As Word
    B_Min = W_Seconds / 60
    B_Sec = W_Seconds // 60
    Locate B_Row, B_Col
    If B_Min < 10 Then
        Print "0";Dec B_Min;":"
    Else
        Print Dec B_Min;":"
    EndIf
    If B_Sec < 10 Then
        Print "0";Dec B_Sec
    Else
        Print Dec B_Sec
    EndIf
EndProc
'---------------------------------------------------------------------
'--- Parse mm:ss editor state -> Word seconds
Proc P_MMSS_ToSecs(B_Min As Byte, B_Sec As Byte), Word
    Result = (B_Min * 60) + B_Sec
EndProc
'---------------------------------------------------------------------
'--- Clamp helper
Proc P_ClampW(ByRef W_Val As Word, W_Min As Word, W_Max As Word)
    If W_Val < W_Min Then W_Val = W_Min
    If W_Val > W_Max Then W_Val = W_Max
EndProc

'=====================================================================
'                    EEPROM READ / WRITE WRAPPERS
'=====================================================================

Proc EWriteB(W_Addr As Word, B_Val As Byte)
    If ERead W_Addr <> B_Val Then EWrite W_Addr, B_Val
EndProc
'---------------------------------------------------------------------
Proc EWriteW(W_Addr As Word, W_Val As Word)
    Dim B_Lo As Byte, B_Hi As Byte
    B_Lo = Low W_Val
    B_Hi = High W_Val
    If ERead W_Addr <> B_Lo Then EWrite W_Addr, B_Lo
    If ERead (W_Addr + 1) <> B_Hi Then EWrite (W_Addr + 1), B_Hi
EndProc
'---------------------------------------------------------------------
Proc EWriteL(W_Addr As Word, L_Val As Dword)
    Dim B_B0 As Byte, B_B1 As Byte, B_B2 As Byte, B_B3 As Byte
    B_B0 = L_Val & $FF
    B_B1 = (L_Val / 256) & $FF
    B_B2 = (L_Val / 65536) & $FF
    B_B3 = (L_Val / 16777216) & $FF
    If ERead W_Addr <> B_B0 Then EWrite W_Addr, B_B0
    If ERead (W_Addr + 1) <> B_B1 Then EWrite (W_Addr + 1), B_B1
    If ERead (W_Addr + 2) <> B_B2 Then EWrite (W_Addr + 2), B_B2
    If ERead (W_Addr + 3) <> B_B3 Then EWrite (W_Addr + 3), B_B3
EndProc
'---------------------------------------------------------------------
' Load settings (only Input1 for now)
Proc P_LoadSettings()
    B_I1_Enabled  = ERead EE_I1_ENABLED
    B_I1_SensorT  = ERead EE_I1_SENSORT
    B_I1_FlowMode = ERead EE_I1_FLOWMODE

    W_I1_Scale4   = ERead EE_I1_SCALE4
    W_I1_Scale20  = ERead EE_I1_SCALE20

    W_I1_BP_High  = ERead EE_I1_BP_HIGH
    W_I1_BP_PLP   = ERead EE_I1_BP_PLP
    W_I1_BP_SLP   = ERead EE_I1_BP_SLP

    B_I1_RlyHigh  = ERead EE_I1_RLY_HIGH
    B_I1_RlyPLP   = ERead EE_I1_RLY_PLP
    B_I1_RlySLP   = ERead EE_I1_RLY_SLP
    B_I1_Display  = ERead EE_I1_DISPLAY
EndProc


Proc P_SaveSettings()
    EWrite EE_I1_ENABLED, [B_I1_Enabled]
    EWrite EE_I1_SENSORT, [B_I1_SensorT]
    EWrite EE_I1_FLOWMODE, [B_I1_FlowMode]
    EWrite EE_I1_SCALE4, [W_I1_Scale4]
    EWrite EE_I1_SCALE20, [W_I1_Scale20]
    EWrite EE_I1_BP_HIGH, [W_I1_BP_High]
    EWrite EE_I1_BP_PLP,  [W_I1_BP_PLP]
    EWrite EE_I1_BP_SLP,  [W_I1_BP_SLP]
    EWrite EE_I1_RLY_HIGH, [B_I1_RlyHigh]
    EWrite EE_I1_RLY_PLP,  [B_I1_RlyPLP]
    EWrite EE_I1_RLY_SLP,  [B_I1_RlySLP]
    EWrite EE_I1_DISPLAY,  [B_I1_Display]
EndProc

'=====================================================================
'                          INPUT / EVENTS
'=====================================================================

'--- Read encoder once and produce delta -1/0/+1
Proc P_ReadEncoder()
    Dim W_Pos As Word
    W_Pos = W_EncoderPos

    If W_Pos > W_EncReadPos Then
        B_EncDelta = 1
    ElseIf W_Pos < W_EncReadPos Then
        B_EncDelta = -1
    Else
        B_EncDelta = 0
    EndIf

    W_EncReadPos = W_Pos
EndProc

'---------------------------------------------------------------------
'--- Read button and set key events (edge/level with duration)
'Proc P_ReadButton()
'    Dim b_BtnDown As Bit
    
'    If BTN = 0 Then           ' active-low -> pressed
'        b_BtnDown = 1
'    Else
'        b_BtnDown = 0
'    EndIf

'    If b_BtnDown = 1 Then
'        If b_BtnLast = 0 Then
'            L_BtnDownMs = L_Millis
'        EndIf
'        b_BtnLast = 1
'    Else
'        If b_BtnLast = 1 Then
'            Dim L_Dur As Dword
'            L_Dur = L_Millis - L_BtnDownMs
'            If L_Dur >= BTN_VLONG_MS Then
'                B_KeyEvent = 3
'            ElseIf L_Dur >= BTN_LONG_MS Then
'                B_KeyEvent = 2
'            ElseIf L_Dur >= BTN_SHORT_MS Then
'                B_KeyEvent = 1
'            Else
'                B_KeyEvent = 0
'            EndIf
'            If B_KeyEvent <> 0 Then L_LastInput = L_Millis
'        EndIf
'    b_BtnLast = 0
'EndIf
'EndProc
'---------------------------------------------------------------------
'--- Consume key event (returns and clears)
Proc P_GetKeyEvent(), Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
EndProc

'--- Inactivity/escape handler: returns non-zero if user wants to unwind
Proc P_UserAborted(), Byte
    Dim B_Ev As Byte
    B_Ev = P_GetKeyEvent()
    If B_Ev = 2 Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf ' long
    If B_Ev = 3 Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf ' very long
    If (L_Millis - L_LastInput) >= UI_TIMEOUT_MS Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf
    Result = 0
    Exit_P_UserAborted:
EndProc

'=====================================================================
'                          EDITORS
'=====================================================================

'--- Yes/No editor
Proc P_EditYN(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: YES/NO")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            If B_Cur = YES Then
                Print At 3,1,"[Yes]  No           "
            Else
                Print At 3,1," Yes  [No]          "
            EndIf
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta <> 0 Then
            If B_Cur = YES Then
                B_Cur = NO
            Else
                B_Cur = YES
            EndIf
            Set b_ScrDirty
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                B_Val = B_Cur
                Result = 1
                GoTo Exit_P_EditYN
            Case 2
                Result = 0
                GoTo Exit_P_EditYN
            Case 3
                Result = 0
                GoTo Exit_P_EditYN
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_P_EditYN
        EndIf
    Wend
    Exit_P_EditYN:
EndProc
'---------------------------------------------------------------------
'--- Enum3 editor: No/Pulse/Latch
Proc P_EditEnum3(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: OUTPUT MODE")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            Select B_Cur
                Case MODE_NO
                    Print At 3,1, "[No]  Pulse  Latch    "
                Case MODE_PULSE
                    Print At 3,1, " No  [Pulse] Latch    "
                Case MODE_LATCH
                    Print At 3,1, " No   Pulse [Latch]  "
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Cur < MODE_LATCH Then B_Cur = B_Cur + 1
            Set b_ScrDirty
        ElseIf B_EncDelta = -1 Then
            If B_Cur > MODE_NO Then B_Cur = B_Cur - 1
            Set b_ScrDirty
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                B_Val = B_Cur
                Result = 1
                GoTo Exit_P_EditEnum3
            Case 2
                Result = 0
                GoTo Exit_P_EditEnum3
            Case 3
                Result = 0
                GoTo Exit_P_EditEnum3
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_P_EditEnum3
        EndIf
    Wend
    Exit_P_EditEnum3:
EndProc
'---------------------------------------------------------------------
'--- Word editor (numeric up/down)
Proc P_EditWordVal(ByRef W_Val As Word, W_Min As Word, W_Max As Word, W_Step As Word), Byte
    Dim W_Cur As Word
    W_Cur = W_Val
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: WORD VALUE")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            'Locate 3, 1
            Print At 3,1, " Value: [",Dec5 W_Cur,"]         "
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If W_Cur + W_Step <= W_Max Then W_Cur = W_Cur + W_Step
            Set b_ScrDirty
        ElseIf B_EncDelta = -1 Then
            If W_Cur >= (W_Min + W_Step) Then
                W_Cur = W_Cur - W_Step
            Else
                W_Cur = W_Min
            EndIf
            Set b_ScrDirty
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                W_Val = W_Cur
                Result = 1
                GoTo Exit_P_EditWordVal
            Case 2
                Result = 0
                GoTo Exit_P_EditWordVal
            Case 3
                Result = 0
                GoTo Exit_P_EditWordVal
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_P_EditWordVal
        EndIf
    Wend
    Exit_P_EditWordVal:
EndProc
'---------------------------------------------------------------------
'--- mm:ss editor (0..99:59). Short=toggle field/save, Long=cancel
Proc P_EditMMSS(ByRef W_Val As Word), Byte
    Dim B_Min As Byte, B_Sec As Byte, B_Field As Byte
    B_Min = W_Val / 60     ' minutes
    B_Sec = W_Val // 60    ' remainder seconds (mod 60)
    B_Field = 0                                   ' 0=mm,1=ss
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: DURATION")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            ' inside P_EditMMSS redraw:
            If B_Field = 0 Then
                Print At 3,1," [",Dec2 B_Min,"] : ",Dec2 B_Sec,"          "
            Else
                Print At 3,1,"  ",Dec2 B_Min," : [",Dec2 B_Sec,"]         "
            EndIf
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Field = 0 Then
                If B_Min < 99 Then Inc B_Min
            Else
                If B_Sec < 59 Then Inc B_Sec
            EndIf
            Set b_ScrDirty
        ElseIf B_EncDelta = -1 Then
            If B_Field = 0 Then
                If B_Min > 0 Then Dec B_Min
            Else
                If B_Sec > 0 Then Dec B_Sec
            EndIf
            Set b_ScrDirty
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                If B_Field = 0 Then
                    B_Field = 1
                    Set b_ScrDirty
                Else
                    W_Val = P_MMSS_ToSecs(B_Min, B_Sec)
                    Result = 1
                    GoTo Exit_P_EditMMSS
                EndIf
            Case 2
                Result = 0
                GoTo Exit_P_EditMMSS
            Case 3
                Result = 0
                GoTo Exit_P_EditMMSS
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_P_EditMMSS
        EndIf
    Wend
    Exit_P_EditMMSS:
EndProc

'=====================================================================
'                      SCALING / OUTPUT STUBS
'=====================================================================

Proc P_Scale()
    ' Recompute any derived factors after changing scale values
    ' Placeholder for now
EndProc

Proc P_Output()
    ' Apply output mode changes to hardware (relays, etc.)
    ' Placeholder for now
EndProc

'=====================================================================
'                           MENUS / VIEWS
'=====================================================================

'--- Main screen
Proc V_Main()
    b_ReInitLCD = 0
    P_DrawTitle("IRRISYS MAIN        ")
    P_ClearLine(2)
    P_ClearLine(3)
    P_ClearLine(4)
    'Locate 3,1 : Print "Press to open menu  "
    Set b_ScrDirty
    While 1=1
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_Main                        ' short -> leave to caller
            Case 2
                ' stay on main
            Case 3
                ' stay on main
        EndSelect
        ' Minimal standby updates could be added here
    Wend
Exit_V_Main:
EndProc
'---------------------------------------------------------------------
'--- Options menu (3-row window example)
Proc V_Options(), Byte
    Dim B_Sel As Byte, B_Top As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3                                     ' Main Menu, Setup Menu, Utility
    B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("OPTIONS             ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)
            ' compute window
            If B_Sel < B_Top Then
                B_Top = B_Sel
            EndIf
            If B_Sel > (B_Top + 2) Then
                B_Top = B_Sel - 2
            EndIf
            ' render items
            Select B_Top
                Case 0
                    If B_Sel=0 Then
                        P_PrintRow(2, "Main Menu", 1)
                    Else
                        P_PrintRow(2, "Main Menu", 0)
                    EndIf
 
                    If B_Sel=0 Then
                        P_PrintRow(3, "Setup Menu", 1)
                    Else
                        P_PrintRow(3, "Setup Menu", 0)
                    EndIf 
 
                    If B_Sel=0 Then
                        P_PrintRow(4, "Utility Menu", 1)
                    Else
                        P_PrintRow(4, "Utility Menu", 0)
                    EndIf 
                Case Else
                    ' not needed for 3 items
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt-1) Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then
                Dec B_Sel
                Set b_ScrDirty
            EndIf
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        Result = 1
                        GoTo Exit_V_Options          ' go to Main Menu (upper)
                    Case 1
                        If V_SetupMenu() = 0 Then
                            Result = 0
                            GoTo Exit_V_Options
                        EndIf
                    Case 2
                        ' Utility not implemented yet
                        ' Placeholder: just show a stub screen
                        V_UtilityStub()
                EndSelect
                Set b_ScrDirty
            Case 2
                Result = 0
                GoTo Exit_V_Options                        ' unwind
            Case 3
                Result = 0
                GoTo Exit_V_Options
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_V_Options
        EndIf
    Wend
    Exit_V_Options:
EndProc
'---------------------------------------------------------------------
Proc V_UtilityStub()
    b_ReInitLCD = 0
    P_DrawTitle("UTILITY (STUB)      ")
    P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
    'Locate 3,1: Print "Coming soon...      "
    While 1=1
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_UtilityStub
            Case 2
                GoTo Exit_V_UtilityStub
            Case 3
                GoTo Exit_V_UtilityStub
        EndSelect
        If P_UserAborted() <> 0 Then GoTo Exit_V_UtilityStub
    Wend
    Exit_V_UtilityStub:
EndProc

'--- Setup Menu: Choose Input 1/2/3 (and Clock off-screen later)
Proc V_SetupMenu(), Byte
    Dim B_Sel As Byte, B_Top As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3                                     ' Input1..3 for this pass
    B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("SETUP               ")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            If B_Sel < B_Top Then B_Top = B_Sel
            If B_Sel > (B_Top + 2) Then B_Top = B_Sel - 2
            Select B_Top
                Case 0
                    If B_Sel = 0 Then
                        P_PrintRow(2, "Input 1", 1)
                    Else
                        P_PrintRow(2, "Input 1", 0)
                    EndIf
            
                    If B_Sel = 1 Then
                        P_PrintRow(3, "Input 2", 1)
                    Else
                        P_PrintRow(3, "Input 2", 0)
                    EndIf
            
                    If B_Sel = 2 Then
                        P_PrintRow(4, "Input 3", 1)
                    Else
                        P_PrintRow(4, "Input 3", 0)
                    EndIf
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt-1) Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then
                Dec B_Sel
                Set b_ScrDirty
            EndIf
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        If V_Input1Menu() = 0 Then
                            Result = 0
                            GoTo Exit_V_SetupMenu
                        EndIf
                    Case 1
                        V_NotImpl("INPUT 2")
                    Case 2
                        V_NotImpl("INPUT 3")
                EndSelect
                Set b_ScrDirty
            Case 2
                Result = 0
                GoTo Exit_V_SetupMenu
            Case 3
                Result = 0
                GoTo Exit_V_SetupMenu
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_V_SetupMenu
        EndIf
    Wend
    Exit_V_SetupMenu:
EndProc

Proc V_NotImpl(S_Name As String)
    b_ReInitLCD = 0
    P_DrawTitle(S_Name + " (STUB)      ")
    P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
    'Locate 3,1: Print "Not implemented     "
    While 1=1
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_NotImpl
            Case 2
                GoTo Exit_V_NotImpl
            Case 3
                GoTo Exit_V_NotImpl
        EndSelect
        If P_UserAborted() <> 0 Then GoTo Exit_V_NotImpl
    Wend
    Exit_V_NotImpl:
EndProc
'---------------------------------------------------------------------
'--- Input 1 menu: Enable + Sensor -> Pressure flow (this pass: Pressure)
Proc V_Input1Menu(), Byte
    Dim B_Sel As Byte
    B_Sel = 0
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1=1
    If b_ScrDirty = 1 Then
        P_DrawTitle("INPUT 1             ")
        P_ClearLine(2)
        P_ClearLine(3)
        P_ClearLine(4)
    
        If B_Sel = 0 Then
            P_PrintRow(2, "Enable", 1)
        Else
            P_PrintRow(2, "Enable", 0)
        EndIf
    
        If B_Sel = 1 Then
            P_PrintRow(3, "Sensor Type", 1)
        Else
            P_PrintRow(3, "Sensor Type", 0)
        EndIf
    
        If B_Sel = 2 Then
            P_PrintRow(4, "Edit Params", 1)
        Else
            P_PrintRow(4, "Edit Params", 0)
        EndIf
    
        b_ScrDirty = 0
    EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < 2 Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then
                Dec B_Sel
                Set b_ScrDirty
            EndIf
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        If P_EditYN(B_I1_Enabled) = 1 Then P_SaveSettings
                    Case 1
                        If V_Input1Sensor() = 0 Then
                            Result = 0
                            GoTo Exit_V_Input1Menu
                        EndIf
                    Case 2
                        If B_I1_Enabled = YES Then
                            Select B_I1_SensorT
                                Case SENSOR_PRES
                                    If V_I1_Pressure() = 0 Then
                                        Result = 0
                                        GoTo Exit_V_Input1Menu
                                    EndIf
                                Case SENSOR_FLOW
                                    V_NotImpl("FLOW EDIT")
                                Case SENSOR_TEMP
                                    V_NotImpl("TEMP EDIT")
                            EndSelect
                        Else
                            V_NotImpl("ENABLE INPUT FIRST")
                        EndIf
                EndSelect
                Set b_ScrDirty
            Case 2
                Result = 0
                GoTo Exit_V_Input1Menu
            Case 3
                Result = 0
                GoTo Exit_V_Input1Menu
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_V_Input1Menu
        EndIf
    Wend
    Exit_V_Input1Menu:
EndProc

'--- Choose sensor type for Input1 (Flow/Pressure/Temperature)
Proc V_Input1Sensor(), Byte
    Dim B_Sel As Byte
    B_Sel = B_I1_SensorT
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("SENSOR TYPE         ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            If B_Sel = 0 Then
                P_PrintRow(2, "Flow", 1)
            Else
                P_PrintRow(2, "Flow", 0)
            EndIf

            If B_Sel = 1 Then
                P_PrintRow(3, "Pressure", 1)
            Else
                P_PrintRow(3, "Pressure", 0)
            EndIf

            If B_Sel = 2 Then
                P_PrintRow(4, "Temperature", 1)
            Else
                P_PrintRow(4, "Temperature", 0)
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < 2 Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then
            Dec B_Sel
            Set b_ScrDirty
        EndIf
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                B_I1_SensorT = B_Sel
                P_SaveSettings()
                Result = 1
                GoTo Exit_V_In1Sens
            Case 2
                Result = 0
                GoTo Exit_V_In1Sens
            Case 3
                Result = 0
                GoTo Exit_V_In1Sens
        EndSelect
        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_V_In1Sens
        EndIf
    Wend
    Exit_V_In1Sens:
EndProc
'---------------------------------------------------------------------
'--- Input1 Pressure editor list (subset implemented)
Proc V_I1_Pressure(), Byte
    Dim B_Sel As Byte, B_Cnt As Byte
    B_Sel = 0
    B_Cnt = 8                                      ' up to Display; extend later
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("I1 PRESSURE         ")
            P_ClearLine(2): P_ClearLine(3): P_ClearLine(4)
            ' Render 3-row list window based on B_Sel
            Dim B_Top As Byte
            If B_Sel <= 1 Then
                B_Top = 0
            ElseIf B_Sel = 2 Then
                B_Top = 1
            ElseIf B_Sel = 3 Then
                B_Top = 2
            ElseIf B_Sel = 4 Then
                B_Top = 3
            ElseIf B_Sel = 5 Then
                B_Top = 4
            ElseIf B_Sel = 6 Then
                B_Top = 5
            Else
                B_Top = B_Sel-2
            EndIf
            ' Lines text (fixed labels, values drawn inline where useful)
            Select B_Top
                Case 0
                    ' Row 2: Scale 4mA
                    If B_Sel = 0 Then
                        Print At 2,1,"[Scale 4ma   [", Dec W_I1_Scale4, "]"
                    Else
                        Print At 2,2,"Scale 4ma   [", Dec W_I1_Scale4, "]"
                    EndIf
            
                    ' Row 3: Scale 20mA
                    If B_Sel = 1 Then
                        Print At 3,1,"[Scale 20ma  [", Dec W_I1_Scale20, "]"
                    Else
                        Print At 3,2,"Scale 20ma  [", Dec W_I1_Scale20, "]"
                    EndIf
            
                    ' Row 4: High BP (static text)
                    If B_Sel = 2 Then
                        P_PrintRow(4, "High BP     [mm:ss]", 1)
                    Else
                        P_PrintRow(4, "High BP     [mm:ss]", 0)
                    EndIf
            
                Case 1
                    ' Row 2: Scale 20mA
                    If B_Sel = 1 Then
                        Print At 2,1,"[Scale 20ma  [", Dec W_I1_Scale20, "]"
                    Else
                        Print At 2,2,"Scale 20ma  [", Dec W_I1_Scale20, "]"
                    EndIf
            
                    ' Row 3/4: static text
                    If B_Sel = 2 Then
                        P_PrintRow(3, "High BP     [mm:ss]", 1)
                    Else
                        P_PrintRow(3, "High BP     [mm:ss]", 0)
                    EndIf
            
                    If B_Sel = 3 Then
                        P_PrintRow(4, "PLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(4, "PLPBP       [mm:ss]", 0)
                    EndIf
            
                Case 2
                    If B_Sel = 2 Then
                        P_PrintRow(2, "High BP     [mm:ss]", 1)
                    Else
                        P_PrintRow(2, "High BP     [mm:ss]", 0)
                    EndIf
            
                    If B_Sel = 3 Then
                        P_PrintRow(3, "PLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(3, "PLPBP       [mm:ss]", 0)
                    EndIf
            
                    If B_Sel = 4 Then
                        P_PrintRow(4, "SLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(4, "SLPBP       [mm:ss]", 0)
                    EndIf
            
                Case 3
                    If B_Sel = 3 Then
                        P_PrintRow(2, "PLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(2, "PLPBP       [mm:ss]", 0)
                    EndIf
                    If B_Sel = 4 Then
                        P_PrintRow(3, "SLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(3, "SLPBP       [mm:ss]", 0)
                    EndIf
                    If B_Sel = 5 Then
                        P_PrintRow(4, "Rly High    [L/P/No]", 1)
                    Else
                        P_PrintRow(4, "Rly High    [L/P/No]", 0)
                    EndIf
            
                Case 4
                    If B_Sel = 4 Then
                        P_PrintRow(2, "SLPBP       [mm:ss]", 1)
                    Else
                        P_PrintRow(2, "SLPBP       [mm:ss]", 0)
                    EndIf
                    If B_Sel = 5 Then
                        P_PrintRow(3, "Rly High    [L/P/No]", 1)
                    Else
                        P_PrintRow(3, "Rly High    [L/P/No]", 0)
                    EndIf
                    If B_Sel = 6 Then
                        P_PrintRow(4, "Rly PLB     [L/P/No]", 1)
                    Else
                        P_PrintRow(4, "Rly PLB     [L/P/No]", 0)
                    EndIf
            
                Case 5
                    If B_Sel = 5 Then
                        P_PrintRow(2, "Rly High    [L/P/No]", 1)
                    Else
                        P_PrintRow(2, "Rly High    [L/P/No]", 0)
                    EndIf
                    If B_Sel = 6 Then
                        P_PrintRow(3, "Rly PLB     [L/P/No]", 1)
                    Else
                        P_PrintRow(3, "Rly PLB     [L/P/No]", 0)
                    EndIf
                    If B_Sel = 7 Then
                        P_PrintRow(4, "Rly SLP     [L/P/No]", 1)
                    Else
                        P_PrintRow(4, "Rly SLP     [L/P/No]", 0)
                    EndIf
            
                Case Else
                    If B_Sel = 6 Then
                        P_PrintRow(2, "Rly PLB     [L/P/No]", 1)
                    Else
                        P_PrintRow(2, "Rly PLB     [L/P/No]", 0)
                    EndIf
                    If B_Sel = 7 Then
                        P_PrintRow(3, "Rly SLP     [L/P/No]", 1)
                    Else
                        P_PrintRow(3, "Rly SLP     [L/P/No]", 0)
                    EndIf
                    If B_Sel = 8 Then
                        P_PrintRow(4, "Display     [Yes/No]", 1)
                    Else
                        P_PrintRow(4, "Display     [Yes/No]", 0)
                    EndIf
            EndSelect

            b_ScrDirty = 0
        EndIf
        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt-1) Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then
                Dec B_Sel
                Set b_ScrDirty
            EndIf
        EndIf
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        If P_EditWordVal(W_I1_Scale4, 0, 65535, 1) = 1 Then
                            P_Scale()
                            P_SaveSettings()
                        EndIf
                    Case 1
                        If P_EditWordVal(W_I1_Scale20, 0, 65535, 1) = 1 Then
                            P_Scale()
                            P_SaveSettings()
                        EndIf
                    Case 2
                        If P_EditMMSS(W_I1_BP_High) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 3
                        If P_EditMMSS(W_I1_BP_PLP) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 4
                        If P_EditMMSS(W_I1_BP_SLP) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 5
                        If P_EditEnum3(B_I1_RlyHigh) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 6
                        If P_EditEnum3(B_I1_RlyPLP) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 7
                        If P_EditEnum3(B_I1_RlySLP) = 1 Then
                            P_Output()
                            P_SaveSettings()
                        EndIf
                    Case 8
                        If P_EditYN(B_I1_Display) = 1 Then
                            P_SaveSettings()
                        EndIf
                EndSelect
                Set b_ScrDirty
            Case 2
                Result = 0
                GoTo Exit_V_I1_Pressure
            Case 3
                Result = 0
                GoTo Exit_V_I1_Pressure
        EndSelect

        If P_UserAborted() <> 0 Then
            Result = 0
            GoTo Exit_V_I1_Pressure
        EndIf
    Wend
    Exit_V_I1_Pressure:
EndProc
'---------------------------------------------------------------------


'--- LCD safe init wrapper
Proc P_LCDSafeInit()
    DelayMsFast(50)
    Cls
    DelayMsFast(10)
EndProc

'=====================================================================
'                              MAIN
'=====================================================================
MAIN:
P_PinInit()
P_LCDSafeInit()
P_InputInit()

' Load settings once at boot
P_LoadSettings()

' Init UI state
b_ScrDirty = 1
b_ReInitLCD = 0
b_Escape = 0
b_BtnLast = 0
B_KeyEvent = 0
B_EncPrev = 0
L_LastInput = 0

' Optional defaults if EEPROM blank
If B_I1_SensorT > 2 Then
    B_I1_SensorT = SENSOR_PRES
EndIf

P_Startup()



While 1 = 1
    ' MAIN SCREEN -> OPTIONS
    V_Main()
    If V_Options() = 0 Then
        ' unwind all the way to main (already here)
    EndIf
Wend

'============================== EOF ==================================

