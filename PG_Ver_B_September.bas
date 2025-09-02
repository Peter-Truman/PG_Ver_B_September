'****************************************************************
'*  Name    : UNTITLED.BAS                                      *
'*  Author  : Peter W Truman                                    *
'*  Notice  : Copyright (c) 2025 PCT Remote Sensing Pty Ltd     *
'*          : All Rights Reserved                               *
'*  Date    : 2/09/2025                                         *
'*  Version : 1.0                                               *
'*  Notes   :                                                   *
'*          :                                                   *
'****************************************************************
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

Xtal   = 32                                   ' MHz (adjust if needed)
All_Digital = True

'---------------------------------------------------------------------
' LCD (HD44780-compatible) 20x4 in 4-bit mode
' NOTE: Adjust pins to your hardware. Uses Positron's LCD driver.
' Safe to change here without touching code below.
Declare LCD_DTPort = PORT                     ' DB4..DB7 on RB4..RB7
Declare LCD_Interface = 4                      ' 4-bit interface
Declare LCD_Lines = 4
Declare LCD_RS_Pin = PORTA.0
Declare LCD_EN_Pin = PORTA.1
' If you wire RW, declare it; otherwise the driver uses timed delays.
' Declare LCD_RW_Pin = PORTA.2

'---------------------------------------------------------------------
' Encoder & button pins (adjust to your board)
Symbol ENC_A = PORTC.0
Symbol ENC_B = PORTC.1
Symbol BTN   = PORTC.2                         ' Active-low push button

'---------------------------------------------------------------------
' UART (optional debug; keep disabled by default to save flash)
' Declare Hserial_Baud = 115200
' Declare Hserial_RCSTA = %10010000
' Declare Hserial_TXSTA = %00100100

'---------------------------------------------------------------------
' Timing & UI constants
Const TICK_MS          = 1                     ' ISR tick period
Const BTN_SHORT_MS     = 200                   ' short press threshold
Const BTN_LONG_MS      = 900                   ' long press threshold
Const BTN_VLONG_MS     = 1800                  ' very long press threshold
Const UI_TIMEOUT_MS    = 30000                 ' inactivity timeout to unwind

'---------------------------------------------------------------------
' Enums and modes
Const MODE_NO    = 0
Const MODE_PULSE = 1
Const MODE_LATCH = 2

Const YES = 1
Const NO  = 0

Const SENSOR_FLOW = 0
Const SENSOR_PRES = 1
Const SENSOR_TEMP = 2

'---------------------------------------------------------------------
' Global state
Dim B_ReInitLCD  As Bit                       ' cleared at each menu entry
Dim B_ScrDirty   As Bit                       ' request redraw of current view
Dim B_Escape     As Bit                       ' shared escape flag for unwind

Dim L_Millis     As Dword                     ' millisecond counter
Dim L_LastInput  As Dword                     ' last input ms (activity)

' Encoder state
Dim B_EncPrev    As Byte
Dim B_EncNow     As Byte
Dim S_EncDelta   As SByte                     ' -1, 0, +1 per poll

' Button state
Dim B_BtnLast    As Byte                      ' 0=up,1=down
Dim L_BtnDownMs  As Dword
Dim B_KeyEvent   As Byte                      ' 0=none 1=short 2=long 3=very long

' Temp string helper
Dim S_T As String * 18

'---------------------------------------------------------------------
' EEPROM map (Input1 only for this pass; extend as needed)
' Offsets kept symbolic for clarity and easy expansion
Const EE_VER           = 0                     ' config version
Const EE_I1_BASE       = 10
Const EE_I1_ENABLED    = EE_I1_BASE + 0        ' Byte: 0/1
Const EE_I1_SENSORT    = EE_I1_BASE + 1        ' Byte: SENSOR_*
Const EE_I1_FLOWMODE   = EE_I1_BASE + 2        ' Byte: 0=meter 1=switch
Const EE_I1_SCALE4     = EE_I1_BASE + 4        ' Word
Const EE_I1_SCALE20    = EE_I1_BASE + 6        ' Word
Const EE_I1_BP_HIGH    = EE_I1_BASE + 8        ' Word seconds (mm:ss)
Const EE_I1_BP_PLP     = EE_I1_BASE + 10       ' Word seconds (mm:ss)
Const EE_I1_BP_SLP     = EE_I1_BASE + 12       ' Word seconds (mm:ss)
Const EE_I1_RLY_HIGH   = EE_I1_BASE + 14       ' Byte: MODE_*
Const EE_I1_RLY_PLP    = EE_I1_BASE + 15       ' Byte: MODE_*
Const EE_I1_RLY_SLP    = EE_I1_BASE + 16       ' Byte: MODE_*
Const EE_I1_DISPLAY    = EE_I1_BASE + 17       ' Byte: YES/NO

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

'=====================================================================
'                          UTILS / HELPERS
'=====================================================================

'--- Delay helper (ms)
Proc DelayMsFast(pMs As Word)
    Dim i As Word
    For i = 1 To pMs
        DelayMS 1
    Next
EndProc

'--- Clear a LCD line (1..4)
Proc P_ClearLine(pRow As Byte)
    Locate pRow, 1
    Print "                    "   ' 20 spaces
EndProc

'--- Draw fixed title on line 1 (exactly 20 cols)
Proc P_DrawTitle(ByRef pTitle As String)
    Dim S As String * 20
    S = Left$(pTitle + Space$(20), 20)
    Locate 1, 1
    Print S
EndProc

'--- Print a bracketed row at given LCD line (2..4). If active, prefix "[" else space.
Proc P_PrintRow(pRow As Byte, ByRef pText As String, pActive As Byte)
    Dim S As String * 20
    If pActive = 1 Then
        S = "[" + pText
    Else
        S = " " + pText
    EndIf
    S = Left$(S + Space$(20), 20)
    Locate pRow, 1
    Print S
EndProc

'--- Format mm:ss from seconds (0..5999 => 99:59)
Proc P_PrintMMSS(pRow As Byte, pCol As Byte, pSeconds As Word)
    Dim m As Word, s As Word
    m = pSeconds / 60
    s = pSeconds % 60
    Locate pRow, pCol
    If m < 10 Then
        Print "0";Dec m;":"
    Else
        Print Dec m;":"
    EndIf
    If s < 10 Then
        Print "0";Dec s
    Else
        Print Dec s
    EndIf
EndProc

'--- Parse mm:ss editor state -> Word seconds
Proc P_MMSS_ToSecs(m As Byte, s As Byte) As Word
    Result = (m * 60) + s
EndProc

'--- Request a redraw of the current screen
Proc P_RequestRedraw()
    B_ScrDirty = 1
EndProc

'--- Activity bump (resets idle timer)
Proc P_MarkActive()
    L_LastInput = L_Millis
EndProc

'--- Clamp helper
Proc P_ClampW(ByRef v As Word, mn As Word, mx As Word)
    If v < mn Then: v = mn : EndIf
    If v > mx Then: v = mx : EndIf
EndProc

'=====================================================================
'                    EEPROM READ / WRITE WRAPPERS
'=====================================================================

' Byte
Proc EReadB(addr As Word) As Byte
    Result = ERead addr
EndProc

Proc EWriteB(addr As Word, v As Byte)
    If ERead addr <> v Then: EWrite addr, v : EndIf
EndProc

' Word
Proc EReadW(addr As Word) As Word
    Dim lo As Byte, hi As Byte
    lo = ERead addr
    hi = ERead (addr + 1)
    Result = MakeWord(hi, lo)
EndProc

Proc EWriteW(addr As Word, v As Word)
    Dim lo As Byte, hi As Byte
    lo = Low v
    hi = High v
    If ERead addr <> lo Then: EWrite addr, lo : EndIf
    If ERead (addr + 1) <> hi Then: EWrite (addr + 1), hi : EndIf
EndProc

' Dword (Long)
Proc EReadL(addr As Word) As Dword
    Dim b0 As Byte, b1 As Byte, b2 As Byte, b3 As Byte
    b0 = ERead addr
    b1 = ERead (addr + 1)
    b2 = ERead (addr + 2)
    b3 = ERead (addr + 3)
    Result = (Dword)b3 << 24
    Result = Result | ((Dword)b2 << 16)
    Result = Result | ((Dword)b1 << 8)
    Result = Result | (Dword)b0
EndProc

Proc EWriteL(addr As Word, v As Dword)
    Dim b0 As Byte, b1 As Byte, b2 As Byte, b3 As Byte
    b0 = v & $FF
    b1 = (v >> 8) & $FF
    b2 = (v >> 16) & $FF
    b3 = (v >> 24) & $FF
    If ERead addr <> b0 Then: EWrite addr, b0 : EndIf
    If ERead (addr + 1) <> b1 Then: EWrite (addr + 1), b1 : EndIf
    If ERead (addr + 2) <> b2 Then: EWrite (addr + 2), b2 : EndIf
    If ERead (addr + 3) <> b3 Then: EWrite (addr + 3), b3 : EndIf
EndProc

' Load settings (only Input1 for now)
Proc P_LoadSettings()
    B_I1_Enabled = EReadB(EE_I1_ENABLED)
    B_I1_SensorT = EReadB(EE_I1_SENSORT)
    B_I1_FlowMode = EReadB(EE_I1_FLOWMODE)
    W_I1_Scale4 = EReadW(EE_I1_SCALE4)
    W_I1_Scale20 = EReadW(EE_I1_SCALE20)
    W_I1_BP_High = EReadW(EE_I1_BP_HIGH)
    W_I1_BP_PLP  = EReadW(EE_I1_BP_PLP)
    W_I1_BP_SLP  = EReadW(EE_I1_BP_SLP)
    B_I1_RlyHigh = EReadB(EE_I1_RLY_HIGH)
    B_I1_RlyPLP  = EReadB(EE_I1_RLY_PLP)
    B_I1_RlySLP  = EReadB(EE_I1_RLY_SLP)
    B_I1_Display = EReadB(EE_I1_DISPLAY)
EndProc

Proc P_SaveSettings()
    EWriteB EE_I1_ENABLED, B_I1_Enabled
    EWriteB EE_I1_SENSORT, B_I1_SensorT
    EWriteB EE_I1_FLOWMODE, B_I1_FlowMode
    EWriteW EE_I1_SCALE4, W_I1_Scale4
    EWriteW EE_I1_SCALE20, W_I1_Scale20
    EWriteW EE_I1_BP_HIGH, W_I1_BP_High
    EWriteW EE_I1_BP_PLP,  W_I1_BP_PLP
    EWriteW EE_I1_BP_SLP,  W_I1_BP_SLP
    EWriteB EE_I1_RLY_HIGH, B_I1_RlyHigh
    EWriteB EE_I1_RLY_PLP,  B_I1_RlyPLP
    EWriteB EE_I1_RLY_SLP,  B_I1_RlySLP
    EWriteB EE_I1_DISPLAY,  B_I1_Display
EndProc

'=====================================================================
'                          INPUT / EVENTS
'=====================================================================

'--- Read encoder once and produce delta -1/0/+1
Proc P_ReadEncoder()
    Dim a As Byte, b As Byte, state As Byte
    a = ENC_A
    b = ENC_B
    If a = 0 Then: a = 0 Else: a = 1 : EndIf
    If b = 0 Then: b = 0 Else: b = 1 : EndIf
    state = (a << 1) | b
    S_EncDelta = 0
    Select B_EncPrev
        Case 0
            If state = 1 Then: S_EncDelta = +1 : EndIf
            If state = 2 Then: S_EncDelta = -1 : EndIf
        Case 1
            If state = 3 Then: S_EncDelta = +1 : EndIf
            If state = 0 Then: S_EncDelta = -1 : EndIf
        Case 3
            If state = 2 Then: S_EncDelta = +1 : EndIf
            If state = 1 Then: S_EncDelta = -1 : EndIf
        Case 2
            If state = 0 Then: S_EncDelta = +1 : EndIf
            If state = 3 Then: S_EncDelta = -1 : EndIf
    EndSelect
    B_EncPrev = state
    If S_EncDelta <> 0 Then: P_MarkActive : EndIf
EndProc

'--- Read button and set key events (edge/level with duration)
Proc P_ReadButton()
    Dim down As Byte
    down = (BTN = 0)                          ' active low
    If down = 1 Then
        If B_BtnLast = 0 Then                 ' just pressed
            L_BtnDownMs = L_Millis
        EndIf
        B_BtnLast = 1
    Else
        If B_BtnLast = 1 Then                 ' just released
            Dim dur As Dword
            dur = L_Millis - L_BtnDownMs
            If dur >= BTN_VLONG_MS Then
                B_KeyEvent = 3
            ElseIf dur >= BTN_LONG_MS Then
                B_KeyEvent = 2
            ElseIf dur >= BTN_SHORT_MS Then
                B_KeyEvent = 1
            Else
                B_KeyEvent = 0
            EndIf
            If B_KeyEvent <> 0 Then: P_MarkActive : EndIf
        EndIf
        B_BtnLast = 0
    EndIf
EndProc

'--- Consume key event (returns and clears)
Proc P_GetKeyEvent() As Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
EndProc

'--- Inactivity/escape handler: returns non-zero if user wants to unwind
Proc P_UserAborted() As Byte
    Dim ev As Byte
    ev = P_GetKeyEvent()
    If ev = 2 Then: B_Escape = 1 : Result = 1 : ExitProc : EndIf ' long
    If ev = 3 Then: B_Escape = 1 : Result = 1 : ExitProc : EndIf ' very long
    If (L_Millis - L_LastInput) >= UI_TIMEOUT_MS Then
        B_Escape = 1
        Result = 1
        ExitProc
    EndIf
    Result = 0
EndProc

'=====================================================================
'                          EDITORS
'=====================================================================

'--- Yes/No editor
Proc P_EditYN(ByRef v As Byte) As Byte
    Dim cur As Byte
    cur = v
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("EDIT: YES/NO")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            If cur = YES Then
                Print "[Yes]  No           "
            Else
                Print " Yes  [No]          "
            EndIf
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta <> 0 Then
            If cur = YES Then: cur = NO Else: cur = YES : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1 : v = cur : Result = 1 : ExitProc       ' short=save
            Case 2,3 : Result = 0 : ExitProc               ' long=cancel
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'--- Enum3 editor: No/Pulse/Latch
Proc P_EditEnum3(ByRef v As Byte) As Byte
    Dim cur As Byte
    cur = v
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("EDIT: OUTPUT MODE")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            Select cur
                Case MODE_NO    : Print "[No]  Pulse  Latch    "
                Case MODE_PULSE : Print " No  [Pulse] Latch    "
                Case MODE_LATCH : Print " No   Pulse [Latch]  "
            EndSelect
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If cur < MODE_LATCH Then: cur = cur + 1 : EndIf
            P_RequestRedraw
        ElseIf S_EncDelta = -1 Then
            If cur > MODE_NO Then: cur = cur - 1 : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1 : v = cur : Result = 1 : ExitProc
            Case 2,3 : Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'--- Word editor (numeric up/down)
Proc P_EditWordVal(ByRef v As Word, mn As Word, mx As Word, stepv As Word) As Byte
    Dim cur As Word
    cur = v
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("EDIT: WORD VALUE")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            Print " Value: [";Dec cur;"]         "
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If cur + stepv <= mx Then: cur = cur + stepv : EndIf
            P_RequestRedraw
        ElseIf S_EncDelta = -1 Then
            If cur >= (mn + stepv) Then: cur = cur - stepv Else: cur = mn : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1 : v = cur : Result = 1 : ExitProc
            Case 2,3 : Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'--- mm:ss editor (0..99:59). Short=toggle field/save, Long=cancel
Proc P_EditMMSS(ByRef v As Word) As Byte
    Dim m As Byte, s As Byte, field As Byte
    m = v / 60
    s = v % 60
    field = 0                                   ' 0=mm,1=ss
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("EDIT: DURATION")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            If field = 0 Then
                Print " [";If m<10 Then: Print "0" : EndIf;Dec m;
                Print "] : ";If s<10 Then: Print "0" : EndIf;Dec s
            Else
                Print "  ";If m<10 Then: Print "0" : EndIf;Dec m;" : [";
                If s<10 Then: Print "0" : EndIf;Dec s;
                Print "]"
            EndIf
            Print Space$(20 - 13)                 ' pad to 20 cols
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If field = 0 Then
                If m < 99 Then: Inc m : EndIf
            Else
                If s < 59 Then: Inc s : EndIf
            EndIf
            P_RequestRedraw
        ElseIf S_EncDelta = -1 Then
            If field = 0 Then
                If m > 0 Then: Dec m : EndIf
            Else
                If s > 0 Then: Dec s : EndIf
            EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                If field = 0 Then
                    field = 1
                    P_RequestRedraw
                Else
                    v = P_MMSS_ToSecs(m, s)
                    Result = 1
                    ExitProc
                EndIf
            Case 2,3
                Result = 0
                ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
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
    B_ReInitLCD = 0
    P_DrawTitle("IRRISYS MAIN        ")
    P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
    Locate 3,1 : Print "Press to open menu  "
    P_RequestRedraw
    While 1=1
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                ExitProc                        ' short -> leave to caller
            Case 2,3
                ' stay on main
        EndSelect
        ' Minimal standby updates could be added here
    Wend
EndProc

'--- Options menu (3-row window example)
Proc V_Options() As Byte
    Dim sel As Byte, top As Byte
    Dim cnt As Byte
    cnt = 3                                     ' Main Menu, Setup Menu, Utility
    sel = 0 : top = 0
    B_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("OPTIONS             ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            ' compute window
            If sel < top Then: top = sel : EndIf
            If sel > (top + 2) Then: top = sel - 2 : EndIf
            ' render items
            Select top
                Case 0
                    P_PrintRow 2, "Main Menu", IIf(sel=0,1,0)
                    P_PrintRow 3, "Setup Menu", IIf(sel=1,1,0)
                    P_PrintRow 4, "Utility Menu", IIf(sel=2,1,0)
                Case Else
                    ' not needed for 3 items
            EndSelect
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If sel < (cnt-1) Then: Inc sel : P_RequestRedraw : EndIf
        ElseIf S_EncDelta = -1 Then
            If sel > 0 Then: Dec sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select sel
                    Case 0 : Result = 1 : ExitProc          ' go to Main Menu (upper)
                    Case 1 : If V_SetupMenu() = 0 Then: Result = 0 : ExitProc : EndIf
                    Case 2 : ' Utility not implemented yet
                             ' Placeholder: just show a stub screen
                             V_UtilityStub()
                EndSelect
                P_RequestRedraw
            Case 2,3
                Result = 0 : ExitProc                        ' unwind
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

Proc V_UtilityStub()
    B_ReInitLCD = 0
    P_DrawTitle("UTILITY (STUB)      ")
    P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
    Locate 3,1: Print "Coming soon...      "
    While 1=1
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1,2,3 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: ExitProc : EndIf
    Wend
EndProc

'--- Setup Menu: Choose Input 1/2/3 (and Clock off-screen later)
Proc V_SetupMenu() As Byte
    Dim sel As Byte, top As Byte
    Dim cnt As Byte
    cnt = 3                                     ' Input1..3 for this pass
    sel = 0 : top = 0
    B_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("SETUP               ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            If sel < top Then: top = sel : EndIf
            If sel > (top + 2) Then: top = sel - 2 : EndIf
            Select top
                Case 0
                    P_PrintRow 2, "Input 1", IIf(sel=0,1,0)
                    P_PrintRow 3, "Input 2", IIf(sel=1,1,0)
                    P_PrintRow 4, "Input 3", IIf(sel=2,1,0)
            EndSelect
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If sel < (cnt-1) Then: Inc sel : P_RequestRedraw : EndIf
        ElseIf S_EncDelta = -1 Then
            If sel > 0 Then: Dec sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select sel
                    Case 0 : If V_Input1Menu() = 0 Then: Result = 0 : ExitProc : EndIf
                    Case 1 : V_NotImpl("INPUT 2")
                    Case 2 : V_NotImpl("INPUT 3")
                EndSelect
                P_RequestRedraw
            Case 2,3
                Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

Proc V_NotImpl(ByRef name As String)
    B_ReInitLCD = 0
    P_DrawTitle(name + " (STUB)      ")
    P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
    Locate 3,1: Print "Not implemented     "
    While 1=1
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1,2,3 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: ExitProc : EndIf
    Wend
EndProc

'--- Input 1 menu: Enable + Sensor -> Pressure flow (this pass: Pressure)
Proc V_Input1Menu() As Byte
    Dim sel As Byte
    sel = 0
    B_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("INPUT 1             ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            P_PrintRow 2, "Enable", IIf(sel=0,1,0)
            P_PrintRow 3, "Sensor Type", IIf(sel=1,1,0)
            P_PrintRow 4, "Edit Params", IIf(sel=2,1,0)
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If sel < 2 Then: Inc sel : P_RequestRedraw : EndIf
        ElseIf S_EncDelta = -1 Then
            If sel > 0 Then: Dec sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select sel
                    Case 0 : If P_EditYN(B_I1_Enabled) = 1 Then: P_SaveSettings : EndIf
                    Case 1 : If V_Input1Sensor() = 0 Then: Result = 0 : ExitProc : EndIf
                    Case 2 : If B_I1_Enabled = YES Then
                                Select B_I1_SensorT
                                    Case SENSOR_PRES : If V_I1_Pressure() = 0 Then: Result = 0 : ExitProc : EndIf
                                    Case SENSOR_FLOW : V_NotImpl("FLOW EDIT")
                                    Case SENSOR_TEMP : V_NotImpl("TEMP EDIT")
                                EndSelect
                             Else
                                V_NotImpl("ENABLE INPUT FIRST")
                             EndIf
                EndSelect
                P_RequestRedraw
            Case 2,3
                Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'--- Choose sensor type for Input1 (Flow/Pressure/Temperature)
Proc V_Input1Sensor() As Byte
    Dim sel As Byte
    sel = B_I1_SensorT
    B_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("SENSOR TYPE         ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            P_PrintRow 2, "Flow", IIf(sel=0,1,0)
            P_PrintRow 3, "Pressure", IIf(sel=1,1,0)
            P_PrintRow 4, "Temperature", IIf(sel=2,1,0)
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If sel < 2 Then: Inc sel : P_RequestRedraw : EndIf
        ElseIf S_EncDelta = -1 Then
            If sel > 0 Then: Dec sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1 : B_I1_SensorT = sel : P_SaveSettings : Result = 1 : ExitProc
            Case 2,3 : Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'--- Input1 Pressure editor list (subset implemented)
Proc V_I1_Pressure() As Byte
    Dim sel As Byte, cnt As Byte
    sel = 0
    cnt = 8                                      ' up to Display; extend later
    B_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If B_ScrDirty = 1 Then
            P_DrawTitle("I1 PRESSURE         ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            ' Render 3-row list window based on sel
            Dim top As Byte
            If sel <= 1 Then: top = 0 ElseIf sel = 2 Then: top = 1 ElseIf sel = 3 Then: top = 2 ElseIf sel = 4 Then: top = 3 ElseIf sel = 5 Then: top = 4 ElseIf sel = 6 Then: top = 5 Else: top = sel-2 : EndIf
            ' Lines text (fixed labels, values drawn inline where useful)
            Select top
                Case 0
                    P_PrintRow 2, "Scale 4ma   [" + Str$(W_I1_Scale4) + "]", IIf(sel=0,1,0)
                    P_PrintRow 3, "Scale 20ma  [" + Str$(W_I1_Scale20) + "]", IIf(sel=1,1,0)
                    P_PrintRow 4, "High BP     [mm:ss]", IIf(sel=2,1,0)
                Case 1
                    P_PrintRow 2, "Scale 20ma  [" + Str$(W_I1_Scale20) + "]", IIf(sel=1,1,0)
                    P_PrintRow 3, "High BP     [mm:ss]", IIf(sel=2,1,0)
                    P_PrintRow 4, "PLPBP       [mm:ss]", IIf(sel=3,1,0)
                Case 2
                    P_PrintRow 2, "High BP     [mm:ss]", IIf(sel=2,1,0)
                    P_PrintRow 3, "PLPBP       [mm:ss]", IIf(sel=3,1,0)
                    P_PrintRow 4, "SLPBP       [mm:ss]", IIf(sel=4,1,0)
                Case 3
                    P_PrintRow 2, "PLPBP       [mm:ss]", IIf(sel=3,1,0)
                    P_PrintRow 3, "SLPBP       [mm:ss]", IIf(sel=4,1,0)
                    P_PrintRow 4, "Rly High    [L/P/No]", IIf(sel=5,1,0)
                Case 4
                    P_PrintRow 2, "SLPBP       [mm:ss]", IIf(sel=4,1,0)
                    P_PrintRow 3, "Rly High    [L/P/No]", IIf(sel=5,1,0)
                    P_PrintRow 4, "Rly PLB     [L/P/No]", IIf(sel=6,1,0)
                Case 5
                    P_PrintRow 2, "Rly High    [L/P/No]", IIf(sel=5,1,0)
                    P_PrintRow 3, "Rly PLB     [L/P/No]", IIf(sel=6,1,0)
                    P_PrintRow 4, "Rly SLP     [L/P/No]", IIf(sel=7,1,0)
                Case Else
                    P_PrintRow 2, "Rly PLB     [L/P/No]", IIf(sel=6,1,0)
                    P_PrintRow 3, "Rly SLP     [L/P/No]", IIf(sel=7,1,0)
                    P_PrintRow 4, "Display     [Yes/No]", IIf(sel=8,1,0)
            EndSelect
            B_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If S_EncDelta = +1 Then
            If sel < (cnt-1) Then: Inc sel : P_RequestRedraw : EndIf
        ElseIf S_EncDelta = -1 Then
            If sel > 0 Then: Dec sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select sel
                    Case 0 : If P_EditWordVal(W_I1_Scale4, 0, 65535, 1) = 1 Then: P_Scale : P_SaveSettings : EndIf
                    Case 1 : If P_EditWordVal(W_I1_Scale20, 0, 65535, 1) = 1 Then: P_Scale : P_SaveSettings : EndIf
                    Case 2 : If P_EditMMSS(W_I1_BP_High) = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 3 : If P_EditMMSS(W_I1_BP_PLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 4 : If P_EditMMSS(W_I1_BP_SLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 5 : If P_EditEnum3(B_I1_RlyHigh) = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 6 : If P_EditEnum3(B_I1_RlyPLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 7 : If P_EditEnum3(B_I1_RlySLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 8 : If P_EditYN(B_I1_Display)     = 1 Then: P_SaveSettings : EndIf
                EndSelect
                P_RequestRedraw
            Case 2,3
                Result = 0 : ExitProc
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : ExitProc : EndIf
    Wend
EndProc

'=====================================================================
'                TIMER0 TICK (1ms) AND INITIALISATION
'=====================================================================

'--- Configure Timer0 for ~1ms tick @ 20MHz (adjust as required)
Proc P_Timer0Init()
    ' 20MHz => Fosc/4 = 5MHz => 0.2us per tick
    ' Use 8-bit, prescaler 1:64 => 12.8us per tick. Need ~1ms => 1000/12.8 ˜ 78 ticks.
    ' Preload so overflow in ~1ms: 256 - 78 = 178
    T0CON = %11000100          ' TMR0 ON, 8-bit, prescale 1:32 (adjust below if needed)
    TMR0L = 178
    INTCON.TMR0IF = 0
    INTCON.TMR0IE = 1
    INTCON.GIE = 1
EndProc

'--- ISR
On_Hardware_Interrupt GoTo Isr

Isr:
    If INTCON.TMR0IF = 1 Then
        TMR0L = 178
        INTCON.TMR0IF = 0
        Inc L_Millis
    EndIf
    Context Restore

'--- GPIO directions
Proc P_PinInit()
    TRISA = %11111000        ' RA0..RA2 LCD ctrl as outputs; others inputs by default
    TRISB = %00000000        ' LCD D4..D7 on RB4..RB7 => outputs (low nibble can be outputs too)
    TRISC = %11111111        ' ENC_A/B and BTN as inputs
    ' Enable weak pullups if needed for BTN/ENC via WPUC (device-dependent)
EndProc

'--- LCD safe init wrapper
Proc P_LCDSafeInit()
    DelayMsFast 50
    Cls
    DelayMsFast 10
EndProc

'=====================================================================
'                              MAIN
'=====================================================================

P_PinInit
P_Timer0Init
P_LCDSafeInit

' Load settings once at boot
P_LoadSettings

' Init UI state
B_ScrDirty = 1
B_ReInitLCD = 0
B_Escape = 0
B_BtnLast = 0
B_KeyEvent = 0
B_EncPrev = 0
L_LastInput = 0

' Optional defaults if EEPROM blank
If B_I1_SensorT > 2 Then: B_I1_SensorT = SENSOR_PRES : EndIf

While 1 = 1
    ' MAIN SCREEN -> OPTIONS
    V_Main
    If V_Options() = 0 Then
        ' unwind all the way to main (already here)
    EndIf
Wend

'============================== EOF ==================================

