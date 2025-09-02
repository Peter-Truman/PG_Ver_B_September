'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
'=====================================================================
'
' Device & clock (adjust as required for your board)
Device = 18F2525
Xtal   = 32                                   ' MHz (adjust if needed)
All_Digital = True

'---------------------------------------------------------------------
' LCD (HD44780-compatible) 20x4 in 4-bit mode
Declare LCD_Type = 0
Declare LCD_DTPin = PORTA.0
Declare LCD_ENPin = PORTA.7
Declare LCD_RSPin = PORTA.6
Declare LCD_Interface = 4
Declare LCD_Lines = 4

'---------------------------------------------------------------------
' Encoder & button pins (adjust to your board)
Symbol ENC_A = PORTB.1
Symbol ENC_B = PORTB.2
Symbol BTN   = PORTB.6                         ' Active-low push button
Symbol BUZZER = PORTC.2

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

'=====================================================================
'                          UTILS / HELPERS
'=====================================================================

'--- Delay helper (ms)
Proc DelayMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
GoTo Exit_DelayMsFast
Exit_DelayMsFast:
EndProc

'--- Clear a LCD line (1..4)
Proc P_ClearLine(B_Row As Byte)
    Locate B_Row, 1
    Print "                    "   ' 20 spaces
GoTo Exit_P_ClearLine
Exit_P_ClearLine:
EndProc

'--- Draw fixed title on line 1 (exactly 20 cols)
Proc P_DrawTitle(S_Title As String)
    Dim S_Line20 As String * 20
    S_Line20 = Left$(S_Title + Space$(20), 20)
    Locate 1, 1
    Print S_Line20
GoTo Exit_P_DrawTitle
Exit_P_DrawTitle:
EndProc

'--- Print a bracketed row at given LCD line (2..4). If active, prefix "[" else space.
Proc P_PrintRow(B_Row As Byte, S_Text As String, B_Active As Byte)
    Dim S_Line20 As String * 20
    If B_Active = 1 Then
        S_Line20 = "[" + S_Text
    Else
        S_Line20 = " " + S_Text
    EndIf
    S_Line20 = Left$(S_Line20 + Space$(20), 20)
    Locate B_Row, 1
    Print S_Line20
GoTo Exit_P_PrintRow
Exit_P_PrintRow:
EndProc

'--- Format mm:ss from seconds (0..5999 => 99:59)
Proc P_PrintMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word, B_Sec As Word
    B_Min = W_Seconds / 60
    B_Sec = W_Seconds % 60
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
GoTo Exit_P_PrintMMSS
Exit_P_PrintMMSS:
EndProc

'--- Parse mm:ss editor state -> Word seconds
Proc P_MMSS_ToSecs(B_Min As Byte, B_Sec As Byte), Word
    Result = (B_Min * 60) + B_Sec
GoTo Exit_P_MMSS_ToSecs
Exit_P_MMSS_ToSecs:
EndProc

'--- Request a redraw of the current screen
Proc P_RequestRedraw()
    b_ScrDirty = 1
GoTo Exit_P_RequestRedraw
Exit_P_RequestRedraw:
EndProc

'--- Activity bump (resets idle timer)
Proc P_MarkActive()
    L_LastInput = L_Millis
GoTo Exit_P_MarkActive
Exit_P_MarkActive:
EndProc

'--- Clamp helper
Proc P_ClampW(ByRef W_Val As Word, W_Min As Word, W_Max As Word)
    If W_Val < W_Min Then: W_Val = W_Min : EndIf
    If W_Val > W_Max Then: W_Val = W_Max : EndIf
GoTo Exit_P_ClampW
Exit_P_ClampW:
EndProc

'=====================================================================
'                    EEPROM READ / WRITE WRAPPERS
'=====================================================================

' Byte
Proc EReadB(W_Addr As Word), Byte
    Result = ERead W_Addr
GoTo Exit_EReadB
Exit_EReadB:
EndProc

Proc EWriteB(W_Addr As Word, B_Val As Byte)
    If ERead W_Addr <> B_Val Then: EWrite W_Addr, B_Val : EndIf
GoTo Exit_EWriteB
Exit_EWriteB:
EndProc

' Word
Proc EReadW(W_Addr As Word), Word
    Dim B_Lo As Byte, B_Hi As Byte
    B_Lo = ERead W_Addr
    B_Hi = ERead (W_Addr + 1)
    Result = MakeWord(B_Hi, B_Lo)
GoTo Exit_EReadW
Exit_EReadW:
EndProc

Proc EWriteW(W_Addr As Word, W_Val As Word)
    Dim B_Lo As Byte, B_Hi As Byte
    B_Lo = Low W_Val
    B_Hi = High W_Val
    If ERead W_Addr <> B_Lo Then: EWrite W_Addr, B_Lo : EndIf
    If ERead (W_Addr + 1) <> B_Hi Then: EWrite (W_Addr + 1), B_Hi : EndIf
GoTo Exit_EWriteW
Exit_EWriteW:
EndProc

' Dword (Long)
Proc EReadL(W_Addr As Word), Dword
    Dim B_B0 As Byte, B_B1 As Byte, B_B2 As Byte, B_B3 As Byte
    Dim L_Res As Dword
    B_B0 = ERead W_Addr
    B_B1 = ERead (W_Addr + 1)
    B_B2 = ERead (W_Addr + 2)
    B_B3 = ERead (W_Addr + 3)
    L_Res = B_B3
    L_Res = (L_Res * 256) + B_B2
    L_Res = (L_Res * 256) + B_B1
    L_Res = (L_Res * 256) + B_B0
    Result = L_Res
GoTo Exit_EReadL
Exit_EReadL:
EndProc

Proc EWriteL(W_Addr As Word, L_Val As Dword)
    Dim B_B0 As Byte, B_B1 As Byte, B_B2 As Byte, B_B3 As Byte
    B_B0 = L_Val & $FF
    B_B1 = (L_Val / 256) & $FF
    B_B2 = (L_Val / 65536) & $FF
    B_B3 = (L_Val / 16777216) & $FF
    If ERead W_Addr <> B_B0 Then: EWrite W_Addr, B_B0 : EndIf
    If ERead (W_Addr + 1) <> B_B1 Then: EWrite (W_Addr + 1), B_B1 : EndIf
    If ERead (W_Addr + 2) <> B_B2 Then: EWrite (W_Addr + 2), B_B2 : EndIf
    If ERead (W_Addr + 3) <> B_B3 Then: EWrite (W_Addr + 3), B_B3 : EndIf
GoTo Exit_EWriteL
Exit_EWriteL:
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
    Dim B_A As Byte, B_B As Byte, B_State As Byte
    B_A = ENC_A
    B_B = ENC_B
    If B_A = 0 Then: B_A = 0 Else: B_A = 1 : EndIf
    If B_B = 0 Then: B_B = 0 Else: B_B = 1 : EndIf
    B_State = (B_A << 1) | B_B
    B_EncDelta = 0
    Select B_EncPrev
        Case 0
            If B_State = 1 Then: B_EncDelta = +1 : EndIf
            If B_State = 2 Then: B_EncDelta = -1 : EndIf
        Case 1
            If B_State = 3 Then: B_EncDelta = +1 : EndIf
            If B_State = 0 Then: B_EncDelta = -1 : EndIf
        Case 3
            If B_State = 2 Then: B_EncDelta = +1 : EndIf
            If B_State = 1 Then: B_EncDelta = -1 : EndIf
        Case 2
            If B_State = 0 Then: B_EncDelta = +1 : EndIf
            If B_State = 3 Then: B_EncDelta = -1 : EndIf
    EndSelect
    B_EncPrev = B_State
    If B_EncDelta <> 0 Then: P_MarkActive : EndIf
GoTo Exit_P_ReadEncoder
Exit_P_ReadEncoder:
EndProc

'--- Read button and set key events (edge/level with duration)
Proc P_ReadButton()
    Dim b_BtnDown As Bit
    
    If BTN = 0 Then           ' active-low -> pressed
        b_BtnDown = 1
    Else
        b_BtnDown = 0
    EndIf

    If b_BtnDown = 1 Then
        If b_BtnLast = 0 Then
            L_BtnDownMs = L_Millis
        EndIf
        b_BtnLast = 1
    Else
        If b_BtnLast = 1 Then
            Dim L_Dur As Dword
            L_Dur = L_Millis - L_BtnDownMs
            If L_Dur >= BTN_VLONG_MS Then
                B_KeyEvent = 3
            ElseIf L_Dur >= BTN_LONG_MS Then
                B_KeyEvent = 2
            ElseIf L_Dur >= BTN_SHORT_MS Then
                B_KeyEvent = 1
            Else
                B_KeyEvent = 0
            EndIf
            If B_KeyEvent <> 0 Then: P_MarkActive : EndIf
        EndIf
    b_BtnLast = 0
EndIf
GoTo Exit_P_ReadButton
Exit_P_ReadButton:
EndProc

'--- Consume key event (returns and clears)
Proc P_GetKeyEvent(), Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
GoTo Exit_P_GetKeyEvent
Exit_P_GetKeyEvent:
EndProc

'--- Inactivity/escape handler: returns non-zero if user wants to unwind
Proc P_UserAborted(), Byte
    Dim B_Ev As Byte
    B_Ev = P_GetKeyEvent()
    If B_Ev = 2 Then: b_Escape = 1 : Result = 1 : GoTo Exit_P_UserAborted : EndIf ' long
    If B_Ev = 3 Then: b_Escape = 1 : Result = 1 : GoTo Exit_P_UserAborted : EndIf ' very long
    If (L_Millis - L_LastInput) >= UI_TIMEOUT_MS Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf
    Result = 0
GoTo Exit_P_UserAborted
Exit_P_UserAborted:
EndProc

'=====================================================================
'                          EDITORS
'=====================================================================

'--- Yes/No editor
Proc P_EditYN(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: YES/NO")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            If B_Cur = YES Then
                Print "[Yes]  No           "
            Else
                Print " Yes  [No]          "
            EndIf
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta <> 0 Then
            If B_Cur = YES Then: B_Cur = NO Else: B_Cur = YES : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
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
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_P_EditYN : EndIf
    Wend
GoTo Exit_P_EditYN
Exit_P_EditYN:
EndProc

'--- Enum3 editor: No/Pulse/Latch
Proc P_EditEnum3(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: OUTPUT MODE")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            Select B_Cur
                Case MODE_NO
                    Print "[No]  Pulse  Latch    "
                Case MODE_PULSE
                    Print " No  [Pulse] Latch    "
                Case MODE_LATCH
                    Print " No   Pulse [Latch]  "
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Cur < MODE_LATCH Then: B_Cur = B_Cur + 1 : EndIf
            P_RequestRedraw
        ElseIf B_EncDelta = -1 Then
            If B_Cur > MODE_NO Then: B_Cur = B_Cur - 1 : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
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
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_P_EditEnum3 : EndIf
    Wend
GoTo Exit_P_EditEnum3
Exit_P_EditEnum3:
EndProc

'--- Word editor (numeric up/down)
Proc P_EditWordVal(ByRef W_Val As Word, W_Min As Word, W_Max As Word, W_Step As Word), Byte
    Dim W_Cur As Word
    W_Cur = W_Val
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: WORD VALUE")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            Print " Value: [";Dec W_Cur;"]         "
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If W_Cur + W_Step <= W_Max Then: W_Cur = W_Cur + W_Step : EndIf
            P_RequestRedraw
        ElseIf B_EncDelta = -1 Then
            If W_Cur >= (W_Min + W_Step) Then: W_Cur = W_Cur - W_Step Else: W_Cur = W_Min : EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
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
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_P_EditWordVal : EndIf
    Wend
GoTo Exit_P_EditWordVal
Exit_P_EditWordVal:
EndProc

'--- mm:ss editor (0..99:59). Short=toggle field/save, Long=cancel
Proc P_EditMMSS(ByRef W_Val As Word), Byte
    Dim B_Min As Byte, B_Sec As Byte, B_Field As Byte
    B_Min = W_Val / 60
    B_Sec = W_Val % 60
    B_Field = 0                                   ' 0=mm,1=ss
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("EDIT: DURATION")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            Locate 3, 1
            If B_Field = 0 Then
                Print " [";If B_Min<10 Then: Print "0" : EndIf;Dec B_Min;
                Print "] : ";If B_Sec<10 Then: Print "0" : EndIf;Dec B_Sec
            Else
                Print "  ";If B_Min<10 Then: Print "0" : EndIf;Dec B_Min;" : [";
                If B_Sec<10 Then: Print "0" : EndIf;Dec B_Sec;
                Print "]"
            EndIf
            Print Space$(20 - 13)                 ' pad to 20 cols
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Field = 0 Then
                If B_Min < 99 Then: Inc B_Min : EndIf
            Else
                If B_Sec < 59 Then: Inc B_Sec : EndIf
            EndIf
            P_RequestRedraw
        ElseIf B_EncDelta = -1 Then
            If B_Field = 0 Then
                If B_Min > 0 Then: Dec B_Min : EndIf
            Else
                If B_Sec > 0 Then: Dec B_Sec : EndIf
            EndIf
            P_RequestRedraw
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                If B_Field = 0 Then
                    B_Field = 1
                    P_RequestRedraw
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
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_P_EditMMSS : EndIf
    Wend
GoTo Exit_P_EditMMSS
Exit_P_EditMMSS:
EndProc

'=====================================================================
'                      SCALING / OUTPUT STUBS
'=====================================================================

Proc P_Scale()
    ' Recompute any derived factors after changing scale values
    ' Placeholder for now
GoTo Exit_P_Scale
Exit_P_Scale:
EndProc

Proc P_Output()
    ' Apply output mode changes to hardware (relays, etc.)
    ' Placeholder for now
GoTo Exit_P_Output
Exit_P_Output:
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
    P_RequestRedraw()
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
GoTo Exit_V_Main
Exit_V_Main:
EndProc

'--- Options menu (3-row window example)
Proc V_Options(), Byte
    Dim B_Sel As Byte, B_Top As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3                                     ' Main Menu, Setup Menu, Utility
    B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    P_RequestRedraw()
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
                    P_PrintRow(2, "Main Menu", IIf(B_Sel=0,1,0))
                    P_PrintRow(3, "Setup Menu", IIf(B_Sel=1,1,0))
                    P_PrintRow(4, "Utility Menu", IIf(B_Sel=2,1,0))
                Case Else
                    ' not needed for 3 items
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Sel < (B_Cnt-1) Then: Inc B_Sel : P_RequestRedraw : EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then: Dec B_Sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
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
                P_RequestRedraw
            Case 2
                Result = 0
                GoTo Exit_V_Options                        ' unwind
            Case 3
                Result = 0
                GoTo Exit_V_Options
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_V_Options : EndIf
    Wend
GoTo Exit_V_Options
Exit_V_Options:
EndProc

Proc V_UtilityStub()
    b_ReInitLCD = 0
    P_DrawTitle("UTILITY (STUB)      ")
    P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
    Locate 3,1: Print "Coming soon...      "
    While 1=1
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_UtilityStub
            Case 2
                GoTo Exit_V_UtilityStub
            Case 3
                GoTo Exit_V_UtilityStub
        EndSelect
        If P_UserAborted() <> 0 Then: GoTo Exit_V_UtilityStub : EndIf
    Wend
GoTo Exit_V_UtilityStub
Exit_V_UtilityStub:
EndProc

'--- Setup Menu: Choose Input 1/2/3 (and Clock off-screen later)
Proc V_SetupMenu(), Byte
    Dim B_Sel As Byte, B_Top As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3                                     ' Input1..3 for this pass
    B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("SETUP               ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            If B_Sel < B_Top Then: B_Top = B_Sel : EndIf
            If B_Sel > (B_Top + 2) Then: B_Top = B_Sel - 2 : EndIf
            Select B_Top
                Case 0
                    P_PrintRow 2, "Input 1", IIf(B_Sel=0,1,0)
                    P_PrintRow 3, "Input 2", IIf(B_Sel=1,1,0)
                    P_PrintRow 4, "Input 3", IIf(B_Sel=2,1,0)
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Sel < (B_Cnt-1) Then: Inc B_Sel : P_RequestRedraw : EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then: Dec B_Sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
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
                P_RequestRedraw
            Case 2
                Result = 0
                GoTo Exit_V_SetupMenu
            Case 3
                Result = 0
                GoTo Exit_V_SetupMenu
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_V_SetupMenu : EndIf
    Wend
GoTo Exit_V_SetupMenu
Exit_V_SetupMenu:
EndProc

Proc V_NotImpl(S_Name As String)
    b_ReInitLCD = 0
    P_DrawTitle(S_Name + " (STUB)      ")
    P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
    Locate 3,1: Print "Not implemented     "
    While 1=1
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_NotImpl
            Case 2
                GoTo Exit_V_NotImpl
            Case 3
                GoTo Exit_V_NotImpl
        EndSelect
        If P_UserAborted() <> 0 Then: GoTo Exit_V_NotImpl : EndIf
    Wend
GoTo Exit_V_NotImpl
Exit_V_NotImpl:
EndProc

'--- Input 1 menu: Enable + Sensor -> Pressure flow (this pass: Pressure)
Proc V_Input1Menu(), Byte
    Dim B_Sel As Byte
    B_Sel = 0
    b_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("INPUT 1             ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            P_PrintRow 2, "Enable", IIf(B_Sel=0,1,0)
            P_PrintRow 3, "Sensor Type", IIf(B_Sel=1,1,0)
            P_PrintRow 4, "Edit Params", IIf(B_Sel=2,1,0)
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Sel < 2 Then: Inc B_Sel : P_RequestRedraw : EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then: Dec B_Sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        If P_EditYN(B_I1_Enabled) = 1 Then: P_SaveSettings : EndIf
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
                P_RequestRedraw
            Case 2
                Result = 0
                GoTo Exit_V_Input1Menu
            Case 3
                Result = 0
                GoTo Exit_V_Input1Menu
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_V_Input1Menu : EndIf
    Wend
GoTo Exit_V_Input1Menu
Exit_V_Input1Menu:
EndProc

'--- Choose sensor type for Input1 (Flow/Pressure/Temperature)
Proc V_Input1Sensor(), Byte
    Dim B_Sel As Byte
    B_Sel = B_I1_SensorT
    b_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("SENSOR TYPE         ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            P_PrintRow 2, "Flow", IIf(B_Sel=0,1,0)
            P_PrintRow 3, "Pressure", IIf(B_Sel=1,1,0)
            P_PrintRow 4, "Temperature", IIf(B_Sel=2,1,0)
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Sel < 2 Then: Inc B_Sel : P_RequestRedraw : EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then: Dec B_Sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                B_I1_SensorT = B_Sel
                P_SaveSettings
                Result = 1
                GoTo Exit_V_Input1Sensor
            Case 2
                Result = 0
                GoTo Exit_V_Input1Sensor
            Case 3
                Result = 0
                GoTo Exit_V_Input1Sensor
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_V_Input1Sensor : EndIf
    Wend
GoTo Exit_V_Input1Sensor
Exit_V_Input1Sensor:
EndProc

'--- Input1 Pressure editor list (subset implemented)
Proc V_I1_Pressure(), Byte
    Dim B_Sel As Byte, B_Cnt As Byte
    B_Sel = 0
    B_Cnt = 8                                      ' up to Display; extend later
    b_ReInitLCD = 0
    P_RequestRedraw
    While 1=1
        If b_ScrDirty = 1 Then
            P_DrawTitle("I1 PRESSURE         ")
            P_ClearLine 2: P_ClearLine 3: P_ClearLine 4
            ' Render 3-row list window based on B_Sel
            Dim B_Top As Byte
            If B_Sel <= 1 Then: B_Top = 0 ElseIf B_Sel = 2 Then: B_Top = 1 ElseIf B_Sel = 3 Then: B_Top = 2 ElseIf B_Sel = 4 Then: B_Top = 3 ElseIf B_Sel = 5 Then: B_Top = 4 ElseIf B_Sel = 6 Then: B_Top = 5 Else: B_Top = B_Sel-2 : EndIf
            ' Lines text (fixed labels, values drawn inline where useful)
            Select B_Top
                Case 0
                    P_PrintRow 2, "Scale 4ma   [" + Str$(W_I1_Scale4) + "]", IIf(B_Sel=0,1,0)
                    P_PrintRow 3, "Scale 20ma  [" + Str$(W_I1_Scale20) + "]", IIf(B_Sel=1,1,0)
                    P_PrintRow 4, "High BP     [mm:ss]", IIf(B_Sel=2,1,0)
                Case 1
                    P_PrintRow 2, "Scale 20ma  [" + Str$(W_I1_Scale20) + "]", IIf(B_Sel=1,1,0)
                    P_PrintRow 3, "High BP     [mm:ss]", IIf(B_Sel=2,1,0)
                    P_PrintRow 4, "PLPBP       [mm:ss]", IIf(B_Sel=3,1,0)
                Case 2
                    P_PrintRow 2, "High BP     [mm:ss]", IIf(B_Sel=2,1,0)
                    P_PrintRow 3, "PLPBP       [mm:ss]", IIf(B_Sel=3,1,0)
                    P_PrintRow 4, "SLPBP       [mm:ss]", IIf(B_Sel=4,1,0)
                Case 3
                    P_PrintRow 2, "PLPBP       [mm:ss]", IIf(B_Sel=3,1,0)
                    P_PrintRow 3, "SLPBP       [mm:ss]", IIf(B_Sel=4,1,0)
                    P_PrintRow 4, "Rly High    [L/P/No]", IIf(B_Sel=5,1,0)
                Case 4
                    P_PrintRow 2, "SLPBP       [mm:ss]", IIf(B_Sel=4,1,0)
                    P_PrintRow 3, "Rly High    [L/P/No]", IIf(B_Sel=5,1,0)
                    P_PrintRow 4, "Rly PLB     [L/P/No]", IIf(B_Sel=6,1,0)
                Case 5
                    P_PrintRow 2, "Rly High    [L/P/No]", IIf(B_Sel=5,1,0)
                    P_PrintRow 3, "Rly PLB     [L/P/No]", IIf(B_Sel=6,1,0)
                    P_PrintRow 4, "Rly SLP     [L/P/No]", IIf(B_Sel=7,1,0)
                Case Else
                    P_PrintRow 2, "Rly PLB     [L/P/No]", IIf(B_Sel=6,1,0)
                    P_PrintRow 3, "Rly SLP     [L/P/No]", IIf(B_Sel=7,1,0)
                    P_PrintRow 4, "Display     [Yes/No]", IIf(B_Sel=8,1,0)
            EndSelect
            b_ScrDirty = 0
        EndIf
        P_ReadEncoder
        If B_EncDelta = +1 Then
            If B_Sel < (B_Cnt-1) Then: Inc B_Sel : P_RequestRedraw : EndIf
        ElseIf B_EncDelta = -1 Then
            If B_Sel > 0 Then: Dec B_Sel : P_RequestRedraw : EndIf
        EndIf
        P_ReadButton
        Select P_GetKeyEvent()
            Case 1
                Select B_Sel
                    Case 0
                        If P_EditWordVal(W_I1_Scale4, 0, 65535, 1) = 1 Then: P_Scale : P_SaveSettings : EndIf
                    Case 1
                        If P_EditWordVal(W_I1_Scale20, 0, 65535, 1) = 1 Then: P_Scale : P_SaveSettings : EndIf
                    Case 2
                        If P_EditMMSS(W_I1_BP_High) = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 3
                        If P_EditMMSS(W_I1_BP_PLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 4
                        If P_EditMMSS(W_I1_BP_SLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 5
                        If P_EditEnum3(B_I1_RlyHigh) = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 6
                        If P_EditEnum3(B_I1_RlyPLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 7
                        If P_EditEnum3(B_I1_RlySLP)  = 1 Then: P_Output : P_SaveSettings : EndIf
                    Case 8
                        If P_EditYN(B_I1_Display)     = 1 Then: P_SaveSettings : EndIf
                EndSelect
                P_RequestRedraw
            Case 2
                Result = 0
                GoTo Exit_V_I1_Pressure
            Case 3
                Result = 0
                GoTo Exit_V_I1_Pressure
        EndSelect
        If P_UserAborted() <> 0 Then: Result = 0 : GoTo Exit_V_I1_Pressure : EndIf
    Wend
GoTo Exit_V_I1_Pressure
Exit_V_I1_Pressure:
EndProc

'=====================================================================
'                TIMER0 TICK (1ms) AND INITIALISATION
'=====================================================================

'--- Configure Timer0 for ~1ms tick @ 32MHz (adjust as required)
Proc P_Timer0Init()
    ' 32MHz => Fosc/4 = 8MHz => 0.125us per tick
    ' 8-bit, prescaler 1:32 => 4us per tick. Need ~1ms => 250 ticks.
    ' Preload so overflow in ~1ms: 256 - 250 = 6
    T0CON = %11000100          ' TMR0 ON, 8-bit, prescale 1:32 @ 32MHz
    TMR0L = 6
    INTCON.TMR0IF = 0
    INTCON.TMR0IE = 1
    INTCON.GIE = 1
GoTo Exit_P_Timer0Init
Exit_P_Timer0Init:
EndProc

'--- ISR
On_Hardware_Interrupt GoTo Isr


INTCONbits_T0IF = 0
INTCONbits_T0IE = 1
INTCONbits_GIE  = 1
T0CONbits_TMR0ON = 1
GoTo Over_Interrupt


Isr:


    Context Save
    If INTCONbits_T0IF = 1 Then
        TMR0L = 6
        INTCONbits_T0IF = 0
        Inc L_Millis
    EndIf
    Context Restore

Over_Interrupt:



'--- GPIO directions
Proc P_PinInit()
    TRISA = %00010000        ' RA4 input, others outputs for LCD
    TRISB = %01000110        ' RB6,RB2,RB1 inputs (SW,B,A)
    TRISC = %00000000        ' RC2 buzzer output; others as required
    ' Enable weak pullups if needed for BTN/ENC via WPUx (device-dependent)
GoTo Exit_P_PinInit
Exit_P_PinInit:
EndProc

'--- LCD safe init wrapper
Proc P_LCDSafeInit()
    DelayMsFast 50
    Cls
    DelayMsFast 10
GoTo Exit_P_LCDSafeInit
Exit_P_LCDSafeInit:
EndProc

'=====================================================================
'                              MAIN
'=====================================================================

P_PinInit()
P_Timer0Init()
P_LCDSafeInit()

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

While 1 = 1
    ' MAIN SCREEN -> OPTIONS
    V_Main()
    If V_Options() = 0 Then
        ' unwind all the way to main (already here)
    EndIf
Wend

'============================== EOF ==================================

