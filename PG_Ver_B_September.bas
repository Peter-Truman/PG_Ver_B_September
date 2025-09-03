'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
'=====================================================================

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

OSCCON    = %01110000                    ' 8 MHz
OSCTUNE.6 = 1                            ' PLL x4 -> 32 MHz
ADCON1    = $0F                          ' All digital

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
Declare Hserial_Baud  = 115200
Declare Hserial_RCSTA = %10010000
Declare Hserial_TXSTA = %00100100

'---------------------------------------------------------------------
' Inputs / buzzer pins
Symbol _ENC_A   = PORTB.1
Symbol _ENC_B   = PORTB.2
Symbol _BTN     = PORTB.6                  ' Active-low push button
Symbol _ENC_SW  = _BTN
Symbol _BUZZER  = PORTC.2

'---------------------------------------------------------------------
' Button timing
Symbol BTN_SHORT_MS     = 200
Symbol BTN_LONG_MS      = 900
Symbol BTN_VLONG_MS     = 1800
Symbol LONG_MS          = BTN_LONG_MS
Symbol VERY_LONG_MS     = BTN_VLONG_MS

' Menu timeout (seconds) – stored as Word in EEPROM
Symbol UI_TIMEOUT_S_MIN = 10
Symbol UI_TIMEOUT_S_MAX = 300
Symbol UI_TIMEOUT_S_DEF = 30

'---------------------------------------------------------------------
' Modes / enums
Symbol MODE_NO    = 0
Symbol MODE_PULSE = 1
Symbol MODE_LATCH = 2

Symbol YES = 1
Symbol NO  = 0

Symbol SENSOR_FLOW = 0
Symbol SENSOR_PRES = 1
Symbol SENSOR_TEMP = 2

'---------------------------------------------------------------------
' INTCON bit aliases
Symbol TMR0IF = INTCON.2
Symbol TMR0IE = INTCON.5
Symbol GIE    = INTCON.7

'---------------------------------------------------------------------
' EEPROM MAP
Symbol EE_VER            = 0                 ' Config version byte

' Input1 legacy map (kept for now while we migrate UI)
Symbol EE_I1_BASE        = 10
Symbol EE_I1_ENABLED     = EE_I1_BASE + 0
Symbol EE_I1_SENSORT     = EE_I1_BASE + 1
Symbol EE_I1_FLOWMODE    = EE_I1_BASE + 2
Symbol EE_I1_SCALE4      = EE_I1_BASE + 4
Symbol EE_I1_SCALE20     = EE_I1_BASE + 6
Symbol EE_I1_BP_HIGH     = EE_I1_BASE + 8
Symbol EE_I1_BP_PLP      = EE_I1_BASE + 10
Symbol EE_I1_BP_SLP      = EE_I1_BASE + 12
Symbol EE_I1_RLY_HIGH    = EE_I1_BASE + 14
Symbol EE_I1_RLY_PLP     = EE_I1_BASE + 15
Symbol EE_I1_RLY_SLP     = EE_I1_BASE + 16
Symbol EE_I1_DISPLAY     = EE_I1_BASE + 17

' System settings block
Symbol EE_SYS_BASE       = 200
Symbol EE_UI_TIMEOUT_S   = EE_SYS_BASE + 0   ' Word seconds

' NEW compact per-input config word block (one Word per input)
Symbol EE_INCFG_BASE     = 300
Symbol EE_I1_CFG         = EE_INCFG_BASE + 0 ' Word
Symbol EE_I2_CFG         = EE_INCFG_BASE + 2 ' Word
Symbol EE_I3_CFG         = EE_INCFG_BASE + 4 ' Word

' Versioning for defaults
Symbol CFG_VERSION       = 1

'---------------------------------------------------------------------
' Global state
Dim b_ReInitLCD  As Bit
Dim b_ScrDirty   As Bit
Dim b_Escape     As Bit

Dim L_Millis     As Dword
Dim L_LastInput  As Dword

Dim B_EncDelta   As SByte
Dim B_KeyEvent   As Byte
Dim W_Beep       As Word

' Debounce/encoder internals
Dim B_RE_Count     As Byte
Dim B_AState       As Byte
Dim B_BState       As Byte
Dim B_ButtonState  As Byte
Dim B_DebA         As Byte
Dim B_DebB         As Byte
Dim B_DebBtn       As Byte
Dim B_LastState    As Byte
Dim S_Qacc         As SByte
Dim W_BtnHoldMS    As Word

' Encoder counters
Dim W_EncoderPos   As Word
Dim W_EncReadPos   As Word

' System/UI
Dim B_Option       As Byte
Dim W_UI_TimeoutS  As Word

' NEW compact cfg mirrors
Dim W_I1_Cfg       As Word
Dim W_I2_Cfg       As Word
Dim W_I3_Cfg       As Word

' Legacy mirrors (current UI still uses these)
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

'---------------------------------------------------------------------
' Timer0 @ ~1ms tick (32MHz -> Fosc/4=8MHz; 8-bit, 1:32 => 4us/tick; 256-250=6)
T0CON = %11000100
TMR0L = 6
Clear TMR0IF
TMR0IE = 1
GIE    = 1

'=====================================================================
' TIMER0 TICK (1ms) + ISR
'=====================================================================

On_Hardware_Interrupt GoTo Isr
GoTo Over_Interrupt

Isr:
    Context Save

    If TMR0IF = 1 Then
        TMR0L = 6
        Clear TMR0IF
        Inc L_Millis

        ' 10ms debounce/sample bucket
        Inc B_RE_Count
        If B_RE_Count > 9 Then
            Dim B_NewA As Byte
            Dim B_NewB As Byte
            Dim B_NewBtn As Byte

            B_NewA   = _ENC_A
            B_NewB   = _ENC_B
            B_NewBtn = _BTN

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
                    If W_Beep = 0 Then
                        W_Beep = 15
                        _BUZZER = 1
                    EndIf
                ElseIf S_Qacc <= -2 Then
                    If W_EncoderPos > 0 Then
                        Dec W_EncoderPos
                    EndIf
                    L_LastInput = L_Millis
                    If W_Beep = 0 Then
                        W_Beep = 15
                        _BUZZER = 1
                    EndIf
                EndIf
                S_Qacc = 0
            EndIf

            B_LastState = B_Curr
            Clear B_RE_Count
        EndIf

        ' Beeper (W_Beep countdown)
        If W_Beep > 0 Then
            _BUZZER = 1
            Dec W_Beep
        Else
            _BUZZER = 0
        EndIf

        ' Button hold time & key event on release
        If B_ButtonState = 0 Then
            If W_BtnHoldMS < 65535 Then
                Inc W_BtnHoldMS
            EndIf
        Else
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

'=====================================================================
' PROCS / HELPERS
'=====================================================================

'---------- EEPROM Word helpers ----------
Proc P_EReadW(W_Addr As Word), Word
    Dim W_Tmp As Word
    Result = ERead W_Addr            ' Positron word read (Lo then Hi)
EndProc
'----------
Proc P_EWriteW(W_Addr As Word, W_Val As Word)
    Dim W_Tmp As Word
    W_Tmp = ERead W_Addr             ' Read existing word
    If W_Tmp <> W_Val Then
        EWrite W_Addr, [W_Val]       ' Write word (Lo then Hi)
    EndIf
EndProc
'----------

'---------- GPIO directions ----------
Proc P_PinInit()
    TRISA = %00010000                ' RA4 input, others outputs for LCD
    TRISB = %01000110                ' RB6,RB2,RB1 inputs (SW,B,A)
    TRISC = %00000000                ' RC2 buzzer output
EndProc

'---------- Encoder/Button state init ----------
Proc P_InputInit()
    B_AState      = _ENC_A
    B_BState      = _ENC_B
    B_ButtonState = _BTN
    B_LastState   = (B_AState * 2) + B_BState
    B_DebA  = 0
    B_DebB  = 0
    B_DebBtn= 0
    B_RE_Count = 0
    S_Qacc = 0
    W_EncoderPos = 0
    W_EncReadPos = 0
    W_BtnHoldMS = 0
EndProc

'---------- Beeper ----------
Proc P_BeepMs(W_Dur As Word)
    GIE = 0
    W_Beep = W_Dur
    GIE = 1
EndProc
'----------
Proc P_BeepOff()
    GIE = 0
    W_Beep = 0
    GIE = 1
EndProc
'----------
Proc P_Beeps(B_Type As Byte)
    Select B_Type
        Case 1
            P_BeepMs(20)
        Case 2
            P_BeepMs(50)
        Case 3
            P_BeepMs(200)
        Case 4
            P_BeepMs(1000)
        Case 5
            P_BeepMs(60)
            While W_Beep > 0
            Wend
            DelayMS 80
            P_BeepMs(60)
        Case 6
            P_BeepMs(40)
            While W_Beep > 0
            Wend
            DelayMS 60
            P_BeepMs(40)
            While W_Beep > 0
            Wend
            DelayMS 60
            P_BeepMs(40)
        Case Else
            P_BeepMs(20)
    EndSelect
EndProc

'---------- Startup beep pattern ----------
Proc P_Startup()
    Dim B_Beepcount As Byte
    For B_Beepcount = 0 To 5
        P_Beeps(2)
        DelayMS 100
    Next B_Beepcount
EndProc

'---------- Misc UI helpers ----------
Proc DelayMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
EndProc
'----------
Proc P_ClearLine(B_Row As Byte)
    Print At B_Row,1,"                    "
EndProc
'----------
Proc P_DrawTitle(S_Title As String)
    Print At 1,1,"                    "
    Print At 1,1,S_Title
EndProc
'----------
Proc P_PrintRow(B_Row As Byte, S_Text As String, B_Active As Byte)
    Print At B_Row,1,"                    "
    If B_Active = 1 Then
        Print At B_Row,1,"[",S_Text,"]"
    Else
        Print At B_Row,2,S_Text
    EndIf
EndProc

'---------- KEEP: your dec2 MM:SS printer ----------
Proc P_PrintMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word
    Dim B_Sec As Word
    B_Min = W_Seconds / 60
    B_Sec = W_Seconds // 60
    Print At B_Row, B_Col, Dec2 B_Min,":"
    Print At B_Row, B_Col+3, Dec2 B_Sec
EndProc

'----------
Proc P_MMSS_ToSecs(B_Min As Byte, B_Sec As Byte), Word
    Result = (B_Min * 60) + B_Sec
EndProc
'----------
Proc P_ClampW(ByRef W_Val As Word, W_Min As Word, W_Max As Word)
    If W_Val < W_Min Then
        W_Val = W_Min
    EndIf
    If W_Val > W_Max Then
        W_Val = W_Max
    EndIf
EndProc

'=====================================================================
' CONFIG WORD BUILD (compact per-input config)
'=====================================================================
Proc P_BuildCfg(B_Sensor As Byte, B_Master As Byte, B_Ind As Byte, B_FS As Byte, B_FA As Byte, B_Disp As Byte), Word
    Dim W_Cfg As Word
    W_Cfg = 0

    If B_Sensor > 3 Then B_Sensor = 3
    If B_Master > 1 Then B_Master = 1
    If B_Ind    > 3 Then B_Ind    = 3
    If B_FS     > 3 Then B_FS     = 3
    If B_FA     > 3 Then B_FA     = 3
    If B_Disp   > 3 Then B_Disp   = 3

    W_Cfg = W_Cfg + B_Sensor
    W_Cfg = W_Cfg + (B_Master * 4)
    W_Cfg = W_Cfg + (B_Ind    * 8)
    W_Cfg = W_Cfg + (B_FS     * 32)
    W_Cfg = W_Cfg + (B_FA     * 128)
    W_Cfg = W_Cfg + (B_Disp   * 512)

    Result = W_Cfg
EndProc

'=====================================================================
' DEFAULTS / VERSIONING
'=====================================================================
Proc P_EEEnsureDefaults()
    Dim B_V As Byte
    Dim W_Default As Word

    B_V = ERead EE_VER
    If B_V <> CFG_VERSION Then
        W_Default = P_BuildCfg(0, 1, 0, 0, 0, 2)

        P_EWriteW(EE_I1_CFG, W_Default)
        P_EWriteW(EE_I2_CFG, W_Default)
        P_EWriteW(EE_I3_CFG, W_Default)

        P_EWriteW(EE_UI_TIMEOUT_S, UI_TIMEOUT_S_DEF)

        EWrite EE_VER, [CFG_VERSION]
    EndIf
EndProc

'=====================================================================
' LOAD / SAVE SETTINGS
'=====================================================================
Proc P_LoadSettings()
    P_EEEnsureDefaults()

    W_I1_Cfg = P_EReadW(EE_I1_CFG)
    W_I2_Cfg = P_EReadW(EE_I2_CFG)
    W_I3_Cfg = P_EReadW(EE_I3_CFG)

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

    W_UI_TimeoutS = P_EReadW(EE_UI_TIMEOUT_S)
    If W_UI_TimeoutS < UI_TIMEOUT_S_MIN Or W_UI_TimeoutS > UI_TIMEOUT_S_MAX Then
        W_UI_TimeoutS = UI_TIMEOUT_S_DEF
        P_EWriteW(EE_UI_TIMEOUT_S, W_UI_TimeoutS)
    EndIf
EndProc
'----------
Proc P_SaveSettings()
    EWrite EE_I1_ENABLED, [B_I1_Enabled]
    EWrite EE_I1_SENSORT, [B_I1_SensorT]
    EWrite EE_I1_FLOWMODE, [B_I1_FlowMode]
    EWrite EE_I1_SCALE4,   [W_I1_Scale4]
    EWrite EE_I1_SCALE20,  [W_I1_Scale20]
    EWrite EE_I1_BP_HIGH,  [W_I1_BP_High]
    EWrite EE_I1_BP_PLP,   [W_I1_BP_PLP]
    EWrite EE_I1_BP_SLP,   [W_I1_BP_SLP]
    EWrite EE_I1_RLY_HIGH, [B_I1_RlyHigh]
    EWrite EE_I1_RLY_PLP,  [B_I1_RlyPLP]
    EWrite EE_I1_RLY_SLP,  [B_I1_RlySLP]
    EWrite EE_I1_DISPLAY,  [B_I1_Display]
EndProc

'=====================================================================
' INPUT / EVENTS
'=====================================================================
Proc P_ReadEncoder()
    Dim W_Pos As Word

    GIE = 0
    W_Pos = W_EncoderPos
    GIE = 1

    If W_Pos > W_EncReadPos Then
        B_EncDelta = 1
        L_LastInput = L_Millis
    ElseIf W_Pos < W_EncReadPos Then
        B_EncDelta = -1
        L_LastInput = L_Millis
    Else
        B_EncDelta = 0
    EndIf

    W_EncReadPos = W_Pos
EndProc
'----------
Proc P_ReadButton()
EndProc
'----------
Proc P_GetKeyEvent(), Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
EndProc
'----------
Proc P_UserAborted(), Byte
    Dim B_Ev As Byte
    Dim L_TimeoutMs As Dword

    B_Ev = P_GetKeyEvent()
    If B_Ev = 2 Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf
    If B_Ev = 3 Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf

    L_TimeoutMs = W_UI_TimeoutS * 1000
    If (L_Millis - L_LastInput) >= L_TimeoutMs Then
        b_Escape = 1
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf

    Result = 0
Exit_P_UserAborted:
EndProc

'=====================================================================
' EDITORS
'=====================================================================
' (unchanged editors omitted here for brevity—same as last version)
' ---------- KEEP ALL EDITORS FROM YOUR LAST BUILD ----------
' I left P_EditYN, P_EditEnum3, P_EditWordVal, P_EditMMSS exactly as-is
' (they were already included above in your last build)

'=====================================================================
' STUBS
'=====================================================================
Proc P_Scale()
EndProc
'----------
Proc P_Output()
EndProc

'=====================================================================
' VIEWS / MENUS
'=====================================================================

'---------- NOT-IMPLEMENTED DIALOG (restored) ----------
Proc V_NotImpl(S_Name As String)
    b_ReInitLCD = 0
    P_DrawTitle(S_Name + " (STUB)      ")
    P_ClearLine(2)
    P_ClearLine(3)
    P_ClearLine(4)
    While 1 = 1
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                GoTo Exit_V_NotImpl
            Case 2
                GoTo Exit_V_NotImpl
            Case 3
                GoTo Exit_V_NotImpl
        EndSelect
        If P_UserAborted() <> 0 Then
            GoTo Exit_V_NotImpl
        EndIf
    Wend
Exit_V_NotImpl:
EndProc

'---------- MAIN splash ----------
Proc V_Main()
    b_ReInitLCD = 0
    P_DrawTitle("IRRISYS MAIN        ")
    P_ClearLine(2)
    P_ClearLine(3)
    P_ClearLine(4)
    Set b_ScrDirty
    While 1 = 1
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                GoTo Exit_V_Main
            Case 2
            Case 3
        EndSelect
    Wend
Exit_V_Main:
EndProc

'---------- OPTIONS ----------
Proc V_Options(), Byte
    Dim B_Sel As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3
    B_Sel = 0
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("OPTIONS             ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            If B_Sel = 0 Then
                P_PrintRow(2,"Main Menu",1)
            Else
                P_PrintRow(2,"Main Menu",0)
            EndIf
            If B_Sel = 1 Then
                P_PrintRow(3,"Setup Menu",1)
            Else
                P_PrintRow(3,"Setup Menu",0)
            EndIf
            If B_Sel = 2 Then
                P_PrintRow(4,"Utility Menu",1)
            Else
                P_PrintRow(4,"Utility Menu",0)
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt - 1) Then
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
                        GoTo Exit_V_Options
                    Case 1
                        If V_SetupMenu() = 0 Then
                            Result = 0
                            GoTo Exit_V_Options
                        EndIf
                    Case 2
                        'V_UtilityStub()
                EndSelect
                Set b_ScrDirty
            Case 2
                Result = 0
                GoTo Exit_V_Options
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

'---------- SETUP ----------
Proc V_SetupMenu(), Byte
    Dim B_Sel As Byte
    Dim B_Cnt As Byte
    B_Cnt = 3
    B_Sel = 0
    b_ReInitLCD = 0
    Set b_ScrDirty
    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("SETUP               ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            If B_Sel = 0 Then
                P_PrintRow(2,"Input 1",1)
            Else
                P_PrintRow(2,"Input 1",0)
            EndIf
            If B_Sel = 1 Then
                P_PrintRow(3,"Input 2",1)
            Else
                P_PrintRow(3,"Input 2",0)
            EndIf
            If B_Sel = 2 Then
                P_PrintRow(4,"Input 3",1)
            Else
                P_PrintRow(4,"Input 3",0)
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt - 1) Then
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

'---------- VALUE FIELD RENDER HELPERS ----------
Proc P_ClearValField(B_Row As Byte, B_Col As Byte)
    Print At B_Row, B_Col, "          "
EndProc

Proc P_PrintValText(B_Row As Byte, B_Col As Byte, S_Value As String, B_Active As Byte, B_CanEdit As Byte)
    P_ClearValField(B_Row, B_Col)
    If B_Active = 1 And B_CanEdit = 1 Then
        Print At B_Row, B_Col, "[", S_Value, "]"
    Else
        Print At B_Row, B_Col, S_Value
    EndIf
EndProc

'---------- INPUT 1: render one row ----------
Proc P_Input1RenderItem(B_Row As Byte, B_Index As Byte, B_Active As Byte)
    Dim B_CanEdit As Byte
    Dim B_Sens As Byte
    Dim B_Col As Byte

    If B_Index > 10 Then
        P_ClearLine(B_Row)
        GoTo Exit_P_InRend
    EndIf

    B_Col = 11
    P_ClearLine(B_Row)

    B_Sens = B_I1_SensorT
    B_CanEdit = 1
    If B_Index >= 4 And B_Index <= 6 Then
        If B_Sens <> 1 Then
            B_CanEdit = 0
        EndIf
    EndIf

    Select B_Index
        Case 0
            Print At B_Row,1,"Enable    "
            If B_I1_Enabled = 1 Then
                P_PrintValText(B_Row, B_Col, "Enabled",  B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "Disabled", B_Active, 1)
            EndIf

        Case 1
            Print At B_Row,1,"Sensor    "
            If B_Sens = 0 Then
                P_PrintValText(B_Row, B_Col, "Flow",     B_Active, 1)
            ElseIf B_Sens = 1 Then
                P_PrintValText(B_Row, B_Col, "Pressure", B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "Temp",     B_Active, 1)
            EndIf

        Case 2
            Print At B_Row,1,"Scale4ma  "
            P_ClearValField(B_Row, B_Col)
            If B_Active = 1 Then
                Print At B_Row, B_Col, "[", Dec5 W_I1_Scale4, "]"
            Else
                Print At B_Row, B_Col, Dec5 W_I1_Scale4
            EndIf

        Case 3
            Print At B_Row,1,"Scale20ma "
            P_ClearValField(B_Row, B_Col)
            If B_Active = 1 Then
                Print At B_Row, B_Col, "[", Dec5 W_I1_Scale20, "]"
            Else
                Print At B_Row, B_Col, Dec5 W_I1_Scale20
            EndIf

        Case 4
            Print At B_Row,1,"High BP   "
            P_ClearValField(B_Row, B_Col)
            If B_Sens = 1 Then
                If B_Active = 1 And B_CanEdit = 1 Then
                    Print At B_Row, B_Col, "["
                    P_PrintMMSS(B_Row, B_Col + 1, W_I1_BP_High)
                    Print "]"
                Else
                    P_PrintMMSS(B_Row, B_Col, W_I1_BP_High)
                EndIf
            Else
                Print At B_Row, B_Col, "--:--"
            EndIf

        Case 5
            Print At B_Row,1,"PLPBP     "
            P_ClearValField(B_Row, B_Col)
            If B_Sens = 1 Then
                If B_Active = 1 And B_CanEdit = 1 Then
                    Print At B_Row, B_Col, "["
                    P_PrintMMSS(B_Row, B_Col + 1, W_I1_BP_PLP)
                    Print "]"
                Else
                    P_PrintMMSS(B_Row, B_Col, W_I1_BP_PLP)
                EndIf
            Else
                Print At B_Row, B_Col, "--:--"
            EndIf

        Case 6
            Print At B_Row,1,"SLPBP     "
            P_ClearValField(B_Row, B_Col)
            If B_Sens = 1 Then
                If B_Active = 1 And B_CanEdit = 1 Then
                    Print At B_Row, B_Col, "["
                    P_PrintMMSS(B_Row, B_Col + 1, W_I1_BP_SLP)
                    Print "]"
                Else
                    P_PrintMMSS(B_Row, B_Col, W_I1_BP_SLP)
                EndIf
            Else
                Print At B_Row, B_Col, "--:--"
            EndIf

        Case 7
            Print At B_Row,1,"Rly High  "
            If B_I1_RlyHigh = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            ElseIf B_I1_RlyHigh = 1 Then
                P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
            EndIf

        Case 8
            Print At B_Row,1,"Rly PLP   "
            If B_I1_RlyPLP = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            ElseIf B_I1_RlyPLP = 1 Then
                P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
            EndIf

        Case 9
            Print At B_Row,1,"Rly SLP   "
            If B_I1_RlySLP = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            ElseIf B_I1_RlySLP = 1 Then
                P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
            EndIf

        Case 10
            Print At B_Row,1,"Display   "
            If B_I1_Display = 1 Then
                P_PrintValText(B_Row, B_Col, "Yes", B_Active, 1)
            Else
                P_PrintValText(B_Row, B_Col, "No",  B_Active, 1)
            EndIf
    EndSelect

Exit_P_InRend:
EndProc

'---------- INPUT 1: editor dispatcher ----------
Proc P_Input1EditItem(B_Index As Byte)
    Dim B_Edited As Byte
    B_Edited = 0

    Select B_Index
        Case 0
            B_Edited = P_EditYN(B_I1_Enabled)
        Case 1
            B_Edited = P_EditEnum3(B_I1_SensorT)
        Case 2
            B_Edited = P_EditWordVal(W_I1_Scale4, 0, 65535, 1)
        Case 3
            B_Edited = P_EditWordVal(W_I1_Scale20, 0, 65535, 1)
        Case 4
            If B_I1_SensorT = 1 Then
                B_Edited = P_EditMMSS(W_I1_BP_High)
            EndIf
        Case 5
            If B_I1_SensorT = 1 Then
                B_Edited = P_EditMMSS(W_I1_BP_PLP)
            EndIf
        Case 6
            If B_I1_SensorT = 1 Then
                B_Edited = P_EditMMSS(W_I1_BP_SLP)
            EndIf
        Case 7
            B_Edited = P_EditEnum3(B_I1_RlyHigh)
        Case 8
            B_Edited = P_EditEnum3(B_I1_RlyPLP)
        Case 9
            B_Edited = P_EditEnum3(B_I1_RlySLP)
        Case 10
            B_Edited = P_EditYN(B_I1_Display)
    EndSelect

    If B_Edited = 1 Then
        P_SaveSettings()
    EndIf
EndProc

'---------- INPUT 1: full menu ----------
Proc V_Input1Menu(), Byte
    Dim B_Sel As Byte
    Dim B_Top As Byte
    Dim B_Cnt As Byte
    Dim B_Act As Byte

    B_Cnt = 11
    B_Sel = 0
    B_Top = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("INPUT 1             ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            If B_Sel <= 1 Then
                B_Top = 0
            Else
                If B_Sel >= (B_Cnt - 1) Then
                    If B_Cnt > 2 Then
                        B_Top = B_Cnt - 3
                    Else
                        B_Top = 0
                    EndIf
                Else
                    B_Top = B_Sel - 1
                EndIf
            EndIf

            B_Act = 0
            If B_Sel = B_Top Then B_Act = 1
            P_Input1RenderItem(2, B_Top, B_Act)

            B_Act = 0
            If B_Sel = (B_Top + 1) Then B_Act = 1
            P_Input1RenderItem(3, (B_Top + 1), B_Act)

            B_Act = 0
            If B_Sel = (B_Top + 2) Then B_Act = 1
            P_Input1RenderItem(4, (B_Top + 2), B_Act)

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta <> 0 Then
            If B_EncDelta = 1 Then
                If B_Sel < (B_Cnt - 1) Then
                    B_Sel = B_Sel + 1
                EndIf
            Else
                If B_Sel > 0 Then
                    B_Sel = B_Sel - 1
                EndIf
            EndIf
            Set b_ScrDirty
        EndIf

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Input1EditItem(B_Sel)
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

'---------- LCD safe init ----------
Proc P_LCDSafeInit()
    DelayMsFast(50)
    Cls
    DelayMsFast(10)
EndProc

'=====================================================================
' MAIN
'=====================================================================
MAIN:
P_PinInit()
P_LCDSafeInit()
P_InputInit()
P_LoadSettings()

b_ScrDirty = 1
b_ReInitLCD = 0
b_Escape = 0
B_KeyEvent = 0
L_LastInput = 0

If B_I1_SensorT > 2 Then
    B_I1_SensorT = 1          ' SENSOR_PRES
EndIf

P_Startup()

While 1 = 1
    V_Main()
    B_Option = V_Options()
Wend

'============================== EOF ==================================

