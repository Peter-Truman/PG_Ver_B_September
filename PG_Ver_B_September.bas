'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
' Rev: 2025-09-07a  (no ":" separators, proc names =14 chars, RJ fields)
'=====================================================================

'------------------------------ Device -------------------------------
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

'------------------------- Oscillator & I/O ---------------------------
OSCCON    = %01110000                    ' 8 MHz
OSCTUNE.6 = 1                            ' PLL x4 -> 32 MHz
ADCON1    = $0F                          ' All digital

All_Digital = True
Declare Xtal = 32
Declare PORTB_Pullups = On

'------------------------------- LCD ---------------------------------
' HD44780 20x4 in 4-bit mode (Positron driver pins)
Declare LCD_Type = 0
Declare LCD_DTPin = PORTA.0              ' D4..D7 = RA0..RA3
Declare LCD_ENPin = PORTA.7              ' E
Declare LCD_RSPin = PORTA.6              ' RS
Declare LCD_Interface = 4
Declare LCD_Lines = 4

'------------------------------ UART ---------------------------------
Declare Hserial_Baud  = 115200
Declare Hserial_RCSTA = %10010000
Declare Hserial_TXSTA = %00100100

'-------------------------- Pins & Symbols ---------------------------
Symbol _ENC_A   = PORTB.1
Symbol _ENC_B   = PORTB.2
Symbol _BTN     = PORTB.6                ' Active-low push button
Symbol _ENC_SW  = _BTN
Symbol _BUZZER  = PORTC.2                ' Active-high drive

'--------------------------- UI Timing -------------------------------
Symbol BTN_SHORT_MS     = 200
Symbol BTN_LONG_MS      = 900
Symbol BTN_VLONG_MS     = 1800
Symbol LONG_MS          = BTN_LONG_MS
Symbol VERY_LONG_MS     = BTN_VLONG_MS

' Menu timeout (seconds) – stored as Word in EEPROM
Symbol UI_TIMEOUT_S_MIN = 10
Symbol UI_TIMEOUT_S_MAX = 300
Symbol UI_TIMEOUT_S_DEF = 30

' Buzzer base pulse (ms) – stored as Word in EEPROM
Symbol UI_PULSE_MS_DEF  = 50
Symbol UI_PULSE_MS_MIN  = 10
Symbol UI_PULSE_MS_MAX  = 2000

'----------------------------- Enums ---------------------------------
Symbol MODE_NO    = 0
Symbol MODE_PULSE = 1
Symbol MODE_LATCH = 2

Symbol YES = 1
Symbol NO  = 0

Symbol SENSOR_FLOW = 0
Symbol SENSOR_PRES = 1
Symbol SENSOR_TEMP = 2

'------------------------------ IRQ Bits -----------------------------
Symbol TMR0IF = INTCON.2
Symbol TMR0IE = INTCON.5
Symbol GIE    = INTCON.7

'---------------------------- EEPROM Map -----------------------------
Symbol EE_VER            = 0                 ' Config version byte

' Legacy per-input block (Input1 base kept; mirrored to 2/3)
Symbol EE_I1_BASE        = 10
Symbol EE_I1_ENABLED     = EE_I1_BASE + 0
Symbol EE_I1_SENSORT     = EE_I1_BASE + 1
Symbol EE_I1_FLOWMODE    = EE_I1_BASE + 2
Symbol EE_I1_SCALE4      = EE_I1_BASE + 4   ' signed stored as Word (2C)
Symbol EE_I1_SCALE20     = EE_I1_BASE + 6   ' signed stored as Word (2C)
Symbol EE_I1_BP_HIGH     = EE_I1_BASE + 8
Symbol EE_I1_BP_PLP      = EE_I1_BASE + 10
Symbol EE_I1_BP_SLP      = EE_I1_BASE + 12
Symbol EE_I1_RLY_HIGH    = EE_I1_BASE + 14
Symbol EE_I1_RLY_PLP     = EE_I1_BASE + 15
Symbol EE_I1_RLY_SLP     = EE_I1_BASE + 16
Symbol EE_I1_DISPLAY     = EE_I1_BASE + 17

Symbol EE_I_BLOCK_SIZE   = 18
Symbol EE_I2_BASE        = EE_I1_BASE + EE_I_BLOCK_SIZE
Symbol EE_I3_BASE        = EE_I2_BASE + EE_I_BLOCK_SIZE

' System settings block
Symbol EE_SYS_BASE       = 200
Symbol EE_UI_TIMEOUT_S   = EE_SYS_BASE + 0   ' Word seconds
Symbol EE_UI_PULSE_MS    = EE_SYS_BASE + 2   ' Word ms

' Compact per-input config words (reserved for future)
Symbol EE_INCFG_BASE     = 300
Symbol EE_I1_CFG         = EE_INCFG_BASE + 0 ' Word
Symbol EE_I2_CFG         = EE_INCFG_BASE + 2 ' Word
Symbol EE_I3_CFG         = EE_INCFG_BASE + 4 ' Word

' Versioning
Symbol CFG_VERSION       = 2

'----------------------------- Globals -------------------------------
Dim b_ReInitLCD  As Bit
Dim b_ScrDirty   As Bit
Dim b_Escape     As Bit

Dim L_Millis     As Dword
Dim L_LastInput  As Dword

Dim B_EncDelta   As SByte
Dim B_KeyEvent   As Byte
Dim W_Beep       As Word

' Dynamic menu item IDs (Input1)
Symbol I1_ITEM_BACK    = 200
Symbol I1_ITEM_LOWBP   = 11      ' Temp/Flow "Low" BP
Symbol I1_ITEM_RLYLOW  = 12
Symbol I1_ITEM_TYPE    = 13      ' Flow: Analog/Digital
Symbol I1_ITEM_UNITS   = 14      ' Flow: %, LpS

' Flow sub-options
Symbol FLOWTYPE_ANALOG = 0
Symbol FLOWTYPE_DIGITAL= 1
Symbol FLOWU_PERCENT   = 0
Symbol FLOWU_LPS       = 1

' Input1 extra mirrors (not yet persisted)
Dim W_I1_BP_Low    As Word
Dim B_I1_RlyLow    As Byte
Dim B_I1_FlowType  As Byte
Dim B_I1_FlowUnits As Byte

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

' System/UI mirrors
Dim B_Option       As Byte
Dim W_UI_TimeoutS  As Word
Dim W_UI_PulseMs   As Word

' Button latches
Dim B_MedSent As Byte
Dim B_LongSent As Byte

' Navigation propagate code: 0=normal, 1=back, 2=to main
Dim B_NavCode      As Byte

' Compact cfg mirrors
Dim W_I1_Cfg       As Word
Dim W_I2_Cfg       As Word
Dim W_I3_Cfg       As Word

' Legacy mirrors in use by UI
Dim B_I1_Enabled  As Byte
Dim B_I1_SensorT  As Byte
Dim B_I1_FlowMode As Byte
Dim W_I1_Scale4   As Word     ' signed 2C in EEPROM
Dim W_I1_Scale20  As Word     ' signed 2C in EEPROM
Dim W_I1_BP_High  As Word
Dim W_I1_BP_PLP   As Word
Dim W_I1_BP_SLP   As Word
Dim B_I1_RlyHigh  As Byte
Dim B_I1_RlyPLP   As Byte
Dim B_I1_RlySLP   As Byte
Dim B_I1_Display  As Byte

' Input2 mirrors
Dim B_I2_Enabled  As Byte
Dim B_I2_SensorT  As Byte
Dim B_I2_FlowMode As Byte
Dim W_I2_Scale4   As Word
Dim W_I2_Scale20  As Word
Dim W_I2_BP_High  As Word
Dim W_I2_BP_PLP   As Word
Dim W_I2_BP_SLP   As Word
Dim B_I2_RlyHigh  As Byte
Dim B_I2_RlyPLP   As Byte
Dim B_I2_RlySLP   As Byte
Dim B_I2_Display  As Byte

' Input3 mirrors
Dim B_I3_Enabled  As Byte
Dim B_I3_SensorT  As Byte
Dim B_I3_FlowMode As Byte
Dim W_I3_Scale4   As Word
Dim W_I3_Scale20  As Word
Dim W_I3_BP_High  As Word
Dim W_I3_BP_PLP   As Word
Dim W_I3_BP_SLP   As Word
Dim B_I3_RlyHigh  As Byte
Dim B_I3_RlyPLP   As Byte
Dim B_I3_RlySLP   As Byte
Dim B_I3_Display  As Byte

'=====================================================================
' TIMER0 ISR (1ms) – debounce, encoder, button, beeper
'=====================================================================
T0CON = %11000100
TMR0L = 6
Clear TMR0IF
TMR0IE = 1
GIE    = 1

On_Hardware_Interrupt GoTo Isr
GoTo Over_Interrupt

Isr:
    Context Save

    If TMR0IF = 1 Then
        TMR0L = 6
        Clear TMR0IF
        Inc L_Millis

        Inc B_RE_Count
        If B_RE_Count > 9 Then
            Dim B_NewA     As Byte
            Dim B_NewB     As Byte
            Dim B_NewBtn   As Byte
            Dim B_Curr     As Byte
            Dim B_Combined As Byte

            B_NewA   = _ENC_A
            B_NewB   = _ENC_B
            B_NewBtn = _BTN

            ' Debounce A
            If B_NewA <> B_AState Then
                Inc B_DebA
                If B_DebA >= 2 Then
                    B_AState = B_NewA
                    B_DebA = 0
                    L_LastInput = L_Millis
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
                    L_LastInput = L_Millis
                EndIf
            Else
                B_DebB = 0
            EndIf

            ' Debounce Button (active-low)
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
            B_Curr = (B_AState * 2) + B_BState
            B_Combined = (B_LastState * 4) + B_Curr

            Select B_Combined
                Case %0001
                    Dec S_Qacc
                Case %0111
                    Dec S_Qacc
                Case %1110
                    Dec S_Qacc
                Case %1000
                    Dec S_Qacc
                Case %0010
                    Inc S_Qacc
                Case %1011
                    Inc S_Qacc
                Case %1101
                    Inc S_Qacc
                Case %0100
                    Inc S_Qacc
            EndSelect

            ' Commit on detent (00) with threshold
            If B_Curr = 0 Then
                If S_Qacc >= 2 Then
                    Inc W_EncoderPos
                    L_LastInput = L_Millis
                Else
                    If S_Qacc <= -2 Then
                        If W_EncoderPos > 0 Then
                            Dec W_EncoderPos
                        EndIf
                        L_LastInput = L_Millis
                    EndIf
                EndIf
                S_Qacc = 0
            EndIf
            B_LastState = B_Curr

            ' Button press timing -> SHORT only (on release)
            If B_ButtonState = 0 Then
                If W_BtnHoldMS <= 65525 Then
                    W_BtnHoldMS = W_BtnHoldMS + 10
                EndIf
            Else
                If W_BtnHoldMS >= BTN_SHORT_MS Then
                    B_KeyEvent = 1
                    L_LastInput = L_Millis
                EndIf
                W_BtnHoldMS = 0
            EndIf

            Clear B_RE_Count
        EndIf

        ' Beeper service
        If W_Beep > 0 Then
            _BUZZER = 1
            Dec W_Beep
        Else
            _BUZZER = 0
        EndIf
    EndIf

    Context Restore
'=====================================================================
Over_Interrupt:

'=====================================================================
' LOW-LEVEL LCD (robust 4-bit init for NHD-0420AZ / ST7066U)
'=====================================================================
Symbol LCD_RS = PORTA.6
Symbol LCD_E  = PORTA.7

Proc LCD_SetNib(B_N As Byte)
    If B_N.0 = 1 Then
        Set PORTA.0
    Else
        Clear PORTA.0
    EndIf

    If B_N.1 = 1 Then
        Set PORTA.1
    Else
        Clear PORTA.1
    EndIf

    If B_N.2 = 1 Then
        Set PORTA.2
    Else
        Clear PORTA.2
    EndIf

    If B_N.3 = 1 Then
        Set PORTA.3
    Else
        Clear PORTA.3
    EndIf
EndProc

Proc LCD_PulseE()
    Set LCD_E
    DelayUS 1
    Clear LCD_E
    DelayUS 40
EndProc

Proc LCD_WriteNib(B_N As Byte)
    LCD_SetNib(B_N)
    LCD_PulseE()
EndProc

Proc LCD_WriteCmd(B_Cmd As Byte)
    Dim B_N As Byte
    LCD_RS = 0

    B_N = B_Cmd / 16
    LCD_WriteNib(B_N)

    B_N = B_Cmd // 16
    LCD_WriteNib(B_N)

    If B_Cmd = $01 Or B_Cmd = $02 Then
        DelayMS 2
    Else
        DelayUS 50
    EndIf
EndProc

Proc LCD_WriteDat(B_Data As Byte)
    Dim B_N As Byte
    LCD_RS = 1

    B_N = B_Data / 16
    LCD_WriteNib(B_N)

    B_N = B_Data // 16
    LCD_WriteNib(B_N)

    DelayUS 50
EndProc

Proc P_LCDHardInit()
    TRISA = %00010000
    TRISB = %01000110
    TRISC = %00000000

    Clear PORTA
    Clear PORTB
    Clear PORTC

    Clear LCD_RS
    Clear LCD_E

    DelayMS 50

    LCD_RS = 0

    ' Force 8-bit mode three times (hi-nibble 0x3)
    LCD_WriteNib($03)
    DelayMS 5

    LCD_WriteNib($03)
    DelayUS 150

    LCD_WriteNib($03)
    DelayUS 150

    ' Switch to 4-bit
    LCD_WriteNib($02)
    DelayUS 150

    ' Function set: 4-bit, 2 lines, 5x8
    LCD_WriteCmd($28)
    LCD_WriteCmd($08)    ' Display OFF
    LCD_WriteCmd($01)    ' Clear
    LCD_WriteCmd($06)    ' Entry mode
    LCD_WriteCmd($0C)    ' Display ON, cursor off
EndProc

'--------------------- LCD cursor underline control -------------------
Proc LCD_CursorOn()
    LCD_WriteCmd($0E)    ' display on, cursor on, blink off
EndProc

Proc LCD_CursorOff()
    LCD_WriteCmd($0C)    ' display on, cursor off, blink off
EndProc

Proc LCD_SetCursor(B_Row As Byte, B_Col As Byte)
    Dim B_Base As Byte
    Select B_Row
        Case 1
            B_Base = $00
        Case 2
            B_Base = $40
        Case 3
            B_Base = $14
        Case Else
            B_Base = $54
    EndSelect
    Dim B_Addr As Byte
    B_Addr = $80 + B_Base + (B_Col - 1)
    LCD_WriteCmd(B_Addr)

EndProc

'=====================================================================
' EEPROM HELPERS
'=====================================================================
Proc P_EReadW(W_Addr As Word), Word
    Result = ERead W_Addr
EndProc

Proc P_EWriteW(W_Addr As Word, W_Val As Word)
    Dim W_Tmp As Word
    W_Tmp = ERead W_Addr
    If W_Tmp <> W_Val Then
        EWrite W_Addr, [W_Val]
    EndIf
EndProc

Proc P_EEBaseIn(B_In As Byte), Word
    If B_In = 1 Then
        Result = EE_I1_BASE
    Else
        If B_In = 2 Then
            Result = EE_I2_BASE
        Else
            Result = EE_I3_BASE
        EndIf
    EndIf
EndProc

Proc P_LoadInput(B_In As Byte)
    Dim W_Base As Word
    W_Base = P_EEBaseIn(B_In)

    If B_In = 1 Then
        B_I1_Enabled  = ERead (W_Base + 0)
        B_I1_SensorT  = ERead (W_Base + 1)
        B_I1_FlowMode = ERead (W_Base + 2)
        W_I1_Scale4   = P_EReadW(W_Base + 4)
        W_I1_Scale20  = P_EReadW(W_Base + 6)
        W_I1_BP_High  = P_EReadW(W_Base + 8)
        W_I1_BP_PLP   = P_EReadW(W_Base + 10)
        W_I1_BP_SLP   = P_EReadW(W_Base + 12)
        B_I1_RlyHigh  = ERead (W_Base + 14)
        B_I1_RlyPLP   = ERead (W_Base + 15)
        B_I1_RlySLP   = ERead (W_Base + 16)
        B_I1_Display  = ERead (W_Base + 17)
    Else
        If B_In = 2 Then
            B_I2_Enabled  = ERead (W_Base + 0)
            B_I2_SensorT  = ERead (W_Base + 1)
            B_I2_FlowMode = ERead (W_Base + 2)
            W_I2_Scale4   = P_EReadW(W_Base + 4)
            W_I2_Scale20  = P_EReadW(W_Base + 6)
            W_I2_BP_High  = P_EReadW(W_Base + 8)
            W_I2_BP_PLP   = P_EReadW(W_Base + 10)
            W_I2_BP_SLP   = P_EReadW(W_Base + 12)
            B_I2_RlyHigh  = ERead (W_Base + 14)
            B_I2_RlyPLP   = ERead (W_Base + 15)
            B_I2_RlySLP   = ERead (W_Base + 16)
            B_I2_Display  = ERead (W_Base + 17)
        Else
            B_I3_Enabled  = ERead (W_Base + 0)
            B_I3_SensorT  = ERead (W_Base + 1)
            B_I3_FlowMode = ERead (W_Base + 2)
            W_I3_Scale4   = P_EReadW(W_Base + 4)
            W_I3_Scale20  = P_EReadW(W_Base + 6)
            W_I3_BP_High  = P_EReadW(W_Base + 8)
            W_I3_BP_PLP   = P_EReadW(W_Base + 10)
            W_I3_BP_SLP   = P_EReadW(W_Base + 12)
            B_I3_RlyHigh  = ERead (W_Base + 14)
            B_I3_RlyPLP   = ERead (W_Base + 15)
            B_I3_RlySLP   = ERead (W_Base + 16)
            B_I3_Display  = ERead (W_Base + 17)
        EndIf
    EndIf
EndProc

Proc P_SaveInput(B_In As Byte)
    Dim W_Base As Word
    W_Base = P_EEBaseIn(B_In)

    If B_In = 1 Then
        EWrite (W_Base + 0),  [B_I1_Enabled]
        EWrite (W_Base + 1),  [B_I1_SensorT]
        EWrite (W_Base + 2),  [B_I1_FlowMode]
        P_EWriteW(W_Base + 4,  W_I1_Scale4)
        P_EWriteW(W_Base + 6,  W_I1_Scale20)
        P_EWriteW(W_Base + 8,  W_I1_BP_High)
        P_EWriteW(W_Base + 10, W_I1_BP_PLP)
        P_EWriteW(W_Base + 12, W_I1_BP_SLP)
        EWrite (W_Base + 14), [B_I1_RlyHigh]
        EWrite (W_Base + 15), [B_I1_RlyPLP]
        EWrite (W_Base + 16), [B_I1_RlySLP]
        EWrite (W_Base + 17), [B_I1_Display]
    Else
        If B_In = 2 Then
            EWrite (W_Base + 0),  [B_I2_Enabled]
            EWrite (W_Base + 1),  [B_I2_SensorT]
            EWrite (W_Base + 2),  [B_I2_FlowMode]
            P_EWriteW(W_Base + 4,  W_I2_Scale4)
            P_EWriteW(W_Base + 6,  W_I2_Scale20)
            P_EWriteW(W_Base + 8,  W_I2_BP_High)
            P_EWriteW(W_Base + 10, W_I2_BP_PLP)
            P_EWriteW(W_Base + 12, W_I2_BP_SLP)
            EWrite (W_Base + 14), [B_I2_RlyHigh]
            EWrite (W_Base + 15), [B_I2_RlyPLP]
            EWrite (W_Base + 16), [B_I2_RlySLP]
            EWrite (W_Base + 17), [B_I2_Display]
        Else
            EWrite (W_Base + 0),  [B_I3_Enabled]
            EWrite (W_Base + 1),  [B_I3_SensorT]
            EWrite (W_Base + 2),  [B_I3_FlowMode]
            P_EWriteW(W_Base + 4,  W_I3_Scale4)
            P_EWriteW(W_Base + 6,  W_I3_Scale20)
            P_EWriteW(W_Base + 8,  W_I3_BP_High)
            P_EWriteW(W_Base + 10, W_I3_BP_PLP)
            P_EWriteW(W_Base + 12, W_I3_BP_SLP)
            EWrite (W_Base + 14), [B_I3_RlyHigh]
            EWrite (W_Base + 15), [B_I3_RlyPLP]
            EWrite (W_Base + 16), [B_I3_RlySLP]
            EWrite (W_Base + 17), [B_I3_Display]
        EndIf
    EndIf
EndProc

'=====================================================================
' SIGNED CONVERSION HELPERS (two's complement cast)
'=====================================================================
Proc P_W2S(W_Val As Word), SWord
    Result = W_Val
EndProc

Proc P_S2W(I_Val As SWord), Word
    Result = I_Val
EndProc

'=====================================================================
' INPUT1 MIRROR SYNC (legacy compat)
'=====================================================================
Proc P_I1SyncLoad()
    If B_I1_FlowMode.0 = 0 Then
        B_I1_FlowType = FLOWTYPE_ANALOG
    Else
        B_I1_FlowType = FLOWTYPE_DIGITAL
    EndIf

    If B_I1_FlowMode.1 = 0 Then
        B_I1_FlowUnits = FLOWU_PERCENT
    Else
        B_I1_FlowUnits = FLOWU_LPS
    EndIf

    W_I1_BP_Low = W_I1_BP_SLP
    B_I1_RlyLow = B_I1_RlySLP
EndProc

Proc P_I1SyncSave()
    Dim B_Mode As Byte

    B_Mode = 0
    If B_I1_FlowType = FLOWTYPE_DIGITAL Then
        Set B_Mode.0
    Else
        Clear B_Mode.0
    EndIf

    If B_I1_FlowUnits = FLOWU_LPS Then
        Set B_Mode.1
    Else
        Clear B_Mode.1
    EndIf

    B_I1_FlowMode = B_Mode

    W_I1_BP_SLP = W_I1_BP_Low
    B_I1_RlySLP = B_I1_RlyLow
EndProc

'=====================================================================
' SYSTEM / GPIO / BEEPER
'=====================================================================
Proc P_SaveSystem()
    P_EWriteW(EE_UI_TIMEOUT_S, W_UI_TimeoutS)
    P_EWriteW(EE_UI_PULSE_MS,  W_UI_PulseMs)
EndProc

Proc P_PinInit()
    TRISA = %00010000
    TRISB = %01000110
    TRISC = %00000000
EndProc

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

Proc P_Beeps(B_Type As Byte)
    Select B_Type
        Case 0
            W_Beep = 10
        Case 1
            W_Beep = 20
        Case 2
            W_Beep = 50
        Case 3
            W_Beep = 200
        Case 4
            W_Beep = 1000
    EndSelect
EndProc

Proc P_Startup()
    Dim B_Beepcount As Byte
    For B_Beepcount = 0 To 5
        P_Beeps(2)
        DelayMS 100
    Next B_Beepcount
EndProc

Proc DlyMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
EndProc

'=====================================================================
' UI TEXT / DRAW HELPERS
'=====================================================================
Proc P_ClrLine(B_Row As Byte)
    Print At B_Row,1,"                    "
EndProc

Proc P_DrawTitle(S_Title As String)
    Print At 1,1,"                    "
    Print At 1,1,S_Title
EndProc

Proc P_PrintRow(B_Row As Byte, S_Text As String, B_Active As Byte)
    Print At B_Row,1,"                    "
    If B_Active = 1 Then
        Print At B_Row,1,"[",S_Text,"]"
    Else
        Print At B_Row,2,S_Text
    EndIf
EndProc

Proc P_PrnMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word
    Dim B_Sec As Word

    B_Min = W_Seconds / 60
    B_Sec = W_Seconds // 60

    Print At B_Row, B_Col,   Dec2 B_Min,":"
    Print At B_Row, B_Col+3, Dec2 B_Sec
EndProc

Proc P_MMSStoSec(B_Min As Byte, B_Sec As Byte), Word
    Result = (B_Min * 60) + B_Sec
EndProc

Proc P_ClampW(ByRef W_Val As Word, W_Min As Word, W_Max As Word)
    If W_Val < W_Min Then
        W_Val = W_Min
    EndIf
    If W_Val > W_Max Then
        W_Val = W_Max
    EndIf
EndProc

Proc P_PrintRowRJ(B_Row As Byte, S_Text As String, B_Active As Byte)
    Dim B_Len   As Byte
    Dim B_Start As Byte

    P_ClrLine(B_Row)

    B_Len = Len S_Text
    If B_Len > 19 Then
        B_Len = 19
    EndIf

    B_Start = 20 - B_Len

    Print At B_Row, B_Start, S_Text

    If B_Active = 1 Then
        If B_Start > 1 Then
            Print At B_Row, B_Start - 1, "["
        EndIf
        Print At B_Row, 20, "]"
    EndIf
EndProc

Proc P_UserAbort(), Byte
    Dim L_TimeoutMs As Dword

    L_TimeoutMs = W_UI_TimeoutS * 1000

    If (L_Millis - L_LastInput) >= L_TimeoutMs Then
        If b_Escape = 0 Then
            b_Escape = 1
            P_Beeps(4)
            B_NavCode = 2
        EndIf
        Result = 1
    Else
        Result = 0
    EndIf
EndProc

'=====================================================================
' VALUE FIELD HELPERS (RIGHT-JUSTIFIED, field B_Col..B_Col+9)
'=====================================================================
Proc P_ClrValFld(B_Row As Byte, B_Col As Byte)
    Print At B_Row, B_Col, "          "
EndProc

Proc P_PValTxtRJ(B_Row As Byte, B_Col As Byte, S_Value As String, B_Active As Byte, B_Edit As Byte)
    Dim B_Len   As Byte
    Dim B_Start As Byte

    B_Len = Len S_Value
    If B_Len > 8 Then
        B_Len = 8
    EndIf

    B_Start = B_Col + 1 + (8 - B_Len)

    P_ClrValFld(B_Row, B_Col)

    If B_Active = 1 Then
        If B_Edit = 1 Then
            Print At B_Row, B_Start - 1, "("
            Print At B_Row, B_Col + 9, ")"
        Else
            Print At B_Row, B_Start - 1, "["
            Print At B_Row, B_Col + 9, "]"
        EndIf
    EndIf

    Print At B_Row, B_Start, S_Value
EndProc

Proc P_PValWrdRJ(B_Row As Byte, B_Col As Byte, W_Val As Word, B_Active As Byte)
    Dim B_Start As Byte

    B_Start = B_Col + 1 + (8 - 5)

    P_ClrValFld(B_Row, B_Col)

    If B_Active = 1 Then
        Print At B_Row, B_Start - 1, "["
        Print At B_Row, B_Col + 9, "]"
    EndIf

    Print At B_Row, B_Start, Dec5 W_Val
EndProc

Proc P_PValTmeRJ(B_Row As Byte, B_Col As Byte, W_Seconds As Word, B_Active As Byte)
    Dim B_Start As Byte

    B_Start = B_Col + 1 + (8 - 5)

    P_ClrValFld(B_Row, B_Col)

    If B_Active = 1 Then
        Print At B_Row, B_Start - 1, "["
        Print At B_Row, B_Col + 9, "]"
    EndIf

    P_PrnMMSS(B_Row, B_Start, W_Seconds)
EndProc
'---------------------------------------------------------------------
Proc P_PValIntRJ4(B_Row As Byte, B_Col As Byte, I_Val As SWord, B_Active As Byte, B_Edit As Byte)
    Dim B_Len   As Byte
    Dim B_Start As Byte
    Dim W_A     As Word

    If I_Val < 0 Then
        B_Len = 5
    Else
        B_Len = 4
    EndIf
    If B_Len > 8 Then
        B_Len = 8
    EndIf

    B_Start = B_Col + 1 + (8 - B_Len)
    Print At B_Row, B_Col, "          "       ' clear 10-col value field

    If B_Active = 1 Then
        If B_Edit = 1 Then
            Print At B_Row, B_Start - 1, "("
            Print At B_Row, B_Col + 9, ")"
        Else
            Print At B_Row, B_Start - 1, "["
            Print At B_Row, B_Col + 9, "]"
        EndIf
    EndIf

    If I_Val < 0 Then
        Print At B_Row, B_Start, "-"
        W_A = 0 - I_Val
        Print At B_Row, B_Start + 1, Dec4 W_A
    Else
        Print At B_Row, B_Start, Dec4 I_Val
    EndIf
EndProc

'--------------------- Signed 3-digit editor render -------------------
' RJ inside the 10-col value field at B_Col..B_Col+9
' Always prints "(+ddd)" while editing, returns the start col of sign
Proc P_PrintSInt3E(B_Row As Byte, B_Col As Byte, I_Val As SWord), Byte
    Dim B_Start As Byte
    Dim W_A     As Word

    ' Fixed width: 4 chars => sign + 3 digits
    B_Start = B_Col + 1 + (8 - 4)

    P_ClrValFld(B_Row, B_Col)

    ' Open and close parens for edit
    Print At B_Row, B_Start - 1, "("
    Print At B_Row, B_Col + 9, ")"

    If I_Val < 0 Then
        Print At B_Row, B_Start, "-"
        W_A = 0 - I_Val
        Print At B_Row, B_Start + 1, Dec3 W_A
    Else
        Print At B_Row, B_Start, "+"
        Print At B_Row, B_Start + 1, Dec3 I_Val
    EndIf

    Result = B_Start
EndProc

'--------------------- Signed 3-digit inline editor -------------------
' Edits sign+3 digits in place on the provided row/col.
' Steps per active digit: 100, 10, 1. Bounds applied each step.
' Returns 1 if value committed, 0 if aborted.
Proc P_EditSInt3Inl(B_Row As Byte, B_Col As Byte, ByRef I_Val As SWord, I_Min As SWord, I_Max As SWord), Byte
    Dim I_Edit   As SWord
    Dim B_Digit  As Byte
    Dim B_Start  As Byte
    Dim B_ColDig As Byte
    Dim B_Step   As Byte
    Dim B_Redraw As Byte

    ' Clamp initial
    If I_Val < I_Min Then
        I_Edit = I_Min
    Else
        If I_Val > I_Max Then
            I_Edit = I_Max
        Else
            I_Edit = I_Val
        EndIf
    EndIf

    B_Digit = 0
    B_Redraw = 1

    LCD_CursorOn()

    While 1 = 1
        If B_Redraw = 1 Then
            B_Start = P_PrintSInt3E(B_Row, B_Col, I_Edit)
            B_ColDig = B_Start + 1 + B_Digit   ' hundreds=+1, tens=+2, units=+3
            LCD_SetCursor(B_Row, B_ColDig)
            B_Redraw = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_Digit = 0 Then
                B_Step = 100
            Else
                If B_Digit = 1 Then
                    B_Step = 10
                Else
                    B_Step = 1
                EndIf
            EndIf

            If B_EncDelta = 1 Then
                If I_Edit + B_Step <= I_Max Then
                    I_Edit = I_Edit + B_Step
                    B_Redraw = 1
                EndIf
            Else
                If I_Edit >= (I_Min + B_Step) Then
                    I_Edit = I_Edit - B_Step
                    B_Redraw = 1
                Else
                    If I_Edit > I_Min Then
                        I_Edit = I_Min
                        B_Redraw = 1
                    EndIf
                EndIf
            EndIf
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                If B_Digit < 2 Then
                    Inc B_Digit
                    B_Redraw = 1
                Else
                    I_Val = I_Edit
                    LCD_CursorOff()
                    Result = 1
                    ExitProc
                EndIf
        EndSelect

        If P_UserAbort() = 1 Then
            LCD_CursorOff()
            Result = 0
            ExitProc
        EndIf
    Wend
EndProc

'=====================================================================
' CONFIG WORD (reserved; unchanged)
'=====================================================================
Proc P_BuildCfg(B_Sensor As Byte, B_Master As Byte, B_Ind As Byte, B_FS As Byte, B_FA As Byte, B_Disp As Byte), Word
    Dim W_Cfg As Word

    W_Cfg = 0

    If B_Sensor > 3 Then
        B_Sensor = 3
    EndIf
    If B_Master > 1 Then
        B_Master = 1
    EndIf
    If B_Ind > 3 Then
        B_Ind = 3
    EndIf
    If B_FS > 3 Then
        B_FS = 3
    EndIf
    If B_FA > 3 Then
        B_FA = 3
    EndIf
    If B_Disp > 3 Then
        B_Disp = 3
    EndIf

    W_Cfg = W_Cfg + B_Sensor
    W_Cfg = W_Cfg + (B_Master * 4)
    W_Cfg = W_Cfg + (B_Ind * 8)
    W_Cfg = W_Cfg + (B_FS * 32)
    W_Cfg = W_Cfg + (B_FA * 128)
    W_Cfg = W_Cfg + (B_Disp * 512)

    Result = W_Cfg
EndProc

'=====================================================================
' DEFAULTS / LOAD / SAVE
'=====================================================================
Proc P_EEDefaults()
    Dim B_V As Byte
    Dim W_Def As Word
    Dim W_Base As Word

    B_V = ERead EE_VER

    If B_V <> CFG_VERSION Then
        W_Def = P_BuildCfg(0, 1, 0, 0, 0, 2)
        P_EWriteW(EE_I1_CFG, W_Def)
        P_EWriteW(EE_I2_CFG, W_Def)
        P_EWriteW(EE_I3_CFG, W_Def)

        P_EWriteW(EE_UI_TIMEOUT_S, UI_TIMEOUT_S_DEF)
        P_EWriteW(EE_UI_PULSE_MS,  UI_PULSE_MS_DEF)

        ' Apply defaults for all three inputs
        For W_Base = EE_I1_BASE To EE_I3_BASE Step EE_I_BLOCK_SIZE
            EWrite (W_Base + 0),  [1]           ' Enabled
            EWrite (W_Base + 1),  [SENSOR_PRES] ' Sensor type default
            EWrite (W_Base + 2),  [0]           ' FlowMode
            P_EWriteW(W_Base + 4,  P_S2W(0))    ' Scale4 = 0 signed
            P_EWriteW(W_Base + 6,  P_S2W(362))  ' Scale20 = +362 signed
            P_EWriteW(W_Base + 8,  0)
            P_EWriteW(W_Base + 10, 0)
            P_EWriteW(W_Base + 12, 0)
            EWrite (W_Base + 14), [MODE_NO]
            EWrite (W_Base + 15), [MODE_NO]
            EWrite (W_Base + 16), [MODE_NO]
            EWrite (W_Base + 17), [1]
        Next W_Base

        EWrite EE_VER, [CFG_VERSION]
    EndIf
EndProc

Proc P_LoadSets()
    P_EEDefaults()

    W_I1_Cfg = P_EReadW(EE_I1_CFG)
    W_I2_Cfg = P_EReadW(EE_I2_CFG)
    W_I3_Cfg = P_EReadW(EE_I3_CFG)

    P_LoadInput(1)
    P_LoadInput(2)
    P_LoadInput(3)

    P_I1SyncLoad()

    W_UI_TimeoutS = P_EReadW(EE_UI_TIMEOUT_S)
    If W_UI_TimeoutS < UI_TIMEOUT_S_MIN Or W_UI_TimeoutS > UI_TIMEOUT_S_MAX Then
        W_UI_TimeoutS = UI_TIMEOUT_S_DEF
        P_EWriteW(EE_UI_TIMEOUT_S, W_UI_TimeoutS)
    EndIf

    W_UI_PulseMs = P_EReadW(EE_UI_PULSE_MS)
    If W_UI_PulseMs < UI_PULSE_MS_MIN Or W_UI_PulseMs > UI_PULSE_MS_MAX Then
        W_UI_PulseMs = UI_PULSE_MS_DEF
        P_EWriteW(EE_UI_PULSE_MS, W_UI_PulseMs)
    EndIf
EndProc

Proc P_SaveSets()
    P_I1SyncSave()
    P_SaveInput(1)
    P_SaveInput(2)
    P_SaveInput(3)
EndProc

'=====================================================================
' INPUT / EVENTS
'=====================================================================
Proc P_ReadEnc()
    Dim W_Pos As Word

    GIE = 0
    W_Pos = W_EncoderPos
    GIE = 1

    If W_Pos > W_EncReadPos Then
        B_EncDelta = 1
        L_LastInput = L_Millis
    Else
        If W_Pos < W_EncReadPos Then
            B_EncDelta = -1
            L_LastInput = L_Millis
        Else
            B_EncDelta = 0
        EndIf
    EndIf

    W_EncReadPos = W_Pos
EndProc

Proc P_ReadBtn()
EndProc

Proc P_GetKeyEvt(), Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
EndProc

'=====================================================================
' EDITORS (modal pages)
'=====================================================================
Proc P_EditYN(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte

    B_Cur = B_Val
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: YES NO       ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            If B_Cur = 1 Then
                Print At 3,1," Yes  [No]          "
            Else
                Print At 3,1,"[Yes]  No           "
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_Cur = 1 Then
                B_Cur = 0
            Else
                B_Cur = 1
            EndIf
            Set b_ScrDirty
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                B_Val = B_Cur
                Result = 1
                ExitProc
        EndSelect
    Wend
EndProc

Proc P_EditEnum3(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte

    B_Cur = B_Val
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: MODE         ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            Select B_Cur
                Case 0
                    Print At 3,1,"[No]  Pulse  Latch   "
                Case 1
                    Print At 3,1," No  [Pulse] Latch   "
                Case 2
                    Print At 3,1," No   Pulse [Latch]  "
            EndSelect

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta = 1 Then
            If B_Cur < 2 Then
                Inc B_Cur
            EndIf
            Set b_ScrDirty
        Else
            If B_EncDelta = -1 Then
                If B_Cur > 0 Then
                    Dec B_Cur
                EndIf
                Set b_ScrDirty
            EndIf
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                B_Val = B_Cur
                Result = 1
                ExitProc
        EndSelect
    Wend
EndProc

Proc P_EditWord(ByRef W_Val As Word, W_Min As Word, W_Max As Word, W_Step As Word), Byte
    Dim W_Cur As Word

    W_Cur = W_Val
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: WORD VALUE   ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)
            Print At 3,1," Value: [",Dec5 W_Cur,"]        "
            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta = 1 Then
            If W_Cur + W_Step <= W_Max Then
                W_Cur = W_Cur + W_Step
            EndIf
            Set b_ScrDirty
        Else
            If B_EncDelta = -1 Then
                If W_Cur >= (W_Min + W_Step) Then
                    W_Cur = W_Cur - W_Step
                Else
                    W_Cur = W_Min
                EndIf
                Set b_ScrDirty
            EndIf
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                W_Val = W_Cur
                Result = 1
                ExitProc
        EndSelect
    Wend
EndProc

Proc P_EditMMSS(ByRef W_Val As Word), Byte
    Dim B_Min As Byte
    Dim B_Sec As Byte
    Dim B_Field As Byte

    B_Min = W_Val / 60
    B_Sec = W_Val // 60
    B_Field = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: DURATION     ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            If B_Field = 0 Then
                Print At 3,1," [",Dec2 B_Min,"] : ",Dec2 B_Sec,"         "
            Else
                Print At 3,1,"  ",Dec2 B_Min," : [",Dec2 B_Sec,"]        "
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta = 1 Then
            If B_Field = 0 Then
                If B_Min < 99 Then
                    Inc B_Min
                EndIf
            Else
                If B_Sec < 59 Then
                    Inc B_Sec
                EndIf
            EndIf
            Set b_ScrDirty
        Else
            If B_EncDelta = -1 Then
                If B_Field = 0 Then
                    If B_Min > 0 Then
                        Dec B_Min
                    EndIf
                Else
                    If B_Sec > 0 Then
                        Dec B_Sec
                    EndIf
                EndIf
                Set b_ScrDirty
            EndIf
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_Field = 0 Then
                    B_Field = 1
                    Set b_ScrDirty
                Else
                    W_Val = (B_Min * 60) + B_Sec
                    Result = 1
                    ExitProc
                EndIf
        EndSelect
    Wend
EndProc

'=====================================================================
' VIEWS / MENUS
'=====================================================================
Proc V_NotImpl(S_Name As String)
    b_ReInitLCD = 0

    P_DrawTitle(S_Name + " (STUB)      ")
    P_ClrLine(2)
    P_ClrLine(3)
    P_ClrLine(4)

    While 1 = 1
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                ExitProc
            Case 2
                P_Beeps(3)
                ExitProc
            Case 3
                P_Beeps(3)
                B_NavCode = 2
                ExitProc
        EndSelect
    Wend
EndProc

Proc V_Main()
    b_ReInitLCD = 0

    P_DrawTitle("IRRISYS MAIN        ")
    P_ClrLine(2)
    P_ClrLine(3)
    P_ClrLine(4)

    Set b_ScrDirty

    While 1 = 1
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                ExitProc
            Case 2
                P_Beeps(3)
                ExitProc
            Case 3
                P_Beeps(3)
        EndSelect
    Wend
EndProc

Proc V_Options(), Byte
    Dim B_Sel  As Byte
    Dim B_Top  As Byte
    Dim B_Cnt  As Byte
    Dim B_Act  As Byte
    Dim B_Row  As Byte
    Dim B_Idx  As Byte

    B_Cnt = 4
    B_Sel = 0
    B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("OPTIONS             ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            If B_Sel <= 1 Then
                B_Top = 0
            Else
                If B_Sel >= B_Cnt - 1 Then
                    If B_Cnt > 2 Then
                        B_Top = B_Cnt - 3
                    Else
                        B_Top = 0
                    EndIf
                Else
                    B_Top = B_Sel - 1
                EndIf
            EndIf

            For B_Row = 2 To 4
                B_Idx = B_Top + B_Row - 2

                If B_Idx = B_Sel Then
                    B_Act = 1
                Else
                    B_Act = 0
                EndIf

                Select B_Idx
                    Case 0
                        P_PrintRow(B_Row, "Main Menu",   B_Act)
                    Case 1
                        P_PrintRow(B_Row, "Setup Menu",  B_Act)
                    Case 2
                        P_PrintRow(B_Row, "Utility Menu",B_Act)
                    Case 3
                        P_PrintRow(B_Row, "Back",        B_Act)
                    Case Else
                        P_ClrLine(B_Row)
                EndSelect
            Next B_Row

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EncDelta = 1 Then
                If B_Sel < B_Cnt - 1 Then
                    B_Sel = B_Sel + 1
                EndIf
            Else
                If B_Sel > 0 Then
                    B_Sel = B_Sel - 1
                EndIf
            EndIf
            Set b_ScrDirty
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    Result = 1
                    ExitProc
                Else
                    If B_Sel = 1 Then
                        Call V_SetupMenu()
                    Else
                        If B_Sel = 2 Then
                            V_NotImpl("UTILITY")
                        Else
                            Result = 1
                            ExitProc
                        EndIf
                    EndIf
                EndIf
                Set b_ScrDirty
        EndSelect
    Wend
EndProc

Proc V_SetupMenu(), Byte
    Dim B_Sel  As Byte
    Dim B_Top  As Byte
    Dim B_Cnt  As Byte
    Dim B_Act  As Byte
    Dim B_Row  As Byte
    Dim B_Idx  As Byte

    B_Cnt = 5
    B_Sel = 0
    B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("SETUP               ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

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

            For B_Row = 2 To 4
                B_Idx = B_Top + (B_Row - 2)

                If B_Idx = B_Sel Then
                    B_Act = 1
                Else
                    B_Act = 0
                EndIf

                Select B_Idx
                    Case 0
                        P_PrintRow(B_Row, "Input 1", B_Act)
                    Case 1
                        P_PrintRow(B_Row, "Input 2", B_Act)
                    Case 2
                        P_PrintRow(B_Row, "Input 3", B_Act)
                    Case 3
                        P_PrintRow(B_Row, "Clock",   B_Act)
                    Case 4
                        P_PrintRow(B_Row, "Back",    B_Act)
                    Case Else
                        P_ClrLine(B_Row)
                EndSelect
            Next B_Row

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
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

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    Call V_Input1Menu()
                Else
                    If B_Sel = 1 Then
                        V_NotImpl("INPUT 2")
                    Else
                        If B_Sel = 2 Then
                            V_NotImpl("INPUT 3")
                        Else
                            If B_Sel = 3 Then
                                Call V_ClockMenu()
                            Else
                                Result = 1
                                ExitProc
                            EndIf
                        EndIf
                    EndIf
                EndIf
                Set b_ScrDirty
        EndSelect
    Wend
EndProc

'-------------------- INPUT 1 MAIN MENU (inline edit) ----------------
Proc V_Input1Menu(), Byte
    Dim B_Sel            As Byte
    Dim B_Top            As Byte
    Dim B_Cnt            As Byte
    Dim B_Act            As Byte
    Dim B_Row            As Byte
    Dim B_Idx            As Byte
    Dim B_BackIdx        As Byte
    Dim B_RowSel         As Byte
    Dim B_FieldId        As Byte

    Symbol F_ENABLE   = 0
    Symbol F_SENSOR   = 1
    Symbol F_SCALE4   = 2
    Symbol F_SCALE20  = 3
    Symbol F_BP_HIGH  = 4
    Symbol F_BP_PLP   = 5
    Symbol F_BP_SLP   = 6
    Symbol F_RLY_HIGH = 7
    Symbol F_RLY_PLP  = 8
    Symbol F_RLY_SLP  = 9
    Symbol F_DISPLAY  = 10
    Symbol F_BACK     = 255

    B_Sel = 0
    B_Top = 0
    Set b_ScrDirty

    While 1 = 1
        If B_I1_SensorT = SENSOR_PRES Then
            B_Cnt = 12
        Else
            If B_I1_SensorT = SENSOR_TEMP Then
                B_Cnt = 10
            Else
                B_Cnt = 3
            EndIf
        EndIf

        B_BackIdx = B_Cnt - 1

        If b_ScrDirty = 1 Then
            P_DrawTitle("INPUT 1             ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            If B_Sel <= 1 Then
                B_Top = 0
            Else
                If B_Sel >= B_Cnt - 1 Then
                    If B_Cnt > 2 Then
                        B_Top = B_Cnt - 3
                    Else
                        B_Top = 0
                    EndIf
                Else
                    B_Top = B_Sel - 1
                EndIf
            EndIf

            For B_Row = 2 To 4
                B_Idx = B_Top + B_Row - 2
                If B_Idx <= B_BackIdx Then
                    If B_Idx = B_Sel Then
                        B_Act = 1
                    Else
                        B_Act = 0
                    EndIf

                    ' Map index -> field id
                    If B_I1_SensorT = SENSOR_PRES Then
                        Select B_Idx
                            Case 0
                                B_FieldId = F_ENABLE
                            Case 1
                                B_FieldId = F_SENSOR
                            Case 2
                                B_FieldId = F_SCALE4
                            Case 3
                                B_FieldId = F_SCALE20
                            Case 4
                                B_FieldId = F_BP_HIGH
                            Case 5
                                B_FieldId = F_BP_PLP
                            Case 6
                                B_FieldId = F_BP_SLP
                            Case 7
                                B_FieldId = F_RLY_HIGH
                            Case 8
                                B_FieldId = F_RLY_PLP
                            Case 9
                                B_FieldId = F_RLY_SLP
                            Case 10
                                B_FieldId = F_DISPLAY
                            Case Else
                                B_FieldId = F_BACK
                        EndSelect
                    Else
                        If B_I1_SensorT = SENSOR_TEMP Then
                            Select B_Idx
                                Case 0
                                    B_FieldId = F_ENABLE
                                Case 1
                                    B_FieldId = F_SENSOR
                                Case 2
                                    B_FieldId = F_SCALE4
                                Case 3
                                    B_FieldId = F_SCALE20
                                Case 4
                                    B_FieldId = F_BP_HIGH
                                Case 5
                                    B_FieldId = F_BP_SLP
                                Case 6
                                    B_FieldId = F_RLY_HIGH
                                Case 7
                                    B_FieldId = F_RLY_SLP
                                Case 8
                                    B_FieldId = F_DISPLAY
                                Case Else
                                    B_FieldId = F_BACK
                            EndSelect
                        Else
                            If B_Idx = 0 Then
                                B_FieldId = F_ENABLE
                            Else
                                If B_Idx = 1 Then
                                    B_FieldId = F_SENSOR
                                Else
                                    B_FieldId = F_BACK
                                EndIf
                            EndIf
                        EndIf
                    EndIf

                    ' Render rows
                    If B_FieldId = F_BACK Then
                        P_PrintRowRJ(B_Row, "Back", B_Act)
                    Else
                        Select B_FieldId
                            Case F_ENABLE
                                Print At B_Row,1,"Enable    "
                                If B_I1_Enabled = 1 Then
                                    P_PValTxtRJ(B_Row, 11, "Enabled",  B_Act, 0)
                                Else
                                    P_PValTxtRJ(B_Row, 11, "Disabled", B_Act, 0)
                                EndIf

                            Case F_SENSOR
                                Print At B_Row,1,"Sensor    "
                                If B_I1_SensorT = SENSOR_PRES Then
                                    P_PValTxtRJ(B_Row, 11, "Pressure", B_Act, 0)
                                Else
                                    If B_I1_SensorT = SENSOR_TEMP Then
                                        P_PValTxtRJ(B_Row, 11, "Temp", B_Act, 0)
                                    Else
                                        P_PValTxtRJ(B_Row, 11, "Flow", B_Act, 0)
                                    EndIf
                                EndIf

                            Case F_SCALE4
                                Print At B_Row,1,"Scale4ma  "
                                P_PValIntRJ4(B_Row, 11, P_W2S(W_I1_Scale4), B_Act, 0)

                            Case F_SCALE20
                                Print At B_Row,1,"Scale20ma "
                                P_PValIntRJ4(B_Row, 11, P_W2S(W_I1_Scale20), B_Act, 0)

                            Case F_BP_HIGH
                                If B_I1_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"High BP   "
                                Else
                                    Print At B_Row,1,"High TBP  "
                                EndIf
                                P_PValTmeRJ(B_Row, 11, W_I1_BP_High, B_Act)

                            Case F_BP_PLP
                                Print At B_Row,1,"PLPBP     "
                                P_PValTmeRJ(B_Row, 11, W_I1_BP_PLP, B_Act)

                            Case F_BP_SLP
                                If B_I1_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"SLPBP     "
                                Else
                                    Print At B_Row,1,"Low TBP   "
                                EndIf
                                P_PValTmeRJ(B_Row, 11, W_I1_BP_SLP, B_Act)

                            Case F_RLY_HIGH
                                Print At B_Row,1,"Rly High  "
                                Select B_I1_RlyHigh
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_RLY_PLP
                                Print At B_Row,1,"Rly PLP   "
                                Select B_I1_RlyPLP
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_RLY_SLP
                                If B_I1_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"Rly SLP   "
                                Else
                                    Print At B_Row,1,"Rly Low   "
                                EndIf
                                Select B_I1_RlySLP
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_DISPLAY
                                Print At B_Row,1,"Display   "
                                If B_I1_Display = YES Then
                                    P_PValTxtRJ(B_Row, 11, "Yes", B_Act, 0)
                                Else
                                    P_PValTxtRJ(B_Row, 11, "No",  B_Act, 0)
                                EndIf
                        EndSelect
                    EndIf
                Else
                    P_ClrLine(B_Row)
                EndIf
            Next B_Row

            b_ScrDirty = 0
        EndIf

        ' Encoder navigation
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EncDelta = 1 Then
                If B_Sel < B_BackIdx Then
                    B_Sel = B_Sel + 1
                EndIf
            Else
                If B_Sel > 0 Then
                    B_Sel = B_Sel - 1
                EndIf
            EndIf
            Set b_ScrDirty
        EndIf

        ' Button handling
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                ' Compute row where the selected item is currently drawn
                If B_Sel <= 1 Then
                    B_Top = 0
                Else
                    If B_Sel >= B_Cnt - 1 Then
                        If B_Cnt > 2 Then
                            B_Top = B_Cnt - 3
                        Else
                            B_Top = 0
                        EndIf
                    Else
                        B_Top = B_Sel - 1
                    EndIf
                EndIf
                B_RowSel = 2 + (B_Sel - B_Top)

                ' Map selection to field id
                If B_I1_SensorT = SENSOR_PRES Then
                    Select B_Sel
                        Case 0
                            B_FieldId = F_ENABLE
                        Case 1
                            B_FieldId = F_SENSOR
                        Case 2
                            B_FieldId = F_SCALE4
                        Case 3
                            B_FieldId = F_SCALE20
                        Case 4
                            B_FieldId = F_BP_HIGH
                        Case 5
                            B_FieldId = F_BP_PLP
                        Case 6
                            B_FieldId = F_BP_SLP
                        Case 7
                            B_FieldId = F_RLY_HIGH
                        Case 8
                            B_FieldId = F_RLY_PLP
                        Case 9
                            B_FieldId = F_RLY_SLP
                        Case 10
                            B_FieldId = F_DISPLAY
                        Case Else
                            B_FieldId = F_BACK
                    EndSelect
                Else
                    If B_I1_SensorT = SENSOR_TEMP Then
                        Select B_Sel
                            Case 0
                                B_FieldId = F_ENABLE
                            Case 1
                                B_FieldId = F_SENSOR
                            Case 2
                                B_FieldId = F_SCALE4
                            Case 3
                                B_FieldId = F_SCALE20
                            Case 4
                                B_FieldId = F_BP_HIGH
                            Case 5
                                B_FieldId = F_BP_SLP
                            Case 6
                                B_FieldId = F_RLY_HIGH
                            Case 7
                                B_FieldId = F_RLY_SLP
                            Case 8
                                B_FieldId = F_DISPLAY
                            Case Else
                                B_FieldId = F_BACK
                        EndSelect
                    Else
                        If B_Sel = 0 Then
                            B_FieldId = F_ENABLE
                        Else
                            If B_Sel = 1 Then
                                B_FieldId = F_SENSOR
                            Else
                                B_FieldId = F_BACK
                            EndIf
                        EndIf
                    EndIf
                EndIf

                ' Act on selection
                Select B_FieldId
                    Case F_BACK
                        P_Beeps(2)
                        Result = 1
                        ExitProc

                    Case F_ENABLE
                        P_Beeps(1)
                        DelayMS 60
                        P_Beeps(1)
                        If B_I1_Enabled = 0 Then
                            B_I1_Enabled = 1
                        Else
                            B_I1_Enabled = 0
                        EndIf
                        P_SaveInput(1)
                        Set b_ScrDirty

                    Case F_SENSOR
                        P_Beeps(1)
                        DelayMS 60
                        P_Beeps(1)
                        If B_I1_SensorT = SENSOR_PRES Then
                            B_I1_SensorT = SENSOR_TEMP
                        Else
                            If B_I1_SensorT = SENSOR_TEMP Then
                                B_I1_SensorT = SENSOR_FLOW
                            Else
                                B_I1_SensorT = SENSOR_PRES
                            EndIf
                        EndIf
                        P_SaveInput(1)
                        Set b_ScrDirty

                    Case F_SCALE4
                        Dim I_Tmp As SWord
                        I_Tmp = P_W2S(W_I1_Scale4)
                        If P_EditSInt3Inl(B_RowSel, 11, I_Tmp, -100, 999) = 1 Then
                            W_I1_Scale4 = P_S2W(I_Tmp)
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_SCALE20
                        Dim I_Tmp2 As SWord
                        I_Tmp2 = P_W2S(W_I1_Scale20)
                        If P_EditSInt3Inl(B_RowSel, 11, I_Tmp2, -100, 999) = 1 Then
                            W_I1_Scale20 = P_S2W(I_Tmp2)
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_BP_HIGH
                        If B_I1_SensorT = SENSOR_PRES Then
                            If P_EditMMSS(W_I1_BP_High) = 1 Then
                                P_SaveInput(1)
                            EndIf
                            Set b_ScrDirty
                        EndIf

                    Case F_BP_PLP
                        If B_I1_SensorT = SENSOR_PRES Then
                            If P_EditMMSS(W_I1_BP_PLP) = 1 Then
                                P_SaveInput(1)
                            EndIf
                            Set b_ScrDirty
                        EndIf

                    Case F_BP_SLP
                        If P_EditMMSS(W_I1_BP_SLP) = 1 Then
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_HIGH
                        If P_EditEnum3(B_I1_RlyHigh) = 1 Then
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_PLP
                        If P_EditEnum3(B_I1_RlyPLP) = 1 Then
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_SLP
                        If P_EditEnum3(B_I1_RlySLP) = 1 Then
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty

                    Case F_DISPLAY
                        If P_EditYN(B_I1_Display) = 1 Then
                            P_SaveInput(1)
                        EndIf
                        Set b_ScrDirty
                EndSelect
        EndSelect

        If P_UserAbort() <> 0 Then
            Result = 1
            ExitProc
        EndIf
    Wend
EndProc

'----------------------------- CLOCK MENU ----------------------------
Proc V_ClockMenu(), Byte
    Dim B_Sel           As Byte
    Dim B_Count         As Byte
    Dim B_Active        As Byte

    Dim B_EditMode      As Byte
    Dim B_EditIndex     As Byte
    Dim W_EditWordOrig  As Word

    B_Count     = 3
    B_Sel       = 0
    B_EditMode  = 0
    B_EditIndex = 255
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("CLOCK               ")
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            If B_Sel = 0 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 2,1,"Timeout   "
            P_PValTmeRJ(2, 11, W_UI_TimeoutS, B_Active)

            If B_Sel = 1 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 3,1,"Pulse     "
            P_PValWrdRJ(3, 11, W_UI_PulseMs, B_Active)

            If B_Sel = 2 Then
                P_PrintRow(4,"Back",1)
            Else
                P_PrintRow(4,"Back",0)
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EditMode = 0 Then
                If B_EncDelta = 1 Then
                    If B_Sel < (B_Count - 1) Then
                        Inc B_Sel
                        Set b_ScrDirty
                    EndIf
                Else
                    If B_Sel > 0 Then
                        Dec B_Sel
                        Set b_ScrDirty
                    EndIf
                EndIf
            Else
                Select B_EditIndex
                    Case 0
                        If B_EncDelta = 1 Then
                            If W_UI_TimeoutS < UI_TIMEOUT_S_MAX Then
                                Inc W_UI_TimeoutS
                            EndIf
                        Else
                            If W_UI_TimeoutS > UI_TIMEOUT_S_MIN Then
                                Dec W_UI_TimeoutS
                            EndIf
                        EndIf
                        Set b_ScrDirty

                    Case 1
                        If B_EncDelta = 1 Then
                            If W_UI_PulseMs < UI_PULSE_MS_MAX Then
                                Inc W_UI_PulseMs
                            EndIf
                        Else
                            If W_UI_PulseMs > UI_PULSE_MS_MIN Then
                                Dec W_UI_PulseMs
                            EndIf
                        EndIf
                        Set b_ScrDirty
                EndSelect
            EndIf
        EndIf

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_EditMode = 0 Then
                    If B_Sel = 2 Then
                        Result = 1
                        ExitProc
                    Else
                        B_EditMode  = 1
                        B_EditIndex = B_Sel
                        If B_Sel = 0 Then
                            W_EditWordOrig = W_UI_TimeoutS
                        Else
                            W_EditWordOrig = W_UI_PulseMs
                        EndIf
                    EndIf
                Else
                    If B_EditIndex = 0 Then
                        P_ClampW(W_UI_TimeoutS, UI_TIMEOUT_S_MIN, UI_TIMEOUT_S_MAX)
                    Else
                        P_ClampW(W_UI_PulseMs, UI_PULSE_MS_MIN, UI_PULSE_MS_MAX)
                    EndIf
                    P_SaveSystem()
                    B_EditMode  = 0
                    B_EditIndex = 255
                EndIf
        EndSelect

        If P_UserAbort() <> 0 Then
            If B_EditMode = 1 Then
                If B_EditIndex = 0 Then
                    W_UI_TimeoutS = W_EditWordOrig
                Else
                    W_UI_PulseMs = W_EditWordOrig
                EndIf
                B_EditMode  = 0
                B_EditIndex = 255
                Set b_ScrDirty
            Else
                Result = 1
                ExitProc
            EndIf
        EndIf
    Wend
EndProc

'---------- LCD safe init wrapper (sync with Positron driver) --------
Proc P_LCDSafeInit()
    DlyMsFast(5)
    Cls
    DlyMsFast(2)
EndProc

'=====================================================================
' MAIN
'=====================================================================
MAIN:
P_PinInit()
P_LCDHardInit()
P_LCDSafeInit()
P_InputInit()
P_LoadSets()

b_ScrDirty = 1
b_ReInitLCD = 0
b_Escape = 0
B_KeyEvent = 0
L_LastInput = 0
B_NavCode = 0

If B_I1_SensorT > 2 Then
    B_I1_SensorT = 1
EndIf

P_Startup()
HRSOut "Startup ok",13
DelayMS 200

While 1 = 1
    B_NavCode = 0
    V_Main()
    B_Option = V_Options()
Wend

'============================== EOF ==================================

