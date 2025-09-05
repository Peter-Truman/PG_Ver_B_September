'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
'=====================================================================

'5/9/25

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
' LCD (HD44780-compatible) 20x4 in 4-bit mode (Positron driver)
Declare LCD_Type = 0
Declare LCD_DTPin = PORTA.0              ' D4..D7 = RA0..RA3
Declare LCD_ENPin = PORTA.7              ' E
Declare LCD_RSPin = PORTA.6              ' RS
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
' Button timing (ms)
Symbol BTN_SHORT_MS     = 200
Symbol BTN_LONG_MS      = 900      ' "Medium" threshold
Symbol BTN_VLONG_MS     = 1800     ' "Long" threshold
Symbol LONG_MS          = BTN_LONG_MS
Symbol VERY_LONG_MS     = BTN_VLONG_MS

' Menu timeout (seconds) – stored as Word in EEPROM
Symbol UI_TIMEOUT_S_MIN = 10
Symbol UI_TIMEOUT_S_MAX = 300
Symbol UI_TIMEOUT_S_DEF = 30

' "Clock ? Pulse" default (beep base in ms)
Symbol UI_PULSE_MS_DEF  = 50
Symbol UI_PULSE_MS_MIN  = 10
Symbol UI_PULSE_MS_MAX  = 2000

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

' Per-input block layout & bases for Input 2/3
Symbol EE_I_BLOCK_SIZE   = 18
Symbol EE_I2_BASE        = EE_I1_BASE + EE_I_BLOCK_SIZE
Symbol EE_I3_BASE        = EE_I2_BASE + EE_I_BLOCK_SIZE

' Offsets within each per-input legacy block
Symbol I_OFF_ENABLED     = 0
Symbol I_OFF_SENSORT     = 1
Symbol I_OFF_FLOWMODE    = 2
Symbol I_OFF_SCALE4      = 4
Symbol I_OFF_SCALE20     = 6
Symbol I_OFF_BP_HIGH     = 8
Symbol I_OFF_BP_PLP      = 10
Symbol I_OFF_BP_SLP      = 12
Symbol I_OFF_RLY_HIGH    = 14
Symbol I_OFF_RLY_PLP     = 15
Symbol I_OFF_RLY_SLP     = 16
Symbol I_OFF_DISPLAY     = 17

' System settings block
Symbol EE_SYS_BASE       = 200
Symbol EE_UI_TIMEOUT_S   = EE_SYS_BASE + 0   ' Word seconds
Symbol EE_UI_PULSE_MS    = EE_SYS_BASE + 2   ' Word ms

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
Dim W_UI_PulseMs   As Word

'Button latches
Dim B_MedSent As Byte
Dim B_LongSent As Byte

' Navigation propagate code: 0=normal, 1=back one, 2=to main
Dim B_NavCode      As Byte

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

'----- Input2 mirrors -----
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

'----- Input3 mirrors -----
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

        ' ---- 10 ms debounce/sample bucket ----
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

            ' --- Debounce A ---
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

            ' --- Debounce B ---
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

            ' --- Debounce Button (active-low) ---
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

            ' --- Quadrature accumulate ---
            B_Curr = (B_AState * 2) + B_BState
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

            ' --- Button press timing -> SHORT only (on release) ---
            ' Sample period ~10 ms
            If B_ButtonState = 0 Then
                If W_BtnHoldMS <= 65525 Then
                    W_BtnHoldMS = W_BtnHoldMS + 10
                EndIf
            Else
                If W_BtnHoldMS >= BTN_SHORT_MS Then
                    B_KeyEvent = 1            ' Short press event
                    L_LastInput = L_Millis
                EndIf
                W_BtnHoldMS = 0              ' Reset for next press
            EndIf

            Clear B_RE_Count
        EndIf

        ' ---- Beeper service (active-high) ----
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
' PROCS / HELPERS
'=====================================================================
Proc P_UserAborted(), Byte
    Dim L_TimeoutMs As Dword
    L_TimeoutMs = W_UI_TimeoutS * 1000

    If (L_Millis - L_LastInput) >= L_TimeoutMs Then
        If b_Escape = 0 Then
            b_Escape = 1
            P_Beeps(4)                  ' one-time timeout beep
            B_NavCode = 2               ' suggest caller go to main
        EndIf
        Result = 1
    Else
        Result = 0
    EndIf
EndProc

'---------- LCD hard init for NHD-0420AZ (ST7066U) ----------
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

Proc LCD_WriteNibble(B_N As Byte)
    LCD_SetNib(B_N)
    LCD_PulseE()
EndProc

Proc LCD_WriteCmd(B_Cmd As Byte)
    Dim B_N As Byte
    LCD_RS = 0
    B_N = B_Cmd / 16
    LCD_WriteNibble(B_N)
    B_N = B_Cmd // 16
    LCD_WriteNibble(B_N)
    If B_Cmd = $01 Then
        DelayMS 2
    Else
        If B_Cmd = $02 Then
            DelayMS 2
        Else
            DelayUS 50
        EndIf
    EndIf
EndProc

Proc P_LCDHardInit()
    ' Ensure outputs and lines low
    TRISA = %00010000                     ' RA4 input, others outputs
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
    LCD_WriteNibble($03)
    DelayMS 5
    LCD_WriteNibble($03)
    DelayUS 150
    LCD_WriteNibble($03)
    DelayUS 150

    ' Set 4-bit interface
    LCD_WriteNibble($02)
    DelayUS 150

    ' Function set: 4-bit, 2 lines, 5x8
    LCD_WriteCmd($28)
    ' Display OFF
    LCD_WriteCmd($08)
    ' Clear
    LCD_WriteCmd($01)
    ' Entry mode: increment, no shift
    LCD_WriteCmd($06)
    ' Display ON, cursor off, blink off
    LCD_WriteCmd($0C)
EndProc
'-------------------------------------------------------------
'-------------------------------------------------------------
'Proc P_BeepOff()
'    GIE = 0
'    W_Beep = 0
'    GIE = 1
'    Set _BUZZER          ' ensure OFF immediately
'EndProc
'-------------------------------------------------------------



'---------- EEPROM Word helpers ----------
Proc P_EReadW(W_Addr As Word), Word
    Dim W_Tmp As Word
    Result = ERead W_Addr
EndProc

Proc P_EWriteW(W_Addr As Word, W_Val As Word)
    Dim W_Tmp As Word
    W_Tmp = ERead W_Addr
    If W_Tmp <> W_Val Then
        EWrite W_Addr, [W_Val]
    EndIf
EndProc

'----- per-input base resolver -----
Proc P_EEBaseForInput(B_In As Byte), Word
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

'----- Load one input block mirrors from EEPROM -----
Proc P_LoadInput(B_In As Byte)
    Dim W_Base As Word
    W_Base = P_EEBaseForInput(B_In)

    If B_In = 1 Then
        B_I1_Enabled  = ERead (W_Base + I_OFF_ENABLED)
        B_I1_SensorT  = ERead (W_Base + I_OFF_SENSORT)
        B_I1_FlowMode = ERead (W_Base + I_OFF_FLOWMODE)
        W_I1_Scale4   = P_EReadW(W_Base + I_OFF_SCALE4)
        W_I1_Scale20  = P_EReadW(W_Base + I_OFF_SCALE20)
        W_I1_BP_High  = P_EReadW(W_Base + I_OFF_BP_HIGH)
        W_I1_BP_PLP   = P_EReadW(W_Base + I_OFF_BP_PLP)
        W_I1_BP_SLP   = P_EReadW(W_Base + I_OFF_BP_SLP)
        B_I1_RlyHigh  = ERead (W_Base + I_OFF_RLY_HIGH)
        B_I1_RlyPLP   = ERead (W_Base + I_OFF_RLY_PLP)
        B_I1_RlySLP   = ERead (W_Base + I_OFF_RLY_SLP)
        B_I1_Display  = ERead (W_Base + I_OFF_DISPLAY)
    Else
        If B_In = 2 Then
            B_I2_Enabled  = ERead (W_Base + I_OFF_ENABLED)
            B_I2_SensorT  = ERead (W_Base + I_OFF_SENSORT)
            B_I2_FlowMode = ERead (W_Base + I_OFF_FLOWMODE)
            W_I2_Scale4   = P_EReadW(W_Base + I_OFF_SCALE4)
            W_I2_Scale20  = P_EReadW(W_Base + I_OFF_SCALE20)
            W_I2_BP_High  = P_EReadW(W_Base + I_OFF_BP_HIGH)
            W_I2_BP_PLP   = P_EReadW(W_Base + I_OFF_BP_PLP)
            W_I2_BP_SLP   = P_EReadW(W_Base + I_OFF_BP_SLP)
            B_I2_RlyHigh  = ERead (W_Base + I_OFF_RLY_HIGH)
            B_I2_RlyPLP   = ERead (W_Base + I_OFF_RLY_PLP)
            B_I2_RlySLP   = ERead (W_Base + I_OFF_RLY_SLP)
            B_I2_Display  = ERead (W_Base + I_OFF_DISPLAY)
        Else
            B_I3_Enabled  = ERead (W_Base + I_OFF_ENABLED)
            B_I3_SensorT  = ERead (W_Base + I_OFF_SENSORT)
            B_I3_FlowMode = ERead (W_Base + I_OFF_FLOWMODE)
            W_I3_Scale4   = P_EReadW(W_Base + I_OFF_SCALE4)
            W_I3_Scale20  = P_EReadW(W_Base + I_OFF_SCALE20)
            W_I3_BP_High  = P_EReadW(W_Base + I_OFF_BP_HIGH)
            W_I3_BP_PLP   = P_EReadW(W_Base + I_OFF_BP_PLP)
            W_I3_BP_SLP   = P_EReadW(W_Base + I_OFF_BP_SLP)
            B_I3_RlyHigh  = ERead (W_Base + I_OFF_RLY_HIGH)
            B_I3_RlyPLP   = ERead (W_Base + I_OFF_RLY_PLP)
            B_I3_RlySLP   = ERead (W_Base + I_OFF_RLY_SLP)
            B_I3_Display  = ERead (W_Base + I_OFF_DISPLAY)
        EndIf
    EndIf
EndProc

'----- Save one input block mirrors to EEPROM (Words coalesced) -----
Proc P_SaveInput(B_In As Byte)
    Dim W_Base As Word
    W_Base = P_EEBaseForInput(B_In)

    If B_In = 1 Then
        EWrite (W_Base + I_OFF_ENABLED),  [B_I1_Enabled]
        EWrite (W_Base + I_OFF_SENSORT),  [B_I1_SensorT]
        EWrite (W_Base + I_OFF_FLOWMODE), [B_I1_FlowMode]
        P_EWriteW(W_Base + I_OFF_SCALE4,  W_I1_Scale4)
        P_EWriteW(W_Base + I_OFF_SCALE20, W_I1_Scale20)
        P_EWriteW(W_Base + I_OFF_BP_HIGH, W_I1_BP_High)
        P_EWriteW(W_Base + I_OFF_BP_PLP,  W_I1_BP_PLP)
        P_EWriteW(W_Base + I_OFF_BP_SLP,  W_I1_BP_SLP)
        EWrite (W_Base + I_OFF_RLY_HIGH), [B_I1_RlyHigh]
        EWrite (W_Base + I_OFF_RLY_PLP),  [B_I1_RlyPLP]
        EWrite (W_Base + I_OFF_RLY_SLP),  [B_I1_RlySLP]
        EWrite (W_Base + I_OFF_DISPLAY),  [B_I1_Display]
    Else
        If B_In = 2 Then
            EWrite (W_Base + I_OFF_ENABLED),  [B_I2_Enabled]
            EWrite (W_Base + I_OFF_SENSORT),  [B_I2_SensorT]
            EWrite (W_Base + I_OFF_FLOWMODE), [B_I2_FlowMode]
            P_EWriteW(W_Base + I_OFF_SCALE4,  W_I2_Scale4)
            P_EWriteW(W_Base + I_OFF_SCALE20, W_I2_Scale20)
            P_EWriteW(W_Base + I_OFF_BP_HIGH, W_I2_BP_High)
            P_EWriteW(W_Base + I_OFF_BP_PLP,  W_I2_BP_PLP)
            P_EWriteW(W_Base + I_OFF_BP_SLP,  W_I2_BP_SLP)
            EWrite (W_Base + I_OFF_RLY_HIGH), [B_I2_RlyHigh]
            EWrite (W_Base + I_OFF_RLY_PLP),  [B_I2_RlyPLP]
            EWrite (W_Base + I_OFF_RLY_SLP),  [B_I2_RlySLP]
            EWrite (W_Base + I_OFF_DISPLAY),  [B_I2_Display]
        Else
            EWrite (W_Base + I_OFF_ENABLED),  [B_I3_Enabled]
            EWrite (W_Base + I_OFF_SENSORT),  [B_I3_SensorT]
            EWrite (W_Base + I_OFF_FLOWMODE), [B_I3_FlowMode]
            P_EWriteW(W_Base + I_OFF_SCALE4,  W_I3_Scale4)
            P_EWriteW(W_Base + I_OFF_SCALE20, W_I3_Scale20)
            P_EWriteW(W_Base + I_OFF_BP_HIGH, W_I3_BP_High)
            P_EWriteW(W_Base + I_OFF_BP_PLP,  W_I3_BP_PLP)
            P_EWriteW(W_Base + I_OFF_BP_SLP,  W_I3_BP_SLP)
            EWrite (W_Base + I_OFF_RLY_HIGH), [B_I3_RlyHigh]
            EWrite (W_Base + I_OFF_RLY_PLP),  [B_I3_RlyPLP]
            EWrite (W_Base + I_OFF_RLY_SLP),  [B_I3_RlySLP]
            EWrite (W_Base + I_OFF_DISPLAY),  [B_I3_Display]
        EndIf
    EndIf
EndProc

'---------- Save system settings ----------
Proc P_SaveSystem()
    P_EWriteW(EE_UI_TIMEOUT_S, W_UI_TimeoutS)
    P_EWriteW(EE_UI_PULSE_MS,  W_UI_PulseMs)
EndProc

'---------- GPIO directions ----------
Proc P_PinInit()
    TRISA = %00010000                ' RA4 input, others outputs for LCD
    TRISB = %01000110                ' RB6,RB2,RB1 inputs (SW,B,A)
    TRISC = %00000000                ' RC2 buzzer output

    ' Do not Clear PORTC here (it would pull RC2 low = buzzer ON)
    ' If you must clear other bits, do it explicitly, then:
    'Set _BUZZER                      ' RC2 = 1 -> buzzer OFF (active-low)
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



' Standardized beeps
Proc P_Beeps(B_Type As Byte)
    HRSOut "P_Beeps(", Dec3 B_Type,")",13
        Select B_Type
        Case 1
            W_Beep=20
        Case 2
            W_Beep=50
        Case 3
            W_Beep=200
        Case 4
            W_Beep=1000
     EndSelect
EndProc

'---------- Startup beep pattern ----------
Proc P_Startup()
    Dim B_Beepcount As Byte
    For B_Beepcount = 0 To 5
        HRSOut "P_Beeps(2)",13
        P_Beeps(2)
        DelayMS 100
    Next B_Beepcount
    HRSOut "End Startup",13
EndProc

'-------------------------------------
Proc DelayMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
EndProc

Proc P_ClearLine(B_Row As Byte)
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

'---------- Your Dec2-based MM:SS printer (KEEP) ----------
Proc P_PrintMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word
    Dim B_Sec As Word
    B_Min = W_Seconds / 60
    B_Sec = W_Seconds // 60
    Print At B_Row, B_Col,Dec2 B_Min,":"
    Print At B_Row, B_Col+3, Dec2 B_Sec
EndProc

Proc P_MMSS_ToSecs(B_Min As Byte, B_Sec As Byte), Word
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
        P_EWriteW(EE_UI_PULSE_MS,  UI_PULSE_MS_DEF)

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

    P_LoadInput(1)
    P_LoadInput(2)
    P_LoadInput(3)

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



Proc P_SaveSettings()
    P_SaveInput(1)
    P_SaveInput(2)
    P_SaveInput(3)
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

Proc P_ReadButton()
EndProc

Proc P_GetKeyEvent(), Byte
    Result = B_KeyEvent
    B_KeyEvent = 0
EndProc

'Proc P_UserAborted(), Byte
'    Dim L_TimeoutMs As Dword
'    L_TimeoutMs = W_UI_TimeoutS * 1000
'    If (L_Millis - L_LastInput) >= L_TimeoutMs Then
'        b_Escape = 1
'        P_Beeps(4)
'        Result = 1
'    Else
'        Result = 0
'    EndIf
'EndProc

'=====================================================================
' EDITORS
'=====================================================================

Proc P_EditYN(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty
    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: YES/NO       ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)
            If B_Cur = 1 Then
                Print At 3,1," Yes  [No]          "
            Else
                Print At 3,1,"[Yes]  No           "
            EndIf
            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta <> 0 Then
            If B_Cur = 1 Then
                B_Cur = 0
            Else
                B_Cur = 1
            EndIf
            Set b_ScrDirty
        EndIf

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                B_Val = B_Cur
                Result = 1
                GoTo Exit_P_EditYN
            Case 2
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditYN
            Case 3
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditYN
        EndSelect

'        If P_UserAborted() <> 0 Then
'            Result = 0
'            GoTo Exit_P_EditYN
'        EndIf
    Wend
Exit_P_EditYN:
EndProc

Proc P_EditEnum3(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty
    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: MODE         ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)
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

        P_ReadEncoder()
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

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                B_Val = B_Cur
                Result = 1
                GoTo Exit_P_EditEnum3
            Case 2
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditEnum3
            Case 3
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditEnum3
        EndSelect

'        If P_UserAborted() <> 0 Then
'            Result = 0
'            GoTo Exit_P_EditEnum3
'        EndIf
    Wend
Exit_P_EditEnum3:
EndProc

Proc P_EditWordVal(ByRef W_Val As Word, W_Min As Word, W_Max As Word, W_Step As Word), Byte
    Dim W_Cur As Word
    W_Cur = W_Val
    Set b_ScrDirty
    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: WORD VALUE   ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)
            Print At 3,1," Value: [",Dec5 W_Cur,"]        "
            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
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

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                W_Val = W_Cur
                Result = 1
                GoTo Exit_P_EditWordVal
            Case 2
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditWordVal
            Case 3
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditWordVal
        EndSelect

'        If P_UserAborted() <> 0 Then
'            Result = 0
'            GoTo Exit_P_EditWordVal
'        EndIf
    Wend
Exit_P_EditWordVal:
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
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)
            If B_Field = 0 Then
                Print At 3,1," [",Dec2 B_Min,"] : ",Dec2 B_Sec,"         "
            Else
                Print At 3,1,"  ",Dec2 B_Min," : [",Dec2 B_Sec,"]        "
            EndIf
            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
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

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                If B_Field = 0 Then
                    B_Field = 1
                    Set b_ScrDirty
                Else
                    W_Val = (B_Min * 60) + B_Sec
                    Result = 1
                    GoTo Exit_P_EditMMSS
                EndIf
            Case 2
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditMMSS
            Case 3
                P_Beeps(3)
                Result = 0
                GoTo Exit_P_EditMMSS
        EndSelect

'        If P_UserAborted() <> 0 Then
'            Result = 0
'            GoTo Exit_P_EditMMSS
'        EndIf
    Wend
Exit_P_EditMMSS:
EndProc

'=====================================================================
' VALUE FIELD RENDER HELPERS (fixed alignment)
'=====================================================================
Proc P_ClearValField(B_Row As Byte, B_Col As Byte)
    Print At B_Row, B_Col, "          "
EndProc

Proc P_PrintValText(B_Row As Byte, B_Col As Byte, S_Value As String, B_Active As Byte, B_CanEdit As Byte)
    Dim B_Len As Byte

    P_ClearValField(B_Row, B_Col)                 ' clears 10 chars starting at B_Col

    If B_Active = 1 And B_CanEdit = 1 Then
        B_Len = Len(S_Value)                      ' dynamic closing bracket position
        Print At B_Row, B_Col,   "["
        Print At B_Row, B_Col+1, S_Value
        Print At B_Row, B_Col+1+B_Len, "]"
    Else
        Print At B_Row, B_Col,   " "
        Print At B_Row, B_Col+1, S_Value
    EndIf
EndProc
'---------------------------------------------------------------
Proc P_PrintValWord(B_Row As Byte, B_Col As Byte, W_Val As Word, B_Active As Byte)
    P_ClearValField(B_Row, B_Col)
    If B_Active = 1 Then
        Print At B_Row, B_Col,   "["
        Print At B_Row, B_Col+1, Dec5 W_Val
        Print At B_Row, B_Col+6, "]"
    Else
        Print At B_Row, B_Col,   " "
        Print At B_Row, B_Col+1, Dec5 W_Val
    EndIf
EndProc

Proc P_PrintValMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word, B_Active As Byte)
    P_ClearValField(B_Row, B_Col)
    If B_Active = 1 Then
        Print At B_Row, B_Col,   "["
        P_PrintMMSS(B_Row, B_Col+1, W_Seconds)
        Print At B_Row, B_Col+6, "]"
    Else
        Print At B_Row, B_Col,   " "
        P_PrintMMSS(B_Row, B_Col+1, W_Seconds)
    EndIf
EndProc

'=====================================================================
' INPUT 1: render one row
'=====================================================================
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
            Else
                If B_Sens = 1 Then
                    P_PrintValText(B_Row, B_Col, "Pressure", B_Active, 1)
                Else
                    P_PrintValText(B_Row, B_Col, "Temp",     B_Active, 1)
                EndIf
            EndIf

        Case 2
            Print At B_Row,1,"Scale4ma  "
            P_PrintValWord(B_Row, B_Col, W_I1_Scale4,  B_Active)

        Case 3
            Print At B_Row,1,"Scale20ma "
            P_PrintValWord(B_Row, B_Col, W_I1_Scale20, B_Active)

        Case 4
            Print At B_Row,1,"High BP   "
            If B_Sens = 1 Then
                P_PrintValMMSS(B_Row, B_Col, W_I1_BP_High, B_Active)
            Else
                P_ClearValField(B_Row, B_Col)
                Print At B_Row, B_Col,   " "
                Print At B_Row, B_Col+1, "--:--"
            EndIf

        Case 5
            Print At B_Row,1,"PLPBP     "
            If B_Sens = 1 Then
                P_PrintValMMSS(B_Row, B_Col, W_I1_BP_PLP, B_Active)
            Else
                P_ClearValField(B_Row, B_Col)
                Print At B_Row, B_Col,   " "
                Print At B_Row, B_Col+1, "--:--"
            EndIf

        Case 6
            Print At B_Row,1,"SLPBP     "
            If B_Sens = 1 Then
                P_PrintValMMSS(B_Row, B_Col, W_I1_BP_SLP, B_Active)
            Else
                P_ClearValField(B_Row, B_Col)
                Print At B_Row, B_Col,   " "
                Print At B_Row, B_Col+1, "--:--"
            EndIf

        Case 7
            Print At B_Row,1,"Rly High  "
            If B_I1_RlyHigh = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            Else
                If B_I1_RlyHigh = 1 Then
                    P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
                Else
                    P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
                EndIf
            EndIf

        Case 8
            Print At B_Row,1,"Rly PLP   "
            If B_I1_RlyPLP = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            Else
                If B_I1_RlyPLP = 1 Then
                    P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
                Else
                    P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
                EndIf
            EndIf

        Case 9
            Print At B_Row,1,"Rly SLP   "
            If B_I1_RlySLP = 0 Then
                P_PrintValText(B_Row, B_Col, "No",    B_Active, 1)
            Else
                If B_I1_RlySLP = 1 Then
                    P_PrintValText(B_Row, B_Col, "Pulse", B_Active, 1)
                Else
                    P_PrintValText(B_Row, B_Col, "Latch", B_Active, 1)
                EndIf
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

'=====================================================================
' INPUT 1: editor dispatcher
'=====================================================================
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

'=====================================================================
' VIEWS / MENUS
'=====================================================================

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
                P_Beeps(2)
                GoTo Exit_V_NotImpl
            Case 2
                P_Beeps(3)
                GoTo Exit_V_NotImpl
            Case 3
                P_Beeps(3)
                B_NavCode = 2
                GoTo Exit_V_NotImpl
        EndSelect
'        If P_UserAborted() <> 0 Then
'            GoTo Exit_V_NotImpl
'        EndIf
    Wend
Exit_V_NotImpl:
EndProc

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
                P_Beeps(3)
                GoTo Exit_V_Main
            Case 3
                P_Beeps(3)
        EndSelect
'        If P_UserAborted() <> 0 Then
'        EndIf
    Wend
Exit_V_Main:
EndProc
'--------------------------------------------------------
Proc V_Options(), Byte
    Dim B_Sel        As Byte
    Dim B_Count      As Byte
    Dim B_Top        As Byte
    Dim B_Row        As Byte
    Dim B_ListIndex  As Byte
    Dim B_Active     As Byte

    B_Count = 4                      ' 0 Main, 1 Setup, 2 Utility, 3 Back
    B_Sel = 0
    B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)               ' short click on redraw (keep per your rule)

            P_DrawTitle("OPTIONS             ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            B_Top = B_Sel
            If B_Count > 3 Then
                If B_Top > B_Count - 3 Then
                    B_Top = B_Count - 3
                EndIf
            Else
                B_Top = 0
            EndIf

            For B_Row = 2 To 4
                B_ListIndex = B_Top + (B_Row - 2)

                If B_ListIndex = B_Sel Then
                    B_Active = 1
                Else
                    B_Active = 0
                EndIf

                If B_ListIndex = 0 Then
                    P_PrintRow(B_Row, "Main Menu", B_Active)
                Else
                    If B_ListIndex = 1 Then
                        P_PrintRow(B_Row, "Setup Menu", B_Active)
                    Else
                        If B_ListIndex = 2 Then
                            P_PrintRow(B_Row, "Utility Menu", B_Active)
                        Else
                            P_PrintRow(B_Row, "Back", B_Active)
                        EndIf
                    EndIf
                EndIf
            Next B_Row

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < B_Count - 1 Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        Else
            If B_EncDelta = -1 Then
                If B_Sel > 0 Then
                    Dec B_Sel
                    Set b_ScrDirty
                EndIf
            EndIf
        EndIf

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    Result = 1                      ' Main Menu -> return to previous screen
                    GoTo Exit_V_Options
                Else
                    If B_Sel = 1 Then
                        If V_SetupMenu() = 0 Then
                            ' no special handling required
                        EndIf
                        Set b_ScrDirty
                    Else
                        If B_Sel = 2 Then
                            V_NotImpl("UTILITY")
                            Set b_ScrDirty
                        Else
                            Result = 1              ' Back -> return to previous screen
                            GoTo Exit_V_Options
                        EndIf
                    EndIf
                EndIf
        EndSelect

        ' Menu timeout -> return to previous screen
        If P_UserAborted() <> 0 Then
            Result = 1
            GoTo Exit_V_Options
        EndIf
    Wend

Exit_V_Options:
EndProc


'--------------------------------------------------------
Proc V_SetupMenu(), Byte
    Dim B_Sel As Byte
    Dim B_Cnt As Byte
    Dim B_Top As Byte
    Dim B_I As Byte
    Dim B_Row As Byte
    Dim B_Act As Byte
    Dim B_Idx As Byte

    B_Cnt = 5                             ' Input1, Input2, Input3, Clock, Back
    B_Sel = 0
    B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            ' Window top so selection is visible on lines 2..4
            B_Top = B_Sel
            If B_Cnt > 3 Then
                If B_Top > (B_Cnt - 3) Then
                    B_Top = B_Cnt - 3
                EndIf
            Else
                B_Top = 0
            EndIf

            P_DrawTitle("SETUP               ")
            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            For B_I = 0 To 2
                B_Row = 2 + B_I
                B_Idx = B_Top + B_I
                If B_Idx < B_Cnt Then
                    If B_Idx = B_Sel Then
                        B_Act = 1
                    Else
                        B_Act = 0
                    EndIf

                    If B_Idx = 0 Then
                        P_PrintRow(B_Row, "Input 1", B_Act)
                    Else
                        If B_Idx = 1 Then
                            P_PrintRow(B_Row, "Input 2", B_Act)
                        Else
                            If B_Idx = 2 Then
                                P_PrintRow(B_Row, "Input 3", B_Act)
                            Else
                                If B_Idx = 3 Then
                                    P_PrintRow(B_Row, "Clock", B_Act)
                                Else
                                    P_PrintRow(B_Row, "Back", B_Act)
                                EndIf
                            EndIf
                        EndIf
                    EndIf
                Else
                    P_ClearLine(B_Row)
                EndIf
            Next B_I

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta = 1 Then
            If B_Sel < (B_Cnt - 1) Then
                Inc B_Sel
                Set b_ScrDirty
            EndIf
        Else
            If B_EncDelta = -1 Then
                If B_Sel > 0 Then
                    Dec B_Sel
                    Set b_ScrDirty
                EndIf
            EndIf
        EndIf

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    If V_Input1Menu() = 0 Then
                    EndIf
                Else
                    If B_Sel = 1 Then
                        V_NotImpl("INPUT 2")
                    Else
                        If B_Sel = 2 Then
                            V_NotImpl("INPUT 3")
                        Else
                            If B_Sel = 3 Then
                                If V_ClockMenu() = 0 Then
                                EndIf
                            Else
                                ' Back -> return to Options
                                Result = 1
                                GoTo Exit_V_SetupMenu
                            EndIf
                        EndIf
                    EndIf
                EndIf
                Set b_ScrDirty
        EndSelect

        If P_UserAborted() <> 0 Then
            Result = 1                     ' timeout -> back to Options
            GoTo Exit_V_SetupMenu
        EndIf
    Wend
Exit_V_SetupMenu:
EndProc


'-------------------------------------------------------------
Proc V_Input1Menu(), Byte
    Dim B_Sel      As Byte      ' selected index
    Dim B_Top      As Byte      ' top index of 3-line window
    Dim B_Cnt      As Byte      ' item count (fields + Back)
    Dim B_Row      As Byte
    Dim B_Idx      As Byte
    Dim B_Active   As Byte
    Dim B_EditMode As Byte      ' 0 = navigating, 1 = inline editing current item

    B_Cnt = 12                  ' 11 fields (0..10) + Back (11)
    B_Sel = 0
    B_Top = 0
    B_EditMode = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)                          ' short click on every redraw
            P_DrawTitle("INPUT 1             ")

            ' compute window
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

            P_ClearLine(2)
            P_ClearLine(3)
            P_ClearLine(4)

            For B_Row = 2 To 4
                B_Idx = B_Top + (B_Row - 2)
                If B_Idx < B_Cnt Then
                    If B_Idx = B_Sel Then
                        B_Active = 1
                    Else
                        B_Active = 0
                    EndIf

                    ' render normal fields via existing renderer, "Back" as a plain row
                    If B_Idx <= 10 Then
                        P_Input1RenderItem(B_Row, B_Idx, B_Active)
                    Else
                        P_PrintRow(B_Row, "Back", B_Active)
                    EndIf
                Else
                    P_ClearLine(B_Row)
                EndIf
            Next B_Row

            b_ScrDirty = 0
        EndIf

        ' encoder
        P_ReadEncoder()
        If B_EditMode = 0 Then
            If B_EncDelta = 1 Then
                If B_Sel < (B_Cnt - 1) Then
                    Inc B_Sel
                    Set b_ScrDirty
                EndIf
            Else
                If B_EncDelta = -1 Then
                    If B_Sel > 0 Then
                        Dec B_Sel
                        Set b_ScrDirty
                    EndIf
                EndIf
            EndIf
        Else
            ' inline edit for item 0 (Enable/Disable)
            If B_Sel = 0 Then
                If B_EncDelta <> 0 Then
                    If B_I1_Enabled = 0 Then
                        B_I1_Enabled = 1
                    Else
                        B_I1_Enabled = 0
                    EndIf
                    P_Beeps(1)
                    Set b_ScrDirty
                EndIf
            EndIf
        EndIf

        ' button (short press)
        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                If B_EditMode = 0 Then
                    ' enter inline edit for supported items, or act on Back
                    If B_Sel = 0 Then
                        B_EditMode = 1
                        Set b_ScrDirty
                    Else
                        If B_Sel = 11 Then
                            P_Beeps(2)
                            Result = 1         ' return to previous screen
                            GoTo Exit_V_Input1Menu
                        Else
                            ' other items: leave for subsequent tasks
                            P_Beeps(2)
                        EndIf
                    EndIf
                Else
                    ' commit and exit inline edit
                    If B_Sel = 0 Then
                        P_SaveSettings()
                    EndIf
                    B_EditMode = 0
                    Set b_ScrDirty
                EndIf
        EndSelect
    Wend

Exit_V_Input1Menu:
EndProc
'--------------------------------------------------------------
Proc V_ClockMenu(), Byte
    Dim B_Sel           As Byte
    Dim B_Count         As Byte
    Dim B_Active        As Byte

    Dim B_EditMode      As Byte
    Dim B_EditIndex     As Byte
    Dim W_EditWordOrig  As Word

    B_Count     = 3                      ' Timeout, Pulse, Back
    B_Sel       = 0
    B_EditMode  = 0
    B_EditIndex = 255
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            ' STYLE: play short click on redraw (user keeps local P_Beeps(1) here)
            ' P_Beeps(1)

            P_DrawTitle("CLOCK               ")
            P_ClearLine(2) : P_ClearLine(3) : P_ClearLine(4)

            If B_Sel = 0 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 2,1,"Timeout   "
            P_PrintValMMSS(2, 11, W_UI_TimeoutS, B_Active)

            If B_Sel = 1 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 3,1,"Pulse     "
            P_PrintValWord(3, 11, W_UI_PulseMs, B_Active)

            If B_Sel = 2 Then
                P_PrintRow(4,"Back",1)
            Else
                P_PrintRow(4,"Back",0)
            EndIf

            b_ScrDirty = 0
        EndIf

        P_ReadEncoder()
        If B_EncDelta <> 0 Then
            If B_EditMode = 0 Then
                If B_EncDelta = 1 Then
                    If B_Sel < (B_Count - 1) Then Inc B_Sel : Set b_ScrDirty
                Else
                    If B_Sel > 0 Then Dec B_Sel : Set b_ScrDirty
                EndIf
            Else
                ' Edit values in place
                Select B_EditIndex
                    Case 0                  ' Timeout seconds
                        If B_EncDelta = 1 Then
                            If W_UI_TimeoutS < UI_TIMEOUT_S_MAX Then Inc W_UI_TimeoutS
                        Else
                            If W_UI_TimeoutS > UI_TIMEOUT_S_MIN Then Dec W_UI_TimeoutS
                        EndIf
                        Set b_ScrDirty

                    Case 1                  ' Pulse ms
                        If B_EncDelta = 1 Then
                            If W_UI_PulseMs < UI_PULSE_MS_MAX Then Inc W_UI_PulseMs
                        Else
                            If W_UI_PulseMs > UI_PULSE_MS_MIN Then Dec W_UI_PulseMs
                        EndIf
                        Set b_ScrDirty
                EndSelect
            EndIf
        EndIf

        P_ReadButton()
        Select P_GetKeyEvent()
            Case 1
                P_Beeps(2)
                If B_EditMode = 0 Then
                    If B_Sel = 2 Then
                        Result = 1                  ' Back to Setup
                        GoTo Exit_V_Clock
                    Else
                        ' Enter edit mode, snapshot original
                        B_EditMode  = 1
                        B_EditIndex = B_Sel
                        If B_Sel = 0 Then
                            W_EditWordOrig = W_UI_TimeoutS
                        Else
                            W_EditWordOrig = W_UI_PulseMs
                        EndIf
                    EndIf
                Else
                    ' Confirm, clamp & save
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

        If P_UserAborted() <> 0 Then
            If B_EditMode = 1 Then
                ' Revert on timeout
                If B_EditIndex = 0 Then
                    W_UI_TimeoutS = W_EditWordOrig
                Else
                    W_UI_PulseMs = W_EditWordOrig
                EndIf
                B_EditMode  = 0
                B_EditIndex = 255
                Set b_ScrDirty
            Else
                Result = 1                      ' timeout -> back to Setup
                GoTo Exit_V_Clock
            EndIf
        EndIf
    Wend
Exit_V_Clock:
EndProc



'---------- LCD safe init ----------
Proc P_LCDSafeInit()
    DelayMsFast(5)
    Cls
    DelayMsFast(2)
EndProc

'=====================================================================
' MAIN
'=====================================================================
MAIN:
P_PinInit()
P_LCDHardInit()      ' Robust 4-bit init per Newhaven
P_LCDSafeInit()      ' Sync with Positron driver
P_InputInit()
P_LoadSettings()

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

