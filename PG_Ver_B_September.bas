'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------END SECTION 1 -------------------------------
Section_1:
'------------------------------------------------------------------------------
'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
' Rev: 2025-12-10 (Reorganized with fixed procedure names)
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

'=====================================================================
'  IMPROVED EEPROM CONFIGURATION LAYOUT
'=====================================================================

'------------------------ EEPROM LAYOUT CONSTANTS -------------------
Symbol EE_MAGIC_BYTE    = $A5           ' Magic byte for validation
Symbol EE_VERSION       = $01           ' Configuration version
Symbol EE_BLOCK_SIZE    = 32            ' Bytes per config block

' Block addresses (each block is 32 bytes with checksum)
Symbol EE_SYSTEM_BLOCK  = 0             ' System config block
Symbol EE_INPUT1_BLOCK  = 32            ' Input 1 config block  
Symbol EE_INPUT2_BLOCK  = 64            ' Input 2 config block
Symbol EE_INPUT3_BLOCK  = 96            ' Input 3 config block
Symbol EE_BACKUP_START  = 128           ' Backup area starts here

' Legacy EEPROM area for migration (old addresses)
Symbol EE_LEGACY_START  = 200           ' Old system data area

' System block offsets
Symbol EE_SYS_MAGIC     = 0
Symbol EE_SYS_VERSION   = 1
Symbol EE_SYS_TIMEOUT   = 2             ' Word: UI timeout seconds
Symbol EE_SYS_PULSE     = 4             ' Word: Beeper pulse ms
Symbol EE_SYS_W_CNT     = 6             ' Word: Write cycle counter
Symbol EE_SYS_FLAGS     = 8             ' Byte: System flags
Symbol EE_SYS_RESERVED  = 9             ' Reserved bytes 9-30
Symbol EE_SYS_CHECKSUM  = 31            ' Block checksum

' Input block offsets (for each input 1-3)
Symbol EE_IN_MAGIC      = 0
Symbol EE_IN_VERSION    = 1
Symbol EE_IN_ENABLED    = 2
Symbol EE_IN_SENSOR_T   = 3
Symbol EE_IN_FLOW_MODE  = 4
Symbol EE_IN_SCALE4     = 5             ' Word: 4mA scale (signed)
Symbol EE_IN_SCALE20    = 7             ' Word: 20mA scale (signed) 
Symbol EE_IN_BP_HIGH    = 9             ' Word: Bypass high seconds
Symbol EE_IN_BP_PLP     = 11            ' Word: Bypass PLP seconds
Symbol EE_IN_BP_SLP     = 13            ' Word: Bypass SLP seconds
Symbol EE_IN_RLY_HIGH   = 15            ' Byte: Relay high mode
Symbol EE_IN_RLY_PLP    = 16            ' Byte: Relay PLP mode
Symbol EE_IN_RLY_SLP    = 17            ' Byte: Relay SLP mode
Symbol EE_IN_DISPLAY    = 18            ' Byte: Display enable
Symbol EE_IN_WRITE_CNT  = 19            ' Word: Write cycle counter
Symbol EE_IN_RESERVED   = 21            ' Reserved bytes 21-30
Symbol EE_IN_CHECKSUM   = 31            ' Block checksum

' Write cycle limits for wear leveling
Symbol EE_MAX_WRITES    = 10000         ' Conservative limit
Symbol EE_BkUp_Thresh   = 1000          ' Backup after this many writes

'------------------------ EEPROM ERROR CODES ------------------------
Symbol EE_OK            = 0
Symbol EE_ERR_MAGIC     = 1             ' Magic byte mismatch
Symbol EE_ERR_VERSION   = 2             ' Version mismatch  
Symbol EE_ERR_CHECKSUM  = 3             ' Checksum error
Symbol EE_ERR_WEAR      = 4             ' Excessive wear detected
Symbol EE_ERR_BACKUP    = 5             ' Backup operation failed

'------------------------ EEPROM STATUS FLAGS -----------------------
Symbol EE_FLAG_FACTORY  = 0             ' Factory defaults loaded
Symbol EE_FLAG_BACKUP   = 1             ' Backup valid
Symbol EE_Flg_Restrd    = 2             ' Restored from backup
Symbol EE_Flg_Mig       = 3             ' Migrated from legacy

' Legacy EEPROM addresses for migration
Symbol LEG_I1_Enabled   = 200
Symbol LEG_I1_SensorT   = 201
Symbol LEG_I1_FlowMode  = 202
Symbol LEG_I1_Scale4    = 204
Symbol LEG_I1_Scale20   = 206
Symbol LEG_UI_TimeoutS  = 250
Symbol LEG_UI_PulseMs   = 252

'------------------------- Oscillator & I/O ---------------------------
OSCCON    = %01110000                    ' 8 MHz
OSCTUNE.6 = 1                            ' PLL x4 -> 32 MHz
ADCON1    = $0F                          ' All digital

All_Digital = True
Declare Xtal = 32
Declare PORTB_Pullups = On

'------------------------------- LCD ---------------------------------
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
Symbol UO_TOut_S_mn     = 10
Symbol UI_TOut_S_Mx     = 300
Symbol UI_PULSE_MS_MIN  = 10
Symbol UI_Pls_MS_Mx     = 2000

'----------------------------- Enums ---------------------------------
Symbol MODE_NO    = 0
Symbol MODE_PULSE = 1
Symbol MODE_LATCH = 2

Symbol YES = 1
Symbol NO  = 0

Symbol SENSOR_PRES = 0
Symbol SENSOR_TEMP = 1
Symbol SENSOR_FLOW = 2

' Flow sub-options
Symbol FLOWTYPE_ANALOG = 0
Symbol Flow_Type_Dig   = 1
Symbol FLOWU_PERCENT   = 0
Symbol FLOWU_LPS       = 1

'------------------------------ IRQ Bits -----------------------------
Symbol TMR0IF = INTCON.2
Symbol TMR0IE = INTCON.5
Symbol GIE    = INTCON.7

'=====================================================================
'  GLOBAL VARIABLES
'=====================================================================

'----------------------------- System State --------------------------
Dim b_ReInitLCD  As Bit
Dim b_ScrDirty   As Bit
Dim b_Escape     As Bit

Dim L_Millis     As Dword
Dim L_LastInput  As Dword

Dim B_EncDelta   As SByte
Dim B_KeyEvent   As Byte

'----------------------------- Non-blocking Beeper System -----------
Dim W_BeepTimer   As Word            ' Beep duration countdown (ms)
Dim B_BeepPattern As Byte            ' Current beep pattern being executed
Dim B_BeepState   As Byte            ' State machine position within pattern
Dim B_BeepCount   As Byte            ' Counter for multi-beep patterns
Dim B_BeepMax     As Byte            ' Maximum beeps for current pattern
Dim W_BeepOnTime  As Word            ' Configurable on time (ms)
Dim W_BeepOffTime As Word            ' Configurable off time (ms)

'----------------------------- EEPROM Status -------------------------
Dim B_EE_Status         As Byte         ' Last operation status
Dim B_EE_Flags          As Byte         ' System EEPROM flags
Dim W_EE_Wrt_Cnt        As Word         ' Current write cycle count

'----------------------------- Encoder/Button ------------------------
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
Dim W_EncoderPos   As Word
Dim W_EncReadPos   As Word
Dim B_VLongPress As Byte                 ' Very long press flag set by ISR

'----------------------------- Navigation ----------------------------
Dim B_NavCode      As Byte              ' 0=normal, 1=back, 2=to main

'----------------------------- System Config -------------------------
Dim W_UI_TimeoutS  As Word
Dim W_UI_PulseMs   As Word

'----------------------------- Input 1 Config -----------------------
Dim B_I1_Enabled  As Byte
Dim B_I1_SensorT  As Byte
Dim B_I1_FlowMode As Byte
Dim W_I1_Scale4   As Word     ' signed 2C
Dim W_I1_Scale20  As Word     ' signed 2C
Dim W_I1_BP_High  As Word
Dim W_I1_BP_PLP   As Word
Dim W_I1_BP_SLP   As Word
Dim B_I1_RlyHigh  As Byte
Dim B_I1_RlyPLP   As Byte
Dim B_I1_RlySLP   As Byte
Dim B_I1_Display  As Byte

' Input1 extra mirrors (not yet persisted)
Dim W_I1_BP_Low    As Word
Dim B_I1_RlyLow    As Byte
Dim B_I1_FlowType  As Byte
Dim B_I1_FlowUnits As Byte

'----------------------------- Input 2 Config -----------------------
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

'----------------------------- Input 3 Config -----------------------
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

'--------------------------------XXXXXXXXX-------------------------------
Section_2:
'------------------------------------------------------------------------------
'=====================================================================
' TIMER0 ISR (1ms) - debounce, encoder, button, beeper
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
        
        ' Button press timing - runs every 1ms for accurate timing
        If B_ButtonState = 0 Then
            If W_BtnHoldMS = 0 Then
                W_BeepTimer = W_UI_PulseMs
            EndIf
            If W_BtnHoldMS <= 65525 Then
                W_BtnHoldMS = W_BtnHoldMS + 1    ' Increment by 1ms
            EndIf
            
            ' Check for very long press
            If W_BtnHoldMS >= BTN_VLONG_MS And B_VLongPress = 0 Then
                B_VLongPress = 1
            EndIf
        Else
            If W_BtnHoldMS >= BTN_SHORT_MS Then
                B_KeyEvent = 1
                L_LastInput = L_Millis
            EndIf
            W_BtnHoldMS = 0
            B_VLongPress = 0
        EndIf
        
        ' Beeper state machine - runs every 1ms for smooth audio
        If W_BeepTimer > 0 Then
            Dec W_BeepTimer
        EndIf

        Select B_BeepPattern
            Case 0                           ' Silent/off
                _BUZZER = 0
                
            Case 1                           ' Single beep
                If W_BeepTimer > 0 Then
                    _BUZZER = 1
                Else
                    _BUZZER = 0
                    B_BeepPattern = 0        ' Pattern complete
                EndIf
                
            Case 2                           ' Double beep (factory defaults)
                Select B_BeepState
                    Case 0                   ' First beep
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            B_BeepState = 1
                            W_BeepTimer = 100    ' 100ms gap
                        EndIf
                    Case 1                   ' Gap
                        _BUZZER = 0
                        If W_BeepTimer = 0 Then
                            B_BeepState = 2
                            W_BeepTimer = W_UI_PulseMs
                        EndIf
                    Case 2                   ' Second beep
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            B_BeepPattern = 0    ' Pattern complete
                            B_BeepState = 0
                        EndIf
                EndSelect
                
            Case 3                           ' Long beep (normal startup)
                If W_BeepTimer > 0 Then
                    _BUZZER = 1
                Else
                    _BUZZER = 0
                    B_BeepPattern = 0        ' Pattern complete
                EndIf
                
            Case 4                           ' Triple beep (migration)
                Select B_BeepState
                    Case 0, 2, 4             ' Beep phases (1st, 2nd, 3rd)
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            Inc B_BeepState
                            If B_BeepState <= 4 Then
                                W_BeepTimer = 150    ' 150ms gap
                            Else
                                B_BeepPattern = 0    ' Pattern complete
                                B_BeepState = 0
                            EndIf
                        EndIf
                    Case 1, 3                ' Gap phases
                        _BUZZER = 0
                        If W_BeepTimer = 0 Then
                            Inc B_BeepState
                            W_BeepTimer = W_UI_PulseMs
                        EndIf
                EndSelect
                
            Case 5                           ' Quad beep (backup restore)
                Select B_BeepState
                    Case 0, 2, 4, 6          ' Beep phases (1st, 2nd, 3rd, 4th)
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            Inc B_BeepState
                            If B_BeepState <= 6 Then
                                W_BeepTimer = 100    ' 100ms gap
                            Else
                                B_BeepPattern = 0    ' Pattern complete
                                B_BeepState = 0
                            EndIf
                        EndIf
                    Case 1, 3, 5             ' Gap phases
                        _BUZZER = 0
                        If W_BeepTimer = 0 Then
                            Inc B_BeepState
                            W_BeepTimer = W_UI_PulseMs
                        EndIf
                EndSelect
                
            Case 6                           ' Flexible multi-beep pattern (exit/error)
                Select B_BeepState
                    Case 0                   ' Beep phase
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            Inc B_BeepCount
                            If B_BeepCount >= B_BeepMax Then
                                ' All beeps complete
                                B_BeepPattern = 0
                                B_BeepState = 0
                                B_BeepCount = 0
                            Else
                                ' Start gap phase
                                B_BeepState = 1
                                W_BeepTimer = W_BeepOffTime
                            EndIf
                        EndIf
                    Case 1                   ' Gap phase
                        _BUZZER = 0
                        If W_BeepTimer = 0 Then
                            ' Start next beep
                            B_BeepState = 0
                            W_BeepTimer = W_BeepOnTime
                        EndIf
                EndSelect
                
            Case 8                           ' Menu timeout pattern (100ms + 100ms gap + 50ms)
                Select B_BeepState
                    Case 0                   ' First 100ms beep
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            B_BeepState = 1
                            W_BeepTimer = 100    ' 100ms gap
                        EndIf
                    Case 1                   ' 100ms gap
                        _BUZZER = 0
                        If W_BeepTimer = 0 Then
                            B_BeepState = 2
                            W_BeepTimer = 50     ' 50ms second beep
                        EndIf
                    Case 2                   ' Final 50ms beep
                        If W_BeepTimer > 0 Then
                            _BUZZER = 1
                        Else
                            _BUZZER = 0
                            B_BeepPattern = 0    ' Pattern complete
                            B_BeepState = 0
                        EndIf
                EndSelect
        EndSelect
        
        ' Encoder and debouncing - runs every 10ms (100Hz)
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
            
            B_RE_Count = 0
        EndIf
        
    EndIf
    Context Restore

'--------------------------------------------------------------------------------


Proc P_Beeps(B_Type As Byte)
    B_BeepPattern = B_Type
    B_BeepState = 0
    B_BeepCount = 0
    
    Select B_Type
        Case 0                           ' Silent
            W_BeepTimer = 0
        Case 1                           ' Single beep
            W_BeepTimer = W_UI_PulseMs
        Case 2                           ' Double beep
            W_BeepTimer = W_UI_PulseMs
        Case 3                           ' Long beep
            W_BeepTimer = W_UI_PulseMs * 6   ' Make it noticeably longer
        Case 4                           ' Triple beep
            W_BeepTimer = W_UI_PulseMs
        Case 5                           ' Quad beep
            W_BeepTimer = W_UI_PulseMs
        Case 6                           ' Flexible pattern (use P_BeepsFlex instead)
            W_BeepTimer = W_BeepOnTime
    EndSelect
EndProc
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

Proc LCD_CursorOn()
	LCD_WriteCmd($0E)
EndProc

Proc LCD_CursorOff()
	LCD_WriteCmd($0C)
EndProc

Proc LCD_SetCursor(B_Row As Byte, B_Col As Byte)
	Dim B_Base As Byte
	Dim B_Addr As Byte
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
	B_Addr = $80 + B_Base + (B_Col - 1)
	LCD_WriteCmd(B_Addr)
EndProc

Proc P_LCDSafeInit()
	DlyMsFast(5)
	Cls
	DlyMsFast(2)
EndProc

'=====================================================================
'  BASIC EEPROM MANAGEMENT FUNCTIONS
'=====================================================================

'------------------------ BIT OPERATION HELPERS ---------------------
Proc P_SetFlag(ByRef B_Flags As Byte, B_BitNum As Byte)
    Select B_BitNum
        Case 0
            B_Flags = B_Flags | $01
        Case 1
            B_Flags = B_Flags | $02
        Case 2
            B_Flags = B_Flags | $04
        Case 3
            B_Flags = B_Flags | $08
        Case 4
            B_Flags = B_Flags | $10
        Case 5
            B_Flags = B_Flags | $20
        Case 6
            B_Flags = B_Flags | $40
        Case 7
            B_Flags = B_Flags | $80
    EndSelect
EndProc

Proc P_TestFlag(B_Flags As Byte, B_BitNum As Byte), Byte
    Select B_BitNum
        Case 0
            Result = B_Flags & $01
        Case 1
            Result = B_Flags & $02
        Case 2
            Result = B_Flags & $04
        Case 3
            Result = B_Flags & $08
        Case 4
            Result = B_Flags & $10
        Case 5
            Result = B_Flags & $20
        Case 6
            Result = B_Flags & $40
        Case 7
            Result = B_Flags & $80
    EndSelect
EndProc

'------------------------ SAFE EEPROM OPERATIONS --------------------
Proc P_EE_SafeWriteB(W_Addr As Word, B_Val As Byte), Byte
    Dim B_Verify As Byte
    Dim B_Retry  As Byte
    
    For B_Retry = 1 To 3
        EWrite W_Addr, [B_Val]
        DelayMS 10                      ' Allow write to complete
        
        B_Verify = ERead W_Addr
        If B_Verify = B_Val Then
            Result = EE_OK
            ExitProc
        EndIf
        
        DelayMS 5                       ' Brief delay before retry
    Next B_Retry
    
    Result = EE_ERR_BACKUP              ' Write verification failed
EndProc

Proc P_EE_SafeWriteW(W_Addr As Word, W_Val As Word), Byte
    Dim B_Status As Byte
    
    ' Write low byte first
    B_Status = P_EE_SafeWriteB(W_Addr, W_Val.LowByte)
    If B_Status <> EE_OK Then
        Result = B_Status
        ExitProc
    EndIf
    
    ' Write high byte
    B_Status = P_EE_SafeWriteB(W_Addr + 1, W_Val.HighByte)
    Result = B_Status
EndProc

Proc P_EE_SafeReadW(W_Addr As Word), Word
    Dim B_Low  As Byte
    Dim B_High As Byte
    
    B_Low  = ERead W_Addr
    B_High = ERead W_Addr + 1
    
    Result = (B_High * 256) + B_Low
EndProc

'------------------------ SIGNED CONVERSION HELPERS ------------------
Proc P_W2S(W_Val As Word), SWord
    Result = W_Val
EndProc

Proc P_S2W(I_Val As SWord), Word
    Result = I_Val
EndProc

'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_3:
'------------------------------------------------------------------------------
'=====================================================================
' EEPROM BLOCK OPERATIONS AND VALIDATION WITH DEBUG LOGGING
'=====================================================================

'------------------------ CHECKSUM OPERATIONS ------------------------
Proc P_EE_CalcChkSum(W_StartAddr As Word, B_Length As Byte), Byte
    Dim B_Sum As Byte
    Dim B_I   As Byte
    Dim B_Val As Byte
    
    B_Sum = 0
    For B_I = 0 To B_Length - 1
        B_Val = ERead W_StartAddr + B_I
        B_Sum = B_Sum + B_Val
    Next B_I
    
    Result = (256 - B_Sum) And $FF        ' Two's complement checksum
EndProc

Proc P_EE_ValidBlk(W_BlockAddr As Word), Byte
    Dim B_StoredSum As Byte
    Dim B_CalcSum   As Byte
    Dim B_Magic     As Byte
    Dim B_Version   As Byte
    
    ' Check magic byte
    B_Magic = ERead W_BlockAddr + EE_IN_MAGIC
    If B_Magic <> EE_MAGIC_BYTE Then
        Result = EE_ERR_MAGIC
        ExitProc
    EndIf
    
    ' Check version
    B_Version = ERead W_BlockAddr + EE_IN_VERSION
    If B_Version <> EE_VERSION Then
        Result = EE_ERR_VERSION
        ExitProc
    EndIf
    
    ' Verify checksum (exclude checksum byte itself)
    B_StoredSum = ERead W_BlockAddr + EE_IN_CHECKSUM
    B_CalcSum = P_EE_CalcChkSum(W_BlockAddr, EE_BLOCK_SIZE - 1)
    
    If B_StoredSum <> B_CalcSum Then
        Result = EE_ERR_CHECKSUM
        ExitProc
    EndIf
    
    Result = EE_OK
EndProc

'------------------------ BLOCK WRITE AND COPY OPERATIONS -----------
Proc P_EE_WriteBlk(W_BlockAddr As Word), Byte
    Dim B_Checksum As Byte
    Dim B_Status   As Byte
    
    HRSOut "DEBUG: P_EE_WriteBlk entry - BlockAddr=", Dec W_BlockAddr,13
    
    ' Write magic and version
    HRSOut "DEBUG: Writing magic byte",13
    B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_MAGIC, EE_MAGIC_BYTE)
    If B_Status <> EE_OK Then
        HRSOut "DEBUG: Magic write failed, status=", Dec B_Status,13
        Result = B_Status
        ExitProc
    EndIf
    
    HRSOut "DEBUG: Writing version byte",13
    B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_VERSION, EE_VERSION)
    If B_Status <> EE_OK Then
        HRSOut "DEBUG: Version write failed, status=", Dec B_Status,13
        Result = B_Status
        ExitProc
    EndIf
    
    ' Calculate and write checksum (exclude checksum byte itself)
    HRSOut "DEBUG: Calculating checksum",13
    B_Checksum = P_EE_CalcChkSum(W_BlockAddr, EE_BLOCK_SIZE - 1)
    HRSOut "DEBUG: Calculated checksum=", Hex2 B_Checksum,13
    
    B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_CHECKSUM, B_Checksum)
    If B_Status <> EE_OK Then
        HRSOut "DEBUG: Checksum write failed, status=", Dec B_Status,13
    Else
        HRSOut "DEBUG: Block finalization successful",13
    EndIf
    
    Result = B_Status
EndProc


Proc P_EE_CopyBlk(W_SrcAddr As Word, W_DstAddr As Word), Byte
    Dim B_I      As Byte
    Dim B_Val    As Byte
    Dim B_Status As Byte
    
    HRSOut "DEBUG: P_EE_CopyBlk - Src=", Dec W_SrcAddr, " Dst=", Dec W_DstAddr,13
    
    For B_I = 0 To EE_BLOCK_SIZE - 1
        B_Val = ERead W_SrcAddr + B_I
        B_Status = P_EE_SafeWriteB(W_DstAddr + B_I, B_Val)
        If B_Status <> EE_OK Then
            HRSOut "DEBUG: Copy failed at byte ", Dec B_I, " status=", Dec B_Status,13
            Result = B_Status
            ExitProc
        EndIf
    Next B_I
    
    HRSOut "DEBUG: Block copy completed successfully",13
    Result = EE_OK
EndProc

'--------------------------------XXXXXXXXX-------------------------------



'--------------------------------XXXXXXXXX-------------------------------
Section_4:
'------------------------------------------------------------------------------
'=====================================================================
' SYSTEM HELPERS AND DISPLAY UTILITIES
'=====================================================================

'------------------------ HARDWARE INITIALIZATION -------------------
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

''------------------------ SYSTEM FEEDBACK ----------------------------
'Proc P_Beeps(B_Type As Byte)
'    B_BeepPattern = B_Type
'    B_BeepState = 0
'    B_BeepCount = 0
    
'    Select B_Type
'        Case 0                           ' Silent
'            W_BeepTimer = 0
'        Case 1                           ' Single beep
'            W_BeepTimer = W_UI_PulseMs
'        Case 2                           ' Double beep
'            W_BeepTimer = W_UI_PulseMs
'        Case 3                           ' Long beep
'            W_BeepTimer = W_UI_PulseMs * 2
'        Case 4                           ' Triple beep
'            W_BeepTimer = W_UI_PulseMs
'        Case 5                           ' Quad beep
'            W_BeepTimer = W_UI_PulseMs
'        Case 6                           ' Flexible pattern (use P_BeepsFlex instead)
'            W_BeepTimer = W_BeepOnTime
'    EndSelect
'EndProc

Proc P_BeepsFlex(B_Count As Byte, W_OnMs As Word, W_OffMs As Word)
    B_BeepPattern = 6                ' Use flexible pattern
    B_BeepState = 0                  ' Start with first beep
    B_BeepCount = 0                  ' Reset counter
    B_BeepMax = B_Count              ' Set total beep count
    W_BeepOnTime = W_OnMs            ' Set on duration
    W_BeepOffTime = W_OffMs          ' Set off duration
    W_BeepTimer = W_OnMs             ' Start first beep
EndProc




Proc P_B_Pat(B_Pattern As Byte)
    Select B_Pattern
        Case 0                       ' Startup beep pattern
            ' Signal startup and EEPROM status with non-blocking beeps
            If P_TestFlag(B_EE_Flags, EE_FLAG_FACTORY) <> 0 Then
                ' Factory defaults loaded - 2 quick beeps
                HRSOut "DEBUG: Factory defaults - triggering pattern 2",13
                P_Beeps(2)
            Else
                If P_TestFlag(B_EE_Flags, EE_Flg_Mig) <> 0 Then
                    ' Migration completed - 3 beeps
                    HRSOut "DEBUG: Migration - triggering pattern 4",13
                    P_Beeps(4)
                Else
                    If P_TestFlag(B_EE_Flags, EE_Flg_Restrd) <> 0 Then
                        ' Restored from backup - 4 beeps
                        HRSOut "DEBUG: Restore - triggering pattern 5",13
                        P_Beeps(5)
                    Else
                        ' Normal startup - 5 short beeps with gaps
                        HRSOut "DEBUG: Normal startup - triggering flexible pattern",13
                        P_BeepsFlex(5, 75, 100)  ' 5 beeps, 75ms on, 100ms off
                    EndIf
                EndIf
            EndIf
            
        Case 1                       ' Menu timeout pattern
            HRSOut "DEBUG: Menu timeout pattern",13
            ' Need custom pattern: 100ms beep, 100ms silence, 50ms beep
            ' This requires a new ISR pattern since it's not a simple repeat
            B_BeepPattern = 8        ' New custom pattern
            B_BeepState = 0
            W_BeepTimer = 100        ' Start with 100ms beep
            
        Case 2                       ' Long press pattern
            HRSOut "DEBUG: Long press pattern",13
            P_BeepsFlex(1, 1000, 0)  ' 1 beep, 1000ms long, no gap needed
            
        Case Else                    ' Default to silent
            HRSOut "DEBUG: Unknown pattern - silent",13
            P_Beeps(0)
    EndSelect
    
    HRSOut "DEBUG: B_BeepPattern set to: ", Dec B_BeepPattern,13
    
    ' Wait for beep pattern to complete (non-blocking check)
    While B_BeepPattern <> 0
        DelayMS 10                   ' Small delay while pattern plays
    Wend
    
    DelayMS 500                      ' Final pause after beeps complete
EndProc
'------------------------ BASIC DISPLAY HELPERS ---------------------
Proc DlyMsFast(W_Ms As Word)
    Dim W_I As Word
    For W_I = 1 To W_Ms
        DelayMS 1
    Next
EndProc

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

Proc P_PrintRowRJ(B_Row As Byte, S_Text As String, B_Active As Byte)
    Dim B_Len As Byte
    Dim B_Start As Byte
    P_ClrLine(B_Row)
    B_Len = Len S_Text
    If B_Len > 19 Then
        B_Len = 19
    EndIf
    B_Start = 20 - B_Len
    Print At B_Row, B_Start, S_Text
    If B_Active = 1 Then
        If B_Start > 1 Then Print At B_Row, B_Start - 1, "["
        Print At B_Row, 20, "]"
    EndIf
EndProc

'------------------------ UTILITY FUNCTIONS --------------------------
Proc P_ClampW(ByRef W_Val As Word, W_Min As Word, W_Max As Word)
    If W_Val < W_Min Then
        W_Val = W_Min
    EndIf
    If W_Val > W_Max Then
        W_Val = W_Max
    EndIf
EndProc

'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_5:
'------------------------------------------------------------------------------
'=====================================================================
' VALUE FIELD HELPERS AND DISPLAY FORMATTERS
'=====================================================================
Proc P_CheckVLongPress(), Byte
    'HRSOut "DEBUG: P_CheckVLongPress - B_VLongPress=", Dec B_VLongPress, 13
    If B_VLongPress = 1 Then
        B_VLongPress = 0                    ' Clear flag
        P_B_Pat(1)                         ' Timeout beep pattern
        HRSOut "DEBUG: Very long press detected - exiting menu", 13
        Result = 1                         ' Signal very long press detected
    Else
        Result = 0                         ' No very long press
    EndIf
EndProc

'------------------------ VALUE FIELD DISPLAY HELPERS ---------------
Proc P_ClrValFld(B_Row As Byte, B_Col As Byte)
    Print At B_Row, B_Col, "          "
EndProc

Proc P_PValTxtRJ(B_Row As Byte, B_Col As Byte, S_Value As String, B_Active As Byte, B_Edit As Byte)
    Dim B_Len As Byte, B_Start As Byte
    B_Len = Len S_Value
    If B_Len > 8 Then B_Len = 8
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

'------------------------ SIGNED NUMBER DISPLAY (3 DIGITS + SIGN) ---
Proc P_PValSIntRJ4(B_Row As Byte, B_Col As Byte, I_Val As SWord, B_Active As Byte, B_Edit As Byte)
    Dim B_Start As Byte, W_A As Word
    
    ' Fixed format: [-001] or [001] (3 digits + optional sign)
    B_Start = B_Col + 1 + (8 - 4)
    
    Print At B_Row, B_Col, "          "
    If B_Active = 1 Then
        If B_Edit = 1 Then
            Print At B_Row, B_Start - 1, "("
            Print At B_Row, B_Col + 9, ")"
        Else
            If I_Val < 0 Then
                ' Negative: bracket before sign [-001]
                Print At B_Row, B_Start - 1, "["
                Print At B_Row, B_Col + 9, "]"
            Else
                ' Positive: bracket before first digit [001]
                Print At B_Row, B_Start, "["
                Print At B_Row, B_Col + 9, "]"
            EndIf
        EndIf
    EndIf
    
    ' Format: [-001] or [001] - 3 digits with leading zeros, no + sign for list
    If I_Val < 0 Then
        Print At B_Row, B_Start, "-"
        W_A = 0 - I_Val
        Print At B_Row, B_Start + 1, Dec3 W_A
    Else
        ' For positive numbers, show only 3 digits with leading zeros
        Print At B_Row, B_Start + 1, Dec3 I_Val
    EndIf
EndProc

'------------------------ TIME FORMATTING HELPERS -------------------
Proc P_PrnMMSS(B_Row As Byte, B_Col As Byte, W_Seconds As Word)
    Dim B_Min As Word, B_Sec As Word
    B_Min = W_Seconds / 60
    B_Sec = W_Seconds // 60
    Print At B_Row, B_Col,   Dec2 B_Min,":"
    Print At B_Row, B_Col+3, Dec2 B_Sec
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

'------------------------ INPUT EVENT PROCESSING --------------------
Proc P_ReadEnc()
    Dim W_Pos As Word
    GIE = 0 : W_Pos = W_EncoderPos : GIE = 1

    If W_Pos > W_EncReadPos Then
        B_EncDelta = 1 : L_LastInput = L_Millis
    Else
        If W_Pos < W_EncReadPos Then
            B_EncDelta = -1 : L_LastInput = L_Millis
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
'--------------------------------XXXXXXXXX-------------------------------
Section_6:
'------------------------------------------------------------------------------
'=====================================================================
' CONFIGURATION LOAD/SAVE OPERATIONS WITH DEBUG LOGGING
'=====================================================================

'------------------------ SYSTEM CONFIGURATION ----------------------
Proc P_LoadSysCfg(), Byte
    Dim W_BlockAddr As Word
    Dim B_Status    As Byte
    
    W_BlockAddr = EE_SYSTEM_BLOCK
    B_Status = P_EE_ValidBlk(W_BlockAddr)
    
    If B_Status = EE_OK Then
        ' Load valid configuration
        W_UI_TimeoutS = P_EE_SafeReadW(W_BlockAddr + EE_SYS_TIMEOUT)
        W_UI_PulseMs  = P_EE_SafeReadW(W_BlockAddr + EE_SYS_PULSE)
        B_EE_Flags    = ERead W_BlockAddr + EE_SYS_FLAGS
        
        ' Validate ranges
        P_ClampW(W_UI_TimeoutS, UO_TOut_S_mn, UI_TOut_S_Mx)
        P_ClampW(W_UI_PulseMs, UI_PULSE_MS_MIN, UI_Pls_MS_Mx)
        
    Else
        ' Try restore from backup first
        If P_RSFromBkUp() = EE_OK Then
            P_SetFlag(B_EE_Flags, EE_Flg_Restrd)
            B_Status = EE_OK
        Else
            ' Check for legacy data migration
            If P_MigFromLeg() = EE_OK Then
                P_SetFlag(B_EE_Flags, EE_Flg_Mig)
                B_Status = EE_OK
            Else
                ' Load factory defaults
                W_UI_TimeoutS = 60
                W_UI_PulseMs  = 50
                B_EE_Flags    = 0
                P_SetFlag(B_EE_Flags, EE_FLAG_FACTORY)
                
                ' Save defaults to EEPROM
                P_SaveSysCfg()
                B_Status = EE_OK
            EndIf
        EndIf
    EndIf
    
    B_EE_Status = B_Status
    Result = B_Status
EndProc

Proc P_SaveSysCfg(), Byte
    Dim W_BlockAddr As Word
    Dim B_Status    As Byte
    
    W_BlockAddr = EE_SYSTEM_BLOCK
    
    ' Write configuration data with logging
    P_EE_LogSaveW("W_UI_TimeoutS", W_UI_TimeoutS)
    B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_SYS_TIMEOUT, W_UI_TimeoutS)
    If B_Status <> EE_OK Then
        Result = B_Status
        ExitProc
    EndIf
    
    P_EE_LogSaveW("W_UI_PulseMs", W_UI_PulseMs)
    B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_SYS_PULSE, W_UI_PulseMs)
    If B_Status <> EE_OK Then
        Result = B_Status
        ExitProc
    EndIf
    
    P_EE_LogSaveB("B_EE_Flags", B_EE_Flags)
    B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_SYS_FLAGS, B_EE_Flags)
    If B_Status <> EE_OK Then
        Result = B_Status
        ExitProc
    EndIf
    
    ' Finalize block with checksum
    B_Status = P_EE_WriteBlk(W_BlockAddr)
    
    B_EE_Status = B_Status
    Result = B_Status
EndProc

'------------------------ INPUT CONFIGURATION ------------------------
Proc P_LoadInCfg(B_InputNum As Byte), Byte
    Dim W_BlockAddr As Word
    Dim B_Status    As Byte
    
    ' Calculate block address
    W_BlockAddr = EE_INPUT1_BLOCK + ((B_InputNum - 1) * EE_BLOCK_SIZE)
    
    B_Status = P_EE_ValidBlk(W_BlockAddr)
    
    If B_Status = EE_OK Then
        ' Load configuration based on input number
        Select B_InputNum
            Case 1
                B_I1_Enabled  = ERead W_BlockAddr + EE_IN_ENABLED
                B_I1_SensorT  = ERead W_BlockAddr + EE_IN_SENSOR_T
                B_I1_FlowMode = ERead W_BlockAddr + EE_IN_FLOW_MODE
                W_I1_Scale4   = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE4)
                W_I1_Scale20  = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE20)
                W_I1_BP_High  = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_HIGH)
                W_I1_BP_PLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_PLP)
                W_I1_BP_SLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_SLP)
                B_I1_RlyHigh  = ERead W_BlockAddr + EE_IN_RLY_HIGH
                B_I1_RlyPLP   = ERead W_BlockAddr + EE_IN_RLY_PLP
                B_I1_RlySLP   = ERead W_BlockAddr + EE_IN_RLY_SLP
                B_I1_Display  = ERead W_BlockAddr + EE_IN_DISPLAY
                
            Case 2
                B_I2_Enabled  = ERead W_BlockAddr + EE_IN_ENABLED
                B_I2_SensorT  = ERead W_BlockAddr + EE_IN_SENSOR_T
                B_I2_FlowMode = ERead W_BlockAddr + EE_IN_FLOW_MODE
                W_I2_Scale4   = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE4)
                W_I2_Scale20  = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE20)
                W_I2_BP_High  = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_HIGH)
                W_I2_BP_PLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_PLP)
                W_I2_BP_SLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_SLP)
                B_I2_RlyHigh  = ERead W_BlockAddr + EE_IN_RLY_HIGH
                B_I2_RlyPLP   = ERead W_BlockAddr + EE_IN_RLY_PLP
                B_I2_RlySLP   = ERead W_BlockAddr + EE_IN_RLY_SLP
                B_I2_Display  = ERead W_BlockAddr + EE_IN_DISPLAY
                
            Case 3
                B_I3_Enabled  = ERead W_BlockAddr + EE_IN_ENABLED
                B_I3_SensorT  = ERead W_BlockAddr + EE_IN_SENSOR_T
                B_I3_FlowMode = ERead W_BlockAddr + EE_IN_FLOW_MODE
                W_I3_Scale4   = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE4)
                W_I3_Scale20  = P_EE_SafeReadW(W_BlockAddr + EE_IN_SCALE20)
                W_I3_BP_High  = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_HIGH)
                W_I3_BP_PLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_PLP)
                W_I3_BP_SLP   = P_EE_SafeReadW(W_BlockAddr + EE_IN_BP_SLP)
                B_I3_RlyHigh  = ERead W_BlockAddr + EE_IN_RLY_HIGH
                B_I3_RlyPLP   = ERead W_BlockAddr + EE_IN_RLY_PLP
                B_I3_RlySLP   = ERead W_BlockAddr + EE_IN_RLY_SLP
                B_I3_Display  = ERead W_BlockAddr + EE_IN_DISPLAY
        EndSelect
        
    Else
        ' Try restore from backup first
        If P_RInFromBUp(B_InputNum) = EE_OK Then
            B_Status = EE_OK
        Else
            ' Load factory defaults for this input
            P_LoadInDflt(B_InputNum)
            P_SaveInCfg(B_InputNum)
            B_Status = EE_OK
        EndIf
    EndIf
    
    B_EE_Status = B_Status
    Result = B_Status
EndProc

Proc P_SaveInCfg(B_InputNum As Byte), Byte
    Dim W_BlockAddr As Word
    Dim B_Status    As Byte
    
    W_BlockAddr = EE_INPUT1_BLOCK + ((B_InputNum - 1) * EE_BLOCK_SIZE)

    ' At start of P_SaveInCfg
    HRSOut "P_SaveInCfg: Saving Input ", Dec B_InputNum, 13
    HRSOut "  B_I1_SensorT = ", Dec B_I1_SensorT, 13  ' Or appropriate input


     
    HRSOut "EEPROM SAVE: Input ", Dec B_InputNum, " Configuration",13
    
    ' Write configuration data based on input number with logging
    Select B_InputNum
        Case 1
            P_EE_LogSaveB("B_I1_Enabled", B_I1_Enabled)
            B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_ENABLED, B_I1_Enabled)
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_SensorT", B_I1_SensorT)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_SENSOR_T, B_I1_SensorT)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_FlowMode", B_I1_FlowMode)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_FLOW_MODE, B_I1_FlowMode)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I1_Scale4", P_W2S(W_I1_Scale4))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE4, W_I1_Scale4)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I1_Scale20", P_W2S(W_I1_Scale20))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE20, W_I1_Scale20)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I1_BP_High", W_I1_BP_High)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_HIGH, W_I1_BP_High)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I1_BP_PLP", W_I1_BP_PLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_PLP, W_I1_BP_PLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I1_BP_SLP", W_I1_BP_SLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_SLP, W_I1_BP_SLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_RlyHigh", B_I1_RlyHigh)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_HIGH, B_I1_RlyHigh)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_RlyPLP", B_I1_RlyPLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_PLP, B_I1_RlyPLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_RlySLP", B_I1_RlySLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_SLP, B_I1_RlySLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I1_Display", B_I1_Display)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_DISPLAY, B_I1_Display)
            EndIf
            
        Case 2
            P_EE_LogSaveB("B_I2_Enabled", B_I2_Enabled)
            B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_ENABLED, B_I2_Enabled)
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_SensorT", B_I2_SensorT)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_SENSOR_T, B_I2_SensorT)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_FlowMode", B_I2_FlowMode)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_FLOW_MODE, B_I2_FlowMode)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I2_Scale4", P_W2S(W_I2_Scale4))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE4, W_I2_Scale4)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I2_Scale20", P_W2S(W_I2_Scale20))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE20, W_I2_Scale20)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I2_BP_High", W_I2_BP_High)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_HIGH, W_I2_BP_High)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I2_BP_PLP", W_I2_BP_PLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_PLP, W_I2_BP_PLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I2_BP_SLP", W_I2_BP_SLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_SLP, W_I2_BP_SLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_RlyHigh", B_I2_RlyHigh)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_HIGH, B_I2_RlyHigh)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_RlyPLP", B_I2_RlyPLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_PLP, B_I2_RlyPLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_RlySLP", B_I2_RlySLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_SLP, B_I2_RlySLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I2_Display", B_I2_Display)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_DISPLAY, B_I2_Display)
            EndIf
            
        Case 3
            P_EE_LogSaveB("B_I3_Enabled", B_I3_Enabled)
            B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_ENABLED, B_I3_Enabled)
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_SensorT", B_I3_SensorT)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_SENSOR_T, B_I3_SensorT)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_FlowMode", B_I3_FlowMode)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_FLOW_MODE, B_I3_FlowMode)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I3_Scale4", P_W2S(W_I3_Scale4))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE4, W_I3_Scale4)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSave("W_I3_Scale20", P_W2S(W_I3_Scale20))
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_SCALE20, W_I3_Scale20)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I3_BP_High", W_I3_BP_High)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_HIGH, W_I3_BP_High)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I3_BP_PLP", W_I3_BP_PLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_PLP, W_I3_BP_PLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveW("W_I3_BP_SLP", W_I3_BP_SLP)
                B_Status = P_EE_SafeWriteW(W_BlockAddr + EE_IN_BP_SLP, W_I3_BP_SLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_RlyHigh", B_I3_RlyHigh)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_HIGH, B_I3_RlyHigh)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_RlyPLP", B_I3_RlyPLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_PLP, B_I3_RlyPLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_RlySLP", B_I3_RlySLP)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_RLY_SLP, B_I3_RlySLP)
            EndIf
            If B_Status = EE_OK Then
                P_EE_LogSaveB("B_I3_Display", B_I3_Display)
                B_Status = P_EE_SafeWriteB(W_BlockAddr + EE_IN_DISPLAY, B_I3_Display)
            EndIf
    EndSelect
    
    If B_Status = EE_OK Then
        B_Status = P_EE_WriteBlk(W_BlockAddr)
        If B_Status = EE_OK Then
            HRSOut "EEPROM SAVE: Input ", Dec B_InputNum, " block finalized successfully",13
        Else
            HRSOut "EEPROM ERROR: Input ", Dec B_InputNum, " block finalization failed",13
        EndIf
    Else
        HRSOut "EEPROM ERROR: Input ", Dec B_InputNum, " configuration save failed",13
    EndIf
    
    B_EE_Status = B_Status
    Result = B_Status
EndProc

'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_7:
'------------------------------------------------------------------------------
'=====================================================================
' BACKUP, RESTORE, AND MIGRATION OPERATIONS
'=====================================================================

'------------------------ BACKUP AND RESTORE -------------------------
Proc P_RSFromBkUp(), Byte
    Dim W_BUP_Add As Word
    Dim B_Status     As Byte
    
    W_BUP_Add = EE_BACKUP_START + EE_SYSTEM_BLOCK
    B_Status = P_EE_ValidBlk(W_BUP_Add)
    
    If B_Status = EE_OK Then
        B_Status = P_EE_CopyBlk(W_BUP_Add, EE_SYSTEM_BLOCK)
        If B_Status = EE_OK Then
            ' Reload from restored block
            B_Status = P_LoadSysCfg()
        EndIf
    EndIf
    
    Result = B_Status
EndProc

Proc P_RInFromBUp(B_InputNum As Byte), Byte
    Dim W_BUP_Add As Word
    Dim W_BlockAddr  As Word
    Dim B_Status     As Byte
    
    W_BlockAddr  = EE_INPUT1_BLOCK + ((B_InputNum - 1) * EE_BLOCK_SIZE)
    W_BUP_Add = EE_BACKUP_START + W_BlockAddr
    
    B_Status = P_EE_ValidBlk(W_BUP_Add)
    
    If B_Status = EE_OK Then
        B_Status = P_EE_CopyBlk(W_BUP_Add, W_BlockAddr)
        If B_Status = EE_OK Then
            ' Reload from restored block
            B_Status = P_LoadInCfg(B_InputNum)
        EndIf
    EndIf
    
    Result = B_Status
EndProc

'------------------------ LEGACY MIGRATION ---------------------------
Proc P_MigFromLeg(), Byte
    Dim B_Status As Byte
    
    ' Check if legacy data exists (look for reasonable timeout value)
    W_UI_TimeoutS = P_EE_SafeReadW(LEG_UI_TimeoutS)
    If W_UI_TimeoutS >= UO_TOut_S_mn And W_UI_TimeoutS <= UI_TOut_S_Mx Then
        ' Legacy data appears valid, migrate it
        W_UI_PulseMs = P_EE_SafeReadW(LEG_UI_PulseMs)
        P_ClampW(W_UI_PulseMs, UI_PULSE_MS_MIN, UI_Pls_MS_Mx)
        
        ' Migrate Input 1
        B_I1_Enabled  = ERead LEG_I1_Enabled
        B_I1_SensorT  = ERead LEG_I1_SensorT
        B_I1_FlowMode = ERead LEG_I1_FlowMode
        W_I1_Scale4   = P_EE_SafeReadW(LEG_I1_Scale4)
        W_I1_Scale20  = P_EE_SafeReadW(LEG_I1_Scale20)
        
        ' Set other values to defaults
        W_I1_BP_High = 0
        W_I1_BP_PLP  = 0
        W_I1_BP_SLP  = 0
        B_I1_RlyHigh = MODE_LATCH
        B_I1_RlyPLP  = MODE_LATCH
        B_I1_RlySLP  = MODE_LATCH
        B_I1_Display = YES
        
        ' Set Input 2 & 3 to defaults
        P_LoadInDflt(2)
        P_LoadInDflt(3)
        
        ' Save migrated data
        B_Status = P_SaveSysCfg()
        If B_Status = EE_OK Then
            B_Status = P_SaveInCfg(1)
        EndIf
        If B_Status = EE_OK Then
            B_Status = P_SaveInCfg(2)
        EndIf
        If B_Status = EE_OK Then
            B_Status = P_SaveInCfg(3)
        EndIf
        
        Result = B_Status
    Else
        Result = EE_ERR_MAGIC   ' No valid legacy data found
    EndIf
EndProc

'------------------------ FACTORY DEFAULTS ---------------------------
Proc P_LoadInDflt(B_InputNum As Byte)
    Select B_InputNum
        Case 1  ' Pressure sensor defaults
            B_I1_Enabled  = 1
            B_I1_SensorT  = SENSOR_PRES
            B_I1_FlowMode = 0
            W_I1_Scale4   = 0
            W_I1_Scale20  = 362
            W_I1_BP_High  = 0
            W_I1_BP_PLP   = 0
            W_I1_BP_SLP   = 0
            B_I1_RlyHigh  = MODE_LATCH
            B_I1_RlyPLP   = MODE_LATCH
            B_I1_RlySLP   = MODE_LATCH
            B_I1_Display  = YES
            
        Case 2  ' Temperature sensor defaults
            B_I2_Enabled  = 1
            B_I2_SensorT  = SENSOR_TEMP
            B_I2_FlowMode = 0
            W_I2_Scale4   = 65486       ' -50 in 2's complement
            W_I2_Scale20  = 100
            W_I2_BP_High  = 0
            W_I2_BP_PLP   = 0
            W_I2_BP_SLP   = 0
            B_I2_RlyHigh  = MODE_LATCH
            B_I2_RlyPLP   = MODE_NO
            B_I2_RlySLP   = MODE_NO
            B_I2_Display  = YES
            
        Case 3  ' Flow sensor defaults
            B_I3_Enabled  = 0           ' Disabled by default
            B_I3_SensorT  = SENSOR_FLOW
            B_I3_FlowMode = 0
            W_I3_Scale4   = 0
            W_I3_Scale20  = 100
            W_I3_BP_High  = 0
            W_I3_BP_PLP   = 0
            W_I3_BP_SLP   = 0
            B_I3_RlyHigh  = MODE_NO
            B_I3_RlyPLP   = MODE_LATCH
            B_I3_RlySLP   = MODE_LATCH
            B_I3_Display  = YES
    EndSelect
EndProc

'------------------------ CONVENIENCE FUNCTIONS ----------------------
Proc P_LoadConfig()
    P_LoadSysCfg()
    P_LoadInCfg(1)
    P_LoadInCfg(2)
    P_LoadInCfg(3)
EndProc

'------------------------ INPUT1 SYNC FUNCTIONS ----------------------
Proc P_I1SyncLoad()
    Dim B_Mask As Byte

    B_Mask = B_I1_FlowMode & %00000001
    If B_Mask = 0 Then
        B_I1_FlowType = FLOWTYPE_ANALOG
    Else
        B_I1_FlowType = Flow_Type_Dig
    EndIf

    B_Mask = B_I1_FlowMode & %00000010
    If B_Mask = 0 Then
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
    If B_I1_FlowType  = Flow_Type_Dig Then B_Mode = B_Mode Or %00000001
    If B_I1_FlowUnits = FLOWU_LPS Then B_Mode = B_Mode Or %00000010
    B_I1_FlowMode = B_Mode
    W_I1_BP_SLP = W_I1_BP_Low
    B_I1_RlySLP = B_I1_RlyLow
EndProc

'=====================================================================
' EEPROM DEBUG AND MONITORING FUNCTIONS
'=====================================================================

'------------------------ STARTUP CONFIGURATION DUMP ----------------
Proc P_EE_DumpAll()
    HRSOut "=== EEPROM CONFIG DUMP ===",13
    
    ' System Configuration
    HRSOut "SYSTEM CONFIG:",13
    HRSOut "  W_UI_TimeoutS = ", Dec W_UI_TimeoutS, " seconds",13
    HRSOut "  W_UI_PulseMs  = ", Dec W_UI_PulseMs, " ms",13
    HRSOut "  B_EE_Flags    = $", Hex2 B_EE_Flags,13
    
    ' Input 1 Configuration
    HRSOut "INPUT 1 CONFIG:",13
    HRSOut "  B_I1_Enabled  = ", Dec B_I1_Enabled,13
    HRSOut "  B_I1_SensorT  = ", Dec B_I1_SensorT,13
    HRSOut "  B_I1_FlowMode = ", Dec B_I1_FlowMode,13
    HRSOut "  W_I1_Scale4   = ", SDec W_I1_Scale4,13
    HRSOut "  W_I1_Scale20  = ", SDec W_I1_Scale20,13
    HRSOut "  W_I1_BP_High  = ", Dec W_I1_BP_High, " sec",13
    HRSOut "  W_I1_BP_PLP   = ", Dec W_I1_BP_PLP, " sec",13
    HRSOut "  W_I1_BP_SLP   = ", Dec W_I1_BP_SLP, " sec",13
    HRSOut "  B_I1_RlyHigh  = ", Dec B_I1_RlyHigh,13
    HRSOut "  B_I1_RlyPLP   = ", Dec B_I1_RlyPLP,13
    HRSOut "  B_I1_RlySLP   = ", Dec B_I1_RlySLP,13
    HRSOut "  B_I1_Display  = ", Dec B_I1_Display,13
    
    ' Input 2 Configuration
    HRSOut "INPUT 2 CONFIG:",13
    HRSOut "  B_I2_Enabled  = ", Dec B_I2_Enabled,13
    HRSOut "  B_I2_SensorT  = ", Dec B_I2_SensorT,13
    HRSOut "  B_I2_FlowMode = ", Dec B_I2_FlowMode,13
    HRSOut "  W_I2_Scale4   = ", SDec W_I2_Scale4,13
    HRSOut "  W_I2_Scale20  = ", SDec W_I2_Scale20,13
    HRSOut "  W_I2_BP_High  = ", Dec W_I2_BP_High, " sec",13
    HRSOut "  W_I2_BP_PLP   = ", Dec W_I2_BP_PLP, " sec",13
    HRSOut "  W_I2_BP_SLP   = ", Dec W_I2_BP_SLP, " sec",13
    HRSOut "  B_I2_RlyHigh  = ", Dec B_I2_RlyHigh,13
    HRSOut "  B_I2_RlyPLP   = ", Dec B_I2_RlyPLP,13
    HRSOut "  B_I2_RlySLP   = ", Dec B_I2_RlySLP,13
    HRSOut "  B_I2_Display  = ", Dec B_I2_Display,13
    
    ' Input 3 Configuration
    HRSOut "INPUT 3 CONFIG:",13
    HRSOut "  B_I3_Enabled  = ", Dec B_I3_Enabled,13
    HRSOut "  B_I3_SensorT  = ", Dec B_I3_SensorT,13
    HRSOut "  B_I3_FlowMode = ", Dec B_I3_FlowMode,13
    HRSOut "  W_I3_Scale4   = ", SDec W_I3_Scale4,13
    HRSOut "  W_I3_Scale20  = ", SDec W_I3_Scale20,13
    HRSOut "  W_I3_BP_High  = ", Dec W_I3_BP_High, " sec",13
    HRSOut "  W_I3_BP_PLP   = ", Dec W_I3_BP_PLP, " sec",13
    HRSOut "  W_I3_BP_SLP   = ", Dec W_I3_BP_SLP, " sec",13
    HRSOut "  B_I3_RlyHigh  = ", Dec B_I3_RlyHigh,13
    HRSOut "  B_I3_RlyPLP   = ", Dec B_I3_RlyPLP,13
    HRSOut "  B_I3_RlySLP   = ", Dec B_I3_RlySLP,13
    HRSOut "  B_I3_Display  = ", Dec B_I3_Display,13
    
    HRSOut "=== END CONFIG DUMP ===",13,13
EndProc

'------------------------ SAVE OPERATION LOGGING ---------------------
Proc P_EE_LogSave(S_VarName As String, I_Value As SWord)
    HRSOut "EEPROM SAVE: ", S_VarName, " = ", SDec I_Value,13
EndProc

Proc P_EE_LogSaveB(S_VarName As String, B_Value As Byte)
    HRSOut "EEPROM SAVE: ", S_VarName, " = ", Dec B_Value,13
EndProc

Proc P_EE_LogSaveW(S_VarName As String, W_Value As Word)
    HRSOut "EEPROM SAVE: ", S_VarName, " = ", Dec W_Value,13
EndProc

'--------------------------------XXXXXXXXX-------------------------------
Section_8:
'------------------------------------------------------------------------------
'=====================================================================
' BASIC MODAL EDITORS
'=====================================================================

'------------------------ YES/NO EDITOR ------------------------------
'======================= INLINE YES/NO EDITOR ==============================
'
' Inline Yes/No editor maintaining screen layout consistency
' Changes from [Yes]/[No] to (Yes)/(No) during editing with 2Hz blinking
' Follows the same patterns as P_EditTimeInline for consistent user experience
'
'=============================================================================

Proc P_EditYNInline(B_Current As Byte, B_Row As Byte), Byte
    Dim B_Working     As Byte           ' Working value (0=No, 1=Yes)
    Dim B_Modified    As Byte           ' Track if changes made
    Dim L_LastBlink   As Dword          ' 2Hz blink timing control
    Dim B_BlinkState  As Byte           ' Current blink visibility state
    Dim B_ForceUpdate As Byte           ' Force display update flag
    Dim B_Original    As Byte           ' Store original for restore on cancel
    Dim W_BtnHoldTime As Word           ' Button hold timing from ISR
    Dim B_Col         As Byte           ' Display column position
    Dim B_Start       As Byte           ' Starting position for value display
    
    ' Fixed display positioning to match your layout strategy
    B_Col = 11                          ' Column where value field starts
    B_Start = B_Col + 1 + (8 - 3)       ' Right-justified start for (Yes)/(No)
    
    ' Store original value for potential restore
    B_Original = B_Current
    B_Working = B_Current
    
    ' Initialize editor state
    B_Modified = 0                      ' No changes yet
    L_LastBlink = L_Millis             ' Initialize blink timing
    B_BlinkState = 0                   ' Start with value visible
    B_ForceUpdate = 1                  ' Force initial display
    
    ' Entry beep to indicate edit mode started
    P_Beeps(1)
    
    ' Set up parentheses for edit mode (matches your strategy)
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)                   ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)                   ' ')'
    
    While 1 = 1
        ' Handle 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update display when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Display Yes/No value - blink entire text
            LCD_SetCursor(B_Row, B_Start)
            If B_BlinkState = 1 Then
                LCD_WriteDat(32)       ' Space when blinking
                LCD_WriteDat(32)       ' Space when blinking  
                LCD_WriteDat(32)       ' Space when blinking
            Else
                If B_Working = YES Then
                    LCD_WriteDat(89)   ' 'Y'
                    LCD_WriteDat(101)  ' 'e'
                    LCD_WriteDat(115)  ' 's'
                Else
                    LCD_WriteDat(78)   ' 'N'
                    LCD_WriteDat(111)  ' 'o'
                    LCD_WriteDat(32)   ' ' (pad to 3 chars)
                EndIf
            EndIf
        EndIf
        
        ' Check for long press using ISR timing
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= BTN_LONG_MS And _BTN = 0 Then
            ' Long press detected - cancel and restore original
            P_Beeps(3)                 ' Error/cancel beep sequence
            
            ' Wait for button release
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100                ' Debounce delay
            B_KeyEvent = 0             ' Clear ISR event
            
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        ' Handle encoder input - toggles between Yes/No
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)                 ' Click feedback
            B_ForceUpdate = 1          ' Force display update
            
            ' Toggle value (any encoder movement toggles)
            If B_Working = YES Then
                B_Working = NO
            Else
                B_Working = YES
            EndIf
            
            ' Check if value differs from original
            If B_Working <> B_Original Then
                B_Modified = 1
            Else
                B_Modified = 0
            EndIf
        EndIf
        
        ' Handle button press - commit the value
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1                     ' Short press - commit
                P_Beeps(2)             ' Success confirmation
                Result = B_Working
                ExitProc
        EndSelect
        
        ' Check for timeout
        If (L_Millis - L_LastInput) > (W_UI_TimeoutS * 1000) Then
            P_Beeps(3)                 ' Timeout error beep
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        DelayMS 1                      ' Small delay for system stability
    Wend
EndProc

'------------------------ 3-OPTION ENUM EDITOR -----------------------
Proc P_EditEnum3(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: MODE         ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)
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
            If B_Cur < 2 Then Inc B_Cur
            Set b_ScrDirty
        Else
            If B_EncDelta = -1 Then
                If B_Cur > 0 Then Dec B_Cur
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

'------------------------ TIME EDITOR (MM:SS) ------------------------
Proc P_EditMMSS(ByRef W_Val As Word), Byte
    Dim B_Min As Byte, B_Sec As Byte, B_Field As Byte
    B_Min = W_Val / 60
    B_Sec = W_Val // 60
    B_Field = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: DURATION     ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)
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
                If B_Min < 99 Then Inc B_Min
            Else
                If B_Sec < 59 Then Inc B_Sec
            EndIf
            Set b_ScrDirty
        Else
            If B_EncDelta = -1 Then
                If B_Field = 0 Then
                    If B_Min > 0 Then Dec B_Min
                Else
                    If B_Sec > 0 Then Dec B_Sec
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

'------------------------ SENSOR TYPE EDITOR -------------------------
'======================= INLINE SENSOR TYPE EDITOR ======================
'
' Inline sensor type editor for Pressure/Temp/Flow selection
' Maintains screen layout consistency with bracket/parentheses strategy
' Encoder cycles through sensor options, 2Hz blinking during edit
'
'=======================================================================
Proc P_EditEnableInline(B_Current As Byte, B_Row As Byte), Byte
    Dim B_Working     As Byte           ' Working value (0=Disabled, 1=Enabled)
    Dim B_Modified    As Byte           ' Track if changes made
    Dim L_LastBlink   As Dword          ' 2Hz blink timing control
    Dim B_BlinkState  As Byte           ' Current blink visibility state
    Dim B_ForceUpdate As Byte           ' Force display update flag
    Dim B_Original    As Byte           ' Store original for restore on cancel
    Dim W_BtnHoldTime As Word           ' Button hold timing from ISR
    Dim B_Col         As Byte           ' Display column position
    Dim B_Start       As Byte           ' Starting position for value display
    
    ' Fixed display positioning to match your layout strategy
    B_Col = 11                          ' Column where value field starts
    B_Start = B_Col + 1 + (8 - 8)       ' Right-justified start for (Disabled/Enabled)
    
    ' Store original value for potential restore
    B_Original = B_Current
    B_Working = B_Current
    
    ' Validate input range (0=Disabled, 1=Enabled)
    If B_Working > 1 Then B_Working = 0
    
    ' Initialize editor state
    B_Modified = 0                      ' No changes yet
    L_LastBlink = L_Millis             ' Initialize blink timing
    B_BlinkState = 0                   ' Start with value visible
    B_ForceUpdate = 1                  ' Force initial display
    
    ' Entry beep to indicate edit mode started
    P_Beeps(1)
    
    ' Set up parentheses for edit mode (matches your strategy)
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)                   ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)                   ' ')'
    
    While 1 = 1
        ' Handle 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update display when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Clear the value field first
            P_ClrValFld(B_Row, B_Col)
            
            ' Display enable/disable text - blink entire text
            LCD_SetCursor(B_Row, B_Start)
            If B_BlinkState = 1 Then
                ' Clear 8 characters during blink
                LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32)
                LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32)
            Else
                Select B_Working
                    Case 0             ' "Disabled" (8 chars exactly)
                        LCD_WriteDat(68)  : LCD_WriteDat(105) : LCD_WriteDat(115) : LCD_WriteDat(97)
                        LCD_WriteDat(98)  : LCD_WriteDat(108) : LCD_WriteDat(101) : LCD_WriteDat(100)
                    Case 1             ' " Enabled" (1 space + 7 chars)
                        LCD_WriteDat(32)  : LCD_WriteDat(69)  : LCD_WriteDat(110) : LCD_WriteDat(97)
                        LCD_WriteDat(98)  : LCD_WriteDat(108) : LCD_WriteDat(101) : LCD_WriteDat(100)
                    Case Else          ' "   Error" (3 spaces + 5 chars)
                        LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(69)
                        LCD_WriteDat(114) : LCD_WriteDat(114) : LCD_WriteDat(111) : LCD_WriteDat(114)
                EndSelect
            EndIf
            
            ' Position the non-blinking opening bracket AFTER text display
            If B_Working = 1 Then
                ' "Enabled" (7 chars) - bracket goes at B_Start (before the space)
                LCD_SetCursor(B_Row, B_Start)
                LCD_WriteDat(40)                   ' '('
            Else
                ' "Disabled" (8 chars) - bracket goes at B_Start - 1 (before the 'D')
                LCD_SetCursor(B_Row, B_Start - 1)
                LCD_WriteDat(40)                   ' '('
            EndIf
            
            ' Always show closing bracket (non-blinking)
            LCD_SetCursor(B_Row, B_Col + 9)
            LCD_WriteDat(41)                   ' ')'
        EndIf
        
        ' Check for long press using ISR timing
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= BTN_LONG_MS And _BTN = 0 Then
            ' Long press detected - cancel and restore original
            P_Beeps(3)                 ' Error/cancel beep sequence
            
            ' Wait for button release
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100                ' Debounce delay
            B_KeyEvent = 0             ' Clear ISR event
            
            Result = B_Original        ' Return original value
            GoTo Exit_EnDis          ' Exit the procedure
        EndIf
        
        ' Handle encoder input - toggles between Disabled/Enabled
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)                 ' Click feedback
            B_ForceUpdate = 1          ' Force display update
            
            ' Toggle between 0 (Disabled) and 1 (Enabled)
            If B_Working = 0 Then
                B_Working = 1
            Else
                B_Working = 0
            EndIf
            
            ' Check if value differs from original
            If B_Working <> B_Original Then
                B_Modified = 1
            Else
                B_Modified = 0
            EndIf
        EndIf
        
        ' Handle button press - commit the value
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1                     ' Short press - commit
                P_Beeps(2)             ' Success confirmation
                HRSOut "Enable editor returning: ", Dec B_Working, 13
                Result = B_Working
                GoTo Exit_EnDis  ' Exit the procedure
        EndSelect
        
        ' Check for timeout
        If (L_Millis - L_LastInput) > (W_UI_TimeoutS * 1000) Then
            P_Beeps(3)                 ' Timeout error beep
            Result = B_Original        ' Return original value
            GoTo Exit_EnDis      ' Exit the procedure
        EndIf
        
        DelayMS 1                      ' Small delay for system stability
    Wend
    
Exit_EnDis:
EndProc

Proc P_EditSensorInline(B_Current As Byte, B_Row As Byte), Byte
    Dim B_Working     As Byte           ' Working value (0=Pres, 1=Temp, 2=Flow)
    Dim B_Modified    As Byte           ' Track if changes made
    Dim L_LastBlink   As Dword          ' 2Hz blink timing control
    Dim B_BlinkState  As Byte           ' Current blink visibility state
    Dim B_ForceUpdate As Byte           ' Force display update flag
    Dim B_Original    As Byte           ' Store original for restore on cancel
    Dim W_BtnHoldTime As Word           ' Button hold timing from ISR
    Dim B_Col         As Byte           ' Display column position
    Dim B_Start       As Byte           ' Starting position for value display
    
    ' Fixed display positioning to match your layout strategy
    B_Col = 11                          ' Column where value field starts
    B_Start = B_Col + 1 + (8 - 8)       ' Right-justified start for (Pressure)
    
    ' Store original value for potential restore
    B_Original = B_Current
    B_Working = B_Current
    
    ' Validate input range
    If B_Working > SENSOR_FLOW Then B_Working = SENSOR_PRES
    
    ' Initialize editor state
    B_Modified = 0                      ' No changes yet
    L_LastBlink = L_Millis             ' Initialize blink timing
    B_BlinkState = 0                   ' Start with value visible
    B_ForceUpdate = 1                  ' Force initial display
    
    ' Entry beep to indicate edit mode started
    P_Beeps(1)
    
    ' Set up parentheses for edit mode (matches your strategy)
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)                   ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)                   ' ')'
    
    While 1 = 1
        ' Handle 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update display when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Display sensor type text - blink entire text
            LCD_SetCursor(B_Row, B_Start)
            If B_BlinkState = 1 Then
                ' Clear 8 characters during blink
                LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32)
                LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32)
            Else
                Select B_Working
                    Case SENSOR_PRES   ' "Pressure"
                        LCD_WriteDat(80)  : LCD_WriteDat(114) : LCD_WriteDat(101) : LCD_WriteDat(115)
                        LCD_WriteDat(115) : LCD_WriteDat(117) : LCD_WriteDat(114) : LCD_WriteDat(101)
                    Case SENSOR_TEMP   ' "Temp    " (padded to 8 chars)
                        LCD_WriteDat(84)  : LCD_WriteDat(101) : LCD_WriteDat(109) : LCD_WriteDat(112)
                        LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)
                    Case SENSOR_FLOW   ' "Flow    " (padded to 8 chars)
                        LCD_WriteDat(70)  : LCD_WriteDat(108) : LCD_WriteDat(111) : LCD_WriteDat(119)
                        LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)
                    Case Else          ' "Error   " (padded to 8 chars)
                        LCD_WriteDat(69)  : LCD_WriteDat(114) : LCD_WriteDat(114) : LCD_WriteDat(111)
                        LCD_WriteDat(114) : LCD_WriteDat(32)  : LCD_WriteDat(32)  : LCD_WriteDat(32)
                EndSelect
            EndIf
        EndIf
        
        ' Check for long press using ISR timing
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= BTN_LONG_MS And _BTN = 0 Then
            ' Long press detected - cancel and restore original
            P_Beeps(3)                 ' Error/cancel beep sequence
            
            ' Wait for button release
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100                ' Debounce delay
            B_KeyEvent = 0             ' Clear ISR event
            
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        ' Handle encoder input - cycles through sensor types
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)                 ' Click feedback
            B_ForceUpdate = 1          ' Force display update
            
            If B_EncDelta = 1 Then     ' Forward: Pres -> Temp -> Flow -> Pres
                Inc B_Working
                If B_Working > SENSOR_FLOW Then
                    B_Working = SENSOR_PRES
                EndIf
            Else                       ' Backward: Pres -> Flow -> Temp -> Pres  
                If B_Working = SENSOR_PRES Then
                    B_Working = SENSOR_FLOW
                Else
                    Dec B_Working
                EndIf
            EndIf
            
            ' Check if value differs from original
            If B_Working <> B_Original Then
                B_Modified = 1
            Else
                B_Modified = 0
            EndIf
        EndIf
        
        ' Handle button press - commit the value
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1                     ' Short press - commit
                P_Beeps(2)             ' Success confirmation
' At the end of P_EditSensorInline, before Result = B_Working
HRSOut "Sensor editor returning: ", Dec B_Working, 13


                Result = B_Working
                ExitProc
        EndSelect
        
        ' Check for timeout
        If (L_Millis - L_LastInput) > (W_UI_TimeoutS * 1000) Then
            P_Beeps(3)                 ' Timeout error beep
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        DelayMS 1                      ' Small delay for system stability
    Wend
EndProc

'======================= INLINE TIME EDITOR (MM:SS FORMAT) ==================
'
' Inline time editor maintaining screen layout consistency with your strategy
' Displays time in (MM:SS) format with field-by-field editing and 2Hz blinking
' Maximum time: 60:00 (3600 seconds) for irrigation bypass timing
'
'=============================================================================
'
Proc P_EditTimeInline(W_Seconds As Word, B_Row As Byte), Word
    Dim B_Minutes     As Byte           ' Working minutes value (0-60)
    Dim B_Secs        As Byte           ' Working seconds value (0-59)
    Dim B_Field       As Byte           ' Current field: 0=minutes, 1=seconds
    Dim B_Modified    As Byte           ' Track if any changes made
    Dim L_LastBlink   As Dword          ' 2Hz blink timing control
    Dim B_BlinkState  As Byte           ' Current blink visibility state
    Dim B_ForceUpdate As Byte           ' Force display update flag
    Dim W_Original    As Word           ' Store original for restore on cancel
    Dim W_BtnHoldTime As Word           ' Button hold timing from ISR
    Dim B_Col         As Byte           ' Display column position
    Dim B_Start       As Byte           ' Starting position for time display
    
    ' Fixed display positioning to match your layout strategy
    B_Col = 11                          ' Column where value field starts
    B_Start = B_Col + 1 + (8 - 5)       ' Right-justified start for (MM:SS)
    
    ' Store original value for potential restore
    W_Original = W_Seconds
    
    ' Clamp input to maximum 3600 seconds
    If W_Seconds > 3600 Then W_Seconds = 3600
    
    ' Extract minutes and seconds from input parameter
    B_Minutes = W_Seconds / 60          ' Integer division for minutes
    B_Secs = W_Seconds // 60            ' Modulo for remaining seconds
    
    ' Initialize editor state
    B_Field = 0                         ' Start with minutes field
    B_Modified = 0                      ' No changes yet
    L_LastBlink = L_Millis             ' Initialize blink timing
    B_BlinkState = 0                   ' Start with field visible
    B_ForceUpdate = 1                  ' Force initial display
    
    ' Entry beep to indicate edit mode started
    P_Beeps(1)
    
    ' Set up parentheses for edit mode (matches your strategy)
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)                   ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)                   ' ')'
    
    While 1 = 1
        ' Handle 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update display when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Display minutes (MM) - blink if active field
            LCD_SetCursor(B_Row, B_Start)
            If B_Field = 0 And B_BlinkState = 1 Then
                LCD_WriteDat(32)       ' Space when blinking
                LCD_WriteDat(32)       ' Space when blinking
            Else
                LCD_WriteDat(48 + (B_Minutes / 10))  ' Tens digit
                LCD_WriteDat(48 + (B_Minutes // 10)) ' Units digit
            EndIf
            
            ' Display colon (always visible)
            LCD_SetCursor(B_Row, B_Start + 2)
            LCD_WriteDat(58)           ' ':'
            
            ' Display seconds (SS) - blink if active field
            LCD_SetCursor(B_Row, B_Start + 3)
            If B_Field = 1 And B_BlinkState = 1 Then
                LCD_WriteDat(32)       ' Space when blinking
                LCD_WriteDat(32)       ' Space when blinking
            Else
                LCD_WriteDat(48 + (B_Secs / 10))     ' Tens digit
                LCD_WriteDat(48 + (B_Secs // 10))    ' Units digit
            EndIf
        EndIf
        
        ' Check for long press using ISR timing
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= BTN_LONG_MS And _BTN = 0 Then
            ' Long press detected - cancel and restore original
            P_Beeps(3)                 ' Error/cancel beep sequence
            
            ' Wait for button release
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100                ' Debounce delay
            B_KeyEvent = 0             ' Clear ISR event
            
            Result = W_Original        ' Return original value
            ExitProc
        EndIf
        
        ' Handle encoder input for current field
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)                 ' Click feedback
            B_ForceUpdate = 1          ' Force display update
            
            Select B_Field
                Case 0                 ' Minutes field active (0-60)
                    If B_EncDelta = 1 Then
                        If B_Minutes < 60 Then
                            Inc B_Minutes
                        Else
                            B_Minutes = 0  ' Roll over from 60 to 0
                        EndIf
                    Else
                        If B_Minutes > 0 Then
                            Dec B_Minutes
                        Else
                            B_Minutes = 60  ' Roll under from 0 to 60
                        EndIf
                    EndIf
                    
                    ' If minutes = 60, force seconds to 0 (max is 60:00)
                    If B_Minutes = 60 Then
                        B_Secs = 0
                    EndIf
                    
                Case 1                 ' Seconds field active (0-59)
                    ' Only allow seconds editing if minutes < 60
                    If B_Minutes < 60 Then
                        If B_EncDelta = 1 Then
                            If B_Secs < 59 Then
                                Inc B_Secs
                            Else
                                B_Secs = 0     ' Roll over from 59 to 0
                            EndIf
                        Else
                            If B_Secs > 0 Then
                                Dec B_Secs
                            Else
                                B_Secs = 59    ' Roll under from 0 to 59
                            EndIf
                        EndIf
                    EndIf
            EndSelect
            
            ' Check if values differ from original
            Dim W_Current As Word
            W_Current = (B_Minutes * 60) + B_Secs
            If W_Current <> W_Original Then
                B_Modified = 1
            Else
                B_Modified = 0
            EndIf
        EndIf
        
        ' Handle button press
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1                     ' Short press
                P_Beeps(1)
                B_ForceUpdate = 1
                
                If B_Field = 0 Then    ' Currently on minutes
                    ' Move to seconds field (only if minutes < 60)
                    If B_Minutes < 60 Then
                        B_Field = 1
                        B_BlinkState = 0   ' Reset blink state
                        L_LastBlink = L_Millis
                    Else
                        ' At 60:00, commit immediately
                        P_Beeps(2)         ' Success confirmation
                        Result = 3600      ' Return 60:00 in seconds
                        ExitProc
                    EndIf
                Else                   ' Currently on seconds
                    ' Commit the final value
                    Dim W_Result As Word
                    W_Result = (B_Minutes * 60) + B_Secs
                    
                    ' Final boundary check (max 3600 seconds)
                    If W_Result > 3600 Then
                        W_Result = 3600
                    EndIf
                    
                    P_Beeps(2)         ' Success confirmation
                    Result = W_Result
                    ExitProc
                EndIf
        EndSelect
        
        ' Check for timeout
        If (L_Millis - L_LastInput) > (W_UI_TimeoutS * 1000) Then
            P_Beeps(3)                 ' Timeout error beep
            Result = W_Original        ' Return original value
            ExitProc
        EndIf
        
        DelayMS 1                      ' Small delay for system stability
    Wend
EndProc










'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_9:
'------------------------------------------------------------------------------
'=====================================================================
' ADVANCED SIGNED NUMBER EDITOR
'=====================================================================

'------------------------ STANDALONE 3-DIGIT SIGNED EDITOR (CORRECTED) -----
'------------------------ STANDALONE 3-DIGIT SIGNED EDITOR (CORRECTED) -----
'------------------------ STANDALONE 3-DIGIT SIGNED EDITOR (CORRECTED) -----
Proc P_EditS3Stand(I_Val As SWord, B_Row As Byte), SWord
    Dim W_Temp      As Word
    Dim B_100s      As Byte
    Dim B_10s       As Byte
    Dim B_Units     As Byte
    Dim B_Sign      As Byte         ' 0=positive, 1=negative
    Dim B_Field     As Byte         ' 0=sign, 1=hundreds, 2=tens, 3=units
    Dim W_Composite As Word
    Dim B_Max10s    As Byte
    Dim B_MaxUnits  As Byte
    Dim B_Col       As Byte
    Dim B_Start     As Byte
    Dim I_OrigVal   As SWord        ' Store original value for restore
    Dim W_BtnHoldTime As Word       ' Track button hold time using ISR data
    Dim L_LastBlink As Dword        ' Track blink timing
    Dim B_BlinkState As Byte        ' 0=show char, 1=hide char
    Dim B_ForceUpdate As Byte       ' Force display update flag
    
    B_Col = 11  ' Column where the value field starts
    B_Start = B_Col + 1 + (8 - 4)  ' Right-justified start position for 4 chars
    
    ' Store original value for potential restore
    I_OrigVal = I_Val
    
    ' DEBUG: Show what we're starting with
    HRSOut "DEBUG: P_EditS3Stand entry - I_Val=", SDec I_Val,13
    
    ' Extract current value and initialize all variables
    If I_Val < 0 Then
        B_Sign = 1
        W_Temp = 0 - I_Val          ' Convert to positive for digit extraction
    Else
        B_Sign = 0
        W_Temp = I_Val              ' Already positive
    EndIf
    
    ' Clamp to limits before digit extraction
    If W_Temp > 500 Then W_Temp = 500
    
    ' Extract digits from the actual input value
    B_100s = W_Temp / 100           ' Extract hundreds digit
    W_Temp = W_Temp // 100          ' Get remainder after hundreds
    B_10s = W_Temp / 10             ' Extract tens digit  
    B_Units = W_Temp // 10          ' Extract units digit
    
    ' DEBUG: Show extracted digits
    HRSOut "DEBUG: Extracted - Sign=", Dec B_Sign, " 100s=", Dec B_100s, " 10s=", Dec B_10s, " units=", Dec B_Units,13
    
    ' Initialize editor state
    B_Field = 0                     ' Start at sign field
    L_LastBlink = L_Millis
    B_BlinkState = 0
    B_ForceUpdate = 1
    
    ' Entry beep when starting edit mode
    P_Beeps(1)
    
    ' Initial display - show parentheses to indicate edit mode
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)  ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)  ' ')'
    
    While 1 = 1
        ' Check for 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update the display only when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Always display as: +123 or -123 (sign + 3 digits) in edit mode
            ' Display sign (always show + or -)
            LCD_SetCursor(B_Row, B_Start)
            If B_Field = 0 And B_BlinkState = 1 Then
                LCD_WriteDat(32)  ' Space when blinking
            Else
                If B_Sign = 1 Then
                    LCD_WriteDat(45)  ' '-'
                Else
                    LCD_WriteDat(43)  ' '+'
                EndIf
            EndIf
            
            ' Display hundreds
            LCD_SetCursor(B_Row, B_Start + 1)
            If B_Field = 1 And B_BlinkState = 1 Then
                LCD_WriteDat(32)  ' Space when blinking
            Else
                LCD_WriteDat(48 + B_100s)
            EndIf
            
            ' Display tens
            LCD_SetCursor(B_Row, B_Start + 2)
            If B_Field = 2 And B_BlinkState = 1 Then
                LCD_WriteDat(32)  ' Space when blinking
            Else
                LCD_WriteDat(48 + B_10s)
            EndIf
            
            ' Display units
            LCD_SetCursor(B_Row, B_Start + 3)
            If B_Field = 3 And B_BlinkState = 1 Then
                LCD_WriteDat(32)  ' Space when blinking
            Else
                LCD_WriteDat(48 + B_Units)
            EndIf
        EndIf
        
        ' Check for long press using ISR button hold timer
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= 750 And _BTN = 0 Then
            ' Long press detected while button still held
            P_Beeps(3)
            
            ' DEBUG: Show restored value
            HRSOut "DEBUG: Long press - restoring I_OrigVal=", SDec I_OrigVal,13
            
            ' Wait for button release to prevent re-entry
            While _BTN = 0
                DelayMS 10
            Wend
            
            ' Additional debounce delay
            DelayMS 100
            
            ' Clear any pending button events from ISR
            B_KeyEvent = 0
            
            Result = I_OrigVal
            ExitProc
        EndIf
        
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)  ' Beep on any encoder movement
            B_ForceUpdate = 1  ' Force display update after encoder change
            
            Select B_Field
                Case 0  ' Sign field
                    If B_Sign = 0 Then
                        B_Sign = 1
                    Else
                        B_Sign = 0
                    EndIf
                    
                Case 1  ' Hundreds field
                    If B_EncDelta = 1 Then
                        If B_100s < 5 Then
                            Inc B_100s
                        Else
                            B_100s = 0  ' Roll over from 5 to 0
                        EndIf
                    Else
                        If B_100s > 0 Then
                            Dec B_100s
                        Else
                            B_100s = 5  ' Roll under from 0 to 5
                        EndIf
                    EndIf
                    
                    ' If 100s = 5, clear 10s and units
                    If B_100s = 5 Then
                        B_10s = 0
                        B_Units = 0
                    EndIf
                    
                Case 2  ' Tens field
                    ' Calculate boundaries
                    If B_100s > 4 Then
                        B_Max10s = 0
                    Else
                        W_Composite = (B_100s * 100) + (B_10s * 10)
                        If W_Composite >= 490 Then
                            B_Max10s = (500 - (B_100s * 100)) / 10
                        Else
                            B_Max10s = 9
                        EndIf
                    EndIf
                    
                    If B_EncDelta = 1 Then
                        If B_10s < B_Max10s Then
                            Inc B_10s
                        Else
                            B_10s = 0  ' Roll over to 0
                        EndIf
                    Else
                        If B_10s > 0 Then
                            Dec B_10s
                        Else
                            B_10s = B_Max10s  ' Roll under to max
                        EndIf
                    EndIf
                    
                    ' Clamp units if needed
                    W_Composite = (B_100s * 100) + (B_10s * 10) + B_Units
                    If W_Composite > 500 Then
                        B_Units = 500 - (B_100s * 100) - (B_10s * 10)
                    EndIf
                    
                Case 3  ' Units field
                    ' Calculate max units
                    W_Composite = (B_100s * 100) + (B_10s * 10)
                    If W_Composite >= 500 Then
                        B_MaxUnits = 0
                    Else
                        B_MaxUnits = 500 - W_Composite
                        If B_MaxUnits > 9 Then B_MaxUnits = 9
                    EndIf
                    
                    If B_EncDelta = 1 Then
                        If B_Units < B_MaxUnits Then
                            Inc B_Units
                        Else
                            B_Units = 0  ' Roll over to 0
                        EndIf
                    Else
                        If B_Units > 0 Then
                            Dec B_Units
                        Else
                            B_Units = B_MaxUnits  ' Roll under to max
                        EndIf
                    EndIf
            EndSelect
        EndIf
        
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(1)
                B_ForceUpdate = 1  ' Force display update after button press
                If B_Field < 3 Then
                    Inc B_Field
                    ' Reset blink state when moving to new field
                    B_BlinkState = 0
                    L_LastBlink = L_Millis
                Else
                    ' Commit the value
                    W_Composite = (B_100s * 100) + (B_10s * 10) + B_Units
                    If B_Sign = 1 Then
                        I_Val = 0 - W_Composite
                    Else
                        I_Val = W_Composite
                    EndIf
                    
                    ' Final boundary check
                    If I_Val > 500 Then I_Val = 500
                    If I_Val < -500 Then I_Val = -500
                    
                    ' DEBUG: Show final value
                    HRSOut "DEBUG: Committing I_Val=", SDec I_Val,13
                    
                    P_Beeps(2)
                    Result = I_Val
                    ExitProc
                EndIf
        EndSelect
        
        DelayMS 1
    Wend
EndProc
'----------------------------------------
'======================= INLINE 3-OPTION ENUM EDITOR ===================
'
' Inline enum editor for relay modes: No/Pulse/Latch (0/1/2)
' Maintains screen layout consistency with bracket/parentheses strategy
' Encoder cycles through options, 2Hz blinking during edit
'
'=========================================================================

Proc P_EditEnum3Inline(B_Current As Byte, B_Row As Byte), Byte
    Dim B_Working     As Byte           ' Working value (0=No, 1=Pulse, 2=Latch)
    Dim B_Modified    As Byte           ' Track if changes made
    Dim L_LastBlink   As Dword          ' 2Hz blink timing control
    Dim B_BlinkState  As Byte           ' Current blink visibility state
    Dim B_ForceUpdate As Byte           ' Force display update flag
    Dim B_Original    As Byte           ' Store original for restore on cancel
    Dim W_BtnHoldTime As Word           ' Button hold timing from ISR
    Dim B_Col         As Byte           ' Display column position
    Dim B_Start       As Byte           ' Starting position for value display
    
    ' Fixed display positioning to match your layout strategy
    B_Col = 11                          ' Column where value field starts
    B_Start = B_Col + 1 + (8 - 5)       ' Right-justified start for (Latch)
    
    ' Store original value for potential restore
    B_Original = B_Current
    B_Working = B_Current
    
    ' Validate input range
    If B_Working > MODE_LATCH Then B_Working = MODE_NO
    
    ' Initialize editor state
    B_Modified = 0                      ' No changes yet
    L_LastBlink = L_Millis             ' Initialize blink timing
    B_BlinkState = 0                   ' Start with value visible
    B_ForceUpdate = 1                  ' Force initial display
    
    ' Entry beep to indicate edit mode started
    P_Beeps(1)
    
    ' Set up parentheses for edit mode (matches your strategy)
    P_ClrValFld(B_Row, B_Col)
    LCD_SetCursor(B_Row, B_Start - 1)
    LCD_WriteDat(40)                   ' '('
    LCD_SetCursor(B_Row, B_Col + 9)
    LCD_WriteDat(41)                   ' ')'
    
    While 1 = 1
        ' Handle 2Hz blink timing (250ms intervals)
        If (L_Millis - L_LastBlink) >= 250 Then
            L_LastBlink = L_Millis
            If B_BlinkState = 0 Then
                B_BlinkState = 1
            Else
                B_BlinkState = 0
            EndIf
            B_ForceUpdate = 1
        EndIf
        
        ' Update display when needed
        If B_ForceUpdate = 1 Then
            B_ForceUpdate = 0
            
            ' Display mode text - blink entire text
            LCD_SetCursor(B_Row, B_Start)
            If B_BlinkState = 1 Then
                ' Clear 5 characters during blink
                LCD_WriteDat(32) : LCD_WriteDat(32) : LCD_WriteDat(32)
                LCD_WriteDat(32) : LCD_WriteDat(32)
            Else
                Select B_Working
                    Case MODE_NO       ' "No   " (padded to 5 chars)
                        LCD_WriteDat(78)  : LCD_WriteDat(111) : LCD_WriteDat(32)
                        LCD_WriteDat(32)  : LCD_WriteDat(32)
                    Case MODE_PULSE    ' "Pulse"
                        LCD_WriteDat(80)  : LCD_WriteDat(117) : LCD_WriteDat(108)
                        LCD_WriteDat(115) : LCD_WriteDat(101)
                    Case MODE_LATCH    ' "Latch"
                        LCD_WriteDat(76)  : LCD_WriteDat(97)  : LCD_WriteDat(116)
                        LCD_WriteDat(99)  : LCD_WriteDat(104)
                    Case Else          ' "Error"
                        LCD_WriteDat(69)  : LCD_WriteDat(114) : LCD_WriteDat(114)
                        LCD_WriteDat(111) : LCD_WriteDat(114)
                EndSelect
            EndIf
        EndIf
        
        ' Check for long press using ISR timing
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= BTN_LONG_MS And _BTN = 0 Then
            ' Long press detected - cancel and restore original
            P_Beeps(3)                 ' Error/cancel beep sequence
            
            ' Wait for button release
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100                ' Debounce delay
            B_KeyEvent = 0             ' Clear ISR event
            
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        ' Handle encoder input - cycles through options
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)                 ' Click feedback
            B_ForceUpdate = 1          ' Force display update
            
            If B_EncDelta = 1 Then     ' Forward: No -> Pulse -> Latch -> No
                Inc B_Working
                If B_Working > MODE_LATCH Then
                    B_Working = MODE_NO
                EndIf
            Else                       ' Backward: No -> Latch -> Pulse -> No  
                If B_Working = MODE_NO Then
                    B_Working = MODE_LATCH
                Else
                    Dec B_Working
                EndIf
            EndIf
            
            ' Check if value differs from original
            If B_Working <> B_Original Then
                B_Modified = 1
            Else
                B_Modified = 0
            EndIf
        EndIf
        
        ' Handle button press - commit the value
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1                     ' Short press - commit
                P_Beeps(2)             ' Success confirmation
                Result = B_Working
                ExitProc
        EndSelect
        
        ' Check for timeout
        If (L_Millis - L_LastInput) > (W_UI_TimeoutS * 1000) Then
            P_Beeps(3)                 ' Timeout error beep
            Result = B_Original        ' Return original value
            ExitProc
        EndIf
        
        DelayMS 1                      ' Small delay for system stability
    Wend
EndProc





'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_10:
'------------------------------------------------------------------------------
'=====================================================================
' GENERIC INPUT MENU SYSTEM WITH CORRECTED PARAMETER PASSING
'=====================================================================

'------------------------ GENERIC INPUT MENU -------------------------
Proc V_InputMenu(B_In As Byte), Byte
    ' Work with local copies of global variables based on input number
    Dim B_Enabled  As Byte
    Dim B_SensorT  As Byte  
    Dim W_Scale4   As Word
    Dim W_Scale20  As Word
    Dim W_BP_High  As Word
    Dim W_BP_PLP   As Word
    Dim W_BP_SLP   As Word
    Dim B_RlyHigh  As Byte
    Dim B_RlyPLP   As Byte
    Dim B_RlySLP   As Byte
    Dim B_Display  As Byte
    Dim B_NewValue As Byte
    
    ' DEBUG: Show input number
    HRSOut "DEBUG: V_InputMenu entry - Input ", Dec B_In,13
    
    ' Initialize ALL local variables from globals based on input number
    Select B_In
        Case 1
            B_Enabled = B_I1_Enabled
            B_SensorT = B_I1_SensorT
            W_Scale4 = W_I1_Scale4
            W_Scale20 = W_I1_Scale20
            W_BP_High = W_I1_BP_High
            W_BP_PLP = W_I1_BP_PLP
            W_BP_SLP = W_I1_BP_SLP
            B_RlyHigh = B_I1_RlyHigh
            B_RlyPLP = B_I1_RlyPLP
            B_RlySLP = B_I1_RlySLP
            B_Display = B_I1_Display
        Case 2
            B_Enabled = B_I2_Enabled
            B_SensorT = B_I2_SensorT
            W_Scale4 = W_I2_Scale4
            W_Scale20 = W_I2_Scale20
            W_BP_High = W_I2_BP_High
            W_BP_PLP = W_I2_BP_PLP
            W_BP_SLP = W_I2_BP_SLP
            B_RlyHigh = B_I2_RlyHigh
            B_RlyPLP = B_I2_RlyPLP
            B_RlySLP = B_I2_RlySLP
            B_Display = B_I2_Display
        Case 3
            B_Enabled = B_I3_Enabled
            B_SensorT = B_I3_SensorT
            W_Scale4 = W_I3_Scale4
            W_Scale20 = W_I3_Scale20
            W_BP_High = W_I3_BP_High
            W_BP_PLP = W_I3_BP_PLP
            W_BP_SLP = W_I3_BP_SLP
            B_RlyHigh = B_I3_RlyHigh
            B_RlyPLP = B_I3_RlyPLP
            B_RlySLP = B_I3_RlySLP
            B_Display = B_I3_Display
    EndSelect
    
    ' DEBUG: Show loaded values
    HRSOut "DEBUG: Loaded W_Scale4=", Dec W_Scale4, " W_Scale20=", Dec W_Scale20,13
    
    Dim B_InpSel  As Byte        ' RENAMED to avoid variable collision
    Dim B_Top     As Byte
    Dim B_Cnt     As Byte
    Dim B_Act     As Byte
    Dim B_Row     As Byte
    Dim B_Idx     As Byte
    Dim B_BackIdx As Byte
    Dim B_FieldId As Byte
    Dim B_RowSel  As Byte
    Dim B_Ed      As Byte

    ' Field ID constants
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

    B_InpSel = 0
    B_Top = 0
    Set b_ScrDirty

    While 1 = 1
        ' Calculate item count based on sensor type
        If B_SensorT = SENSOR_PRES Then
            B_Cnt = 12
        Else
            If B_SensorT = SENSOR_TEMP Then
                B_Cnt = 10
            Else
                B_Cnt = 5
            EndIf
        EndIf
        B_BackIdx = B_Cnt - 1

        If b_ScrDirty = 1 Then
            P_Beeps(1)
            Print At 1,1,"                    "
            Print At 1,1,"INPUT ",Dec1 B_In,"            "
            P_ClrLine(2)
            P_ClrLine(3)
            P_ClrLine(4)

            ' Calculate windowing
            If B_InpSel <= 1 Then
                B_Top = 0
            Else
                If B_InpSel >= (B_Cnt - 1) Then
                    If B_Cnt > 2 Then
                        B_Top = B_Cnt - 3
                    Else
                        B_Top = 0
                    EndIf
                Else
                    B_Top = B_InpSel - 1
                EndIf
            EndIf

            ' Display visible menu items
            For B_Row = 2 To 4
                B_Idx = B_Top + (B_Row - 2)
                If B_Idx <= B_BackIdx Then
                    If B_Idx = B_InpSel Then
                        B_Act = 1
                    Else
                        B_Act = 0
                    EndIf

                    ' Map index to field ID based on sensor type
                    If B_SensorT = SENSOR_PRES Then
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
                        If B_SensorT = SENSOR_TEMP Then
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
                            ' FLOW sensor
                            Select B_Idx
                                Case 0
                                    B_FieldId = F_ENABLE
                                Case 1
                                    B_FieldId = F_SENSOR
                                Case 2
                                    B_FieldId = F_DISPLAY
                                Case 3
                                    B_FieldId = F_RLY_SLP
                                Case Else
                                    B_FieldId = F_BACK
                            EndSelect
                        EndIf
                    EndIf

                    ' Render the current row
                    If B_FieldId = F_BACK Then
                        P_PrintRowRJ(B_Row, "Back", B_Act)
                    Else
                        Select B_FieldId
                            Case F_ENABLE
                                Print At B_Row,1,"Enable    "
                                If B_Enabled = 1 Then
                                    P_PValTxtRJ(B_Row, 11, "Enabled",  B_Act, 0)
                                Else
                                    P_PValTxtRJ(B_Row, 11, "Disabled", B_Act, 0)
                                EndIf

                            Case F_SENSOR
                                Print At B_Row,1,"Sensor    "
                                If B_SensorT = SENSOR_PRES Then
                                    P_PValTxtRJ(B_Row, 11, "Pressure", B_Act, 0)
                                Else
                                    If B_SensorT = SENSOR_TEMP Then
                                        P_PValTxtRJ(B_Row, 11, "Temp", B_Act, 0)
                                    Else
                                        P_PValTxtRJ(B_Row, 11, "Flow", B_Act, 0)
                                    EndIf
                                EndIf

                            Case F_SCALE4
                                Print At B_Row,1,"Scale 4 ma"
                                Dim I_S4 As SWord
                                I_S4 = P_W2S(W_Scale4)
                                P_PValSIntRJ4(B_Row, 11, I_S4, B_Act, 0)
                            
                            Case F_SCALE20
                                Print At B_Row,1,"Scale20ma "
                                Dim I_S20 As SWord
                                I_S20 = P_W2S(W_Scale20)
                                P_PValSIntRJ4(B_Row, 11, I_S20, B_Act, 0)

                            Case F_BP_HIGH
                                If B_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"High BP   "
                                Else
                                    Print At B_Row,1,"High TBP  "
                                EndIf
                                P_PValTmeRJ(B_Row, 11, W_BP_High, B_Act)

                            Case F_BP_PLP
                                Print At B_Row,1,"PLPBP     "
                                P_PValTmeRJ(B_Row, 11, W_BP_PLP, B_Act)

                            Case F_BP_SLP
                                If B_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"SLPBP     "
                                Else
                                    Print At B_Row,1,"Low TBP   "
                                EndIf
                                P_PValTmeRJ(B_Row, 11, W_BP_SLP, B_Act)

                            Case F_RLY_HIGH
                                Print At B_Row,1,"Rly High  "
                                Select B_RlyHigh
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_RLY_PLP
                                Print At B_Row,1,"Rly PLP   "
                                Select B_RlyPLP
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_RLY_SLP
                                If B_SensorT = SENSOR_PRES Then
                                    Print At B_Row,1,"Rly SLP   "
                                Else
                                    Print At B_Row,1,"Rly Low   "
                                EndIf
                                Select B_RlySLP
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No", B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_DISPLAY
                                Print At B_Row,1,"Display   "
                                If B_Display = YES Then
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

        ' Handle encoder navigation
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EncDelta = 1 Then
                If B_InpSel < B_BackIdx Then
                    Inc B_InpSel
                EndIf
            Else
                If B_InpSel > 0 Then
                    Dec B_InpSel
                EndIf
            EndIf
            Set b_ScrDirty
        EndIf

        ' Check for very long press exit
        If P_CheckVLongPress() = 1 Then
            Result = 2
            ExitProc
        EndIf

        ' Handle button press
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                B_RowSel = 2 + (B_InpSel - B_Top)

                ' Recompute field ID for current selection
                If B_SensorT = SENSOR_PRES Then
                    Select B_InpSel
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
                    If B_SensorT = SENSOR_TEMP Then
                        Select B_InpSel
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
                        ' FLOW sensor
                        Select B_InpSel
                            Case 0
                                B_FieldId = F_ENABLE
                            Case 1
                                B_FieldId = F_SENSOR
                            Case 2
                                B_FieldId = F_DISPLAY
                            Case 3
                                B_FieldId = F_RLY_SLP
                            Case Else
                                B_FieldId = F_BACK
                        EndSelect
                    EndIf
                EndIf

                ' Execute field action
                Select B_FieldId
                    Case F_BACK
                        P_Beeps(2)
                        Result = 1
                        ExitProc

                    Case F_ENABLE
                        HRSOut "DEBUG: F_ENABLE selected - calling P_EditEnableInline", 13
                        B_NewValue = P_EditEnableInline(B_Enabled, B_RowSel)
                        HRSOut "DEBUG: P_EditEnableInline returned ", Dec B_NewValue, 13
                        If B_NewValue <> B_Enabled Then
                            B_Enabled = B_NewValue
                            HRSOut "DEBUG: Value changed - updating globals", 13
                            Select B_In
                                Case 1
                                    B_I1_Enabled = B_Enabled
                                Case 2
                                    B_I2_Enabled = B_Enabled  
                                Case 3
                                    B_I3_Enabled = B_Enabled
                            EndSelect
                            P_SaveInCfg(B_In)
                        Else
                            HRSOut "DEBUG: No change detected", 13
                        EndIf
                        Set b_ScrDirty

                    Case F_SENSOR
                        HRSOut "Before edit: B_SensorT=", Dec B_SensorT, 13
                        B_NewValue = P_EditSensorInline(B_SensorT, B_RowSel)
                        HRSOut "Editor returned: B_NewValue=", Dec B_NewValue, 13
                        If B_NewValue <> B_SensorT Then
                            B_SensorT = B_NewValue
                            HRSOut "Updated B_SensorT to: ", Dec B_SensorT, 13
                            Select B_In
                                Case 1
                                    B_I1_SensorT = B_SensorT
                                    HRSOut "Updated B_I1_SensorT to: ", Dec B_I1_SensorT, 13
                                Case 2
                                    B_I2_SensorT = B_SensorT
                                Case 3
                                    B_I3_SensorT = B_SensorT
                            EndSelect
                            P_SaveInCfg(B_In)
                            HRSOut "Called P_SaveInCfg(", Dec B_In, ")", 13
                        Else
                            HRSOut "No change detected", 13
                        EndIf
                        Set b_ScrDirty
                        
                    Case F_SCALE4
                        Dim I_Work As SWord
                        Dim I_Original As SWord
                        I_Work = P_W2S(W_Scale4)
                        I_Original = I_Work
                        HRSOut "DEBUG: Before edit - W_Scale4=", Dec W_Scale4, " I_Work=", SDec I_Work,13
                        
                        I_Work = P_EditS3Stand(I_Work, B_RowSel)
                        HRSOut "DEBUG: Edit returned - I_Work=", SDec I_Work,13
                        
                        If I_Work <> I_Original Then
                            W_Scale4 = P_S2W(I_Work)
                            HRSOut "DEBUG: After conversion - W_Scale4=", Dec W_Scale4,13
                            Select B_In
                                Case 1
                                    HRSOut "DEBUG: Saving to W_I1_Scale4 - old=", Dec W_I1_Scale4, " new=", Dec W_Scale4,13
                                    W_I1_Scale4 = W_Scale4
                                Case 2
                                    W_I2_Scale4 = W_Scale4
                                Case 3
                                    W_I3_Scale4 = W_Scale4
                            EndSelect
                            
                            HRSOut "DEBUG: Calling P_SaveInCfg(",Dec B_In,")",13
                            B_Ed = P_SaveInCfg(B_In)
                            HRSOut "DEBUG: P_SaveInCfg returned status=", Dec B_Ed,13
                        Else
                            HRSOut "DEBUG: No change - skipping save",13
                        EndIf
                        Set b_ScrDirty

                    Case F_SCALE20
                        Dim I_Work2 As SWord
                        Dim I_Original2 As SWord
                        I_Work2 = P_W2S(W_Scale20)
                        I_Original2 = I_Work2
                        
                        I_Work2 = P_EditS3Stand(I_Work2, B_RowSel)
                        
                        If I_Work2 <> I_Original2 Then
                            W_Scale20 = P_S2W(I_Work2)
                            Select B_In
                                Case 1
                                    W_I1_Scale20 = W_Scale20
                                Case 2
                                    W_I2_Scale20 = W_Scale20
                                Case 3
                                    W_I3_Scale20 = W_Scale20
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_BP_HIGH
                        Dim W_NewTime As Word
                        W_NewTime = P_EditTimeInline(W_BP_High, B_RowSel)
                        If W_NewTime <> W_BP_High Then 
                            W_BP_High = W_NewTime
                            Select B_In
                                Case 1
                                    W_I1_BP_High = W_BP_High
                                Case 2
                                    W_I2_BP_High = W_BP_High
                                Case 3
                                    W_I3_BP_High = W_BP_High
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_BP_PLP
                        W_NewTime = P_EditTimeInline(W_BP_PLP, B_RowSel)
                        If W_NewTime <> W_BP_PLP Then 
                            W_BP_PLP = W_NewTime
                            Select B_In
                                Case 1
                                    W_I1_BP_PLP = W_BP_PLP
                                Case 2
                                    W_I2_BP_PLP = W_BP_PLP
                                Case 3
                                    W_I3_BP_PLP = W_BP_PLP
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_BP_SLP
                        W_NewTime = P_EditTimeInline(W_BP_SLP, B_RowSel)
                        If W_NewTime <> W_BP_SLP Then 
                            W_BP_SLP = W_NewTime
                            Select B_In
                                Case 1
                                    W_I1_BP_SLP = W_BP_SLP
                                Case 2
                                    W_I2_BP_SLP = W_BP_SLP
                                Case 3
                                    W_I3_BP_SLP = W_BP_SLP
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_HIGH
                        B_NewValue = P_EditEnum3Inline(B_RlyHigh, B_RowSel)
                        If B_NewValue <> B_RlyHigh Then
                            B_RlyHigh = B_NewValue
                            Select B_In
                                Case 1
                                    B_I1_RlyHigh = B_RlyHigh
                                Case 2
                                    B_I2_RlyHigh = B_RlyHigh
                                Case 3
                                    B_I3_RlyHigh = B_RlyHigh
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_PLP
                        B_NewValue = P_EditEnum3Inline(B_RlyPLP, B_RowSel)
                        If B_NewValue <> B_RlyPLP Then
                            B_RlyPLP = B_NewValue
                            Select B_In
                                Case 1
                                    B_I1_RlyPLP = B_RlyPLP
                                Case 2
                                    B_I2_RlyPLP = B_RlyPLP
                                Case 3
                                    B_I3_RlyPLP = B_RlyPLP
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_RLY_SLP
                        B_NewValue = P_EditEnum3Inline(B_RlySLP, B_RowSel)
                        If B_NewValue <> B_RlySLP Then
                            B_RlySLP = B_NewValue
                            Select B_In
                                Case 1
                                    B_I1_RlySLP = B_RlySLP
                                Case 2
                                    B_I2_RlySLP = B_RlySLP
                                Case 3
                                    B_I3_RlySLP = B_RlySLP
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty

                    Case F_DISPLAY
                        B_NewValue = P_EditYNInline(B_Display, B_RowSel)
                        If B_NewValue <> B_Display Then
                            B_Display = B_NewValue
                            Select B_In
                                Case 1
                                    B_I1_Display = B_Display
                                Case 2
                                    B_I2_Display = B_Display
                                Case 3
                                    B_I3_Display = B_Display
                            EndSelect
                            P_SaveInCfg(B_In)
                        EndIf
                        Set b_ScrDirty
                EndSelect
        EndSelect
    Wend
EndProc
'------------------------ INPUT MENU WRAPPERS ------------------------
Proc V_Input1Menu(), Byte
    Result = V_InputMenu(1)
EndProc

Proc V_Input2Menu(), Byte
    Result = V_InputMenu(2)
EndProc

Proc V_Input3Menu(), Byte
    Result = V_InputMenu(3)
EndProc

'--------------------------------XXXXXXXXX-------------------------------
Section_11:
'------------------------------------------------------------------------------
'=====================================================================
' NAVIGATION MENUS AND MAIN PROGRAM
'=====================================================================

'------------------------ CLOCK SETTINGS MENU -----------------------
Proc V_ClockMenu(), Byte
    Dim B_Sel As Byte, B_Count As Byte, B_Active As Byte
    Dim B_EditMode As Byte, B_EditIndex As Byte
    Dim W_EditWordOrig As Word

    B_Count = 3 : B_Sel = 0
    B_EditMode = 0 : B_EditIndex = 255
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("CLOCK               ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)

            If B_Sel = 0 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 2,1,"Timeout   " : P_PValTmeRJ(2, 11, W_UI_TimeoutS, B_Active)

            If B_Sel = 1 Then
                B_Active = 1
            Else
                B_Active = 0
            EndIf
            Print At 3,1,"Pulse     " : P_PValWrdRJ(3, 11, W_UI_PulseMs, B_Active)

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
                            If W_UI_TimeoutS < UI_TOut_S_Mx Then Inc W_UI_TimeoutS
                        Else
                            If W_UI_TimeoutS > UO_TOut_S_mn Then Dec W_UI_TimeoutS
                        EndIf
                        Set b_ScrDirty
                    Case 1
                        If B_EncDelta = 1 Then
                            If W_UI_PulseMs < UI_Pls_MS_Mx Then Inc W_UI_PulseMs
                        Else
                            If W_UI_PulseMs > UI_PULSE_MS_MIN Then Dec W_UI_PulseMs
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
                        P_ClampW(W_UI_TimeoutS, UO_TOut_S_mn, UI_TOut_S_Mx)
                    Else
                        P_ClampW(W_UI_PulseMs, UI_PULSE_MS_MIN, UI_Pls_MS_Mx)
                    EndIf
                    P_SaveSysCfg()
                    B_EditMode  = 0
                    B_EditIndex = 255
                EndIf
        EndSelect
    Wend
EndProc

'------------------------ SETUP MENU ---------------------------------
'======================= SETUP MENU WITH SENSOR DISPLAY =================
'
' Enhanced setup menu showing current sensor types for each input
' Displays sensor information right-justified for quick reference
'
'=========================================================================

Proc V_SetupMenu(), Byte
    Dim B_Sel  As Byte
    Dim B_Top  As Byte
    Dim B_Cnt  As Byte
    Dim B_Act  As Byte
    Dim B_Row  As Byte
    Dim B_Idx  As Byte

    B_Cnt = 5 : B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("SETUP               ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)

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
                    Case 0  ' Input 1
                        Print At B_Row,1,"                    "
                        If B_Act = 1 Then
                            Print At B_Row,1,"[Input 1]   "
                        Else
                            Print At B_Row,2,"Input 1    "
                        EndIf
                        Select B_I1_SensorT
                            Case SENSOR_PRES
                                Print At B_Row,13,"Pressure"    ' "Pressure" (8 chars) ends at column 20
                            Case SENSOR_TEMP
                                Print At B_Row,17,"Temp"        ' "Temp" (4 chars) ends at column 20
                            Case SENSOR_FLOW
                                Print At B_Row,17,"Flow"        ' "Flow" (4 chars) ends at column 20
                        EndSelect                       
                    Case 1  ' Input 2
                        Print At B_Row,1,"                    "
                        If B_Act = 1 Then
                            Print At B_Row,1,"[Input 2]   "
                        Else
                            Print At B_Row,2,"Input 2    "
                        EndIf
                        Select B_I2_SensorT
                            Case SENSOR_PRES
                                Print At B_Row,13,"Pressure"    ' "Pressure" (8 chars) ends at column 20
                            Case SENSOR_TEMP
                                Print At B_Row,17,"Temp"        ' "Temp" (4 chars) ends at column 20
                            Case SENSOR_FLOW
                                Print At B_Row,17,"Flow"        ' "Flow" (4 chars) ends at column 20
                        EndSelect                       
                        
                    Case 2  ' Input 3
                        Print At B_Row,1,"                    "
                        If B_Act = 1 Then
                            Print At B_Row,1,"[Input 3]   "
                        Else
                            Print At B_Row,2,"Input 3    "
                        EndIf
                        Select B_I3_SensorT
                            Case SENSOR_PRES
                                Print At B_Row,13,"Pressure"    ' "Pressure" (8 chars) ends at column 20
                            Case SENSOR_TEMP
                                Print At B_Row,17,"Temp"        ' "Temp" (4 chars) ends at column 20
                            Case SENSOR_FLOW
                                Print At B_Row,17,"Flow"        ' "Flow" (4 chars) ends at column 20
                        EndSelect                       
                        
                    Case 3  ' Clock
                        P_PrintRow(B_Row, "Clock", B_Act)
                        
                    Case 4  ' Back
                        P_PrintRow(B_Row, "Back", B_Act)
                        
                    Case Else
                        P_ClrLine(B_Row)
                EndSelect
            Next B_Row

            b_ScrDirty = 0
        EndIf

        ' Navigation logic remains the same...
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EncDelta = 1 Then
                If B_Sel < (B_Cnt - 1) Then B_Sel = B_Sel + 1
            Else
                If B_Sel > 0 Then B_Sel = B_Sel - 1
            EndIf
            Set b_ScrDirty
        EndIf

        ' Check for very long press exit  <-- ADD HERE
        If P_CheckVLongPress() = 1 Then
            Result = 2
            ExitProc
        EndIf


        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    V_Input1Menu()
                    Set b_ScrDirty
                Else
                    If B_Sel = 1 Then
                        V_Input2Menu()
                        Set b_ScrDirty
                    Else
                        If B_Sel = 2 Then
                            V_Input3Menu()
                            Set b_ScrDirty
                        Else
                            If B_Sel = 3 Then
                                V_ClockMenu()
                                Set b_ScrDirty
                            Else
                                Result = 1
                                ExitProc
                            EndIf
                        EndIf
                    EndIf
                EndIf
        EndSelect
    Wend
EndProc
'------------------------ OPTIONS MENU -------------------------------
Proc V_Options(), Byte
    Dim B_Sel  As Byte
    Dim B_Top  As Byte
    Dim B_Cnt  As Byte
    Dim B_Act  As Byte
    Dim B_Row  As Byte
    Dim B_Idx  As Byte

    B_Cnt = 4 : B_Sel = 0 : B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("OPTIONS             ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)

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
                If B_Sel < B_Cnt - 1 Then B_Sel = B_Sel + 1
            Else
                If B_Sel > 0 Then B_Sel = B_Sel - 1
            EndIf
            Set b_ScrDirty
        EndIf

        ' Check for very long press exit
        If P_CheckVLongPress() = 1 Then
            Result = 2
            ExitProc
        EndIf
    
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                Select B_Sel
                    Case 0  ' Main Menu
                        Result = 1
                        ExitProc
                        
                    Case 1  ' Setup Menu
                        HRSOut "DEBUG: Before V_SetupMenu - B_Sel=", Dec B_Sel, 13
                        Dim B_Res As Byte
                        B_Res = V_SetupMenu()
                        HRSOut "DEBUG: After V_SetupMenu - B_Sel=", Dec B_Sel, 13
                        ' Handle return codes from submenu
                        If B_Res = 2 Then
                            ' Very long press - exit to main
                            Result = 2
                            ExitProc
                        EndIf
                        ' Otherwise continue in options menu
                        Set b_ScrDirty
                        
                    Case 2  ' Utility Menu
                        V_NotImpl("UTILITY")
                        ' After utility stub, refresh display
                        Set b_ScrDirty
                        
                    Case 3  ' Back
                        Result = 1
                        ExitProc
                        
                    Case Else
                        ' Safety fallback
                        Result = 1
                        ExitProc
                EndSelect
        EndSelect
    Wend
EndProc

' === TEMPORARY DEBUG VERSION FOR TESTING ===

Proc V_Options_Debug(), Byte
    Dim B_OptSel  As Byte        ' Renamed to avoid collision
    Dim B_Top     As Byte
    Dim B_Cnt     As Byte
    Dim B_Act     As Byte
    Dim B_Row     As Byte
    Dim B_Idx     As Byte

    B_Cnt = 4 : B_OptSel = 0 : B_Top = 0
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("OPTIONS (DEBUG)     ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)

            If B_OptSel <= 1 Then
                B_Top = 0
            Else
                If B_OptSel >= B_Cnt - 1 Then
                    If B_Cnt > 2 Then
                        B_Top = B_Cnt - 3
                    Else
                        B_Top = 0
                    EndIf
                Else
                    B_Top = B_OptSel - 1
                EndIf
            EndIf

            For B_Row = 2 To 4
                B_Idx = B_Top + B_Row - 2
                If B_Idx = B_OptSel Then
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
                If B_OptSel < B_Cnt - 1 Then B_OptSel = B_OptSel + 1
            Else
                If B_OptSel > 0 Then B_OptSel = B_OptSel - 1
            EndIf
            Set b_ScrDirty
        EndIf

        ' Check for very long press exit
        If P_CheckVLongPress() = 1 Then
            Result = 2
            ExitProc
        EndIf
    
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                HRSOut "DEBUG: Button pressed - B_OptSel=", Dec B_OptSel, 13
                Select B_OptSel
                    Case 0  ' Main Menu
                        HRSOut "DEBUG: Exiting to Main Menu", 13
                        Result = 1
                        ExitProc
                        
                    Case 1  ' Setup Menu
                        HRSOut "DEBUG: Before V_SetupMenu - B_OptSel=", Dec B_OptSel, 13
                        Dim B_Res As Byte
                        B_Res = V_SetupMenu()
                        HRSOut "DEBUG: After V_SetupMenu - B_OptSel=", Dec B_OptSel, 13
                        HRSOut "DEBUG: V_SetupMenu returned=", Dec B_Res, 13
                        ' Handle return codes from submenu
                        If B_Res = 2 Then
                            ' Very long press - exit to main
                            HRSOut "DEBUG: V_SetupMenu signaled exit to main", 13
                            Result = 2
                            ExitProc
                        EndIf
                        ' Otherwise continue in options menu
                        HRSOut "DEBUG: Refreshing Options display", 13
                        Set b_ScrDirty
                        
                    Case 2  ' Utility Menu
                        HRSOut "DEBUG: Calling V_NotImpl", 13
                        V_NotImpl("UTILITY")
                        ' After utility stub, refresh display
                        Set b_ScrDirty
                        
                    Case 3  ' Back
                        HRSOut "DEBUG: Back selected - exiting", 13
                        Result = 1
                        ExitProc
                        
                    Case Else
                        ' Safety fallback
                        HRSOut "DEBUG: Unknown selection - exiting", 13
                        Result = 1
                        ExitProc
                EndSelect
        EndSelect
    Wend
EndProc

'------------------------ MAIN VIEW ----------------------------------
Proc V_Main()
    b_ReInitLCD = 0
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_DrawTitle("IRRISYS MAIN        ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)
            b_ScrDirty = 0
        EndIf




        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                ExitProc
        EndSelect
    Wend
EndProc

'------------------------ UTILITY STUB -------------------------------
Proc V_NotImpl(S_Name As String)
    b_ReInitLCD = 0
    P_DrawTitle(S_Name + " (STUB)      ")
    P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)

    While 1 = 1
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                GoTo EXIT_V_NotImpl
        EndSelect
    Wend
    EXIT_V_NotImpl:
EndProc

'=====================================================================
' MAIN PROGRAM INITIALIZATION AND LOOP
'=====================================================================

Main:
    ' Hardware initialization
    P_PinInit()
    P_InputInit()
    P_LCDHardInit()
    P_LCDSafeInit()
    
' DEBUG: Check initial button state
    HRSOut "DEBUG: Initial button states:", 13
    HRSOut "  _BTN pin = ", Dec _BTN, 13
    HRSOut "  B_ButtonState = ", Dec B_ButtonState, 13
    HRSOut "  W_BtnHoldMS = ", Dec W_BtnHoldMS, 13
    HRSOut "  B_VLongPress = ", Dec B_VLongPress, 13



    ' Load configuration from EEPROM (with migration/backup recovery)
    P_LoadConfig()
    P_EE_DumpAll()  ' Add this line for startup debug dump
    
    ' Sync Input1 bit-packed data
    P_I1SyncLoad()
    
    ' Startup beeps indicate EEPROM status
    P_B_Pat(0)
    
' CLEAR BUTTON STATE AFTER SLOW STARTUP OPERATIONS
    HRSOut "DEBUG: Clearing button state after startup", 13
    GIE = 0  ' Disable interrupts briefly
    W_BtnHoldMS = 0
    B_VLongPress = 0
    B_KeyEvent = 0
    GIE = 1  ' Re-enable interrupts



    ' Clear navigation and UI state

B_NavCode = V_Options()

    'B_NavCode = 0
    b_Escape = 0
    B_KeyEvent = 0
    L_LastInput = 0
    b_ScrDirty = 1
    b_ReInitLCD = 0
    
    ' Validate sensor types
    If B_I1_SensorT > 2 Then
        B_I1_SensorT = SENSOR_PRES
    EndIf
    If B_I2_SensorT > 2 Then
        B_I2_SensorT = SENSOR_TEMP
    EndIf  
    If B_I3_SensorT > 2 Then
        B_I3_SensorT = SENSOR_FLOW
    EndIf
    
    ' Debug output
    HRSOut "IRRISYS Startup Complete",13
    DelayMS 200
    
    ' Main application loop
    While 1 = 1
        ' Reset LCD if needed
        If b_ReInitLCD = 1 Then
            P_LCDHardInit()
            P_LCDSafeInit()
            b_ReInitLCD = 0
        EndIf
        
        ' Clear escape condition
        b_Escape = 0
        
        ' Main operational view
        V_Main()
        
        ' Enter options menu
        B_NavCode = V_Options()
        
        ' Handle navigation codes
        If B_NavCode = 2 Then
            ' Force return to main - clear any pending states
            b_Escape = 0
            L_LastInput = L_Millis
        EndIf
    Wend
'=====================================================================
' END OF PROGRAM
'=====================================================================

