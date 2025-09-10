'New Chat - 1532 100925
'--------------------------------XXXXXXXXX-------------------------------
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
Dim W_Beep       As Word

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

'------------------------ SYSTEM FEEDBACK ----------------------------
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
    
    ' Signal startup and EEPROM status
    If P_TestFlag(B_EE_Flags, EE_FLAG_FACTORY) <> 0 Then
        ' Factory defaults loaded - 2 quick beeps
        For B_Beepcount = 0 To 1
            P_Beeps(1)
            DelayMS 100
        Next B_Beepcount
    Else
        If P_TestFlag(B_EE_Flags, EE_Flg_Mig) <> 0 Then
            ' Migration completed - 3 beeps
            For B_Beepcount = 0 To 2
                P_Beeps(2)
                DelayMS 150
            Next B_Beepcount
        Else
            If P_TestFlag(B_EE_Flags, EE_Flg_Restrd) <> 0 Then
                ' Restored from backup - 4 beeps
                For B_Beepcount = 0 To 3
                    P_Beeps(1)
                    DelayMS 100
                Next B_Beepcount
            Else
                ' Normal startup - 1 long beep
                P_Beeps(3)
            EndIf
        EndIf
    EndIf
    
    DelayMS 500
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
'------------------------ INLINE YES/NO TOGGLE EDITOR ----------------
Proc P_EditYN(ByRef B_Val As Byte), Byte
    Dim B_Current       As Byte        ' Current toggle state (0/1)
    Dim B_Original      As Byte        ' Original value for restore
    Dim W_BtnHoldTime   As Word        ' Track button hold time using ISR data
    Dim L_LastBlink     As Dword       ' Track blink timing
    Dim B_BlinkState    As Byte        ' 0=show text, 1=hide text
    Dim B_ForceUpdate   As Byte        ' Force display update flag
    Dim B_Col           As Byte
    Dim B_Start         As Byte
    Dim B_Row           As Byte
    
    ' For inline editing, we need to find which row is currently active
    ' Since we don't have direct access to the menu state, use a reasonable default
    B_Row = 3  ' Middle row is most likely for the Enable field
    
    B_Col = 11  ' Column where the value field starts
    B_Start = B_Col + 1  ' Start position for text
    
    ' Initialize editor state
    B_Current = B_Val
    B_Original = B_Val
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
            
            ' Display the toggle state with blinking
            LCD_SetCursor(B_Row, B_Start)
            If B_BlinkState = 1 Then
                ' Blink by showing spaces (8 characters total)
                Print At B_Row, B_Start, "        "
            Else
                ' Show current state
                If B_Current = 1 Then
                    Print At B_Row, B_Start, "Enabled "
                Else
                    Print At B_Row, B_Start, "Disabled"
                EndIf
            EndIf
        EndIf
        
        ' Check for long press using ISR button hold timer
        GIE = 0
        W_BtnHoldTime = W_BtnHoldMS
        GIE = 1
        
        If W_BtnHoldTime >= 750 And _BTN = 0 Then
            ' Long press detected while button still held
            P_Beeps(3)
            B_Val = B_Original  ' Restore original using ByRef
            
            ' Wait for button release to prevent re-entry
            While _BTN = 0
                DelayMS 10
            Wend
            
            DelayMS 100
            B_KeyEvent = 0
            
            Result = 0  ' Indicate no change
            ExitProc
        EndIf
        
        ' Handle encoder input - toggle between states
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            P_Beeps(1)
            B_ForceUpdate = 1
            
            ' Toggle the state
            If B_Current = 0 Then
                B_Current = 1
            Else
                B_Current = 0
            EndIf
        EndIf
        
        ' Handle button press - commit the change
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                B_Val = B_Current  ' Update using ByRef
                
                Result = 1  ' Indicate change made
                ExitProc
        EndSelect
        
        DelayMS 1
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
Proc P_EditSnsType(ByRef B_Val As Byte), Byte
    Dim B_Cur As Byte
    B_Cur = B_Val
    Set b_ScrDirty

    While 1 = 1
        If b_ScrDirty = 1 Then
            P_Beeps(1)
            P_DrawTitle("EDIT: SENSOR TYPE  ")
            P_ClrLine(2) : P_ClrLine(3) : P_ClrLine(4)
            Select B_Cur
                Case SENSOR_PRES
                    Print At 3,1,"[Pressure] Temp Flow "
                Case SENSOR_TEMP
                    Print At 3,1," Pressure [Temp] Flow "
                Case SENSOR_FLOW
                    Print At 3,1," Pressure  Temp [Flow]"
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
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
'--------------------------------XXXXXXXXX-------------------------------
Section_10:
'------------------------------------------------------------------------------
'=====================================================================
' GENERIC INPUT MENU SYSTEM WITH PROPER INLINE EDITING
'=====================================================================

'------------------------ GENERIC INPUT MENU -------------------------
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
    
    ' Inline editing state variables
    Dim B_EditMode    As Byte        ' 0=navigate, 1=editing
    Dim B_EditField   As Byte        ' Which field is being edited
    Dim B_EditValue   As Byte        ' Current edit value for Enable/Display/Sensor
    Dim L_LastBlink   As Dword       ' Track blink timing
    Dim B_BlinkState  As Byte        ' 0=show text, 1=hide text
    Dim B_ForceUpdate As Byte        ' Force display update flag
    
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
    
    Dim B_Sel     As Byte
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

    B_Sel = 0
    B_Top = 0
    B_EditMode = 0
    B_EditField = 255
    L_LastBlink = L_Millis
    B_BlinkState = 0
    Set b_ScrDirty

    While 1 = 1
        ' Handle 2Hz blinking in edit mode
        If B_EditMode = 1 Then
            If (L_Millis - L_LastBlink) >= 250 Then
                L_LastBlink = L_Millis
                If B_BlinkState = 0 Then
                    B_BlinkState = 1
                Else
                    B_BlinkState = 0
                EndIf
                B_ForceUpdate = 1
            EndIf
        EndIf
        
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

        If b_ScrDirty = 1 Or B_ForceUpdate = 1 Then
            If b_ScrDirty = 1 Then
                P_Beeps(1)
                Print At 1,1,"                    "
                Print At 1,1,"INPUT ",Dec1 B_In,"            "
                P_ClrLine(2)
                P_ClrLine(3)
                P_ClrLine(4)
            EndIf
            
            B_ForceUpdate = 0

            ' Calculate windowing
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

            ' Display visible menu items
            For B_Row = 2 To 4
                B_Idx = B_Top + (B_Row - 2)
                If B_Idx <= B_BackIdx Then
                    If B_Idx = B_Sel Then
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
                                
                                ' Handle inline editing display for Enable
                                If B_EditMode = 1 And B_EditField = F_ENABLE And B_Act = 1 Then
                                    ' In edit mode - fixed parentheses, only text blinks
                                    P_ClrValFld(B_Row, 11)
                                    
                                    If B_EditValue = 1 Then
                                        ' "Enabled" - ( in column 12, ) in column 20
                                        Print At B_Row, 12, "("
                                        Print At B_Row, 20, ")"
                                        If B_BlinkState = 0 Then
                                            Print At B_Row, 13, "Enabled"
                                        EndIf
                                    Else
                                        ' "Disabled" - ( in column 11, ) in column 20  
                                        Print At B_Row, 11, "("
                                        Print At B_Row, 20, ")"
                                        If B_BlinkState = 0 Then
                                            Print At B_Row, 12, "Disabled"
                                        EndIf
                                    EndIf
                                Else
                                    ' Normal display mode
                                    If B_Enabled = 1 Then
                                        P_PValTxtRJ(B_Row, 11, "Enabled", B_Act, 0)
                                    Else
                                        P_PValTxtRJ(B_Row, 11, "Disabled", B_Act, 0)
                                    EndIf
                                EndIf
                                
                            Case F_SENSOR
                                Print At B_Row,1,"Sensor    "
                                
                                ' Handle inline editing display for Sensor Type
                                If B_EditMode = 1 And B_EditField = F_SENSOR And B_Act = 1 Then
                                    ' In edit mode - fixed parentheses, only text blinks
                                    P_ClrValFld(B_Row, 11)
                                    
                                    Select B_EditValue
                                        Case SENSOR_PRES
                                            ' "Pressure" - ( in column 11, ) in column 20
                                            Print At B_Row, 11, "("
                                            Print At B_Row, 20, ")"
                                            If B_BlinkState = 0 Then
                                                Print At B_Row, 12, "Pressure"
                                            EndIf
                                        Case SENSOR_TEMP
                                            ' "Temp" - ( in column 15, ) in column 20
                                            Print At B_Row, 15, "("
                                            Print At B_Row, 20, ")"
                                            If B_BlinkState = 0 Then
                                                Print At B_Row, 16, "Temp"
                                            EndIf
                                        Case SENSOR_FLOW
                                            ' "Flow" - ( in column 15, ) in column 20
                                            Print At B_Row, 15, "("
                                            Print At B_Row, 20, ")"
                                            If B_BlinkState = 0 Then
                                                Print At B_Row, 16, "Flow"
                                            EndIf
                                    EndSelect
                                Else
                                    ' Normal display mode
                                    If B_SensorT = SENSOR_PRES Then
                                        P_PValTxtRJ(B_Row, 11, "Pressure", B_Act, 0)
                                    Else
                                        If B_SensorT = SENSOR_TEMP Then
                                            P_PValTxtRJ(B_Row, 11, "Temp", B_Act, 0)
                                        Else
                                            P_PValTxtRJ(B_Row, 11, "Flow", B_Act, 0)
                                        EndIf
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
                                        P_PValTxtRJ(B_Row, 11, "No",    B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_RLY_PLP
                                Print At B_Row,1,"Rly PLP   "
                                Select B_RlyPLP
                                    Case MODE_NO
                                        P_PValTxtRJ(B_Row, 11, "No",    B_Act, 0)
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
                                        P_PValTxtRJ(B_Row, 11, "No",    B_Act, 0)
                                    Case MODE_PULSE
                                        P_PValTxtRJ(B_Row, 11, "Pulse", B_Act, 0)
                                    Case MODE_LATCH
                                        P_PValTxtRJ(B_Row, 11, "Latch", B_Act, 0)
                                EndSelect

                            Case F_DISPLAY
                                Print At B_Row,1,"Display   "
                                
                                ' Handle inline editing display for Display (same pattern as Enable)
                                If B_EditMode = 1 And B_EditField = F_DISPLAY And B_Act = 1 Then
                                    ' In edit mode - show parentheses and handle blinking
                                    P_ClrValFld(B_Row, 11)
                                    
                                    If B_EditValue = YES Then
                                        ' "Yes" - ( in column 16, ) in column 20
                                        Print At B_Row, 16, "("
                                        Print At B_Row, 20, ")"
                                        If B_BlinkState = 0 Then
                                            Print At B_Row, 17, "Yes"
                                        EndIf
                                    Else
                                        ' "No" - ( in column 17, ) in column 20
                                        Print At B_Row, 17, "("
                                        Print At B_Row, 20, ")"
                                        If B_BlinkState = 0 Then
                                            Print At B_Row, 18, "No"
                                        EndIf
                                    EndIf
                                Else
                                    ' Normal display mode
                                    If B_Display = YES Then
                                        P_PValTxtRJ(B_Row, 11, "Yes", B_Act, 0)
                                    Else
                                        P_PValTxtRJ(B_Row, 11, "No",  B_Act, 0)
                                    EndIf
                                EndIf
                        EndSelect
                    EndIf
                Else
                    If b_ScrDirty = 1 Then
                        P_ClrLine(B_Row)
                    EndIf
                EndIf
            Next B_Row

            b_ScrDirty = 0
        EndIf

        ' Handle encoder input
        P_ReadEnc()
        If B_EncDelta <> 0 Then
            If B_EditMode = 0 Then
                ' Navigation mode
                If B_EncDelta = 1 Then
                    If B_Sel < B_BackIdx Then
                        Inc B_Sel
                    EndIf
                Else
                    If B_Sel > 0 Then
                        Dec B_Sel
                    EndIf
                EndIf
                Set b_ScrDirty
            Else
                ' Edit mode - handle field-specific editing
                P_Beeps(1)
                Select B_EditField
                    Case F_ENABLE
                        If B_EditValue = 0 Then
                            B_EditValue = 1
                        Else
                            B_EditValue = 0
                        EndIf
                        B_ForceUpdate = 1
                        
                    Case F_SENSOR
                        ' Cycle through sensor types: PRESSURE -> TEMP -> FLOW -> PRESSURE
                        If B_EncDelta = 1 Then
                            If B_EditValue < SENSOR_FLOW Then
                                Inc B_EditValue
                            Else
                                B_EditValue = SENSOR_PRES  ' Roll over to first
                            EndIf
                        Else
                            If B_EditValue > SENSOR_PRES Then
                                Dec B_EditValue
                            Else
                                B_EditValue = SENSOR_FLOW  ' Roll under to last
                            EndIf
                        EndIf
                        B_ForceUpdate = 1
                        
                    Case F_DISPLAY
                        If B_EditValue = YES Then
                            B_EditValue = NO
                        Else
                            B_EditValue = YES
                        EndIf
                        B_ForceUpdate = 1
                EndSelect
            EndIf
        EndIf

        ' Handle button press
        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                If B_EditMode = 0 Then
                    ' Navigation mode - determine field action
                    B_RowSel = 2 + (B_Sel - B_Top)

                    ' Recompute field ID for current selection
                    If B_SensorT = SENSOR_PRES Then
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
                        If B_SensorT = SENSOR_TEMP Then
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
                            ' FLOW sensor
                            Select B_Sel
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
                            ' Enter edit mode for Enable
                            P_Beeps(2)
                            B_EditMode = 1
                            B_EditField = F_ENABLE
                            B_EditValue = B_Enabled
                            L_LastBlink = L_Millis
                            B_BlinkState = 0
                            B_ForceUpdate = 1

                        Case F_SENSOR
                            ' Enter edit mode for Sensor Type
                            P_Beeps(2)
                            B_EditMode = 1
                            B_EditField = F_SENSOR
                            B_EditValue = B_SensorT
                            L_LastBlink = L_Millis
                            B_BlinkState = 0
                            B_ForceUpdate = 1

                        Case F_DISPLAY
                            ' Enter edit mode for Display
                            P_Beeps(2)
                            B_EditMode = 1
                            B_EditField = F_DISPLAY
                            B_EditValue = B_Display
                            L_LastBlink = L_Millis
                            B_BlinkState = 0
                            B_ForceUpdate = 1

                        Case F_SCALE4
                            Dim I_Work As SWord
                            Dim I_Original As SWord
                            I_Work = P_W2S(W_Scale4)
                            I_Original = I_Work
                            
                            I_Work = P_EditS3Stand(I_Work, B_RowSel)
                            
                            If I_Work <> I_Original Then
                                W_Scale4 = P_S2W(I_Work)
                                Select B_In
                                    Case 1
                                        W_I1_Scale4 = W_Scale4
                                    Case 2
                                        W_I2_Scale4 = W_Scale4
                                    Case 3
                                        W_I3_Scale4 = W_Scale4
                                EndSelect
                                P_SaveInCfg(B_In)
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
                            B_Ed = P_EditMMSS(W_BP_High)
                            If B_Ed = 1 Then 
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
                            B_Ed = P_EditMMSS(W_BP_PLP)
                            If B_Ed = 1 Then 
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
                            B_Ed = P_EditMMSS(W_BP_SLP)
                            If B_Ed = 1 Then 
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
                            B_Ed = P_EditEnum3(B_RlyHigh)
                            If B_Ed = 1 Then 
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
                            B_Ed = P_EditEnum3(B_RlyPLP)
                            If B_Ed = 1 Then 
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
                            B_Ed = P_EditEnum3(B_RlySLP)
                            If B_Ed = 1 Then 
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
                    EndSelect
                Else
                    ' Edit mode - commit changes
                    P_Beeps(2)
                    
                    Select B_EditField
                        Case F_ENABLE
                            B_Enabled = B_EditValue
                            Select B_In
                                Case 1
                                    B_I1_Enabled = B_Enabled
                                Case 2
                                    B_I2_Enabled = B_Enabled  
                                Case 3
                                    B_I3_Enabled = B_Enabled
                            EndSelect
                            P_SaveInCfg(B_In)
                            
                        Case F_SENSOR
                            B_SensorT = B_EditValue
                            Select B_In
                                Case 1
                                    B_I1_SensorT = B_SensorT
                                Case 2
                                    B_I2_SensorT = B_SensorT
                                Case 3
                                    B_I3_SensorT = B_SensorT
                            EndSelect
                            P_SaveInCfg(B_In)
                            
                        Case F_DISPLAY
                            B_Display = B_EditValue
                            Select B_In
                                Case 1
                                    B_I1_Display = B_Display
                                Case 2
                                    B_I2_Display = B_Display
                                Case 3
                                    B_I3_Display = B_Display
                            EndSelect
                            P_SaveInCfg(B_In)
                    EndSelect
                    
                    ' Exit edit mode
                    B_EditMode = 0
                    B_EditField = 255
                    Set b_ScrDirty
                EndIf
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
                If B_Sel < (B_Cnt - 1) Then B_Sel = B_Sel + 1
            Else
                If B_Sel > 0 Then B_Sel = B_Sel - 1
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
                        Call V_Input2Menu()
                    Else
                        If B_Sel = 2 Then
                            Call V_Input3Menu()
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

        P_ReadBtn()
        Select P_GetKeyEvt()
            Case 1
                P_Beeps(2)
                If B_Sel = 0 Then
                    Result = 1
                    ExitProc
                Else
                    If B_Sel = 1 Then
                        Dim B_Res As Byte
                        B_Res = V_SetupMenu()
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
    
    ' Load configuration from EEPROM (with migration/backup recovery)
    P_LoadConfig()
    P_EE_DumpAll()  ' Add this line for startup debug dump


    
    ' Sync Input1 bit-packed data
    P_I1SyncLoad()
    
    ' Startup beeps indicate EEPROM status
    P_Startup()
    
    ' Clear navigation and UI state
    B_NavCode = 0
    b_Escape = 0
    B_KeyEvent = 0
    L_LastInput = 0
    b_ScrDirty = 1
    b_ReInitLCD = 0
    
    W_Beep = 0
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

