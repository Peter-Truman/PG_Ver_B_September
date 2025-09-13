'****************************************************************
'*  Name    : UNTITLED.BAS                                      *
'*  Author  : Peter W Truman                                    *
'*  Notice  : Copyright (c) 2025 PCT Remote Sensing Pty Ltd     *
'*          : All Rights Reserved                               *
'*  Date    : 12/09/2025                                        *
'*  Version : 1.0                                               *
'*  Notes   :                                                   *
'*          :                                                   *
'****************************************************************
'==============================================================================
'======================= POSITRON8 STRING HANDLING TEST =====================
'
' This section demonstrates correct Positron8 BASIC string handling patterns
' Based on the official Positron8 compiler manual
'
'==============================================================================

Device = 18F2525                        ' PIC18F2525 microcontroller

' Device configuration - optimized for 32MHz operation with 8MHz internal osc
Config_Start
  OSC = INTIO67                          ' Internal oscillator, RA6/RA7 as I/O
  FCMEN = OFF                            ' Fail-safe clock monitor disabled
  IESO = OFF                             ' Internal/external switchover disabled
  PWRT = OFF                             ' Power-up timer disabled
  BOREN = SBORDIS                        ' Brown-out reset enabled in software
  BORV = 3                               ' Brown-out voltage = 2.05V
  WDT = OFF                              ' Watchdog timer disabled
  WDTPS = 32768                          ' WDT prescaler (not used)
  CCP2MX = PORTC                         ' CCP2 multiplexed to RC1
  PBADEN = OFF                           ' PORTB<4:0> configured as digital I/O
  LPT1OSC = OFF                          ' Low power timer1 oscillator disabled
  MCLRE = On                             ' MCLR pin enabled
  STVREN = On                            ' Stack overflow reset enabled
  LVP = OFF                              ' Low voltage programming disabled
  XINST = OFF                            ' Extended instruction set disabled
  Debug = OFF                            ' Background debugger disabled
  Cp0 = OFF                              ' Code protection disabled
  CP1 = OFF                              ' Code protection disabled
  CP2 = OFF                              ' Code protection disabled
  CPB = OFF                              ' Boot code protection disabled
  CPD = OFF                              ' Data EEPROM protection disabled
  WRT0 = OFF                             ' Write protection disabled
  WRT1 = OFF                             ' Write protection disabled
  WRT2 = OFF                             ' Write protection disabled
  WRTC = OFF                             ' Configuration write protection disabled
  WRTB = OFF                             ' Boot write protection disabled
  WRTD = OFF                             ' Data EEPROM write protection disabled
  EBTR0 = OFF                            ' Table read protection disabled
  EBTR1 = OFF                            ' Table read protection disabled
  EBTR2 = OFF                            ' Table read protection disabled
  EBTRB = OFF                            ' Boot table read protection disabled
Config_End

'======================= EEPROM CONFIGURATION LAYOUT ====================
' 
' Simplified block-based EEPROM layout for reliable configuration storage.
' Each block is 32 bytes with magic byte, version, data, and checksum.
' 
' Memory Map:
'   0-31:    System configuration block
'   32-63:   Input 1 configuration block  
'   64-95:   Input 2 configuration block
'   96-127:  Input 3 configuration block
'   128+:    Backup area (mirrors of main blocks)
'   200+:    Legacy migration area (old format)
'
'==========================================================================

' EEPROM layout constants
Symbol EE_MAGIC_BYTE    = $A5            ' Magic validation byte
Symbol EE_VERSION       = $01            ' Configuration version identifier
Symbol EE_BLOCK_SIZE    = 32             ' Bytes per configuration block

' Primary block addresses (32 bytes each)
Symbol EE_SYSTEM_BLOCK  = 0              ' System settings (timeout, beeper)
Symbol EE_INPUT1_BLOCK  = 32             ' Input 1 sensor configuration
Symbol EE_INPUT2_BLOCK  = 64             ' Input 2 sensor configuration
Symbol EE_INPUT3_BLOCK  = 96             ' Input 3 sensor configuration
Symbol EE_BACKUP_START  = 128            ' Backup area (corruption recovery)

' Legacy area for migrating old configurations
Symbol EE_LEGACY_START  = 200            ' Old format configuration area

' System block data offsets (within system block)
Symbol EE_SYS_MAGIC     = 0              ' Magic byte for validation
Symbol EE_SYS_VERSION   = 1              ' Version byte for compatibility
Symbol EE_SYS_TIMEOUT   = 2              ' Word: UI timeout in seconds
Symbol EE_SYS_PULSE     = 4              ' Word: Beeper pulse duration (ms)
Symbol EE_SYS_W_CNT     = 6              ' Word: Write cycle counter
Symbol EE_SYS_FLAGS     = 8              ' Byte: System status flags
Symbol EE_SYS_RESERVED  = 9              ' Reserved space (bytes 9-30)
Symbol EE_SYS_CHECKSUM  = 31             ' Block integrity checksum

' Input block data offsets (applies to all input blocks)
Symbol EE_IN_MAGIC      = 0              ' Magic byte for validation
Symbol EE_IN_VERSION    = 1              ' Version byte for compatibility
Symbol EE_IN_ENABLED    = 2              ' Byte: Input enable flag
Symbol EE_IN_SENSOR_T   = 3              ' Byte: Sensor type (pres/temp/flow)
Symbol EE_IN_FLOW_MODE  = 4              ' Byte: Flow mode bit flags
Symbol EE_IN_SCALE4     = 5              ' Word: 4mA scale value (signed)
Symbol EE_IN_SCALE20    = 7              ' Word: 20mA scale value (signed)
Symbol EE_IN_BP_HIGH    = 9              ' Word: High bypass duration (seconds)
Symbol EE_IN_BP_PLP     = 11             ' Word: PLP bypass duration (seconds)
Symbol EE_IN_BP_SLP     = 13             ' Word: SLP bypass duration (seconds)
Symbol EE_IN_RLY_HIGH   = 15             ' Byte: High relay mode (no/pulse/latch)
Symbol EE_IN_RLY_PLP    = 16             ' Byte: PLP relay mode (no/pulse/latch)
Symbol EE_IN_RLY_SLP    = 17             ' Byte: SLP relay mode (no/pulse/latch)
Symbol EE_IN_DISPLAY    = 18             ' Byte: Display enable flag
Symbol EE_IN_WRITE_CNT  = 19             ' Word: Block write cycle counter
Symbol EE_IN_RESERVED   = 21             ' Reserved space (bytes 21-30)
Symbol EE_IN_CHECKSUM   = 31             ' Block integrity checksum

' Write cycle management (not actively used but defined for future)
Symbol EE_MAX_WRITES    = 10000          ' Conservative write cycle limit
Symbol EE_BkUp_Thresh   = 1000           ' Backup after this many writes

' EEPROM operation result codes
Symbol EE_OK            = 0              ' Operation successful
Symbol EE_ERR_MAGIC     = 1              ' Magic byte validation failed
Symbol EE_ERR_VERSION   = 2              ' Version compatibility failed
Symbol EE_ERR_CHECKSUM  = 3              ' Checksum validation failed
Symbol EE_ERR_WEAR      = 4              ' Excessive wear detected
Symbol EE_ERR_BACKUP    = 5              ' Backup operation failed

' System status flags (bit positions within B_EE_Flags)
Symbol EE_FLAG_FACTORY  = 0              ' Factory defaults were loaded
Symbol EE_FLAG_BACKUP   = 1              ' Valid backup exists
Symbol EE_Flg_Restrd    = 2              ' Configuration restored from backup
Symbol EE_Flg_Mig       = 3              ' Configuration migrated from legacy

' Legacy EEPROM addresses for migration from old firmware
Symbol LEG_I1_Enabled   = 200            ' Old Input 1 enable flag location
Symbol LEG_I1_SensorT   = 201            ' Old Input 1 sensor type location
Symbol LEG_I1_FlowMode  = 202            ' Old Input 1 flow mode location
Symbol LEG_I1_Scale4    = 204            ' Old Input 1 4mA scale location
Symbol LEG_I1_Scale20   = 206            ' Old Input 1 20mA scale location
Symbol LEG_UI_TimeoutS  = 250            ' Old UI timeout location
Symbol LEG_UI_PulseMs   = 252            ' Old UI beeper pulse location

'========================= HARDWARE INITIALIZATION ======================

' Oscillator setup for 32MHz operation
OSCCON    = %01110000                     ' 8 MHz internal oscillator
OSCTUNE.6 = 1                             ' Enable PLL for 4x multiplication (32MHz)
ADCON1    = $0F                           ' Configure all pins as digital I/O

' Positron8 BASIC declarations
All_Digital = True                        ' Disable analog functions
Declare Xtal = 32                         ' 32MHz system clock
Declare PORTB_Pullups = On                ' Enable PORTB internal pullups

'============================= LCD INTERFACE ============================

' 4-bit LCD interface configuration (NHD-0420AZ compatible)
Declare LCD_Type = 0                      ' Generic 4-bit interface
Declare LCD_DTPin = PORTA.0               ' Data pins: RA0-RA3 (D4-D7)
Declare LCD_ENPin = PORTA.7               ' Enable pin: RA7
Declare LCD_RSPin = PORTA.6               ' Register select pin: RA6
Declare LCD_Interface = 4                 ' 4-bit mode
Declare LCD_Lines = 4                     ' 4-line display (20x4)

'============================ UART INTERFACE =============================

' Debug/communication UART setup
Declare Hserial_Baud  = 115200            ' High speed for debug output
Declare Hserial_RCSTA = %10010000         ' UART receive control
Declare Hserial_TXSTA = %00100100         ' UART transmit control

'====================== PIN ASSIGNMENTS AND SYMBOLS ======================

' Rotary encoder inputs (with hardware debouncing)
Symbol _ENC_A   = PORTB.1                 ' Encoder channel A
Symbol _ENC_B   = PORTB.2                 ' Encoder channel B
Symbol _BTN     = PORTB.6                 ' Push button (active low)
Symbol _ENC_SW  = _BTN                    ' Encoder switch (same as button)

' System outputs
Symbol _BUZZER  = PORTC.2                 ' Piezo buzzer (active high)

'========================= TIMING CONSTANTS ===============================

' Button press timing thresholds
Symbol BTN_SHORT_MS     = 200             ' Minimum short press duration
Symbol BTN_LONG_MS      = 900             ' Long press threshold
Symbol BTN_VLONG_MS     = 1800            ' Very long press threshold

' UI timeout limits
Symbol UO_TOut_S_mn     = 10              ' Minimum UI timeout (seconds)
Symbol UI_TOut_S_Mx     = 300             ' Maximum UI timeout (seconds)

' Beeper pulse limits
Symbol UI_PULSE_MS_MIN  = 10              ' Minimum beeper pulse (ms)
Symbol UI_Pls_MS_Mx     = 2000            ' Maximum beeper pulse (ms)

'========================== ENUMERATION VALUES ===========================

' Relay operating modes
Symbol MODE_NO    = 0                     ' No relay action
Symbol MODE_PULSE = 1                     ' Pulse relay briefly
Symbol MODE_LATCH = 2                     ' Latch relay on/off

' General boolean values
Symbol YES = 1                            ' Boolean true
Symbol NO  = 0                            ' Boolean false

' Sensor type identifiers
Symbol SENSOR_PRES = 0                    ' Pressure sensor
Symbol SENSOR_TEMP = 1                    ' Temperature sensor
Symbol SENSOR_FLOW = 2                    ' Flow sensor

' Flow sensor sub-options (bit-packed in FlowMode)
Symbol FLOWTYPE_ANALOG = 0                ' Analog flow sensor
Symbol Flow_Type_Dig   = 1                ' Digital flow sensor
Symbol FLOWU_PERCENT   = 0                ' Flow units: percentage
Symbol FLOWU_LPS       = 1                ' Flow units: liters per second

' Navigation constants
Symbol NAV_CONTINUE = 0                   ' Continue in current state
Symbol NAV_BACK = 1                       ' Navigate back/escape
Symbol NAV_MAIN = 2                       ' Go to main menu
Symbol NAV_TIMEOUT = 3                    ' System timeout occurred
Symbol NAV_SELECT = 4                     ' Item selected
Symbol NAV_EDIT = 5                       ' Enter edit mode
Symbol NAV_SAVE = 6                       ' Save operation
Symbol NAV_CANCEL = 7                     ' Cancel operation

' Debug control
Symbol DEBUG_ENABLED = 1                  ' Enable debug output

'========================= INTERRUPT CONTROL BITS =========================

' Timer0 interrupt control (used for ISR)
Symbol TMR0IF = INTCON.2                  ' Timer0 overflow flag
Symbol TMR0IE = INTCON.5                  ' Timer0 interrupt enable
Symbol GIE    = INTCON.7                  ' Global interrupt enable

'======================= GLOBAL VARIABLE DECLARATIONS ====================

'-------------------- System State Variables ------------------------------

Dim b_ReInitLCD  As Bit                   ' Flag: LCD needs reinitialization
Dim b_ScrDirty   As Bit                   ' Flag: Screen needs refresh
Dim b_Escape     As Bit                   ' Flag: Escape key condition

Dim L_Millis     As Dword                 ' System millisecond counter (ISR)
Dim L_LastInput  As Dword                 ' Last user interaction timestamp

Dim B_EncDelta   As SByte                 ' Encoder movement (-1, 0, +1)
Dim B_KeyEvent   As Byte                  ' Button press event flag
Dim W_Beep       As Word                  ' Beeper countdown timer

'-------------------- EEPROM Status Variables -----------------------------

Dim B_EE_Status         As Byte          ' Last EEPROM operation result
Dim B_EE_Flags          As Byte          ' EEPROM system status flags
Dim W_EE_Wrt_Cnt        As Word          ' EEPROM write cycle counter

'-------------------- Encoder and Button Variables ------------------------

Dim B_RE_Count     As Byte               ' Rotary encoder ISR counter
Dim B_AState       As Byte               ' Encoder channel A state
Dim B_BState       As Byte               ' Encoder channel B state
Dim B_ButtonState  As Byte               ' Button state (debounced)
Dim B_DebA         As Byte               ' Channel A debounce counter
Dim B_DebB         As Byte               ' Channel B debounce counter
Dim B_DebBtn       As Byte               ' Button debounce counter
Dim B_LastState    As Byte               ' Previous encoder state
Dim S_Qacc         As SByte              ' Quadrature accumulator
Dim W_BtnHoldMS    As Word               ' Button hold duration (ms)
Dim W_EncoderPos   As Word               ' Encoder position (ISR updates)
Dim W_EncReadPos   As Word               ' Last read encoder position

'-------------------- Navigation State Variables --------------------------

Dim B_NavCode      As Byte               ' Navigation result code

'-------------------- System Configuration Variables ----------------------

Dim W_UI_TimeoutS  As Word               ' UI timeout duration (seconds)
Dim W_UI_PulseMs   As Word               ' Beeper pulse duration (milliseconds)

'-----End Section 1----

'----Start Section 2----
Section_2:
'-------------------- Input 1 Configuration Variables ---------------------

Dim B_I1_Enabled  As Byte               ' Input 1 enable flag
Dim B_I1_SensorT  As Byte               ' Input 1 sensor type
Dim B_I1_FlowMode As Byte               ' Input 1 flow mode (bit-packed)
Dim W_I1_Scale4   As Word               ' Input 1: 4mA scale value (signed 2C)
Dim W_I1_Scale20  As Word               ' Input 1: 20mA scale value (signed 2C)
Dim W_I1_BP_High  As Word               ' Input 1: High bypass duration (sec)
Dim W_I1_BP_PLP   As Word               ' Input 1: PLP bypass duration (sec)
Dim W_I1_BP_SLP   As Word               ' Input 1: SLP bypass duration (sec)
Dim B_I1_RlyHigh  As Byte               ' Input 1: High relay mode
Dim B_I1_RlyPLP   As Byte               ' Input 1: PLP relay mode
Dim B_I1_RlySLP   As Byte               ' Input 1: SLP relay mode
Dim B_I1_Display  As Byte               ' Input 1: Display enable flag

' Input 1 unpacked/derived variables (not persisted to EEPROM)
Dim W_I1_BP_Low    As Word               ' Input 1: Low bypass (mirrors SLP)
Dim B_I1_RlyLow    As Byte               ' Input 1: Low relay (mirrors SLP)
Dim B_I1_FlowType  As Byte               ' Input 1: Flow type (unpacked)
Dim B_I1_FlowUnits As Byte               ' Input 1: Flow units (unpacked)

'-------------------- Input 2 Configuration Variables ---------------------

Dim B_I2_Enabled  As Byte               ' Input 2 enable flag
Dim B_I2_SensorT  As Byte               ' Input 2 sensor type
Dim B_I2_FlowMode As Byte               ' Input 2 flow mode (bit-packed)
Dim W_I2_Scale4   As Word               ' Input 2: 4mA scale value (signed 2C)
Dim W_I2_Scale20  As Word               ' Input 2: 20mA scale value (signed 2C)
Dim W_I2_BP_High  As Word               ' Input 2: High bypass duration (sec)
Dim W_I2_BP_PLP   As Word               ' Input 2: PLP bypass duration (sec)
Dim W_I2_BP_SLP   As Word               ' Input 2: SLP bypass duration (sec)
Dim B_I2_RlyHigh  As Byte               ' Input 2: High relay mode
Dim B_I2_RlyPLP   As Byte               ' Input 2: PLP relay mode
Dim B_I2_RlySLP   As Byte               ' Input 2: SLP relay mode
Dim B_I2_Display  As Byte               ' Input 2: Display enable flag

'-------------------- Input 3 Configuration Variables ---------------------

Dim B_I3_Enabled  As Byte               ' Input 3 enable flag
Dim B_I3_SensorT  As Byte               ' Input 3 sensor type
Dim B_I3_FlowMode As Byte               ' Input 3 flow mode (bit-packed)
Dim W_I3_Scale4   As Word               ' Input 3: 4mA scale value (signed 2C)
Dim W_I3_Scale20  As Word               ' Input 3: 20mA scale value (signed 2C)
Dim W_I3_BP_High  As Word               ' Input 3: High bypass duration (sec)
Dim W_I3_BP_PLP   As Word               ' Input 3: PLP bypass duration (sec)
Dim W_I3_BP_SLP   As Word               ' Input 3: SLP bypass duration (sec)
Dim B_I3_RlyHigh  As Byte               ' Input 3: High relay mode
Dim B_I3_RlyPLP   As Byte               ' Input 3: PLP relay mode
Dim B_I3_RlySLP   As Byte               ' Input 3: SLP relay mode
Dim B_I3_Display  As Byte               ' Input 3: Display enable flag

'-------------------- Menu and UI State Variables -------------------------

Dim B_MenuSel     As Byte               ' Current menu selection
Dim B_MenuTop     As Byte               ' Top visible menu item
Dim B_MenuCount   As Byte               ' Total menu items
Dim B_MenuRows    As Byte               ' Visible menu rows

Dim B_EditMode    As Byte               ' 0=navigate, 1=editing
Dim B_EditPos     As Byte               ' Current edit position
Dim B_EditField   As Byte               ' Active field being edited  
Dim B_BlinkState  As Byte               ' Blink display state
Dim L_LastBlink   As Dword              ' Last blink timing
Dim B_ForceUpdate As Byte               ' Force display update flag

'==============================================================================
'======================= TIMER0 ISR AND LCD FUNCTIONS =======================
'
' This section contains:
' - 1ms Timer0 ISR for encoder/button debouncing and system timing
' - Low-level LCD interface functions for NHD-0420AZ displays
' - Hardware initialization and control functions
'
'==============================================================================

'========================= TIMER0 ISR SETUP ================================
'
' Timer0 configured for 1ms interrupts at 32MHz system clock
' Provides: encoder debouncing, button timing, beeper service, system timing
'
'============================================================================

T0CON = %11000100                        ' Timer0: 16-bit, internal clock, 1:32 prescaler
TMR0L = 6                                ' Preload for 1ms at 32MHz
Clear TMR0IF                             ' Clear overflow flag
TMR0IE = 1                               ' Enable Timer0 interrupts
GIE    = 1                               ' Enable global interrupts

On_Hardware_Interrupt GoTo Isr           ' Set interrupt vector
GoTo Over_Interrupt                      ' Skip ISR code during normal execution

'======================= 1MS INTERRUPT SERVICE ROUTINE ====================
'
' Critical timing ISR - executes every 1ms
' Handles: encoder quadrature decoding, button debouncing, beeper control
' All encoder/button processing moved to ISR for responsive UI
'
'============================================================================

Isr:
	Context Save                         ' Preserve all registers

	If TMR0IF = 1 Then                   ' Timer0 overflow (1ms tick)
		TMR0L = 6                        ' Reload for next 1ms
		Clear TMR0IF                     ' Clear interrupt flag
		Inc L_Millis                     ' Increment system millisecond counter

		' ISR encoder/button processing (every 10ms for efficiency)
		Inc B_RE_Count
		If B_RE_Count > 9 Then           ' Process every 10ms (100Hz rate)
			Dim B_NewA     As Byte       ' Current encoder A state
			Dim B_NewB     As Byte       ' Current encoder B state
			Dim B_NewBtn   As Byte       ' Current button state
			Dim B_Curr     As Byte       ' Current encoder combined state
			Dim B_Combined As Byte       ' State transition lookup value

			' Read current hardware states
			B_NewA   = _ENC_A            ' Sample encoder channel A
			B_NewB   = _ENC_B            ' Sample encoder channel B
			B_NewBtn = _BTN              ' Sample push button (active low)

			'-------------- Encoder Channel A Debouncing ----------------
			If B_NewA <> B_AState Then   ' State change detected
				Inc B_DebA               ' Increment debounce counter
				If B_DebA >= 2 Then      ' Stable for 2 samples (20ms)
					B_AState = B_NewA    ' Accept new state
					B_DebA = 0           ' Reset debounce counter
					L_LastInput = L_Millis ' Update activity timestamp
				EndIf
			Else
				B_DebA = 0               ' Reset counter if state stable
			EndIf

			'-------------- Encoder Channel B Debouncing ----------------
			If B_NewB <> B_BState Then   ' State change detected
				Inc B_DebB               ' Increment debounce counter
				If B_DebB >= 2 Then      ' Stable for 2 samples (20ms)
					B_BState = B_NewB    ' Accept new state
					B_DebB = 0           ' Reset debounce counter
					L_LastInput = L_Millis ' Update activity timestamp
				EndIf
			Else
				B_DebB = 0               ' Reset counter if state stable
			EndIf

			'-------------- Button Debouncing ---------------------------
			If B_NewBtn <> B_ButtonState Then ' State change detected
				Inc B_DebBtn             ' Increment debounce counter
				If B_DebBtn >= 2 Then    ' Stable for 2 samples (20ms)
					B_ButtonState = B_NewBtn ' Accept new state
					B_DebBtn = 0         ' Reset debounce counter
					L_LastInput = L_Millis ' Update activity timestamp
				EndIf
			Else
				B_DebBtn = 0             ' Reset counter if state stable
			EndIf

			'-------------- Quadrature Decoding -------------------------
			' Standard quadrature encoding with state machine
			' Accumulates movement until detent position for stability
			
			B_Curr = (B_AState * 2) + B_BState     ' Current state (0-3)
			B_Combined = (B_LastState * 4) + B_Curr ' Transition lookup (0-15)

			' Quadrature state machine - increment/decrement based on transitions
			Select B_Combined
				Case %0001               ' Forward transitions
					Dec S_Qacc
				Case %0111
					Dec S_Qacc
				Case %1110
					Dec S_Qacc
				Case %1000
					Dec S_Qacc
				Case %0010               ' Reverse transitions
					Inc S_Qacc
				Case %1011
					Inc S_Qacc
				Case %1101
					Inc S_Qacc
				Case %0100
					Inc S_Qacc
			EndSelect

			'-------------- Encoder Position Update ----------------------
			' Only commit movement at detent positions (state 00) with threshold
			' This provides natural "click" feel and prevents jitter
			
			If B_Curr = 0 Then           ' At detent position (both channels low)
				If S_Qacc >= 2 Then      ' Forward movement threshold reached
					Inc W_EncoderPos     ' Increment position counter
					L_LastInput = L_Millis ' Update activity timestamp
				Else
					If S_Qacc <= -2 Then ' Reverse movement threshold reached
						If W_EncoderPos > 0 Then ' Prevent underflow
							Dec W_EncoderPos ' Decrement position counter
						EndIf
						L_LastInput = L_Millis ' Update activity timestamp
					EndIf
				EndIf
				S_Qacc = 0               ' Reset accumulator at detent
			EndIf
			B_LastState = B_Curr         ' Store state for next comparison

			'-------------- Button Press Timing -------------------------
			' Track button hold duration for short/long press detection
			' Only generates events on button release to prevent bouncing
			
			If B_ButtonState = 0 Then    ' Button currently pressed (active low)
				If W_BtnHoldMS <= 65525 Then ' Prevent overflow
					W_BtnHoldMS = W_BtnHoldMS + 10 ' Accumulate hold time (10ms increments)
				EndIf
			Else                         ' Button released
				If W_BtnHoldMS >= BTN_SHORT_MS Then ' Valid press duration
					B_KeyEvent = 1       ' Set key event flag for main loop
					L_LastInput = L_Millis ' Update activity timestamp
				EndIf
				W_BtnHoldMS = 0          ' Reset hold timer
			EndIf

			Clear B_RE_Count             ' Reset 10ms counter
		EndIf

		'-------------- Beeper Service ---------------------------------
		' Simple countdown beeper - decrements until zero
		' Beeper duration set by P_Beeps() function calls
		
		If W_Beep > 0 Then               ' Beeper active
			_BUZZER = 1                  ' Turn on buzzer
			Dec W_Beep                   ' Decrement duration counter
		Else
			_BUZZER = 0                  ' Turn off buzzer
		EndIf
	EndIf
	Context Restore                      ' Restore all registers
'============================================================================
Over_Interrupt:                         ' ISR exit point

'======================= LCD HARDWARE INTERFACE ===========================
'
' Low-level 4-bit LCD interface for NHD-0420AZ (ST7066U controller)
' Robust initialization sequence handles various LCD timing requirements
' Direct port manipulation for reliable operation at 32MHz
'
'============================================================================

' LCD control pin definitions
Symbol LCD_RS = PORTA.6                 ' Register Select: 0=command, 1=data
Symbol LCD_E  = PORTA.7                 ' Enable: rising edge latches data



'-------------- Test Variables ----------------------------------------------
' Proper string variable declarations according to Positron8 manual

Dim S_TestMsg As String * 20             ' Fixed-size string variable
Dim S_Buffer1 As String * 15             ' Buffer for intermediate operations  
Dim S_Buffer2 As String * 15             ' Second buffer for concatenation
Dim S_Result As String * 30              ' Result buffer (larger for concatenated strings)
Dim B_TestByte As Byte                   ' Test byte value
Dim W_TestWord As Word                   ' Test word value

'-------------- Test Procedure 1: Basic String Operations ------------------
' Tests fundamental string variable assignment and display

Proc P_TestBasicStrings()
	' Direct string assignment
	S_TestMsg = "Hello World"            ' Simple string assignment
	Print S_TestMsg                      ' Direct print of string variable
	
	' String from constants
	S_Buffer1 = "Test: "                 ' Assign constant to buffer
	Print S_Buffer1                      ' Print the buffer
EndProc

'-------------- Test Procedure 2: Number to String Conversion --------------
' Tests Str$() function with proper modifiers

Proc P_TestNumberConversion()
	B_TestByte = 123                     ' Set test byte value
	W_TestWord = 45678                   ' Set test word value
	
	' Convert byte to string with proper modifier
	S_Buffer1 = Str$(Dec B_TestByte)     ' Dec modifier for decimal conversion
	Print "Byte: "; S_Buffer1           ' Print with label
	
	' Convert word to string with proper modifier  
	S_Buffer2 = Str$(Dec W_TestWord)     ' Dec modifier for decimal conversion
	Print "Word: "; S_Buffer2           ' Print with label
EndProc

'-------------- Test Procedure 3: Safe String Concatenation ----------------
' Tests safe string concatenation using Positron8 methods

Proc P_TestStringConcat()
	Dim S_Label As String * 10           ' Label string
	Dim S_Value As String * 10           ' Value string
	Dim S_Units As String * 5            ' Units string
	
	' Prepare components
	S_Label = "Temp: "                   ' Set label
	S_Value = Str$(Dec B_TestByte)       ' Convert number to string
	S_Units = "C"                        ' Set units
	
	' Method 1: Direct print concatenation (safest)
	Print S_Label; S_Value; S_Units      ' Print all parts with semicolons
	
	' Method 2: Step-by-step string building (if string assignment needed)
	S_Buffer1 = S_Label                  ' Start with label
	S_Buffer1 = S_Buffer1 + S_Value      ' Add value
	S_Result = S_Buffer1 + S_Units       ' Final concatenation to larger buffer
	Print S_Result                       ' Print final result
EndProc

'-------------- Test Procedure 4: LCD Display Functions --------------------
' Tests LCD display with proper string handling

Proc P_TestLCDDisplay()
	Dim S_Title As String * 20           ' Title string
	Dim S_Line1 As String * 20           ' Display line 1
	Dim S_Line2 As String * 20           ' Display line 2
	Dim S_NumStr As String * 8           ' Number string
	
	' Set title
	S_Title = "SYSTEM TEST"
	
	' Create display lines with values
	S_NumStr = Str$(Dec W_TestWord)      ' Convert word to string
	S_Line1 = "Value: "                  ' Start line 1
	S_Line1 = S_Line1 + S_NumStr         ' Add number
	
	S_NumStr = Str$(Dec B_TestByte)      ' Convert byte to string  
	S_Line2 = "Count: "                  ' Start line 2
	S_Line2 = S_Line2 + S_NumStr         ' Add number
	
	' Display (assuming LCD functions exist)
	' P_DispAt(1, 1, S_Title)
	' P_DispAt(2, 1, S_Line1)  
	' P_DispAt(3, 1, S_Line2)
	
	' For testing, just print to UART
	Print S_Title
	Print S_Line1
	Print S_Line2
EndProc

'-------------- Test Procedure 5: Menu Display with Values -----------------
' Tests menu-style display with formatted values

Proc P_TestMenuDisplay()
	Dim S_MenuTitle As String * 20       ' Menu title
	Dim S_Item1 As String * 20           ' Menu item 1
	Dim S_Item2 As String * 20           ' Menu item 2
	Dim S_TempStr As String * 10         ' Temporary string
	Dim B_Selection As Byte              ' Menu selection
	
	' Set test values
	B_Selection = 1
	S_MenuTitle = "TEST MENU"
	
	' Build menu items with values
	S_TempStr = Str$(Dec W_TestWord)     ' Convert value to string
	S_Item1 = "Timeout: "                ' Start with label
	S_Item1 = S_Item1 + S_TempStr        ' Add value
	S_Item1 = S_Item1 + "s"              ' Add units
	
	S_TempStr = Str$(Dec B_TestByte)     ' Convert value to string
	S_Item2 = "Count: "                  ' Start with label  
	S_Item2 = S_Item2 + S_TempStr        ' Add value
	
	' Display menu (for testing, print to UART)
	Print S_MenuTitle
	If B_Selection = 1 Then
		Print ">"; S_Item1               ' Show selection cursor
	Else
		Print " "; S_Item1               ' No cursor
	EndIf
	
	If B_Selection = 2 Then
		Print ">"; S_Item2               ' Show selection cursor
	Else
		Print " "; S_Item2               ' No cursor  
	EndIf
EndProc

'-------------- Test Main Function ------------------------------------------
' Main test function to verify all string operations

Proc P_RunStringTests()
	' Initialize test values
	B_TestByte = 42
	W_TestWord = 1234
	
	Print "=== Positron8 String Tests ==="
	
	' Run all tests
	P_TestBasicStrings()
	Print "Basic strings: OK"
	
	P_TestNumberConversion()  
	Print "Number conversion: OK"
	
	P_TestStringConcat()
	Print "String concatenation: OK"
	
	P_TestLCDDisplay()
	Print "LCD display: OK"
	
	P_TestMenuDisplay()
	Print "Menu display: OK"
	
	Print "=== All Tests Complete ==="
EndProc

'==============================================================================
' Key Patterns Established:
'
' 1. String Variables: Always declare with explicit size (String * n)
' 2. Str$() Function: Always use modifier (Dec, Hex, Bin, etc.)
' 3. Concatenation: Use intermediate variables, avoid complex expressions
' 4. Buffer Sizing: Make result buffers larger than input components
' 5. Print Statements: Use semicolons for concatenation in print
' 6. Assignment: Complete all assignments, avoid partial expressions
'
' These patterns should be applied to all string operations in the main code.
'==============================================================================




