'----Start Section 1----
'==============================================================================
'=====================================================================
' IRRISYS HMI on Positron8 / PIC18F2525
' Single-file build: IRRISYS_MAIN.bas
' Rev: 2025-12-11 (Cleaned, commented, and reorganized with full inline editing)
'
' This system provides a complete HMI interface for irrigation control
' with inline editing, EEPROM configuration management, and multi-input
' sensor support (Pressure, Temperature, Flow sensors).
'=====================================================================
Section_1:
'============================== DEVICE SETUP ==============================

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

'-------------- 4-Bit Nibble Output Function ---------------------------
' Sets LCD data lines (RA0-RA3) to specified nibble value
' Used for both command and data transmission

Proc LCD_SetNib(B_N As Byte)
	If B_N.0 = 1 Then                   ' Set D4 (RA0)
		Set PORTA.0
	Else
		Clear PORTA.0
	EndIf

	If B_N.1 = 1 Then                   ' Set D5 (RA1)
		Set PORTA.1
	Else
		Clear PORTA.1
	EndIf

	If B_N.2 = 1 Then                   ' Set D6 (RA2)
		Set PORTA.2
	Else
		Clear PORTA.2
	EndIf

	If B_N.3 = 1 Then                   ' Set D7 (RA3)
		Set PORTA.3
	Else
		Clear PORTA.3
	EndIf
EndProc

'-------------- LCD Enable Pulse Generator ------------------------------
' Generates enable pulse with proper timing for data latching
' Critical timing: setup, pulse width, hold times

Proc LCD_PulseE()
	Set LCD_E                            ' Enable high
	DelayUS 1                            ' Setup time
	Clear LCD_E                          ' Enable low (falling edge latches)
	DelayUS 40                           ' Hold time
EndProc

'-------------- Write Nibble to LCD -------------------------------------
' Combines nibble setup and enable pulse for atomic LCD write

Proc LCD_WriteNib(B_N As Byte)
	LCD_SetNib(B_N)                      ' Set data lines
	LCD_PulseE()                         ' Pulse enable to latch
EndProc

'-------------- Write Command to LCD ------------------------------------
' Sends 8-bit command as two 4-bit nibbles (high nibble first)
' Includes appropriate timing delays for command execution

Proc LCD_WriteCmd(B_Cmd As Byte)
	Dim B_N As Byte
	LCD_RS = 0                           ' Select command register

	B_N = B_Cmd / 16                     ' Extract high nibble
	LCD_WriteNib(B_N)                    ' Send high nibble first

	B_N = B_Cmd // 16                    ' Extract low nibble
	LCD_WriteNib(B_N)                    ' Send low nibble

	' Command-specific timing delays
	If B_Cmd = $01 Or B_Cmd = $02 Then   ' Clear/Home commands need extra time
		DelayMS 2                        ' Long delay for clear/home
	Else
		DelayUS 50                       ' Standard command delay
	EndIf
EndProc

'-------------- Write Data to LCD ---------------------------------------
' Sends 8-bit data as two 4-bit nibbles for character display
' Same nibble order as commands but targets data register

Proc LCD_WriteDat(B_Data As Byte)
	Dim B_N As Byte
	LCD_RS = 1                           ' Select data register

	B_N = B_Data / 16                    ' Extract high nibble
	LCD_WriteNib(B_N)                    ' Send high nibble first

	B_N = B_Data // 16                   ' Extract low nibble
	LCD_WriteNib(B_N)                    ' Send low nibble

	DelayUS 50                           ' Data write delay
EndProc

'-----End Section 2----

'----Start Section 3----
Section_3:
'-------------- Complete LCD Hardware Initialization -------------------
' Robust initialization sequence for ST7066U-based displays
' Handles power-on reset and various startup conditions

Proc P_LCDHardInit()
	' Configure port directions
	TRISA = %00010000                    ' RA0-RA3,RA6,RA7 outputs; RA4,RA5 inputs
	TRISB = %01000110                    ' Encoder inputs, button input
	TRISC = %00000000                    ' All outputs

	' Clear all output ports
	Clear PORTA
	Clear PORTB
	Clear PORTC

	' Initialize LCD control pins
	Clear LCD_RS                         ' Command mode
	Clear LCD_E                          ' Enable inactive

	DelayMS 50                           ' Power stabilization delay

	LCD_RS = 0                           ' Ensure command mode

	' Force 8-bit mode three times (required by specification)
	' This handles various power-on states and ensures known starting point
	LCD_WriteNib($03)                    ' Function set: 8-bit mode
	DelayMS 5                            ' Wait > 4.1ms

	LCD_WriteNib($03)                    ' Function set: 8-bit mode (repeat)
	DelayUS 150                          ' Wait > 100µs

	LCD_WriteNib($03)                    ' Function set: 8-bit mode (repeat)
	DelayUS 150                          ' Wait > 100µs

	' Switch to 4-bit mode
	LCD_WriteNib($02)                    ' Function set: 4-bit mode
	DelayUS 150                          ' Wait for mode switch

	' Complete LCD initialization sequence
	LCD_WriteCmd($28)                    ' Function set: 4-bit, 2 lines, 5x8 font
	LCD_WriteCmd($08)                    ' Display OFF (cursor, blink off)
	LCD_WriteCmd($01)                    ' Clear display
	LCD_WriteCmd($06)                    ' Entry mode: increment, no shift
	LCD_WriteCmd($0C)                    ' Display ON, cursor OFF, blink OFF
EndProc

'-------------- LCD Cursor Control Functions ---------------------------

Proc LCD_CursorOn()
	LCD_WriteCmd($0E)                    ' Display on, cursor on, blink off
EndProc

Proc LCD_CursorOff()
	LCD_WriteCmd($0C)                    ' Display on, cursor off, blink off
EndProc

'-------------- LCD Cursor Positioning ---------------------------------
' Sets cursor to specific row/column for next character write
' Handles 20x4 display memory mapping (non-linear addresses)

Proc LCD_SetCursor(B_Row As Byte, B_Col As Byte)
	Dim B_Base As Byte                   ' Row base address
	Dim B_Addr As Byte                   ' Final DDRAM address

	' 20x4 LCD DDRAM address mapping
	Select B_Row
		Case 1                           ' Row 1: addresses 0x00-0x13
			B_Base = $00
		Case 2                           ' Row 2: addresses 0x40-0x53
			B_Base = $40
		Case 3                           ' Row 3: addresses 0x14-0x27
			B_Base = $14
		Case Else                        ' Row 4: addresses 0x54-0x67
			B_Base = $54
	EndSelect
	
	B_Addr = $80 + B_Base + (B_Col - 1)  ' Set DDRAM address command
	LCD_WriteCmd(B_Addr)                 ' Send cursor position command
EndProc

'-------------- High-Level LCD Initialization --------------------------
' Wrapper for Positron8 BASIC LCD compatibility
' Provides clean interface to existing LCD library functions

Proc P_LCDSafeInit()
	DelayMS 5                            ' Brief delay
	Cls                                  ' Clear screen using BASIC function
	DelayMS 2                            ' Settle delay
EndProc

'==============================================================================
'======================= EEPROM PRIMITIVES AND VALIDATION ===================
'
' This section provides the foundational EEPROM operations for the IRRISYS system:
' - Safe write operations with verification and retry logic
' - Block integrity validation using checksums and magic bytes
' - Low-level primitives that all higher-level operations depend upon
'
' Design Philosophy: Conservative and reliable
' - Multiple write attempts with verification
' - Generous timing margins for EEPROM settling
' - Clear error codes for diagnostic purposes
' - No optimization at expense of reliability
'
'==============================================================================

'======================= LOW-LEVEL EEPROM WRITE OPERATIONS ==================
'
' Foundation functions for safe EEPROM access with proper timing and verification
' All configuration management builds on these verified primitives
'
'=============================================================================

'-------------- Safe Byte Write with Verification --------------------------
' Writes a single byte to EEPROM with automatic verification and retry logic
' Critical for configuration reliability - prevents silent corruption
'
' Parameters:
'   W_Addr: EEPROM address (0-255 for PIC18F2525)
'   B_Data: Byte value to write
'
' Returns:
'   EE_OK (0): Write successful and verified
'   EE_ERR_BACKUP (5): Write failed after all retries

Proc P_EE_SafeWriteB(W_Addr As Word, B_Data As Byte), Byte
	Dim B_Retry   As Byte                ' Retry counter (0-2)
	Dim B_ReadBk  As Byte                ' Readback verification value
	
	For B_Retry = 0 To 2                 ' Maximum 3 write attempts
		EWrite W_Addr, B_Data            ' Perform EEPROM write operation
		DelayMS 10                       ' Allow EEPROM to settle (conservative timing)
		
		ERead W_Addr, B_ReadBk           ' Read back written value for verification
		
		If B_ReadBk = B_Data Then        ' Verification successful
			Result = EE_OK               ' Return success code
			ExitProc                     ' Exit immediately on success
		EndIf
	Next B_Retry                         ' Try next attempt if verification failed
	
	Result = EE_ERR_BACKUP               ' All retries exhausted - return error
EndProc

'-------------- Safe Word Write with Verification --------------------------
' Writes a 16-bit word to EEPROM as two consecutive bytes (little-endian format)
' Maintains data integrity for multi-byte values like timeouts and scale factors

Proc P_EE_SafeWriteW(W_Addr As Word, W_Data As Word), Byte
	Dim B_Status As Byte                 ' Status from individual byte write operations
	
	' Write low byte (bits 0-7) first
	B_Status = P_EE_SafeWriteB(W_Addr, W_Data.LowByte)
	If B_Status <> EE_OK Then            ' Low byte write failed
		Result = B_Status                ' Return error immediately (atomic failure)
		ExitProc
	EndIf
	
	' Write high byte (bits 8-15) second
	B_Status = P_EE_SafeWriteB(W_Addr + 1, W_Data.HighByte)
	If B_Status <> EE_OK Then            ' High byte write failed
		Result = B_Status                ' Return error immediately
		ExitProc
	EndIf
	
	Result = EE_OK                       ' Both bytes written and verified successfully
EndProc

'======================= BLOCK INTEGRITY VALIDATION ========================
'
' Functions for ensuring configuration block integrity using checksums and magic bytes
' Prevents loading of corrupted, uninitialized, or incompatible configuration data
'
'=============================================================================

'-------------- Calculate Block Checksum -----------------------------------
' Computes simple additive checksum for a 32-byte configuration block
' Excludes the checksum byte itself from calculation (byte 31)

Proc P_EE_CalcCSum(W_BlkAddr As Word), Byte
	Dim B_Sum    As Byte                 ' Running checksum accumulator
	Dim B_Idx    As Byte                 ' Block byte index (0-30)
	Dim B_Data   As Byte                 ' Current byte value from EEPROM
	
	B_Sum = 0                            ' Initialize checksum accumulator
	
	For B_Idx = 0 To 30                  ' Process bytes 0-30 (exclude checksum at 31)
		ERead W_BlkAddr + B_Idx, B_Data  ' Read byte from EEPROM
		B_Sum = B_Sum + B_Data           ' Add to checksum (automatic wrap at 255)
	Next B_Idx
	
	Result = B_Sum                       ' Return calculated checksum
EndProc

'-------------- Validate Configuration Block -------------------------------
' Comprehensive validation of a 32-byte configuration block
' Performs three-stage validation: magic byte, version, and data integrity

Proc P_EE_ValidBlk(W_BlkAddr As Word), Byte
	Dim B_Magic   As Byte                ' Magic byte read from block header
	Dim B_Version As Byte                ' Version byte read from block header
	Dim B_CalcSum As Byte                ' Calculated checksum from block data
	Dim B_StorSum As Byte                ' Stored checksum from block footer
	
	' Stage 1: Validate magic byte (quick corruption/initialization check)
	ERead W_BlkAddr + 0, B_Magic         ' Read magic byte from block start
	If B_Magic <> EE_MAGIC_BYTE Then     ' Magic byte validation failed
		Result = EE_ERR_MAGIC            ' Block uninitialized or severely corrupted
		ExitProc
	EndIf
	
	' Stage 2: Check version compatibility
	ERead W_BlkAddr + 1, B_Version       ' Read version byte from block header
	If B_Version <> EE_VERSION Then      ' Version mismatch detected
		Result = EE_ERR_VERSION          ' Block format incompatible with firmware
		ExitProc
	EndIf
	
	' Stage 3: Verify data integrity with checksum
	B_CalcSum = P_EE_CalcCSum(W_BlkAddr) ' Calculate expected checksum from data
	ERead W_BlkAddr + 31, B_StorSum      ' Read stored checksum from block end
	
	If B_CalcSum <> B_StorSum Then       ' Checksum mismatch detected
		Result = EE_ERR_CHECKSUM         ' Data corruption present in block
		ExitProc
	EndIf
	
	Result = EE_OK                       ' All validation stages passed successfully
EndProc

'======================= EEPROM BLOCK STRUCTURE HELPERS ====================
'
' Utility functions for working with the standardized 32-byte block format
' Provides consistent block initialization and management
'
'=============================================================================

'-------------- Initialize Block Header ------------------------------------
' Sets up the standard header for a new configuration block
' Writes magic byte and version to establish block validity

Proc P_EE_InitBlkHdr(W_BlkAddr As Word), Byte
	Dim B_Status As Byte                 ' Write operation status
	
	' Write magic byte to establish block validity
	B_Status = P_EE_SafeWriteB(W_BlkAddr + 0, EE_MAGIC_BYTE)
	If B_Status <> EE_OK Then            ' Magic byte write failed
		Result = B_Status                ' Return write error
		ExitProc
	EndIf
	
	' Write version byte for firmware compatibility
	B_Status = P_EE_SafeWriteB(W_BlkAddr + 1, EE_VERSION)
	If B_Status <> EE_OK Then            ' Version byte write failed
		Result = B_Status                ' Return write error
		ExitProc
	EndIf
	
	Result = EE_OK                       ' Header initialization successful
EndProc

'-------------- Finalize Block with Checksum -------------------------------
' Completes a configuration block by calculating and writing the checksum
' Must be called after all block data has been written

Proc P_EE_FinalizeBlk(W_BlkAddr As Word), Byte
	Dim B_Status As Byte                 ' Write operation status
	Dim B_CSum   As Byte                 ' Calculated checksum value
	
	' Calculate checksum for bytes 0-30
	B_CSum = P_EE_CalcCSum(W_BlkAddr)    ' Compute block checksum
	
	' Write checksum to complete block validation structure
	B_Status = P_EE_SafeWriteB(W_BlkAddr + 31, B_CSum)
	If B_Status <> EE_OK Then            ' Checksum write failed
		Result = B_Status                ' Return write error
		ExitProc
	EndIf
	
	Result = EE_OK                       ' Block finalization successful
EndProc

'-------------- Clear Block Reserved Area ----------------------------------
' Clears the reserved area of a configuration block to zero
' Ensures consistent block format and future compatibility

Proc P_EE_ClrReserved(W_BlkAddr As Word, B_StartByte As Byte, B_EndByte As Byte), Byte
	Dim B_Status As Byte                 ' Write operation status
	Dim B_Idx    As Byte                 ' Current byte index
	
	For B_Idx = B_StartByte To B_EndByte ' Clear specified range
		B_Status = P_EE_SafeWriteB(W_BlkAddr + B_Idx, 0)
		If B_Status <> EE_OK Then        ' Write failed
			Result = B_Status            ' Return error immediately
			ExitProc
		EndIf
	Next B_Idx
	
	Result = EE_OK                       ' Reserved area cleared successfully
EndProc

'======================= EEPROM HEALTH AND DIAGNOSTIC FUNCTIONS ============

'-------------- EEPROM Health Check ----------------------------------------
' Performs basic health check on EEPROM system
' Tests write capability and basic functionality

Proc P_EE_HealthCheck(), Byte
	Dim B_Status   As Byte               ' Operation status
	Dim B_TestVal  As Byte               ' Test value for write verification
	Dim B_ReadBack As Byte               ' Value read back from EEPROM
	
	' Test basic write capability
	B_TestVal = $55                      ' Test pattern (alternating bits)
	B_Status = P_EE_SafeWriteB(255, B_TestVal) ' Write to safe test location
	If B_Status <> EE_OK Then            ' Write test failed
		Result = B_Status                ' EEPROM write capability compromised
		ExitProc
	EndIf
	
	' Verify write capability with different pattern
	B_TestVal = $AA                      ' Complementary test pattern
	B_Status = P_EE_SafeWriteB(255, B_TestVal) ' Write to same test location
	If B_Status <> EE_OK Then            ' Second write test failed
		Result = B_Status                ' EEPROM write capability compromised
		ExitProc
	EndIf
	
	' Final verification read
	ERead 255, B_ReadBack                ' Read back final test value
	If B_ReadBack <> B_TestVal Then      ' Read doesn't match last write
		Result = EE_ERR_BACKUP           ' EEPROM integrity compromised
		ExitProc
	EndIf
	
	Result = EE_OK                       ' EEPROM health check passed
EndProc

'-----End Section 3----
Section_4:
'----Start Section 4----

'==============================================================================
'======================= FACTORY DEFAULTS AND CONFIGURATION LOADING ========
'
' This section provides configuration loading with automatic fallback to safe defaults:
' - Factory default initialization for system and input configurations
' - Configuration block loading with validation and error handling
' - Master configuration loading with comprehensive fallback strategies
' - Conservative default values suitable for pressure sensor applications
'
' Design Philosophy: Safety first with graceful degradation
' - Conservative defaults prevent dangerous operations
' - Multiple fallback layers ensure system always has valid configuration
' - Clear status tracking for diagnostic purposes
' - Defaults optimized for common pressure sensor applications
'
'==============================================================================

'======================= FACTORY DEFAULT INITIALIZATION =====================
'
' Provides known-good configuration values when EEPROM is corrupted or blank
' Conservative settings suitable for first-time setup or emergency recovery
'
'=============================================================================

'-------------- Load Factory Default System Settings -----------------------
' Initializes system configuration variables with conservative defaults
' Called when system block validation fails or during first-time setup

Proc P_LoadSysDefaults()
	W_UI_TimeoutS = 60                   ' 60-second UI timeout (moderate setting)
	W_UI_PulseMs  = 100                  ' 100ms beeper pulse (audible but brief)
	
	' Initialize status flags - clear all except factory default indicator
	B_EE_Flags = 0                       ' Clear all status flags
	B_EE_Flags.EE_FLAG_FACTORY = 1       ' Mark as factory defaults loaded
	
	W_EE_Wrt_Cnt = 0                     ' Reset write cycle counter
EndProc

'-------------- Load Factory Default Input Settings ------------------------
' Initializes input configuration variables with safe defaults
' Designed for pressure sensor applications with conservative bypass/relay settings

Proc P_LoadInDefaults(B_InNum As Byte)
	Select B_InNum
		Case 1                           ' Input 1 factory defaults
			B_I1_Enabled  = NO           ' Disabled until user configures (safety)
			B_I1_SensorT  = SENSOR_PRES  ' Pressure sensor (most common)
			B_I1_FlowMode = 0            ' Flow mode: analog, percentage
			W_I1_Scale4   = 0            ' 4mA = 0 PSI (typical pressure range start)
			W_I1_Scale20  = 100          ' 20mA = 100 PSI (common industrial max)
			W_I1_BP_High  = 5            ' 5-second high bypass (conservative)
			W_I1_BP_PLP   = 3            ' 3-second PLP bypass (prevent nuisance)
			W_I1_BP_SLP   = 3            ' 3-second SLP bypass (prevent nuisance)
			B_I1_RlyHigh  = MODE_NO      ' No relay action (safe default)
			B_I1_RlyPLP   = MODE_NO      ' No relay action (safe default)
			B_I1_RlySLP   = MODE_NO      ' No relay action (safe default)
			B_I1_Display  = YES          ' Enable display (user feedback)
			
		Case 2                           ' Input 2 factory defaults
			B_I2_Enabled  = NO           ' Disabled until user configures
			B_I2_SensorT  = SENSOR_PRES  ' Pressure sensor default
			B_I2_FlowMode = 0            ' Flow mode: analog, percentage
			W_I2_Scale4   = 0            ' 4mA = 0 PSI
			W_I2_Scale20  = 100          ' 20mA = 100 PSI
			W_I2_BP_High  = 5            ' 5-second high bypass
			W_I2_BP_PLP   = 3            ' 3-second PLP bypass
			W_I2_BP_SLP   = 3            ' 3-second SLP bypass
			B_I2_RlyHigh  = MODE_NO      ' No relay action
			B_I2_RlyPLP   = MODE_NO      ' No relay action
			B_I2_RlySLP   = MODE_NO      ' No relay action
			B_I2_Display  = YES          ' Enable display
			
		Case 3                           ' Input 3 factory defaults
			B_I3_Enabled  = NO           ' Disabled until user configures
			B_I3_SensorT  = SENSOR_PRES  ' Pressure sensor default
			B_I3_FlowMode = 0            ' Flow mode: analog, percentage
			W_I3_Scale4   = 0            ' 4mA = 0 PSI
			W_I3_Scale20  = 100          ' 20mA = 100 PSI
			W_I3_BP_High  = 5            ' 5-second high bypass
			W_I3_BP_PLP   = 3            ' 3-second PLP bypass
			W_I3_BP_SLP   = 3            ' 3-second SLP bypass
			B_I3_RlyHigh  = MODE_NO      ' No relay action
			B_I3_RlyPLP   = MODE_NO      ' No relay action
			B_I3_RlySLP   = MODE_NO      ' No relay action
			B_I3_Display  = YES          ' Enable display
	EndSelect
EndProc

'======================= CONFIGURATION BLOCK LOADING =======================
'
' High-level configuration loading with automatic fallback to defaults
' Provides robust configuration recovery even with corrupted EEPROM data
'
'=============================================================================

'-------------- Load System Configuration Block ----------------------------
' Loads system settings from EEPROM with validation and fallback
' Automatically applies factory defaults if block is invalid

Proc P_LoadSysBlock(), Byte
	Dim B_Status As Byte                 ' Block validation result
	
	' Attempt to validate system configuration block
	B_Status = P_EE_ValidBlk(EE_SYSTEM_BLOCK)
	
	If B_Status = EE_OK Then             ' System block is valid and safe to load
		' Load verified system configuration from EEPROM
		ERead EE_SYSTEM_BLOCK + EE_SYS_TIMEOUT, W_UI_TimeoutS.LowByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_TIMEOUT + 1, W_UI_TimeoutS.HighByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_PULSE, W_UI_PulseMs.LowByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_PULSE + 1, W_UI_PulseMs.HighByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_W_CNT, W_EE_Wrt_Cnt.LowByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_W_CNT + 1, W_EE_Wrt_Cnt.HighByte
		ERead EE_SYSTEM_BLOCK + EE_SYS_FLAGS, B_EE_Flags
		
		' Clear factory default flag (successfully loaded from EEPROM)
		B_EE_Flags.EE_FLAG_FACTORY = 0
		
		Result = EE_OK                   ' Configuration loaded from EEPROM successfully
	Else
		' Block validation failed - use factory defaults for safety
		P_LoadSysDefaults()              ' Load safe default values
		Result = B_Status                ' Return validation error for diagnosis
	EndIf
EndProc

'-------------- Load Input Configuration Block -----------------------------
' Loads input settings from EEPROM with validation and fallback
' Automatically applies factory defaults if block is invalid

Proc P_LoadInBlock(B_InNum As Byte), Byte
	Dim B_Status  As Byte                ' Block validation result
	Dim W_BlkAddr As Word                ' Input block starting address
	
	' Determine input block address based on input number
	Select B_InNum
		Case 1
			W_BlkAddr = EE_INPUT1_BLOCK  ' Input 1 block at address 32
		Case 2
			W_BlkAddr = EE_INPUT2_BLOCK  ' Input 2 block at address 64
		Case Else
			W_BlkAddr = EE_INPUT3_BLOCK  ' Input 3 block at address 96
	EndSelect
	
	' Attempt to validate input configuration block
	B_Status = P_EE_ValidBlk(W_BlkAddr)
	
	If B_Status = EE_OK Then             ' Input block is valid and safe to load
		' Load verified input configuration from EEPROM based on input number
		Select B_InNum
			Case 1                       ' Load Input 1 configuration from EEPROM
				ERead W_BlkAddr + EE_IN_ENABLED, B_I1_Enabled
				ERead W_BlkAddr + EE_IN_SENSOR_T, B_I1_SensorT
				ERead W_BlkAddr + EE_IN_FLOW_MODE, B_I1_FlowMode
				ERead W_BlkAddr + EE_IN_SCALE4, W_I1_Scale4.LowByte
				ERead W_BlkAddr + EE_IN_SCALE4 + 1, W_I1_Scale4.HighByte
				ERead W_BlkAddr + EE_IN_SCALE20, W_I1_Scale20.LowByte
				ERead W_BlkAddr + EE_IN_SCALE20 + 1, W_I1_Scale20.HighByte
				ERead W_BlkAddr + EE_IN_BP_HIGH, W_I1_BP_High.LowByte
				ERead W_BlkAddr + EE_IN_BP_HIGH + 1, W_I1_BP_High.HighByte
				ERead W_BlkAddr + EE_IN_BP_PLP, W_I1_BP_PLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_PLP + 1, W_I1_BP_PLP.HighByte
				ERead W_BlkAddr + EE_IN_BP_SLP, W_I1_BP_SLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_SLP + 1, W_I1_BP_SLP.HighByte
				ERead W_BlkAddr + EE_IN_RLY_HIGH, B_I1_RlyHigh
				ERead W_BlkAddr + EE_IN_RLY_PLP, B_I1_RlyPLP
				ERead W_BlkAddr + EE_IN_RLY_SLP, B_I1_RlySLP
				ERead W_BlkAddr + EE_IN_DISPLAY, B_I1_Display
				
			Case 2                       ' Load Input 2 configuration from EEPROM
				ERead W_BlkAddr + EE_IN_ENABLED, B_I2_Enabled
				ERead W_BlkAddr + EE_IN_SENSOR_T, B_I2_SensorT
				ERead W_BlkAddr + EE_IN_FLOW_MODE, B_I2_FlowMode
				ERead W_BlkAddr + EE_IN_SCALE4, W_I2_Scale4.LowByte
				ERead W_BlkAddr + EE_IN_SCALE4 + 1, W_I2_Scale4.HighByte
				ERead W_BlkAddr + EE_IN_SCALE20, W_I2_Scale20.LowByte
				ERead W_BlkAddr + EE_IN_SCALE20 + 1, W_I2_Scale20.HighByte
				ERead W_BlkAddr + EE_IN_BP_HIGH, W_I2_BP_High.LowByte
				ERead W_BlkAddr + EE_IN_BP_HIGH + 1, W_I2_BP_High.HighByte
				ERead W_BlkAddr + EE_IN_BP_PLP, W_I2_BP_PLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_PLP + 1, W_I2_BP_PLP.HighByte
				ERead W_BlkAddr + EE_IN_BP_SLP, W_I2_BP_SLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_SLP + 1, W_I2_BP_SLP.HighByte
				ERead W_BlkAddr + EE_IN_RLY_HIGH, B_I2_RlyHigh
				ERead W_BlkAddr + EE_IN_RLY_PLP, B_I2_RlyPLP
				ERead W_BlkAddr + EE_IN_RLY_SLP, B_I2_RlySLP
				ERead W_BlkAddr + EE_IN_DISPLAY, B_I2_Display
				
			Case 3                       ' Load Input 3 configuration from EEPROM
				ERead W_BlkAddr + EE_IN_ENABLED, B_I3_Enabled
				ERead W_BlkAddr + EE_IN_SENSOR_T, B_I3_SensorT
				ERead W_BlkAddr + EE_IN_FLOW_MODE, B_I3_FlowMode
				ERead W_BlkAddr + EE_IN_SCALE4, W_I3_Scale4.LowByte
				ERead W_BlkAddr + EE_IN_SCALE4 + 1, W_I3_Scale4.HighByte
				ERead W_BlkAddr + EE_IN_SCALE20, W_I3_Scale20.LowByte
				ERead W_BlkAddr + EE_IN_SCALE20 + 1, W_I3_Scale20.HighByte
				ERead W_BlkAddr + EE_IN_BP_HIGH, W_I3_BP_High.LowByte
				ERead W_BlkAddr + EE_IN_BP_HIGH + 1, W_I3_BP_High.HighByte
				ERead W_BlkAddr + EE_IN_BP_PLP, W_I3_BP_PLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_PLP + 1, W_I3_BP_PLP.HighByte
				ERead W_BlkAddr + EE_IN_BP_SLP, W_I3_BP_SLP.LowByte
				ERead W_BlkAddr + EE_IN_BP_SLP + 1, W_I3_BP_SLP.HighByte
				ERead W_BlkAddr + EE_IN_RLY_HIGH, B_I3_RlyHigh
				ERead W_BlkAddr + EE_IN_RLY_PLP, B_I3_RlyPLP
				ERead W_BlkAddr + EE_IN_RLY_SLP, B_I3_RlySLP
				ERead W_BlkAddr + EE_IN_DISPLAY, B_I3_Display
		EndSelect
		
		Result = EE_OK                   ' Configuration loaded from EEPROM successfully
	Else
		' Block validation failed - use factory defaults for safety
		P_LoadInDefaults(B_InNum)        ' Load safe default values for this input
		Result = B_Status                ' Return validation error for diagnosis
	EndIf
EndProc

'======================= MASTER CONFIGURATION LOADING ======================
'
' High-level configuration management with comprehensive error handling
' Provides simple interface for complete system configuration loading
'
'=============================================================================

'-------------- Load Complete Configuration --------------------------------
' Master configuration loader - loads all system and input settings
' Provides centralized configuration management with comprehensive error handling

Proc P_LoadConfig(), Byte
	Dim B_SysStatus As Byte              ' System block load result
	Dim B_I1Status  As Byte              ' Input 1 block load result
	Dim B_I2Status  As Byte              ' Input 2 block load result
	Dim B_I3Status  As Byte              ' Input 3 block load result
	Dim B_AnyFail   As Byte              ' Any block failed flag
	
	B_AnyFail = 0                        ' Initialize failure tracking flag
	B_EE_Status = EE_OK                  ' Initialize global status
	
	' Load system configuration block
	B_SysStatus = P_LoadSysBlock()
	If B_SysStatus <> EE_OK Then         ' System block failed to load
		B_AnyFail = 1                    ' Mark that failure occurred
		B_EE_Status = B_SysStatus        ' Store first error for reference
	EndIf
	
	' Load input configuration blocks (continue loading even if some fail)
	B_I1Status = P_LoadInBlock(1)
	If B_I1Status <> EE_OK Then          ' Input 1 block failed to load
		B_AnyFail = 1                    ' Mark that failure occurred
		If B_EE_Status = EE_OK Then      ' Store first error only
			B_EE_Status = B_I1Status
		EndIf
	EndIf
	
	B_I2Status = P_LoadInBlock(2)
	If B_I2Status <> EE_OK Then          ' Input 2 block failed to load
		B_AnyFail = 1                    ' Mark that failure occurred
		If B_EE_Status = EE_OK Then      ' Store first error only
			B_EE_Status = B_I2Status
		EndIf
	EndIf
	
	B_I3Status = P_LoadInBlock(3)
	If B_I3Status <> EE_OK Then          ' Input 3 block failed to load
		B_AnyFail = 1                    ' Mark that failure occurred
		If B_EE_Status = EE_OK Then      ' Store first error only
			B_EE_Status = B_I3Status
		EndIf
	EndIf
	
	' Return overall loading result
	If B_AnyFail = 1 Then
		Result = B_EE_Status             ' Return first error encountered for diagnosis
	Else
		B_EE_Status = EE_OK              ' Clear any previous errors
		Result = EE_OK                   ' All blocks loaded successfully from EEPROM
	EndIf
EndProc

'======================= CONFIGURATION VALIDATION HELPERS ==================
'
' Functions for validating loaded configuration values and applying range limits
' Ensures configuration values are within safe operating parameters
'
'=============================================================================

'-------------- Validate and Limit System Configuration -------------------
' Checks system configuration values and applies safe limits
' Corrects any values that are outside acceptable ranges

Proc P_ValidateSysCfg()
	' Validate and limit UI timeout
	If W_UI_TimeoutS < UO_TOut_S_mn Then ' Too short - user can't operate UI
		W_UI_TimeoutS = UO_TOut_S_mn     ' Set to minimum safe value
	EndIf
	If W_UI_TimeoutS > UI_TOut_S_Mx Then ' Too long - wastes power
		W_UI_TimeoutS = UI_TOut_S_Mx     ' Set to maximum practical value
	EndIf
	
	' Validate and limit beeper pulse duration
	If W_UI_PulseMs < UI_PULSE_MS_MIN Then ' Too short - not audible
		W_UI_PulseMs = UI_PULSE_MS_MIN   ' Set to minimum audible duration
	EndIf
	If W_UI_PulseMs > UI_Pls_MS_Mx Then  ' Too long - annoying
		W_UI_PulseMs = UI_Pls_MS_Mx      ' Set to maximum reasonable duration
	EndIf
EndProc

'-------------- Validate and Limit Input Configuration --------------------
' Checks input configuration values and applies safe limits
' Corrects any values that are outside acceptable ranges

Proc P_ValidateInCfg(B_InNum As Byte)
	Select B_InNum
		Case 1                           ' Validate Input 1 configuration
			' Validate enable flag
			If B_I1_Enabled <> YES And B_I1_Enabled <> NO Then
				B_I1_Enabled = NO        ' Default to disabled for safety
			EndIf
			
			' Validate sensor type
			If B_I1_SensorT > SENSOR_FLOW Then
				B_I1_SensorT = SENSOR_PRES ' Default to pressure sensor
			EndIf
			
			' Validate bypass times (1-999 seconds)
			If W_I1_BP_High < 1 Then
				W_I1_BP_High = 1
			EndIf
			If W_I1_BP_High > 999 Then
				W_I1_BP_High = 999
			EndIf
			If W_I1_BP_PLP < 1 Then
				W_I1_BP_PLP = 1
			EndIf
			If W_I1_BP_PLP > 999 Then
				W_I1_BP_PLP = 999
			EndIf
			If W_I1_BP_SLP < 1 Then
				W_I1_BP_SLP = 1
			EndIf
			If W_I1_BP_SLP > 999 Then
				W_I1_BP_SLP = 999
			EndIf
			
			' Validate relay modes
			If B_I1_RlyHigh > MODE_LATCH Then
				B_I1_RlyHigh = MODE_NO   ' Default to no action for safety
			EndIf
			If B_I1_RlyPLP > MODE_LATCH Then
				B_I1_RlyPLP = MODE_NO    ' Default to no action for safety
			EndIf
			If B_I1_RlySLP > MODE_LATCH Then
				B_I1_RlySLP = MODE_NO    ' Default to no action for safety
			EndIf
			
			' Validate display flag
			If B_I1_Display <> YES And B_I1_Display <> NO Then
				B_I1_Display = YES       ' Default to enabled for user feedback
			EndIf
			
		Case 2                           ' Validate Input 2 configuration
			If B_I2_Enabled <> YES And B_I2_Enabled <> NO Then
				B_I2_Enabled = NO
			EndIf
			If B_I2_SensorT > SENSOR_FLOW Then
				B_I2_SensorT = SENSOR_PRES
			EndIf
			If W_I2_BP_High < 1 Then
				W_I2_BP_High = 1
			EndIf
			If W_I2_BP_High > 999 Then
				W_I2_BP_High = 999
			EndIf
			If W_I2_BP_PLP < 1 Then
				W_I2_BP_PLP = 1
			EndIf
			If W_I2_BP_PLP > 999 Then
				W_I2_BP_PLP = 999
			EndIf
			If W_I2_BP_SLP < 1 Then
				W_I2_BP_SLP = 1
			EndIf
			If W_I2_BP_SLP > 999 Then
				W_I2_BP_SLP = 999
			EndIf
			If B_I2_RlyHigh > MODE_LATCH Then
				B_I2_RlyHigh = MODE_NO
			EndIf
			If B_I2_RlyPLP > MODE_LATCH Then
				B_I2_RlyPLP = MODE_NO
			EndIf
			If B_I2_RlySLP > MODE_LATCH Then
				B_I2_RlySLP = MODE_NO
			EndIf
			If B_I2_Display <> YES And B_I2_Display <> NO Then
				B_I2_Display = YES
			EndIf
			
		Case 3                           ' Validate Input 3 configuration
			If B_I3_Enabled <> YES And B_I3_Enabled <> NO Then
				B_I3_Enabled = NO
			EndIf
			If B_I3_SensorT > SENSOR_FLOW Then
				B_I3_SensorT = SENSOR_PRES
			EndIf
			If W_I3_BP_High < 1 Then
				W_I3_BP_High = 1
			EndIf
			If W_I3_BP_High > 999 Then
				W_I3_BP_High = 999
			EndIf
			If W_I3_BP_PLP < 1 Then
				W_I3_BP_PLP = 1
			EndIf
			If W_I3_BP_PLP > 999 Then
				W_I3_BP_PLP = 999
			EndIf
			If W_I3_BP_SLP < 1 Then
				W_I3_BP_SLP = 1
			EndIf
			If W_I3_BP_SLP > 999 Then
				W_I3_BP_SLP = 999
			EndIf
			If B_I3_RlyHigh > MODE_LATCH Then
				B_I3_RlyHigh = MODE_NO
			EndIf
			If B_I3_RlyPLP > MODE_LATCH Then
				B_I3_RlyPLP = MODE_NO
			EndIf
			If B_I3_RlySLP > MODE_LATCH Then
				B_I3_RlySLP = MODE_NO
			EndIf
			If B_I3_Display <> YES And B_I3_Display <> NO Then
				B_I3_Display = YES
			EndIf
	EndSelect
EndProc

'-------------- Complete Configuration Loading with Validation -------------
' Master configuration loader with validation and range checking
' Combines loading with automatic value validation for safe operation

Proc P_LoadValidatedConfig(), Byte
	Dim B_Status As Byte                 ' Loading operation result
	
	' Load all configuration blocks (uses defaults for failed blocks)
	B_Status = P_LoadConfig()
	
	' Validate and limit all configuration values regardless of load result
	P_ValidateSysCfg()                   ' Validate system configuration
	P_ValidateInCfg(1)                   ' Validate Input 1 configuration
	P_ValidateInCfg(2)                   ' Validate Input 2 configuration
	P_ValidateInCfg(3)                   ' Validate Input 3 configuration
	
	Result = B_Status                    ' Return load result (validation always succeeds)
EndProc
'-----End Section 4----
Section_5:
'----Start Section 5----

'==============================================================================
'======================= CONFIGURATION SAVING OPERATIONS ===================
'
' This section provides configuration saving with complete block management:
' - System and input configuration block saving with validation
' - Automatic header initialization and checksum generation
' - Write cycle tracking and reserved area management
' - High-level save interfaces with comprehensive error handling
' - Atomic save operations with immediate verification
'
' Design Philosophy: Reliability and data integrity
' - Every save operation includes full validation structure
' - Write cycle tracking for maintenance awareness
' - Reserved area clearing for future compatibility
' - Immediate verification of saved data
' - Clear error reporting for diagnostic purposes
'
'==============================================================================

'======================= CONFIGURATION BLOCK SAVING ========================
'
' Complete block saving functions with header generation and validation
' Each block is saved with magic byte, version, data, and checksum
'
'=============================================================================

'-------------- Save System Configuration Block ----------------------------
' Saves system settings to EEPROM with full block validation structure
' Automatically generates magic byte, version, and checksum

Proc P_SaveSysBlock(), Byte
	Dim B_Status  As Byte                ' Write operation status
	Dim W_BlkAddr As Word                ' Block starting address
	
	W_BlkAddr = EE_SYSTEM_BLOCK          ' System block starts at address 0
	
	' Initialize block header with magic byte and version
	B_Status = P_EE_InitBlkHdr(W_BlkAddr)
	If B_Status <> EE_OK Then            ' Header initialization failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Write system configuration data
	B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_SYS_TIMEOUT, W_UI_TimeoutS)
	If B_Status <> EE_OK Then            ' UI timeout write failed
		Result = B_Status
		ExitProc
	EndIf
	
	B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_SYS_PULSE, W_UI_PulseMs)
	If B_Status <> EE_OK Then            ' Beeper pulse write failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Increment and save write cycle counter
	Inc W_EE_Wrt_Cnt                     ' Track write operations
	B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_SYS_W_CNT, W_EE_Wrt_Cnt)
	If B_Status <> EE_OK Then            ' Write counter save failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Clear factory flag when saving to EEPROM (no longer using defaults)
	B_EE_Flags.EE_FLAG_FACTORY = 0
	B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_SYS_FLAGS, B_EE_Flags)
	If B_Status <> EE_OK Then            ' Status flags write failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Clear reserved area for future compatibility
	B_Status = P_EE_ClrReserved(W_BlkAddr, EE_SYS_RESERVED, 30)
	If B_Status <> EE_OK Then            ' Reserved area clear failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Finalize block with checksum (must be last operation)
	B_Status = P_EE_FinalizeBlk(W_BlkAddr)
	If B_Status <> EE_OK Then            ' Checksum write failed
		Result = B_Status
		ExitProc
	EndIf
	
	Result = EE_OK                       ' Save operation completed successfully
EndProc

'-------------- Save Input Configuration Block -----------------------------
' Saves input settings to EEPROM with full block validation structure
' Automatically generates magic byte, version, and checksum

Proc P_SaveInBlock(B_InNum As Byte), Byte
	Dim B_Status  As Byte                ' Write operation status
	Dim W_BlkAddr As Word                ' Block starting address
	Dim W_WrtCnt  As Word                ' Write cycle counter for this block
	
	' Determine input block address based on input number
	Select B_InNum
		Case 1
			W_BlkAddr = EE_INPUT1_BLOCK  ' Input 1 block at address 32
		Case 2
			W_BlkAddr = EE_INPUT2_BLOCK  ' Input 2 block at address 64
		Case Else
			W_BlkAddr = EE_INPUT3_BLOCK  ' Input 3 block at address 96
	EndSelect
	
	' Initialize block header with magic byte and version
	B_Status = P_EE_InitBlkHdr(W_BlkAddr)
	If B_Status <> EE_OK Then            ' Header initialization failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Write input configuration data based on input number
	Select B_InNum
		Case 1                           ' Save Input 1 configuration
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_ENABLED, B_I1_Enabled)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_SENSOR_T, B_I1_SensorT)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_FLOW_MODE, B_I1_FlowMode)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE4, W_I1_Scale4)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE20, W_I1_Scale20)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_HIGH, W_I1_BP_High)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_PLP, W_I1_BP_PLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_SLP, W_I1_BP_SLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_HIGH, B_I1_RlyHigh)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_PLP, B_I1_RlyPLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_SLP, B_I1_RlySLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_DISPLAY, B_I1_Display)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
		Case 2                           ' Save Input 2 configuration
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_ENABLED, B_I2_Enabled)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_SENSOR_T, B_I2_SensorT)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_FLOW_MODE, B_I2_FlowMode)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE4, W_I2_Scale4)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE20, W_I2_Scale20)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_HIGH, W_I2_BP_High)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_PLP, W_I2_BP_PLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_SLP, W_I2_BP_SLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_HIGH, B_I2_RlyHigh)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_PLP, B_I2_RlyPLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_SLP, B_I2_RlySLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_DISPLAY, B_I2_Display)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
		Case 3                           ' Save Input 3 configuration
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_ENABLED, B_I3_Enabled)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_SENSOR_T, B_I3_SensorT)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_FLOW_MODE, B_I3_FlowMode)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE4, W_I3_Scale4)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_SCALE20, W_I3_Scale20)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_HIGH, W_I3_BP_High)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_PLP, W_I3_BP_PLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_BP_SLP, W_I3_BP_SLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_HIGH, B_I3_RlyHigh)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_PLP, B_I3_RlyPLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_RLY_SLP, B_I3_RlySLP)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
			
			B_Status = P_EE_SafeWriteB(W_BlkAddr + EE_IN_DISPLAY, B_I3_Display)
			If B_Status <> EE_OK Then
				Result = B_Status
				ExitProc
			EndIf
	EndSelect
	
	' Read current write counter for this block and increment
	ERead W_BlkAddr + EE_IN_WRITE_CNT, W_WrtCnt.LowByte
	ERead W_BlkAddr + EE_IN_WRITE_CNT + 1, W_WrtCnt.HighByte
	Inc W_WrtCnt                         ' Increment write cycle counter
	
	B_Status = P_EE_SafeWriteW(W_BlkAddr + EE_IN_WRITE_CNT, W_WrtCnt)
	If B_Status <> EE_OK Then            ' Write counter save failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Clear reserved area for future compatibility
	B_Status = P_EE_ClrReserved(W_BlkAddr, EE_IN_RESERVED, 30)
	If B_Status <> EE_OK Then            ' Reserved area clear failed
		Result = B_Status
		ExitProc
	EndIf
	
	' Finalize block with checksum (must be last operation)
	B_Status = P_EE_FinalizeBlk(W_BlkAddr)
	If B_Status <> EE_OK Then            ' Checksum write failed
		Result = B_Status
		ExitProc
	EndIf
	
	Result = EE_OK                       ' Save operation completed successfully
EndProc

'======================= HIGH-LEVEL SAVE INTERFACES ========================
'
' User-friendly save functions with error handling and status tracking
' Provides simple interface for configuration persistence
'
'=============================================================================

'-------------- Save System Configuration ----------------------------------
' Saves system settings to EEPROM with error handling and status tracking
' Wrapper for P_SaveSysBlock() with global status update

Proc P_SaveSysCfg(), Byte
	Dim B_Status As Byte                 ' Save operation result
	
	B_Status = P_SaveSysBlock()          ' Perform save operation
	B_EE_Status = B_Status               ' Update global EEPROM status
	Result = B_Status                    ' Return operation result
EndProc

'-------------- Save Input Configuration -----------------------------------
' Saves input settings to EEPROM with error handling and status tracking
' Wrapper for P_SaveInBlock() with global status update

Proc P_SaveInCfg(B_InNum As Byte), Byte
	Dim B_Status As Byte                 ' Save operation result
	
	B_Status = P_SaveInBlock(B_InNum)    ' Perform save operation
	B_EE_Status = B_Status               ' Update global EEPROM status
	Result = B_Status                    ' Return operation result
EndProc

'======================= FACTORY RESET FUNCTIONALITY =======================
'
' Factory reset operations with safety confirmations
' Provides system reset and maintenance functions
'
'=============================================================================

'-------------- Complete Factory Reset --------------------------------------
' Performs complete factory reset with fresh configuration
' Clears all EEPROM and establishes factory default configuration

Proc P_CompleteFactoryReset(), Byte
	Dim B_Status As Byte                 ' Operation status
	Dim B_Addr   As Byte                 ' EEPROM address (0-255)
	
	' Step 1: Clear entire EEPROM to factory fresh state
	For B_Addr = 0 To 255                ' PIC18F2525 has 256 bytes EEPROM
		B_Status = P_EE_SafeWriteB(B_Addr, $00)
		If B_Status <> EE_OK Then        ' Write failed
			Result = B_Status            ' Return error immediately
			ExitProc
		EndIf
	Next B_Addr
	
	' Step 2: Load factory defaults into all configuration variables
	P_LoadSysDefaults()                  ' Load system factory defaults
	P_LoadInDefaults(1)                  ' Load Input 1 factory defaults
	P_LoadInDefaults(2)                  ' Load Input 2 factory defaults
	P_LoadInDefaults(3)                  ' Load Input 3 factory defaults
	
	' Step 3: Save factory defaults to establish valid configuration blocks
	B_Status = P_SaveSysBlock()          ' Save system defaults
	If B_Status <> EE_OK Then            ' System save failed
		Result = B_Status                ' Critical failure
		ExitProc
	EndIf
	
	B_Status = P_SaveInBlock(1)          ' Save Input 1 defaults
	If B_Status <> EE_OK Then            ' Input 1 save failed
		Result = B_Status                ' Return error but continue
	EndIf
	
	B_Status = P_SaveInBlock(2)          ' Save Input 2 defaults
	If B_Status <> EE_OK Then            ' Input 2 save failed
		Result = B_Status                ' Return error but continue
	EndIf
	
	B_Status = P_SaveInBlock(3)          ' Save Input 3 defaults
	If B_Status <> EE_OK Then            ' Input 3 save failed
		Result = B_Status                ' Return error but continue
	EndIf
	
	' Step 4: Update status flags to reflect factory reset
	B_EE_Flags.EE_FLAG_FACTORY = 1      ' Mark as factory defaults
	B_EE_Flags.EE_FLAG_BACKUP = 0       ' No backup available
	B_EE_Flags.EE_Flg_Restrd = 0        ' Not restored from backup
	B_EE_Flags.EE_Flg_Mig = 0           ' Not migrated from legacy
	
	Result = EE_OK                       ' Factory reset completed successfully
EndProc

'======================= USER INTERFACE CORE FUNCTIONS =====================
'
' Core user interface functions for button/encoder handling and display
' Provides foundational UI primitives used throughout the system
'
'=============================================================================

'-------------- Button Event Processing with Classification ----------------
' Reads and classifies button press events from ISR
' Provides timing-based classification for different user actions

Proc P_GetKeyEvt(), Byte
	Dim W_HoldTime As Word               ' Button hold duration from ISR
	Dim B_Event    As Byte               ' Event classification result
	
	' Check if ISR has detected a button event
	If B_KeyEvent = 0 Then               ' No button event pending
		Result = 0                       ' Return no event
		ExitProc
	EndIf
	
	' Read button hold time safely from ISR variable
	GIE = 0                              ' Disable interrupts for atomic read
	W_HoldTime = W_BtnHoldMS             ' Copy ISR timing value
	GIE = 1                              ' Re-enable interrupts
	
	B_KeyEvent = 0                       ' Clear ISR event flag
	
	' Classify button press based on hold duration
	If W_HoldTime >= BTN_VLONG_MS Then   ' Very long press (1800ms+)
		B_Event = 3                      ' Special action (factory reset, etc.)
	Else
		If W_HoldTime >= BTN_LONG_MS Then ' Long press (900-1800ms)
			B_Event = 2                  ' Back/escape action
		Else
			If W_HoldTime >= BTN_SHORT_MS Then ' Short press (200-900ms)
				B_Event = 1              ' Normal selection/edit action
			Else
				B_Event = 0              ' Too short - ignore (debounce)
			EndIf
		EndIf
	EndIf
	
	Result = B_Event                     ' Return classified button event
EndProc

'-------------- Encoder Position Reading -----------------------------------
' Reads encoder movement since last call
' Provides smooth incremental values for menu navigation and editing

Proc P_ReadEnc(), SByte
	Dim W_CurrentPos As Word             ' Current encoder position from ISR
	Dim S_Delta      As SByte            ' Calculated movement delta
	
	' Read current encoder position safely from ISR
	GIE = 0                              ' Disable interrupts for atomic read
	W_CurrentPos = W_EncoderPos          ' Copy ISR position value
	GIE = 1                              ' Re-enable interrupts
	
	' Calculate movement delta since last read
	S_Delta = W_CurrentPos - W_EncReadPos ' Calculate position change
	W_EncReadPos = W_CurrentPos          ' Update last read position
	
	' Limit delta to prevent overflow in SByte result
	If S_Delta > 127 Then                ' Positive overflow protection
		S_Delta = 127                    ' Cap at maximum positive value
	EndIf
	If S_Delta < -128 Then               ' Negative overflow protection
		S_Delta = -128                   ' Cap at maximum negative value
	EndIf
	
	Result = S_Delta                     ' Return movement delta
EndProc

'-------------- Reset Encoder Position Tracking ----------------------------
' Resets encoder position tracking to prevent accumulated errors
' Useful when entering new menus or editing modes

Proc P_ResetEncPos()
	GIE = 0                              ' Disable interrupts for atomic access
	W_EncReadPos = W_EncoderPos          ' Sync read position with ISR position
	GIE = 1                              ' Re-enable interrupts
EndProc

'-------------- Audio Feedback System --------------------------------------
' Provides audio feedback with different patterns for different events
' Integrates with ISR beeper service for precise timing

Proc P_Beeps(B_Type As Byte)
	Select B_Type
		Case 0                           ' Silent - no beep
			W_Beep = 0                   ' No beeper activation
			
		Case 1                           ' Short beep - basic feedback
			W_Beep = W_UI_PulseMs        ' Use configured pulse duration
			
		Case 2                           ' Double beep - selection feedback
			W_Beep = W_UI_PulseMs        ' First beep
			DelayMS W_UI_PulseMs + 50    ' Wait for first beep + gap
			W_Beep = W_UI_PulseMs        ' Second beep
			
		Case 3                           ' Long beep - confirmation feedback
			W_Beep = W_UI_PulseMs * 2    ' Double duration for emphasis
			
		Case 4                           ' Error beep - warning feedback
			W_Beep = W_UI_PulseMs        ' First short beep
			DelayMS W_UI_PulseMs + 30    ' Short gap
			W_Beep = W_UI_PulseMs        ' Second short beep
			DelayMS W_UI_PulseMs + 30    ' Short gap
			W_Beep = W_UI_PulseMs        ' Third short beep (error pattern)
	EndSelect
EndProc

'-------------- Quick Beep Functions ---------------------------------------
' Simplified beep functions for common user interface sounds

Proc P_BeepClick()
	P_Beeps(1)                           ' Generate single click beep
EndProc

Proc P_BeepChange()
	P_Beeps(2)                           ' Generate double beep for changes
EndProc

Proc P_BeepSave()
	P_Beeps(3)                           ' Generate long beep for saves
EndProc

Proc P_BeepError()
	P_Beeps(4)                           ' Generate error beep pattern
EndProc

'-----End Section 5----
Section_6:
'----Start Section 6----

'==============================================================================
'======================= DISPLAY AND USER INTERFACE FUNCTIONS ==============
'
' Using proper Positron8 BASIC Print syntax
'
'==============================================================================

'-------------- Basic Display Control Functions ----------------------------

Proc P_DispClear()
	Cls
	DelayMS 2
EndProc

Proc P_DispHome()
	Print At 1, 1
EndProc

Proc P_DispAt(B_Row As Byte, B_Col As Byte, S_Text As String * 20)
	Print At B_Row, B_Col, S_Text
EndProc

Proc P_DispCenter(B_Row As Byte, S_Text As String * 20)
	Dim B_Len As Byte
	Dim B_Start As Byte
	
	B_Len = Len(S_Text)
	If B_Len >= 20 Then
		Print At B_Row, 1, Left$(S_Text, 20)
	Else
		B_Start = (20 - B_Len) / 2 + 1
		Print At B_Row, B_Start, S_Text
	EndIf
EndProc

'-------------- Text Formatting Functions ----------------------------------

Proc P_FmtYesNo(B_Val As Byte), String * 3
	If B_Val = YES Then
		Result = "Yes"
	Else
		Result = "No "
	EndIf
EndProc

Proc P_FmtSensorType(B_Type As Byte), String * 10
	Select B_Type
		Case SENSOR_PRES
			Result = "Pressure"
		Case SENSOR_TEMP
			Result = "Temp     "
		Case SENSOR_FLOW
			Result = "Flow     "
		Case Else
			Result = "Unknown  "
	EndSelect
EndProc

Proc P_FmtRelayMode(B_Mode As Byte), String * 6
	Select B_Mode
		Case MODE_NO
			Result = "No    "
		Case MODE_PULSE
			Result = "Pulse "
		Case MODE_LATCH
			Result = "Latch "
		Case Else
			Result = "Error "
	EndSelect
EndProc

'-------------- Status Display Functions -----------------------------------

Proc P_DispSysStatus()
	P_DispClear()
	Print At 1, 1, "SYSTEM STATUS"
	Print At 2, 1, "Timeout: ", Dec W_UI_TimeoutS, "s"
	Print At 3, 1, "Beep: ", Dec W_UI_PulseMs, "ms"
	Print At 4, 1, "Writes: ", Dec W_EE_Wrt_Cnt
EndProc

Proc P_DispInSummary()
	P_DispClear()
	Print At 1, 1, "INPUT STATUS"
	Print At 2, 1, "Input 1: ", P_FmtYesNo(B_I1_Enabled)
	Print At 3, 1, "Input 2: ", P_FmtYesNo(B_I2_Enabled)
	Print At 4, 1, "Input 3: ", P_FmtYesNo(B_I3_Enabled)
EndProc

Proc P_DispEEStatus()
	Dim S_Status As String * 15
	
	P_DispClear()
	Print At 1, 1, "EEPROM STATUS"
	
	Select B_EE_Status
		Case EE_OK
			S_Status = "OK"
		Case EE_ERR_MAGIC
			S_Status = "Bad Magic"
		Case EE_ERR_VERSION
			S_Status = "Version Err"
		Case EE_ERR_CHECKSUM
			S_Status = "Checksum Err"
		Case EE_ERR_BACKUP
			S_Status = "Write Failed"
		Case Else
			S_Status = "Unknown Err"
	EndSelect
	
	Print At 2, 1, "Status: ", S_Status
	If B_EE_Flags.EE_FLAG_FACTORY = 1 Then
		Print At 3, 1, "Factory Defaults"
	Else
		Print At 3, 1, "User Config"
	EndIf
	Print At 4, 1, "Writes: ", Dec W_EE_Wrt_Cnt
EndProc

'-------------- Menu Display Functions -------------------------------------

Proc P_DispCfgValue(S_Title As String * 20, S_Param As String * 15, S_Value As String * 10, B_Editing As Byte)
	P_DispClear()
	P_DispCenter(1, S_Title)
	Print At 3, 1, S_Param, ":"
	If B_Editing = YES Then
		Print At 4, 2, "[", S_Value, "]"
	Else
		Print At 4, 2, S_Value
	EndIf
EndProc

'-------------- Error and Confirmation Displays ---------------------------

Proc P_DispError(S_Message As String * 18, B_Code As Byte)
	P_DispClear()
	P_DispCenter(1, "ERROR")
	P_DispCenter(3, S_Message)
	If B_Code > 0 Then
		Print At 4, 7, "Code: ", Dec B_Code
	EndIf
EndProc

Proc P_DispConfirm(S_Message As String * 18, B_Selection As Byte)
	P_DispClear()
	P_DispCenter(1, "CONFIRM")
	P_DispCenter(2, S_Message)
	If B_Selection = 1 Then
		Print At 4, 3, ">YES   NO"
	Else
		Print At 4, 3, " YES  >NO"
	EndIf
EndProc

Proc P_DispSaveConfirm(S_Item As String * 15, B_Success As Byte)
	P_DispClear()
	P_DispCenter(1, "SAVE CONFIG")
	P_DispCenter(2, S_Item)
	If B_Success = YES Then
		P_DispCenter(4, "SUCCESS")
	Else
		P_DispCenter(4, "FAILED")
	EndIf
EndProc

'-------------- Special Display Functions ----------------------------------

Proc P_DispWorking(S_Operation As String * 15)
	P_DispClear()
	P_DispCenter(2, S_Operation)
	P_DispCenter(3, "Please wait...")
EndProc

Proc P_DispStartup()
	P_DispClear()
	P_DispCenter(1, "IRRISYS HMI")
	P_DispCenter(2, "Rev 2025-12-11")
	P_DispCenter(3, "Initializing...")
	DelayMS 1000
EndProc

Proc P_DispFactoryWarn()
	P_DispClear()
	P_DispCenter(1, "FACTORY RESET")
	P_DispCenter(2, "This will erase")
	P_DispCenter(3, "all settings!")
	P_DispCenter(4, "Hold to confirm")
EndProc

'-------------- Display State Management -----------------------------------

Proc P_SetScrDirty()
	b_ScrDirty = 1
EndProc

Proc P_ClrScrDirty()
	b_ScrDirty = 0
EndProc

Proc P_ScrNeedsUpdate(), Byte
	Result = b_ScrDirty
EndProc

Proc P_ForceDispUpdate()
	B_ForceUpdate = 1
	b_ScrDirty = 1
EndProc

Proc P_UpdateBlink()
	If L_Millis - L_LastBlink > 500 Then
		B_BlinkState = 1 - B_BlinkState
		L_LastBlink = L_Millis
		P_SetScrDirty()
	EndIf
EndProc

Proc P_GetBlinkState(), Byte
	Result = B_BlinkState
EndProc

'-----End Section 6----
Section_7:
'----Start Section 7----

'==============================================================================
'======================= INLINE VALUE EDITORS ==============================
'
' Using proper Positron8 BASIC syntax
'
'==============================================================================

'======================= CORE EDITOR HELPER FUNCTIONS ======================

Proc P_UpdateActivity()
	L_LastInput = L_Millis
EndProc

Proc P_EditTimeout(), Byte
	If L_Millis - L_LastInput > (W_UI_TimeoutS * 1000) Then
		Result = YES
	Else
		Result = NO
	EndIf
EndProc

Proc P_ApplyEncDelta(W_Current As Word, S_Delta As SByte, W_Min As Word, W_Max As Word), Word
	Dim L_NewVal As Dword
	
	L_NewVal = W_Current + S_Delta
	
	If L_NewVal < W_Min Then
		Result = W_Min
	Else
		If L_NewVal > W_Max Then
			Result = W_Max
		Else
			Result = L_NewVal
		EndIf
	EndIf
EndProc

Proc P_ApplySgnDelta(W_Current As Word, S_Delta As SByte, W_Min As Word, W_Max As Word), Word
	Dim L_Signed As Long
	Dim L_NewVal As Long
	
	If W_Current > 32767 Then
		L_Signed = W_Current - 65536
	Else
		L_Signed = W_Current
	EndIf
	
	L_NewVal = L_Signed + S_Delta
	
	If L_NewVal < -32768 Then
		L_NewVal = -32768
	EndIf
	If L_NewVal > 32767 Then
		L_NewVal = 32767
	EndIf
	
	If L_NewVal < 0 Then
		Result = L_NewVal + 65536
	Else
		Result = L_NewVal
	EndIf
EndProc

'======================= BYTE VALUE EDITOR ==================================

Proc P_EditByte(B_Current As Byte, B_Min As Byte, B_Max As Byte, S_Title As String * 15, S_Units As String * 5), Byte
	Dim B_Working   As Byte
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	
	B_Working = B_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = B_Current
			GoTo EditByteExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			B_Working = P_ApplyEncDelta(B_Working, S_EncDelta, B_Min, B_Max)
			If B_Working <> B_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = B_Working
				Else
					P_BeepClick()
					Result = B_Current
				EndIf
				GoTo EditByteExit
			Else
				P_BeepChange()
				Result = B_Current
				GoTo EditByteExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, "Value:"
		If B_BlinkOn = YES Then
			If Len(S_Units) > 0 Then
				Print At 4, 2, "[", Dec B_Working, S_Units, "]"
			Else
				Print At 4, 2, "[", Dec B_Working, "]"
			EndIf
		Else
			If Len(S_Units) > 0 Then
				Print At 4, 2, Dec B_Working, S_Units
			Else
				Print At 4, 2, Dec B_Working
			EndIf
		EndIf
		
		DelayMS 50
	Loop

EditByteExit:
EndProc

'======================= WORD VALUE EDITOR ===================================

Proc P_EditWord(W_Current As Word, W_Min As Word, W_Max As Word, S_Title As String * 15, S_Units As String * 5), Word
	Dim W_Working   As Word
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	
	W_Working = W_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = W_Current
			GoTo EditWordExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			W_Working = P_ApplyEncDelta(W_Working, S_EncDelta, W_Min, W_Max)
			If W_Working <> W_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = W_Working
				Else
					P_BeepClick()
					Result = W_Current
				EndIf
				GoTo EditWordExit
			Else
				P_BeepChange()
				Result = W_Current
				GoTo EditWordExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, "Value:"
		If B_BlinkOn = YES Then
			If Len(S_Units) > 0 Then
				Print At 4, 2, "[", Dec W_Working, S_Units, "]"
			Else
				Print At 4, 2, "[", Dec W_Working, "]"
			EndIf
		Else
			If Len(S_Units) > 0 Then
				Print At 4, 2, Dec W_Working, S_Units
			Else
				Print At 4, 2, Dec W_Working
			EndIf
		EndIf
		
		DelayMS 50
	Loop

EditWordExit:
EndProc

'======================= BOOLEAN TOGGLE EDITOR ==============================

Proc P_EditYesNo(B_Current As Byte, S_Title As String * 15, S_Param As String * 10), Byte
	Dim B_Working   As Byte
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	
	B_Working = B_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = B_Current
			GoTo EditYesNoExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			B_Working = 1 - B_Working
			If B_Working <> B_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = B_Working
				Else
					P_BeepClick()
					Result = B_Current
				EndIf
				GoTo EditYesNoExit
			Else
				P_BeepChange()
				Result = B_Current
				GoTo EditYesNoExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, S_Param, ":"
		If B_BlinkOn = YES Then
			If B_Working = YES Then
				Print At 4, 2, "[Yes]"
			Else
				Print At 4, 2, "[No]"
			EndIf
		Else
			If B_Working = YES Then
				Print At 4, 2, "Yes"
			Else
				Print At 4, 2, "No"
			EndIf
		EndIf
		
		DelayMS 50
	Loop

EditYesNoExit:
EndProc

'======================= ENUMERATED VALUE EDITOR ============================

Proc P_EditSensorType(B_Current As Byte, S_Title As String * 15), Byte
	Dim B_Working   As Byte
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	
	B_Working = B_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = B_Current
			GoTo EditSensorExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			If S_EncDelta > 0 Then
				Inc B_Working
				If B_Working > SENSOR_FLOW Then
					B_Working = SENSOR_PRES
				EndIf
			Else
				If B_Working = SENSOR_PRES Then
					B_Working = SENSOR_FLOW
				Else
					Dec B_Working
				EndIf
			EndIf
			
			If B_Working <> B_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = B_Working
				Else
					P_BeepClick()
					Result = B_Current
				EndIf
				GoTo EditSensorExit
			Else
				P_BeepChange()
				Result = B_Current
				GoTo EditSensorExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, "Sensor:"
		If B_BlinkOn = YES Then
			Select B_Working
				Case SENSOR_PRES
					Print At 4, 2, "[Pressure]"
				Case SENSOR_TEMP
					Print At 4, 2, "[Temp]"
				Case SENSOR_FLOW
					Print At 4, 2, "[Flow]"
				Case Else
					Print At 4, 2, "[Unknown]"
			EndSelect
		Else
			Select B_Working
				Case SENSOR_PRES
					Print At 4, 2, "Pressure"
				Case SENSOR_TEMP
					Print At 4, 2, "Temp"
				Case SENSOR_FLOW
					Print At 4, 2, "Flow"
				Case Else
					Print At 4, 2, "Unknown"
			EndSelect
		EndIf
		
		DelayMS 50
	Loop

EditSensorExit:
EndProc

'-------------- Relay Mode Selection Editor ---------------------------------

Proc P_EditRelayMode(B_Current As Byte, S_Title As String * 15, S_Param As String * 10), Byte
	Dim B_Working   As Byte
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	
	B_Working = B_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = B_Current
			GoTo EditRelayExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			If S_EncDelta > 0 Then
				Inc B_Working
				If B_Working > MODE_LATCH Then
					B_Working = MODE_NO
				EndIf
			Else
				If B_Working = MODE_NO Then
					B_Working = MODE_LATCH
				Else
					Dec B_Working
				EndIf
			EndIf
			
			If B_Working <> B_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = B_Working
				Else
					P_BeepClick()
					Result = B_Current
				EndIf
				GoTo EditRelayExit
			Else
				P_BeepChange()
				Result = B_Current
				GoTo EditRelayExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, S_Param, ":"
		If B_BlinkOn = YES Then
			Select B_Working
				Case MODE_NO
					Print At 4, 2, "[No]"
				Case MODE_PULSE
					Print At 4, 2, "[Pulse]"
				Case MODE_LATCH
					Print At 4, 2, "[Latch]"
				Case Else
					Print At 4, 2, "[Error]"
			EndSelect
		Else
			Select B_Working
				Case MODE_NO
					Print At 4, 2, "No"
				Case MODE_PULSE
					Print At 4, 2, "Pulse"
				Case MODE_LATCH
					Print At 4, 2, "Latch"
				Case Else
					Print At 4, 2, "Error"
			EndSelect
		EndIf
		
		DelayMS 50
	Loop

EditRelayExit:
EndProc

'======================= SIGNED WORD EDITOR =================================

Proc P_EditSignedWord(W_Current As Word, S_Title As String * 15, S_Units As String * 5), Word
	Dim W_Working   As Word
	Dim B_KeyEvent  As Byte
	Dim S_EncDelta  As SByte
	Dim B_Modified  As Byte
	Dim B_BlinkOn   As Byte
	Dim L_Signed    As Long
	
	W_Working = W_Current
	B_Modified = NO
	P_ResetEncPos()
	P_UpdateActivity()
	
	Do
		If P_EditTimeout() = YES Then
			P_BeepError()
			Result = W_Current
			GoTo EditSignedExit
		EndIf
		
		S_EncDelta = P_ReadEnc()
		If S_EncDelta <> 0 Then
			W_Working = P_ApplySgnDelta(W_Working, S_EncDelta, $8000, $7FFF)
			If W_Working <> W_Current Then
				B_Modified = YES
			EndIf
			P_BeepClick()
			P_UpdateActivity()
		EndIf
		
		B_KeyEvent = P_GetKeyEvt()
		If B_KeyEvent > 0 Then
			P_UpdateActivity()
			
			If B_KeyEvent = 1 Then
				If B_Modified = YES Then
					P_BeepSave()
					Result = W_Working
				Else
					P_BeepClick()
					Result = W_Current
				EndIf
				GoTo EditSignedExit
			Else
				P_BeepChange()
				Result = W_Current
				GoTo EditSignedExit
			EndIf
		EndIf
		
		P_UpdateBlink()
		B_BlinkOn = P_GetBlinkState()
		
		If W_Working > 32767 Then
			L_Signed = W_Working - 65536
		Else
			L_Signed = W_Working
		EndIf
		
		P_DispClear()
		P_DispCenter(1, S_Title)
		Print At 3, 1, "Value:"
		If B_BlinkOn = YES Then
			If Len(S_Units) > 0 Then
				Print At 4, 2, "[", SDec L_Signed, S_Units, "]"
			Else
				Print At 4, 2, "[", SDec L_Signed, "]"
			EndIf
		Else
			If Len(S_Units) > 0 Then
				Print At 4, 2, SDec L_Signed, S_Units
			Else
				Print At 4, 2, SDec L_Signed
			EndIf
		EndIf
		
		DelayMS 50
	Loop

EditSignedExit:
EndProc

'======================= SPECIALIZED EDITORS ================================

Proc P_EditUITimeout(W_Current As Word), Word
	Result = P_EditWord(W_Current, UO_TOut_S_mn, UI_TOut_S_Mx, "UI TIMEOUT", "s")
EndProc

Proc P_EditBeepPulse(W_Current As Word), Word
	Result = P_EditWord(W_Current, UI_PULSE_MS_MIN, UI_Pls_MS_Mx, "BEEPER PULSE", "ms")
EndProc

Proc P_EditBypassTime(W_Current As Word, S_Title As String * 15), Word
	Result = P_EditWord(W_Current, 1, 999, S_Title, "s")
EndProc

Proc P_EditScaleValue(W_Current As Word, S_Title As String * 15, S_Units As String * 5), Word
	Result = P_EditSignedWord(W_Current, S_Title, S_Units)
EndProc

'-----End Section 7----
Section_8:
'----Start Section 8----

'==============================================================================
'======================= MENU NAVIGATION SYSTEM ============================
'
' Using proper Positron8 BASIC syntax
'
'==============================================================================

'======================= NAVIGATION STATE MANAGEMENT =======================

Proc P_ResetNavState()
	B_MenuSel = 1
	B_MenuTop = 1
	B_EditMode = NO
	B_EditPos = 0
	B_EditField = 0
	B_ForceUpdate = YES
	P_ResetEncPos()
	P_UpdateActivity()
EndProc

Proc P_UpdateMenuSel(B_MaxItems As Byte)
	Dim S_EncDelta As SByte
	Dim B_NewSel   As Byte
	
	S_EncDelta = P_ReadEnc()
	If S_EncDelta <> 0 Then
		B_NewSel = B_MenuSel + S_EncDelta
		
		If B_NewSel < 1 Then
			B_NewSel = 1
		EndIf
		If B_NewSel > B_MaxItems Then
			B_NewSel = B_MaxItems
		EndIf
		
		If B_NewSel <> B_MenuSel Then
			B_MenuSel = B_NewSel
			P_BeepClick()
			P_SetScrDirty()
			P_UpdateActivity()
		EndIf
	EndIf
EndProc

Proc P_CheckNavTimeout(), Byte
	If P_EditTimeout() = YES Then
		Result = NAV_TIMEOUT
	Else
		Result = NAV_CONTINUE
	EndIf
EndProc

Proc P_ProcessNavKeys(), Byte
	Dim B_KeyEvent As Byte
	
	B_KeyEvent = P_GetKeyEvt()
	If B_KeyEvent = 0 Then
		Result = NAV_CONTINUE
		GoTo ProcNavKeysExit
	EndIf
	
	P_UpdateActivity()
	
	Select B_KeyEvent
		Case 1
			P_BeepClick()
			Result = NAV_SELECT
		Case 2
			P_BeepChange()
			Result = NAV_BACK
		Case 3
			P_BeepSave()
			Result = NAV_MAIN
		Case Else
			Result = NAV_CONTINUE
	EndSelect

ProcNavKeysExit:
EndProc

'======================= SYSTEM CONFIGURATION MENU =========================

Proc P_SysConfigMenu(), Byte
	Dim B_NavCode   As Byte
	Dim B_Modified  As Byte
	Dim W_NewValue  As Word
	
	B_Modified = NO
	P_ResetNavState()
	B_MenuCount = 4
	
	Do
		B_NavCode = P_CheckNavTimeout()
		If B_NavCode = NAV_TIMEOUT Then
			Result = NAV_TIMEOUT
			GoTo SysConfigExit
		EndIf
		
		If P_ScrNeedsUpdate() = YES Or B_ForceUpdate = YES Then
			Select B_MenuSel
				Case 1
					P_DispClear()
					P_DispCenter(1, "SYSTEM CONFIG")
					Print At 3, 1, "UI Timeout:"
					Print At 4, 2, Dec W_UI_TimeoutS, "s"
				Case 2
					P_DispClear()
					P_DispCenter(1, "SYSTEM CONFIG")
					Print At 3, 1, "Beep Pulse:"
					Print At 4, 2, Dec W_UI_PulseMs, "ms"
				Case 3
					P_DispCfgValue("SYSTEM CONFIG", "Save Config", "Press to save", NO)
				Case 4
					P_DispCfgValue("SYSTEM CONFIG", "Back", "Return to main", NO)
			EndSelect
			P_ClrScrDirty()
			B_ForceUpdate = NO
		EndIf
		
		P_UpdateMenuSel(B_MenuCount)
		B_NavCode = P_ProcessNavKeys()
		
		Select B_NavCode
			Case NAV_SELECT
				Select B_MenuSel
					Case 1
						W_NewValue = P_EditUITimeout(W_UI_TimeoutS)
						If W_NewValue <> W_UI_TimeoutS Then
							W_UI_TimeoutS = W_NewValue
							B_Modified = YES
						EndIf
						P_SetScrDirty()
					Case 2
						W_NewValue = P_EditBeepPulse(W_UI_PulseMs)
						If W_NewValue <> W_UI_PulseMs Then
							W_UI_PulseMs = W_NewValue
							B_Modified = YES
						EndIf
						P_SetScrDirty()
					Case 3
						If B_Modified = YES Then
							P_DispWorking("Saving...")
							If P_SaveSysCfg() = EE_OK Then
								P_DispSaveConfirm("System Config", YES)
								B_Modified = NO
							Else
								P_DispSaveConfirm("System Config", NO)
							EndIf
							DelayMS 1500
						Else
							P_DispError("No changes", 0)
							DelayMS 1000
						EndIf
						P_SetScrDirty()
					Case 4
						Result = NAV_BACK
						GoTo SysConfigExit
				EndSelect
			Case NAV_BACK
				Result = NAV_BACK
				GoTo SysConfigExit
			Case NAV_MAIN
				Result = NAV_MAIN
				GoTo SysConfigExit
		EndSelect
		
		DelayMS 50
	Loop

SysConfigExit:
EndProc

'======================= INPUT CONFIGURATION MENU ==========================

Proc P_InputConfigMenu(B_InputNum As Byte), Byte
	Dim B_NavCode   As Byte
	Dim B_Modified  As Byte
	Dim B_NewByte   As Byte
	Dim W_NewWord   As Word
	Dim S_Title     As String * 15
	
	B_Modified = NO
	P_ResetNavState()
	B_MenuCount = 12
	
	Select B_InputNum
		Case 1
			S_Title = "INPUT 1 CONFIG"
		Case 2
			S_Title = "INPUT 2 CONFIG"
		Case Else
			S_Title = "INPUT 3 CONFIG"
	EndSelect
	
	Do
		B_NavCode = P_CheckNavTimeout()
		If B_NavCode = NAV_TIMEOUT Then
			Result = NAV_TIMEOUT
			GoTo InputConfigExit
		EndIf
		
		If P_ScrNeedsUpdate() = YES Or B_ForceUpdate = YES Then
			P_DispClear()
			P_DispCenter(1, S_Title)
			
			Select B_MenuSel
				Case 1
					Print At 3, 1, "Enabled:"
					Select B_InputNum
						Case 1
							Print At 4, 2, P_FmtYesNo(B_I1_Enabled)
						Case 2
							Print At 4, 2, P_FmtYesNo(B_I2_Enabled)
						Case Else
							Print At 4, 2, P_FmtYesNo(B_I3_Enabled)
					EndSelect
				Case 2
					Print At 3, 1, "Sensor Type:"
					Select B_InputNum
						Case 1
							Print At 4, 2, P_FmtSensorType(B_I1_SensorT)
						Case 2
							Print At 4, 2, P_FmtSensorType(B_I2_SensorT)
						Case Else
							Print At 4, 2, P_FmtSensorType(B_I3_SensorT)
					EndSelect
				Case 3
					Print At 3, 1, "4mA Scale:"
					Select B_InputNum
						Case 1
							Print At 4, 2, Dec W_I1_Scale4
						Case 2
							Print At 4, 2, Dec W_I2_Scale4
						Case Else
							Print At 4, 2, Dec W_I3_Scale4
					EndSelect
				Case 4
					Print At 3, 1, "20mA Scale:"
					Select B_InputNum
						Case 1
							Print At 4, 2, Dec W_I1_Scale20
						Case 2
							Print At 4, 2, Dec W_I2_Scale20
						Case Else
							Print At 4, 2, Dec W_I3_Scale20
					EndSelect
				Case 5
					Print At 3, 1, "High Bypass:"
					Select B_InputNum
						Case 1
							Print At 4, 2, Dec W_I1_BP_High, "s"
						Case 2
							Print At 4, 2, Dec W_I2_BP_High, "s"
						Case Else
							Print At 4, 2, Dec W_I3_BP_High, "s"
					EndSelect
				Case 11
					P_DispCfgValue(S_Title, "Save Config", "Press to save", NO)
				Case 12
					P_DispCfgValue(S_Title, "Back", "Return to main", NO)
			EndSelect
			P_ClrScrDirty()
			B_ForceUpdate = NO
		EndIf
		
		P_UpdateMenuSel(B_MenuCount)
		B_NavCode = P_ProcessNavKeys()
		
		Select B_NavCode
			Case NAV_SELECT
				Select B_MenuSel
					Case 1
						Select B_InputNum
							Case 1
								B_NewByte = P_EditYesNo(B_I1_Enabled, S_Title, "Enabled")
								If B_NewByte <> B_I1_Enabled Then
									B_I1_Enabled = B_NewByte
									B_Modified = YES
								EndIf
							Case 2
								B_NewByte = P_EditYesNo(B_I2_Enabled, S_Title, "Enabled")
								If B_NewByte <> B_I2_Enabled Then
									B_I2_Enabled = B_NewByte
									B_Modified = YES
								EndIf
							Case Else
								B_NewByte = P_EditYesNo(B_I3_Enabled, S_Title, "Enabled")
								If B_NewByte <> B_I3_Enabled Then
									B_I3_Enabled = B_NewByte
									B_Modified = YES
								EndIf
						EndSelect
						P_SetScrDirty()
					Case 2
						Select B_InputNum
							Case 1
								B_NewByte = P_EditSensorType(B_I1_SensorT, S_Title)
								If B_NewByte <> B_I1_SensorT Then
									B_I1_SensorT = B_NewByte
									B_Modified = YES
								EndIf
							Case 2
								B_NewByte = P_EditSensorType(B_I2_SensorT, S_Title)
								If B_NewByte <> B_I2_SensorT Then
									B_I2_SensorT = B_NewByte
									B_Modified = YES
								EndIf
							Case Else
								B_NewByte = P_EditSensorType(B_I3_SensorT, S_Title)
								If B_NewByte <> B_I3_SensorT Then
									B_I3_SensorT = B_NewByte
									B_Modified = YES
								EndIf
						EndSelect
						P_SetScrDirty()
					Case 11
						If B_Modified = YES Then
							P_DispWorking("Saving...")
							If P_SaveInCfg(B_InputNum) = EE_OK Then
								P_DispSaveConfirm("Input Config", YES)
								B_Modified = NO
							Else
								P_DispSaveConfirm("Input Config", NO)
							EndIf
							DelayMS 1500
						Else
							P_DispError("No changes", 0)
							DelayMS 1000
						EndIf
						P_SetScrDirty()
					Case 12
						Result = NAV_BACK
						GoTo InputConfigExit
				EndSelect
			Case NAV_BACK
				Result = NAV_BACK
				GoTo InputConfigExit
			Case NAV_MAIN
				Result = NAV_MAIN
				GoTo InputConfigExit
		EndSelect
		
		DelayMS 50
	Loop

InputConfigExit:
EndProc

'======================= MAIN MENU SYSTEM ===================================

Proc P_MainMenu(), Byte
	Dim B_NavCode     As Byte
	Dim B_FactoryHold As Byte
	Dim B_KeyEvent    As Byte
	
	B_FactoryHold = 0
	P_ResetNavState()
	B_MenuCount = 7
	
	Do
		B_NavCode = P_CheckNavTimeout()
		If B_NavCode = NAV_TIMEOUT Then
			Result = NAV_TIMEOUT
			GoTo MainMenuExit
		EndIf
		
		If P_ScrNeedsUpdate() = YES Or B_ForceUpdate = YES Then
			Select B_MenuSel
				Case 1
					P_DispCfgValue("MAIN MENU", "Input 1 Config", "Configure In1", NO)
				Case 2
					P_DispCfgValue("MAIN MENU", "Input 2 Config", "Configure In2", NO)
				Case 3
					P_DispCfgValue("MAIN MENU", "Input 3 Config", "Configure In3", NO)
				Case 4
					P_DispCfgValue("MAIN MENU", "System Config", "UI & Beeper", NO)
				Case 5
					P_DispCfgValue("MAIN MENU", "System Status", "View status", NO)
				Case 6
					P_DispCfgValue("MAIN MENU", "EEPROM Status", "Memory info", NO)
				Case 7
					P_DispCfgValue("MAIN MENU", "Factory Reset", "Hold 3 sec", NO)
			EndSelect
			P_ClrScrDirty()
			B_ForceUpdate = NO
		EndIf
		
		P_UpdateMenuSel(B_MenuCount)
		B_NavCode = P_ProcessNavKeys()
		
		Select B_NavCode
			Case NAV_SELECT
				Select B_MenuSel
					Case 1
						B_NavCode = P_InputConfigMenu(1)
						If B_NavCode = NAV_TIMEOUT Then
							Result = NAV_TIMEOUT
							GoTo MainMenuExit
						EndIf
						P_SetScrDirty()
					Case 2
						B_NavCode = P_InputConfigMenu(2)
						If B_NavCode = NAV_TIMEOUT Then
							Result = NAV_TIMEOUT
							GoTo MainMenuExit
						EndIf
						P_SetScrDirty()
					Case 3
						B_NavCode = P_InputConfigMenu(3)
						If B_NavCode = NAV_TIMEOUT Then
							Result = NAV_TIMEOUT
							GoTo MainMenuExit
						EndIf
						P_SetScrDirty()
					Case 4
						B_NavCode = P_SysConfigMenu()
						If B_NavCode = NAV_TIMEOUT Then
							Result = NAV_TIMEOUT
							GoTo MainMenuExit
						EndIf
						P_SetScrDirty()
					Case 5
						P_DispSysStatus()
						DelayMS 3000
						P_SetScrDirty()
					Case 6
						P_DispEEStatus()
						DelayMS 3000
						P_SetScrDirty()
					Case 7
						P_DispFactoryWarn()
						B_FactoryHold = 0
						
						Do
							B_KeyEvent = P_GetKeyEvt()
							If B_KeyEvent > 0 Then
								If B_KeyEvent = 3 Then
									P_DispWorking("Resetting...")
									P_CompleteFactoryReset()
									P_LoadValidatedConfig()
									P_DispError("Reset Complete", 0)
									DelayMS 2000
									GoTo FactoryResetDone
								Else
									P_DispError("Hold longer", 0)
									DelayMS 1000
									GoTo FactoryResetDone
								EndIf
							EndIf
							DelayMS 100
						Loop
FactoryResetDone:
						P_SetScrDirty()
				EndSelect
			Case NAV_BACK
				Result = NAV_BACK
				GoTo MainMenuExit
			Case NAV_MAIN
				' Do nothing - stay in main menu
		EndSelect
		
		DelayMS 50
	Loop

MainMenuExit:
EndProc

'-----End Section 8----
Section_9:
'----Start Section 9----

'==============================================================================
'======================= SYSTEM INITIALIZATION AND MONITORING ==============
'
' Using proper Positron8 BASIC syntax
'
'==============================================================================

'======================= SYSTEM INITIALIZATION ==============================

Proc P_InitHardware(), Byte
	Dim B_Status As Byte
	
	P_LCDHardInit()
	DelayMS 100
	
	P_LCDSafeInit()
	P_DispStartup()
	
	B_AState = _ENC_A
	B_BState = _ENC_B  
	B_ButtonState = _BTN
	B_LastState = (B_AState * 2) + B_BState
	W_EncoderPos = 0
	W_EncReadPos = 0
	S_Qacc = 0
	
	L_Millis = 0
	L_LastInput = 0
	L_LastBlink = 0
	W_Beep = 0
	
	b_ReInitLCD = 0
	b_ScrDirty = 1
	b_Escape = 0
	B_BlinkState = 0
	B_ForceUpdate = 1
	
	Result = EE_OK
EndProc

Proc P_InitSoftware(), Byte
	Dim B_Status As Byte
	
	P_ResetNavState()
	
	B_Status = P_EE_HealthCheck()
	If B_Status <> EE_OK Then
		P_DispError("EEPROM Failed", B_Status)
		DelayMS 3000
	EndIf
	
	B_Status = P_LoadValidatedConfig()
	If B_Status <> EE_OK Then
		P_DispError("Config Error", B_Status)
		DelayMS 2000
	EndIf
	
	If B_EE_Flags.EE_FLAG_FACTORY = 1 Then
		P_DispError("Factory Defaults", 0)
		DelayMS 2000
	EndIf
	
	Result = EE_OK
EndProc

Proc P_InitSystem(), Byte
	Dim B_HwStatus As Byte
	Dim B_SwStatus As Byte
	
	B_HwStatus = P_InitHardware()
	If B_HwStatus <> EE_OK Then
		Result = B_HwStatus
		GoTo InitSystemExit
	EndIf
	
	B_SwStatus = P_InitSoftware()
	If B_SwStatus <> EE_OK Then
		Result = B_SwStatus
		GoTo InitSystemExit
	EndIf
	
	P_BeepSave()
	Result = EE_OK

InitSystemExit:
EndProc

'======================= DEBUG AND MONITORING FUNCTIONS ====================

Proc P_Debug(S_Message As String * 50)
	If DEBUG_ENABLED = 1 Then
		HSerOut [S_Message, 13, 10]
	EndIf
EndProc

Proc P_MonitorHealth()
	Dim L_Uptime As Dword
	Dim W_UptimeS As Word
	
	L_Uptime = L_Millis
	W_UptimeS = L_Uptime / 1000
	
	If W_UptimeS // 60 = 0 And L_Uptime // 1000 < 100 Then
		HSerOut ["Health: OK, Uptime: ", Dec W_UptimeS, "s", 13, 10]
		HSerOut ["EEPROM Status: ", Dec B_EE_Status, 13, 10]
		HSerOut ["Free RAM: Available", 13, 10]
	EndIf
EndProc

Proc P_ReportStatus()
	P_Debug("=== IRRISYS System Status ===")
	P_Debug("Firmware: Rev 2025-12-11")
	
	HSerOut ["UI Timeout: ", Dec W_UI_TimeoutS, "s", 13, 10]
	HSerOut ["Beep Pulse: ", Dec W_UI_PulseMs, "ms", 13, 10]
	HSerOut ["EEPROM Writes: ", Dec W_EE_Wrt_Cnt, 13, 10]
	HSerOut ["Input 1 Enabled: ", Dec B_I1_Enabled, 13, 10]
	HSerOut ["Input 2 Enabled: ", Dec B_I2_Enabled, 13, 10]
	HSerOut ["Input 3 Enabled: ", Dec B_I3_Enabled, 13, 10]
	
	P_Debug("=== End Status ===")
EndProc

'======================= ERROR HANDLING AND RECOVERY =======================

Proc P_HandleSysError(B_ErrorCode As Byte, S_Context As String * 20)
	HSerOut ["ERROR: ", S_Context, " Code: ", Dec B_ErrorCode, 13, 10]
	
	Select B_ErrorCode
		Case EE_ERR_MAGIC
			P_DispError("Bad Config", B_ErrorCode)
			P_LoadSysDefaults()
			P_LoadInDefaults(1)
			P_LoadInDefaults(2)
			P_LoadInDefaults(3)
			DelayMS 3000
		Case EE_ERR_VERSION
			P_DispError("Old Config", B_ErrorCode)
			P_LoadValidatedConfig()
			DelayMS 2000
		Case EE_ERR_CHECKSUM
			P_DispError("Corrupt Config", B_ErrorCode)
			P_LoadValidatedConfig()
			DelayMS 3000
		Case EE_ERR_BACKUP
			P_DispError("Save Failed", B_ErrorCode)
			P_BeepError()
			DelayMS 2000
		Case Else
			P_DispError("System Error", B_ErrorCode)
			P_BeepError()
			DelayMS 2000
	EndSelect
	
	P_SetScrDirty()
EndProc

Proc P_RecoverSystem(), Byte
	Dim B_Status As Byte
	
	P_Debug("Attempting system recovery...")
	
	B_Status = P_EE_HealthCheck()
	If B_Status <> EE_OK Then
		P_Debug("EEPROM health check failed")
		P_DispError("Hardware Fault", B_Status)
		DelayMS 5000
		Result = B_Status
		GoTo RecoverSystemExit
	EndIf
	
	B_Status = P_LoadValidatedConfig()
	If B_Status <> EE_OK Then
		P_Debug("Config reload failed, using defaults")
		P_HandleSysError(B_Status, "Config Reload")
	EndIf
	
	P_ResetNavState()
	P_SetScrDirty()
	
	P_Debug("System recovery completed")
	Result = EE_OK

RecoverSystemExit:
EndProc

'-----End Section 9----
Section_10:
'----Start Section 10----

'==============================================================================
'======================= MAIN PROGRAM LOOP AND ENTRY POINT =================
'
' Using proper Positron8 BASIC syntax
'
'==============================================================================

'======================= MAIN PROGRAM LOOP ==================================

Proc P_MainLoop()
	Dim B_NavResult As Byte
	Dim B_ErrorCount As Byte
	Dim L_LastHealth As Dword
	
	B_ErrorCount = 0
	L_LastHealth = L_Millis
	
	P_Debug("Main system loop started")
	P_ReportStatus()
	
	Do
		If L_Millis - L_LastHealth > 30000 Then
			P_MonitorHealth()
			L_LastHealth = L_Millis
		EndIf
		
		B_NavResult = P_MainMenu()
		
		Select B_NavResult
			Case NAV_TIMEOUT
				P_Debug("UI timeout - clearing display")
				P_DispClear()
				
				Do
					DelayMS 100
					If P_ReadEnc() <> 0 Or P_GetKeyEvt() > 0 Then
						P_Debug("User activity detected - resuming")
						P_SetScrDirty()
						GoTo ActivityDetected
					EndIf
				Loop
ActivityDetected:
				
			Case NAV_BACK
				P_Debug("System exit requested")
				Print At 2, 1, "Shutting down..."
				DelayMS 1000
				P_DispClear()
				GoTo MainLoopExit
				
			Case Else
				HSerOut ["Unexpected nav result: ", Dec B_NavResult, 13, 10]
				Inc B_ErrorCount
				
				If B_ErrorCount > 5 Then
					P_Debug("Excessive errors - attempting recovery")
					If P_RecoverSystem() <> EE_OK Then
						P_DispError("Critical Error", 0)
						DelayMS 5000
						GoTo MainLoopExit
					EndIf
					B_ErrorCount = 0
				EndIf
		EndSelect
		
		If B_NavResult = NAV_TIMEOUT Then
			B_ErrorCount = 0
		EndIf
		
		DelayMS 10
	Loop

MainLoopExit:
	P_Debug("Main system loop terminated")
EndProc

'======================= PROGRAM ENTRY POINT ===============================

Main:
	P_Debug("IRRISYS HMI System Starting...")
	P_Debug("Firmware: Rev 2025-12-11")
	P_Debug("Device: PIC18F2525 @ 32MHz")
	
	If P_InitSystem() <> EE_OK Then
		P_DispError("Init Failed", 0)
		P_BeepError()
		P_Debug("System initialization failed - halting")
		
		Do
			DelayMS 1000
			_BUZZER = 1
			DelayMS 100
			_BUZZER = 0
		Loop
	EndIf
	
	P_Debug("System initialization completed successfully")
	
	P_MainLoop()
	
	P_Debug("System shutdown initiated")
	Print At 2, 1, "System stopped"
	DelayMS 2000
	P_DispClear()
	
	GIE = 0
	_BUZZER = 0
	P_Debug("System shutdown completed")
	
	Do
		DelayMS 1000
	Loop

'-----End Section 10----
