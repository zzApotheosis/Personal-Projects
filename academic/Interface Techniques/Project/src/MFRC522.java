/*
 * Created by Steven Jennings on 20 April 2017.
 * 
 * MFRC522 Registers
 * Page 0: Command and Status
 * 00h - Reserved
 * 01h - CommandReg
 * 02h - ComIEnReg
 * 03h - DivIEnReg
 * 04h - ComIrqReg
 * 05h - DivIrqReg
 * 06h - ErrorReg
 * 07h - Status1Reg
 * 08h - Status2Reg
 * 09h - FIFODataReg
 * 0Ah - FIFOLevelReg
 * 0Bh - WaterLevelReg
 * 0Ch - ControlReg
 * 0Dh - BitFramingnReg
 * 0Eh - CollReg
 * 0Fh - Reserved
 * 
 * Page 1: Command
 * 10h - Reserved
 * 11h - ModeReg
 * 12h - TxModeReg
 * 13h - RxModeReg
 * 14h - TxControlReg
 * 15h - TxASKReg
 * 16h - TxSelReg
 * 17h - RxSelReg
 * 18h - RxThresholdReg
 * 19h - DemodReg
 * 1Ah - Reserved
 * 1Bh - Reserved
 * 1Ch - MfTxReg
 * 1Dh - MfRxReg
 * 1Eh - Reserved
 * 1Fh - SerialSpeedReg
 * 
 * Page 2: Configuration
 * 20h - Reserved
 * 21h - CRCResultReg
 * 22h - CRCResultReg
 * 23h - Reserved
 * 24h - ModWidthReg
 * 25h - Reserved
 * 26h - RFCfgReg
 * 27h - GsNReg
 * 28h - CWGsPReg
 * 29h - ModGsPReg
 * 2Ah - TModeReg
 * 2Bh - TPrescalerReg
 * 2Ch - TReloadReg
 * 2Dh - TReloadReg
 * 2Eh - TCounterValReg
 * 2Fh - TCounterValReg
 * 
 * Page 3: Test Register
 * 30h - Reserved
 * 31h - TestSel1Reg
 * 32h - TestSel2Reg
 * 33h - TestPinEnReg
 * 34h - TestPinValueReg
 * 35h - TestBusReg
 * 36h - AutoTestReg
 * 37h - VersionReg
 * 38h - AnalogTestReg
 * 39h - TestDAC1Reg
 * 3Ah - RestDAC2Reg
 * 3Bh - TestADCReg
 * 3Ch to 3Fh - Reserved
 * 
 * Command Overview:
 * 0000 - Idle, no action, cancels current command execution
 * 0001 - Mem, stores 25 bytes into the internal buffer
 * 0010 - Generate RandomID, generates a 10-byte random ID number
 * 0011 - CalcCRC, activates the CRC coprocessor or performs a self test
 * 0100 - Transmit, transmits data from the FIFO buffer
 * 0111 - NoCmdChange, no command change, can be used to modify the CommandReg register bits without affecting the command, for example, the PowerDown bit
 * 1000 - Receive, activates the receiver circuits
 * 1100 - Transeive, transmits data from FIFO buffer to antenna and automatically activates the receiver after transmission
 * 1101 - reserved for future use
 * 1110 - MFAuthent, performs the MIFARE standard authentication as a reader
 * 1111 - SoftReset, resets the MFRC522
 */

class MFRC522 {
	
	public final int MAX_LEN = 16;
	
    // PCD values
    public final byte PCD_IDLE = 0x00;
    public final byte PCD_AUTHENT = 0x0E;
    public final byte PCD_RECEIVE = 0x08;
    public final byte PCD_TRANSMIT = 0x04;
    public final byte PCD_TRANSCEIVE = 0x0C;
    public final byte PCD_RESETPHASE = 0x0F;
    public final byte PCD_CALCCRC = 0x03;

    // PICC values
    public final byte PICC_REQIDL = 0x26;
    public final byte PICC_REQALL = 0x52;
    public final byte PICC_CT = (byte) 0x88;
    public final byte PICC_ANTICOLL = (byte) 0x93;
    public final byte PICC_SElECTTAG = (byte) 0x93;
    public final byte PICC_AUTHENT1A = 0x60;
    public final byte PICC_AUTHENT1B = 0x61;
    public final byte PICC_READ = 0x30;
    public final byte PICC_WRITE = (byte) 0xA0;
    public final byte PICC_DECREMENT = (byte) 0xC0;
    public final byte PICC_INCREMENT = (byte) 0xC1;
    public final byte PICC_RESTORE = (byte) 0xC2;
    public final byte PICC_TRANSFER = (byte) 0xB0;
    public final byte PICC_HALT = 0x50;

    // MI values
    public final int MI_OK = 0;
    public final int MI_NOTAGERR = 1;
    public final int MI_ERR = 2;
    
    // Interrupt flags
    public final byte TxIRq = 0x40;
    public final byte RxIRq = 0x20;
    public final byte IdleIRq = 0x10;
    public final byte HiAlertIRq = 0x08;
    public final byte LoAlertIRq = 0x04;
    public final byte ErrIRq = 0x02;
    public final byte TimerIRq = 0x01;
    public final byte MfinActIRq = 0x10;
    public final byte CRCIRq = 0x04;
    
    // Error flags
    public final byte CRC_NONE = 0x00;
    public final byte CRC_RX = 0x01;
    public final byte CRC_TX = 0x02;

    // Page 0: Command and Status
    public final byte Reserved00 = 0x00;
    public final byte CommandReg = 0x01;
    public final byte CommIEnReg = 0x02;
    public final byte DivIEnReg = 0x03;
    public final byte CommIrqReg = 0x04;
    public final byte DivIrqReg = 0x05;
    public final byte ErrorReg = 0x06;
    public final byte Status1Reg = 0x07;
    public final byte Status2Reg = 0x08;
    public final byte FIFODataReg = 0x09;
    public final byte FIFOLevelReg = 0x0A;
    public final byte WaterLevelReg = 0x0B;
    public final byte ControlReg = 0x0C;
    public final byte BitFramingReg = 0x0D;
    public final byte CollReg = 0x0E;
    public final byte Reserved01 = 0x0F;

    // Page 1: Command
    public final byte Reserved10 = 0x10;
    public final byte ModeReg = 0x11;
    public final byte TxModeReg = 0x12;
    public final byte RxModeReg = 0x13;
    public final byte TxControlReg = 0x14;
    public final byte TxAutoReg = 0x15;
    public final byte TxSelReg = 0x16;
    public final byte RxSelReg = 0x17;
    public final byte RxThresholdReg = 0x18;
    public final byte DemodReg = 0x19;
    public final byte Reserved11 = 0x1A;
    public final byte Reserved12 = 0x1B;
    public final byte MifareReg = 0x1C;
    public final byte Reserved13 = 0x1D;
    public final byte Reserved14 = 0x1E;
    public final byte SerialSpeedReg = 0x1F;

    // Page 2: Configuration
    public final byte Reserved20 = 0x20;
    public final byte CRCResultRegH = 0x21;
    public final byte CRCResultRegL = 0x22;
    public final byte Reserved21 = 0x23;
    public final byte ModWidthReg = 0x24;
    public final byte Reserved22 = 0x25;
    public final byte RFCfgReg = 0x26;
    public final byte GsNReg = 0x27;
    public final byte CWGsPReg = 0x28;
    public final byte ModGsPReg = 0x29;
    public final byte TModeReg = 0x2A;
    public final byte TPrescalerReg = 0x2B;
    public final byte TReloadRegH = 0x2C;
    public final byte TReloadRegL = 0x2D;
    public final byte TCounterValueRegH = 0x2E;
    public final byte TCounterValueRegL = 0x2F;

    // Page 3: Test Register
    public final byte Reserved30 = 0x30;
    public final byte TestSel1Reg = 0x31;
    public final byte TestSel2Reg = 0x32;
    public final byte TestPinEnReg = 0x33;
    public final byte TestPinValueReg = 0x34;
    public final byte TestBusReg = 0x35;
    public final byte AutoTestReg = 0x36;
    public final byte VersionReg = 0x37;
    public final byte AnalogTestReg = 0x38;
    public final byte TestDAC1Reg = 0x39;
    public final byte TestDAC2Reg = 0x3A;
    public final byte TestADCReg = 0x3B;
    public final byte Reserved31 = 0x3C;
    public final byte Reserved32 = 0x3D;
    public final byte Reserved33 = 0x3E;
    public final byte Reserved34 = 0x3F;
    
    // MRFC522 Commands
    public final byte CommandIdle = 0b0000;
    public final byte CommandMem = 0b0001;
    public final byte CommandRand = 0b0010;
    public final byte CommandCalcCRC = 0b0011;
    public final byte CommandTransmit = 0b0100;
    public final byte CommandNoChange = 0b0111;
    public final byte CommandReceive = 0b1000;
    public final byte CommandTransceive = 0b1100;
    public final byte CommandMFAuthent = 0b1110;
    public final byte CommandSoftReset = 0b1111;
    
    // PCD Gain Settings
    public final byte GAIN_18DB = 0b000;
    public final byte GAIN_23DB = 0b001;
    public final byte GAIN_33DB = 0b100;
    public final byte GAIN_38DB = 0b101;
    public final byte GAIN_43DB = 0b110;
    public final byte GAIN_48DB = 0b111;
    public final byte GAIN_MIN = 0b000;
    public final byte GAIN_MID = 0b100;
    public final byte GAIN_MAX = 0b111;
    
    // PCD Baud Rate
    public final byte BAUD_106 = 0b000;
    public final byte BAUD_212 = 0b001;
    public final byte BAUD_424 = 0b010;
    public final byte BAUD_848 = 0b011;
    
    // PICC SAK Types
    public final byte PICC_UNKNOWN = 0x00;
    public final byte PICC_ISO_14443_4 = 0x01;
    public final byte PICC_MIFARE_MINI = 0x02;
    public final byte PICC_MIFARE_1K = 0x03;
    public final byte PICC_MIFARE_4K = 0x04;
    public final byte PICC_MIFARE_UL = 0x05;
    public final byte PICC_MIFARE_PLUS = 0x06;
    public final byte PICC_ISO_18092 = 0x07;
    public final byte PICC_NOT_COMPLETE = (byte) 0x80;
    
    // Mifare Access Bit Constants
    public final byte ACCESS_BITS_TRANSPORT = 0b001;
    public final byte ACCESS_BITS_KEYB = 0b011;
    public final byte BLOCK_TRANSPORT = 0b000;
    public final byte BLOCK_READ_AB = 0b010;
    public final byte BLOCK_READ_AB_WRITE_B = 0b100;
    public final byte BLOCK_READ_B_WRITE_B = 0b011;
    public final byte BLOCK_READ_B = 0b101;
    public final byte BLOCK_NONE = 0b111;
    public final byte VALUE_DEC_AB_INC_B = 0b110;
    public final byte VALUE_DEC_AB_NO_W_OR_INC = 0b001;
    
    // Error Codes
    public final byte STATUS_OK = 0;
    public final byte STATUS_ERROR = 1;
    public final byte STATUS_CRC_ERROR = 2;
    public final byte STATUS_COLLISION = 3;
    public final byte STATUS_TIMEOUT = 4;
    public final byte STATUS_NAK = 5;
    public final byte STATUS_ACK = 6;
    public final byte STATUS_BUFFER_SIZE = 7;
    
    // Mifare classic key A or B
    public final byte KEY_A = 0;
    public final byte KEY_B = 1;
}