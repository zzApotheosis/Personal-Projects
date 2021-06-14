#include <xc.h>
#include "LCD.h"

//Functions below must be defined for the interface
void LCDWriteByte(char c, char rs);
char LCDReadByte(char rs);
void LCDInitPort(void);
void LCDWrite8(char c, char rs); //Only needed if 4 bit mode is supported

//Private LCD Functions
void LCDCommand(unsigned char command);
void LCDInitCommand(unsigned char command);
void LCDWriteData(char c);
char LCDBusy(void);
char LCDReadData(void);
char LCDGetAC(void);

//Utility functions
unsigned char calculateBase(char line);
void wait(void);
void longDelay(void);
void shortDelay(void);

void LCDInit(void) {
    char functionSet = 0b00111000;
    LCDInitPort();
    __delay_ms(10);
#if LCD_DATA_WIDTH == 4
    LCDWrite8(functionSet, 0);
    __delay_us(40);
    LCDWrite8(functionSet, 0);
    __delay_us(40);
    functionSet = 0b00101000;
    LCDWrite8(functionSet, 0);
    __delay_us(40);
#endif
    LCDInitCommand(functionSet); //Function set
    __delay_us(40);
    LCDInitCommand(0b00001100); //Display on, cursor and blink off
    __delay_us(40);
    LCDInitCommand(0b00000001); //Clear
    __delay_us(1700);
    LCDInitCommand(0b00000110); //Entry mode increment, no shift
    __delay_us(40);
}

void LCDClear(void) {
    LCDCommand(0b00000001);
    longDelay();
}

void LCDPutStr(char *str) {
    while (*str) {
        LCDWriteData(*str);
        ++str;
    }
}

void LCDWriteLine(const char *str, char line) {
    if (line >= LCD_ROWS) {
        return;
    }
    LCDSetPos(line, 0);
    while (*str) {
        if (*str == '\n') {
            ++line;
            if (line == LCD_ROWS) {
                line = 0;
            }
        }
        if (*str == '\r' || *str == '\n') {
            LCDSetPos(line, 0);
        } else {
            LCDWriteData(*str);
        }
        ++str;
    }
}

void LCDClearLine(char line) {
    char c = 0;
    if (line >= LCD_ROWS) {
        return;
    }
    LCDSetPos(line, 0);
    while (c < LCD_CHARS) {
        LCDWriteData(' ');
        ++c;
    }
}

void LCDPutChar(char c) {
    LCDWriteData(c);
}

void LCDSetPos(int row, int col) {
    int pos;
    if (row < 0 || col < 0 || row >= LCD_ROWS || col >= LCD_CHARS) {
        return;
    }
    pos = calculateBase(row) + col;
    LCDCommand(128 + pos);
}

void LCDGetPos(int *row, int *col) {
    char ac;
    ac = LCDGetAC();
    *row = (ac / 64);
    if (LCD_ROWS > 2 && ac % 64 >= 20) {
        ++(*row);
    }
    *col = ac % 64;
    if (LCD_ROWS > 2) {
        *col = *col % 20;
    }
}

void LCDLoadCustomChar(const char *pixels, char pattern) {
    int rows = 8;
    char ac;
    if (pattern < 8) {
        ac = LCDGetAC();
        LCDCommand(64 + (pattern * 8));
        while (rows > 0) {
            LCDWriteData(*pixels);
            ++pixels;
            --rows;
        }
        LCDCommand(128 + ac);
    }
}

void LCDReadLine(char *str, char line) {
    int i;
    if (line >= LCD_ROWS) {
        str[0] = '\0';
        return;
    }
    LCDSetPos(line, 0);
    for (i = 0; i < LCD_CHARS; ++i) {
        str[i] = LCDReadData();
    }
    str[LCD_CHARS] = '\0';
}

char LCDGetChar(void) {
    char c;
    c = LCDReadData();
    return c;
}

void LCDScroll(signed char dir) {
    char str[LCD_CHARS + 1];
    char ac;
    signed char i;
    ac = LCDGetAC();
    if (dir == LCD_SCROLL_UP) {
        for (i = 1; i < LCD_ROWS; ++i) {
            LCDReadLine(str, i);
            LCDWriteLine(str, i - 1);
        }
        LCDClearLine(LCD_ROWS - 1);
    } else {
        for (i = LCD_ROWS - 2; i >= 0; --i) {
            LCDReadLine(str, i);
            LCDWriteLine(str, i + 1);
        }
        LCDClearLine(0);
    }
    LCDCommand(128 + ac);
}

void LCDDisplay(char enableDisplay, char enableCursor, char blink) {
    unsigned char command = 0b00001000;
    if (enableDisplay) {
        command |= 0b00000100;
    }
    if (enableCursor) {
        command |= 0b00000010;
    }
    if (blink) {
        command |= 0b00000001;
    }
    LCDCommand(command);
}

void LCDCommand(unsigned char command) {
    wait();
    LCDWriteByte(command, 0);
    shortDelay();
}

void LCDInitCommand(unsigned char command) {
    LCDWriteByte(command, 0);
}

void LCDWriteData(char c) {
    wait();
    LCDWriteByte(c, 1);
    shortDelay();
}

char LCDBusy(void) {
    char b;
    b = LCDReadByte(0);
    return (b & 0b10000000) != 0;
}

char LCDReadData(void) {
    char c;
    wait();
    c = LCDReadByte(1);
    shortDelay();
    return c;
}

char LCDGetAC(void) {
    char b;
    wait();
    b = LCDReadByte(0);
    shortDelay();
    return b & 0b01111111;
}

unsigned char calculateBase(char line) {
    char base;
    if (line % 2 == 0) {
        base = line * 10;
    } else {
        base = 54 + line * 10;
    }
    return base;
}

void wait(void) {
#ifdef LCD_POLLING
    while (LCDBusy());
#endif
}

void longDelay(void) {
#ifndef LCD_POLLING
    __delay_us(1660);
#endif
}

void shortDelay(void) {
#ifndef LCD_POLLING
    __delay_us(40);
#endif
}

#if defined LCD_MODE_EXP18

#define PORT_A  0x12
#define PORT_B  0x13

void WriteRegister(char, char);

void LCDWriteByte(char c, char rs) {
    unsigned char comFlags = 0;
    if (rs) {
        comFlags = 0b10000000;
    }
    WriteRegister(PORT_A, comFlags);
    WriteRegister(PORT_B, c);
    WriteRegister(PORT_A, comFlags | 0b01000000);
    WriteRegister(PORT_A, comFlags);
}

char LCDReadByte(char rs) {
    return 0;
}

#if defined (__18F47J53)
#define SSPxCON1        SSP2CON1
#define SSPxSTATbits    SSP2STATbits
#define SSPxIF          SSP2IF
#define SSPxBUF         SSP2BUF
#else
#define SSPxCON1        SSPCON1
#define SSPxSTATbits    SSPSTATbits
#define SSPxIF          SSPIF
#define SSPxBUF         SSPBUF
#endif

void LCDInitPort(void) {
    TRISAbits.TRISA2 = 0;
    LATAbits.LATA2 = 1;
#if defined (__18F47J53)
    EECON2 = 0x55;
    EECON2 = 0xAA;
    PPSCONbits.IOLOCK = 0;
    RPOR2 = 10;
    RPOR4 = 11;
    EECON2 = 0x55;
    EECON2 = 0xAA;
    PPSCONbits.IOLOCK = 1;
    TRISBbits.TRISB1 = 0;
    TRISAbits.TRISA5 = 0;
    LATAbits.LATA5 = 0;
    LATBbits.LATB1 = 0;
#else
    TRISCbits.TRISC3 = 0;
    TRISCbits.TRISC5 = 0;
#endif
    SSPxCON1 = 0x21;
    SSPxSTATbits.CKE = 1;
    SSPxIF = 0;
    WriteRegister(0, 0); //Set port A to outputs
    WriteRegister(1, 0); //Set port B to outputs
    WriteRegister(PORT_A, 0); //Clear port A (E and RS pins)

}

//*****************************************************************
// Write to MCP23S17 register
//*****************************************************************

void WriteRegister(char reg, char b) {
    LATAbits.LATA2 = 0;
    SSPxBUF = 0x40;
    while (!SSPxIF);
    SSPxIF = 0;
    SSPxBUF = reg;
    while (!SSPxIF);
    SSPxIF = 0;
    SSPxBUF = b;
    while (!SSPxIF);
    SSPxIF = 0;
    LATAbits.LATA2 = 1;
}

#endif

#if defined LCD_MODE_PMP

void LCDInitPort(void) {
    //Data is PORTD
    //RS = RB5, RW = RE0, E = RE1
    TRISD = 0xff;
    PMCONH = 0b00100011;
    PMCONL = 0b00000011;
    PMMODEH = 0b00000011;
    PMMODEL = 0b00001000;
    PMEH = 0;
    PMEL = 0b00000001;
    PMCONHbits.PMPEN = 1;
}

void LCDWriteByte(char c, char rs) {
    PMADDRL = rs;
    while (PMMODEHbits.BUSY);
    PMDIN1L = c;
}

char LCDReadByte(char rs) {
    char c;
    PMADDRL = rs;
    while (PMMODEHbits.BUSY);
    c = PMDIN1L;
    while (PMMODEHbits.BUSY);
    c = PMDIN1L;
    return c;
}

#endif

#if defined LCD_MODE_DIRECT

#if LCD_DATA_WIDTH == 8
#define ON_MASK     0b11111111
#define OFF_MASK    0b00000000
#elif LCD_DATA_WIDTH == 4
#define ON_MASK     0b11110000
#define OFF_MASK    0b00001111
#else
#error Invalid LCD_DATA_WIDTH
#endif

void LCDInitPort(void) {
    LCD_LAT = LCD_LAT & OFF_MASK;
    LCD_TRIS = LCD_TRIS & OFF_MASK;
    LCD_RW = 0;
    RW_TRIS = 0;
    LCD_RS = 0;
    RS_TRIS = 0;
    LCD_E = 0;
    E_TRIS = 0;
}

void LCDWriteByte(char c, char rs) {
    LCD_LAT = (LCD_LAT & OFF_MASK) | (c & ON_MASK);
    if (rs) {
        LCD_RS = 1;
    } else {
        LCD_RS = 0;
    }
    LCD_RW = 0;
    LCD_E = 1;
    __delay_us(1);
    LCD_E = 0;
#if LCD_DATA_WIDTH == 4
    LCD_LAT = (LCD_LAT & OFF_MASK) | ((c << 4) & ON_MASK);
    LCD_E = 1;
    __delay_us(1);
    LCD_E = 0;
#endif
}

void LCDWrite8(char c, char rs) {
    LCD_LAT = (LCD_LAT & OFF_MASK) | (c & ON_MASK);
    if (rs) {
        LCD_RS = 1;
    } else {
        LCD_RS = 0;
    }
    LCD_RW = 0;
    LCD_E = 1;
    __delay_us(1);
    LCD_E = 0;
}

char LCDReadByte(char rs) {
    char b;
    LCD_TRIS = LCD_TRIS | ON_MASK;
    if (rs) {
        LCD_RS = 1;
    } else {
        LCD_RS = 0;
    }
    LCD_RW = 1;
    LCD_E = 1;
    __delay_us(1);
    b = LCD_PORT & ON_MASK;
    LCD_E = 0;
#if LCD_DATA_WIDTH == 4
    LCD_E = 1;
    __delay_us(1);
    b |= (LCD_PORT >> 4) & OFF_MASK;
    LCD_E = 0;
#endif
    LCD_TRIS = LCD_TRIS & OFF_MASK;
    return b;

}

#endif

#if defined LCD_MODE_I2C

#define RS_ON   0b00000001
#define RW_ON   0b00000010
#define E_ON    0b00000100
#define E_OFF   0b11111011
#define BACKLIGHT_ON    0b00001000

#define BAUD    ((_XTAL_FREQ / 400000) - 1)

#if MSSPx == 1
#define SSPxADD         SSPADD
#define SSPxCON1bits    SSPCON1bits
#define SSPxCON2bits    SSPCON2bits
#define SSPxSTATbits    SSPSTATbits
#define SSPxBUF         SSPBUF
#define I2C_TRIS()      (TRISC |= 0b00011000)  //RC4=SDA, RC3=SCL
//#define I2C_TRIS()    (TRISB |= 0b00000011)  //RB0=SDA, RB1=SCL
#elif MSSPx == 2
#define SSPxADD         SSP2ADD
#define SSPxCON1bits    SSP2CON1bits
#define SSPxCON2bits    SSP2CON2bits
#define SSPxSTATbits    SSP2STATbits
#define SSPxBUF         SSP2BUF
#define I2C_TRIS()      (TRISD |= 0b01100000)  //RD5=SDA, RD6=SCL
#else
#error Invalid MSSPx selection
#endif

void LCDInitPort(void) {
    I2C_TRIS();
    SSPxADD = BAUD; //100kHz
    SSPxCON1bits.SSPM = 0b1000;
    SSPxCON1bits.SSPEN = 1;
    Nop();
    SSPxCON2bits.SEN = 1;
    while (SSPxCON2bits.SEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = BACKLIGHT_ON;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.PEN = 1;
    while (SSPxCON2bits.PEN == 1);
}

void LCDWrite8(char c, char rs) {
    unsigned char dataByte;
    unsigned char comFlags = BACKLIGHT_ON;
    if (rs) {
        comFlags |= RS_ON;
    }
    SSPxCON2bits.SEN = 1;
    while (SSPxCON2bits.SEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    dataByte = c & 0b11110000;
    SSPxBUF = dataByte | E_ON | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = dataByte | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.PEN = 1;
    while (SSPxCON2bits.PEN == 1);
}

void LCDWriteByte(char c, char rs) {
    unsigned char dataByte;
    unsigned char comFlags = BACKLIGHT_ON;
    if (rs) {
        comFlags |= RS_ON;
    }
    SSPxCON2bits.SEN = 1;
    while (SSPxCON2bits.SEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    dataByte = c & 0b11110000;
    SSPxBUF = dataByte | E_ON | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = dataByte | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    dataByte = (c << 4) & 0b11110000;
    SSPxBUF = dataByte | E_ON | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = dataByte | comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = BACKLIGHT_ON;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.PEN = 1;
    while (SSPxCON2bits.PEN == 1);
}

char LCDReadNibble(char rs) {
    char b;
    char comFlags = 0b11111000 | RW_ON;
    if (rs) {
        comFlags |= RS_ON;
    }
    SSPxCON2bits.SEN = 1; //Start
    while (SSPxCON2bits.SEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = comFlags | E_ON;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.RSEN = 1; //restart
    while (SSPxCON2bits.RSEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS | 1;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.RCEN = 1;
    while (SSPxSTATbits.BF == 0); //Wait for byte
    b = SSPxBUF & 0b11110000; //Upper nibble
    SSPxCON2bits.ACKDT = 1;
    SSPxCON2bits.ACKEN = 1; //Send NACK
    while (SSPxCON2bits.ACKEN == 1);
    SSPxCON2bits.RSEN = 1; //restart
    while (SSPxCON2bits.RSEN == 1);
    SSPxBUF = LCD_I2C_ADDRESS;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxBUF = comFlags;
    while (SSPxSTATbits.BF || SSPxSTATbits.R_W);
    SSPxCON2bits.PEN = 1; //stop
    while (SSPxCON2bits.PEN == 1);
    return b;
}

char LCDReadByte(char rs) {
    unsigned char ub;
    unsigned char lb;
    ub = LCDReadNibble(rs);
    lb = LCDReadNibble(rs);
    return ub | (lb >> 4);
}

#endif