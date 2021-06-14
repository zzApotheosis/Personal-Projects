/* 
 * File:   LCD.h
 * Author: bmcgarvey
 *
 * Created on April 18, 2014, 8:07 AM
 */

#ifndef LCD_H
#define	LCD_H

//Uncomment one of the modes below
//#define LCD_MODE_DIRECT
#define LCD_MODE_EXP18
//#define LCD_MODE_I2C
//#define LCD_MODE_PMP

#if defined LCD_MODE_DIRECT
#define LCD_PORT    PORTD
#define LCD_LAT     LATD
#define LCD_TRIS    TRISD
#define LCD_RS      PORTEbits.RE0
#define RS_TRIS     TRISEbits.TRISE0
#define LCD_RW      PORTEbits.RE1
#define RW_TRIS     TRISEbits.TRISE1
#define LCD_E       PORTEbits.RE2
#define E_TRIS      TRISEbits.TRISE2
#define LCD_DATA_WIDTH  8   //Set to 8 or 4
#endif

#if defined LCD_MODE_I2C
#define LCD_I2C_ADDRESS 0x4E
#define LCD_DATA_WIDTH  4   //Must be 4
#define MSSPx           2   //1 or 2 to select MSSP1 or MSSP2 for I2C
#endif

#if defined LCD_MODE_EXP18 || defined LCD_MODE_PMP
#define LCD_DATA_WIDTH  8  //Must be 8
#endif

//#define LCD_POLLING

#define _XTAL_FREQ  48000000L

#define LCD_ROWS    2
#define LCD_CHARS   16

#define LCD_SCROLL_UP   -1
#define LCD_SCROLL_DOWN 1

void LCDInit(void);
void LCDClear(void);
void LCDWriteLine(const char *str, char line);
void LCDReadLine(char *str, char line);
void LCDClearLine(char line);
void LCDPutStr(char *str);
void LCDPutChar(char c);
char LCDGetChar(void);
void LCDSetPos(int row, int col);
void LCDGetPos(int *row, int *col);
void LCDLoadCustomChar(const char *pixels, char pattern);
void LCDScroll(signed char dir);
void LCDDisplay(char enableDisplay, char enableCursor, char blink);

#endif	/* LCD_H */

