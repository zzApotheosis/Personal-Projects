#include <xc.h>
#include <stdio.h>
#include "LCD.h"


#if defined __18F8722
#pragma config OSC=HSPLL
#pragma config WDT=OFF
#pragma config LVP=OFF
#pragma config XINST=OFF
#elif defined __18F87J11
#pragma config FOSC=HSPLL
#pragma config WDTEN=OFF
#pragma config XINST=OFF
#else
#error Invalid processor selection
#endif

/*
Connections:
        Master RD4 <-> Slave RD5
        Master RD5 <-> Slave RD4
        Master RD6 <-> Slave RD6
        Master RD7 <-> Slave RD7
 */

void InitPins(void);
unsigned int ReadPot(void);
void ConfigInterrupts(void);
void ConfigPeriph(void);


#define _XTAL_FREQ   40000000L

char lcdStr[17];
unsigned int count;
unsigned int knob;
int state = 0;
int command;
int cnt = 0;

void main(void) {
    long i;
    count = 0;
    OSCTUNEbits.PLLEN = 1;
    LCDInit();
    LCDClear();
    InitPins();
    ConfigPeriph();
    ConfigInterrupts();

    while (1) {
        sprintf(lcdStr, "%d", cnt);
        LCDClearLine(0);
        LCDWriteLine(lcdStr, 0);
        cnt++;
        for (i = 0; i < 300000; ++i);
    }
}

void InitPins(void) {
    LATD = 0; //LED's are outputs
    TRISD = 0; //Turn off all LED's


    //Set TRIS bits for any required peripherals here.

    TRISD = 0b11100000; //MMSP2 uses RD4 as SDO, RD5 as SDI, RD6 as SCK, RD7 is SS

    //Set up for ADC
    TRISA = 0b00000001;
    ADCON1 = 0b10111010; //Right justify, No calibration, 20 Tad, FOSC/32
    WDTCONbits.ADSHR = 1; //Switch to alternate address to access ANCON0
    ANCON0 = 0b11111110; //AN0 analog - all others digital
    WDTCONbits.ADSHR = 0; //Back to default address
}

unsigned int ReadPot(void) {
    ADCON0bits.CHS = 0; //channel 0
    ADCON0bits.ADON = 1;
    ADCON0bits.GO = 1;
    while (ADCON0bits.GO == 1);
    ADCON0bits.ADON = 0;
    return ADRES;
}

void ConfigInterrupts(void) {

    RCONbits.IPEN = 0; //no priorities.  This is the default.

    //Configure your interrupts here

    SSP2IF = 0;
    SSP2IE = 1;

    INTCONbits.PEIE = 1;
    INTCONbits.GIE = 1; //Turn on interrupts
}

void ConfigPeriph(void) {

    //Configure peripherals here

    SSP2STATbits.CKE = 1;
    SSP2CON1bits.CKP = 0; //SPI mod 0,0
    SSP2CON1bits.SSPM = 0b0100; //SPI Slave with SS
    SSP2CON1bits.SSPEN = 1; //Enable MSSP
}

void interrupt HighIsr(void) {
    //Check the source of the interrupt
    char rx;
    if (SSP2IF) {
        //Handle SPI interrupt here
        rx = SSP2BUF;
        if (state == 0) {
            command = rx;
            if (command == 0x01) {
                SSP2BUF = cnt >> 8;
                state = 1;
            } else if (command == 0x02) {
                knob = ReadPot();
                SSP2BUF = knob >> 8;
                state = 3;
            }
        } else if (state == 1) {
            SSP2BUF = cnt;
            state = 2;
        } else if (state == 2) {
            state = 0;
            command = 0;
        } else if (state == 3) {
            SSP2BUF = knob;
            state = 4;
        } else if (state == 4) {
            state = 0;
            command = 0;
        }
        
        SSP2IF = 0;
    }
}


