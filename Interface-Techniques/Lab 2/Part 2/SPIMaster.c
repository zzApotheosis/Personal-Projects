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
void ConfigInterrupts(void);
void ConfigPeriph(void);

unsigned char SPIReadWrite(unsigned char byte);

#define _XTAL_FREQ 40000000L

char line1str[17];
char line2str[17];

void main(void)
{
	OSCTUNEbits.PLLEN = 1;  
	LCDInit();
	LCDClear();
	InitPins();
	ConfigPeriph();

        int rx;
        long i;

	while (1)
	{
            //Master code here

            //Command 0x01 for the count.
            LATDbits.LATD7 = 0;
            SPIReadWrite(0x01);
            __delay_us(5);
            rx = SPIReadWrite(0);
            __delay_us(5);
            rx <<= 8;
            rx += SPIReadWrite(0);
            LATDbits.LATD7 = 1;

            sprintf(line1str, "%d", rx);
            LCDClearLine(0);
            LCDWriteLine(line1str, 0);

            //Command 0x02 for the knob.
            LATDbits.LATD7 = 0;
            SPIReadWrite(0x02);
            __delay_ms(1);
            rx = SPIReadWrite(0);
            __delay_us(5);
            rx <<= 8;
            rx += SPIReadWrite(0);
            LATDbits.LATD7 = 1;

            sprintf(line2str, "%d", rx);
            LCDClearLine(1);
            LCDWriteLine(line2str, 1);

            for (i = 0; i < 100000; i++);
	}
}

unsigned char SPIReadWrite(unsigned char byte)
{
	unsigned char r;
	SSP2BUF = byte; //transmit byte
	while (SSP2STATbits.BF == 0);  //Wait for tx/rx
	r = SSP2BUF;  //read received byte
	return r;
}

void InitPins(void)
{
	LATD = 0; 	//LED's are outputs
	TRISD = 0;  //Turn off all LED's

	TRISD = 0b00100000;   //MMSP2 uses RD4 as SDO, RD5 as SDI, RD6 as SCK
	LATDbits.LATD7 = 1;  //Use RD7 as SS
}

void ConfigPeriph(void)
{

	//Configure peripherals here

	SSP2STATbits.CKE = 1;
	SSP2CON1bits.CKP = 0;  //SPI mod 0,0
	SSP2CON1bits.SSPM = 0b0001;	//SPI Master - FOSC/16 = 2.5 MHz
	SSP2CON1bits.SSPEN = 1;	//Enable MSSP
}

void ConfigInterrupts(void) {
    //Configure any interrupts here
}

void interrupt HighIsr(void)
{
    //Interrupt code goes here if needed
}


