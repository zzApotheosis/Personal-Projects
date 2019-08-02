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



void InitPins(void);
void ConfigInterrupts(void);
void ConfigPeriph(void);

unsigned char SPIReadWrite(unsigned char byte);

#define _XTAL_FREQ 40000000L

char line1str[17];
char line2str[17];
int rx;
int count;

char flag;
char str[] = "I am Groot";
char rdStr[16] = "Invalid";

void main(void)
{
	long i;
	count = 0;
	OSCTUNEbits.PLLEN = 1;  
	LCDInit();
	LCDClear();
	InitPins();
	ConfigPeriph();
	
    //Read address 0 of EEPROM
    LATAbits.LATA3 = 0;  //enable CS
	SPIReadWrite(0b00000011);  //Read command
	SPIReadWrite(0x03);		//16 bit address (0x0300)
	SPIReadWrite(0x00);
	for (int i = 0; i < 11; i++) {
        rdStr[i] = SPIReadWrite(0);
    }
	LATAbits.LATA3 = 1;  //disable CS

    
//    LATAbits.LATA3 = 0;  //enable CS
//	SPIReadWrite(0b00000011);  //Read command
//	SPIReadWrite(0x20);		//16 bit address (0x2000)
//	SPIReadWrite(0x00);
//	flag = SPIReadWrite(0);  //Read value - data sent is dummy data
//	LATAbits.LATA3 = 1;  //disable CS

    sprintf(line2str, "Read %s", rdStr);
	LCDClearLine(1);
	LCDWriteLine(line2str, 1);

	ConfigInterrupts();

	while (1)
	{
		sprintf(line1str, "%d", count);
		LCDClearLine(0);
		LCDWriteLine(line1str, 0);
		for (i = 0; i < 50000; ++i);
		++count;	
	}
}

unsigned char SPIReadWrite(unsigned char byte)
{
	unsigned char r;
	SSP1BUF = byte; //transmit byte
	while(!PIR1bits.SSPIF);
	PIR1bits.SSPIF = 0;
	r = SSP1BUF;  //read received byte
	return r;
}

void InitPins(void)
{
	LATD = 0; 	//LED's are outputs
	TRISD = 0;  //Turn off all LED's


	//Set TRIS bits for any required peripherals here.
	TRISB = 0b00000001;	//Button0 is input;
	INTCON2bits.RBPU = 0;  //enable weak pullups on port B

	TRISC = 0b00010000; //RC5 out, RC4 in, RC3 out - SPI pins
	LATAbits.LATA3 = 1;
	TRISAbits.TRISA3 = 0;	//RA3 is CS for EEPROM
	
}

void ConfigInterrupts(void)
{

	RCONbits.IPEN = 0; //no priorities.  This is the default.

	//Configure your interrupts here

	//set up INT0 to interrupt on falling edge
	INTCON2bits.INTEDG0 = 0;  //interrupt on falling edge
	INTCONbits.INT0IE = 1;  //Enable the interrupt
	//note that we don't need to set the priority because we disabled priorities (and INT0 is ALWAYS high priority when priorities are enabled.)
	INTCONbits.INT0IF = 0;  //Always clear the flag before enabling interrupts
	
	
	INTCONbits.GIE = 1;  //Turn on interrupts
}

void ConfigPeriph(void)
{

	//Configure peripherals here

	SSP1STATbits.CKE = 1;
	SSP1CON1bits.CKP = 0;  //SPI mode 0,0
	SSP1CON1bits.SSPM = 0b0001;	//SPI Master - FOSC/16 = 2.5 MHz
	SSP1CON1bits.SSPEN = 1;	//Enable MSSP
}


void interrupt HighIsr(void)
{
	//Check the source of the interrupt
	if (INTCONbits.INT0IF == 1)
	{
		//source is INT0

		//Write address 0 of EEPROM
//		LATAbits.LATA3 = 0;  //enable CS
//		SPIReadWrite(0b00000110);  //WREN command
//		LATAbits.LATA3 = 1;  //disable CS
//		Nop();
//		LATAbits.LATA3 = 0;  //enable CS
//		SPIReadWrite(0b00000010);  //Write command
//		SPIReadWrite(0);		//16 bit address (0x0000)
//		SPIReadWrite(0);
//		SPIReadWrite(count >> 8);  //Write value
//        SPIReadWrite(count);
//		LATAbits.LATA3 = 1;  //disable CS
//        __delay_ms(6);

        LATAbits.LATA3 = 0;  //enable CS
		SPIReadWrite(0b00000110);  //WREN command
		LATAbits.LATA3 = 1;  //disable CS
		Nop();
		LATAbits.LATA3 = 0;  //enable CS
		SPIReadWrite(0b00000010);  //Write command
		SPIReadWrite(0x03);		//16 bit address (0x0300)
		SPIReadWrite(0x00);
        
        for (int i = 0; i < 11; i++) {
            SPIReadWrite(str[i]);
        }
		
        LATAbits.LATA3 = 1;  //disable CS
        __delay_ms(6);
        
//        if (count % 2 == 0) {
//            flag = 'E';
//        } else {
//            flag = 'O';
//        }
        
//        LATAbits.LATA3 = 0;  //enable CS
//		SPIReadWrite(0b00000110);  //WREN command
//		LATAbits.LATA3 = 1;  //disable CS
//		Nop();
//		LATAbits.LATA3 = 0;  //enable CS
//		SPIReadWrite(0b00000010);  //Write command
//		SPIReadWrite(0x20);		//16 bit address (0x2000)
//		SPIReadWrite(0x00);
//		SPIReadWrite(flag);  //Write value
//		LATAbits.LATA3 = 1;  //disable CS
//        __delay_ms(6);
        
		sprintf(line2str, "Wrote %s", str);
		LCDClearLine(1);
		LCDWriteLine(line2str, 1);
		INTCONbits.INT0IF = 0; //must clear the flag to avoid recursive interrupts
	}		
}
