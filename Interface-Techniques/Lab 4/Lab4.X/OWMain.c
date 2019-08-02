#include <xc.h>
#include <stdio.h>
#include "LCD.h"
#include "OneWire.h"
#include "DS18x20.h"

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

char str0[17];
char str1[17];
//28fe15ce04000063 - DS18B20
//10a214d502080013 - DS18S20
OWRomCode code0 = {0x28, 0xfe, 0x15, 0xce, 0x04, 0x00, 0x00, 0x63};
OWRomCode code1 = {0x10, 0xa2, 0x14, 0xd5, 0x02, 0x08, 0x00, 0x13};
void main(void) {
    double t0 = 0;
    double t1 = 0;
    TRISD = 0;
    OSCTUNEbits.PLLEN = 1;
    LCDInit();
    LCDClear();
    OWInit();
    
    while (1) {
//        OWReadROM(&code1);
//        t0 = ReadDS18B20(&code0);
//        t1 = ReadDS18S20(&code1);
        t0 = ReadDS18S20PAR(NULL);
        LATDbits.LATD0 ^= 1;
        sprintf(str0, "T0=%.2f C", t0);
//        sprintf(str1, "T1=%.2f C", t1);

//        sprintf(str, "%02x%02x%02x%02x%02x%02x%02x%02x", code1.bytes[0], code1.bytes[1], code1.bytes[2], code1.bytes[3], code1.bytes[4], code1.bytes[5], code1.bytes[6], code1.bytes[7]);
        LCDWriteLine(str0, 0);
//        LCDWriteLine(str1, 1);
    }
}


