#include <xc.h>
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

char string[17];

char rxBuffer[20];
volatile int txCount;
volatile int rxCount;

char txStr[] = "Hello World\n";

void main(void) {
    txCount = rxCount = 0;
    OSCTUNEbits.PLLEN = 1;
    LCDInit();
    LCDClear();
    InitPins();
    ConfigPeriph();
    ConfigInterrupts();
    TXSTA1bits.TXEN = 1;
    
    char temp;
    
    while (1) {
        while (PIR1bits.RC1IF == 0);  //Wait for a byte
        temp = RCREG1;
        TXREG1 = temp;
        rxBuffer[rxCount] = temp; //Read the byte into the string buffer
        if (rxBuffer[rxCount] == '\r') {  //check for CR
            rxBuffer[rxCount] = '\0'; //Terminate the string
            LCDClearLine(0);
            LCDWriteLine(rxBuffer, 0);
            rxCount = 0;
//            LATDbits.LATD1 ^= 1;
        } else if (rxCount < 16) {
            ++rxCount;
//            LATDbits.LATD0 ^= 1;
        }
        
//        LATD = 0;
        if (temp == '0') {
            LATDbits.LD0 ^= 1;
        } else if (temp == '1') {
            LATDbits.LD1 ^= 1;
        } else if (temp == '2') {
            LATDbits.LD2 ^= 1;
        } else if (temp == '3') {
            LATDbits.LD3 ^= 1;
        } else if (temp == '4') {
            LATDbits.LD4 ^= 1;
        } else if (temp == '5') {
            LATDbits.LD5 ^= 1;
        } else if (temp == '6') {
            LATDbits.LD6 ^= 1;
        } else if (temp == '7') {
            LATDbits.LD7 ^= 1;
        }
        
//        if (TXSTA1bits.TXEN == 0) {  //Turn on TX if it's off
//            TXSTA1bits.TXEN = 1;
//        }
        
//        if (PORTBbits.RB0 == 0) {
//            LATDbits.LATD7 = 1;
//            TXSTA1bits.TXEN = 1;
//        } else {
//            LATDbits.LATD7 = 0;
//            TXSTA1bits.TXEN = 0;
//        }
    }
}

void InitPins(void) {
    LATD = 0; //LED's are outputs
    TRISD = 0; //Turn off all LED's
    TRISBbits.TRISB0 = 1;
    //Set TRIS bits for any required peripherals here.
    TRISC = 0b10000000; //RC7 is RX, RC6 is TX

}

void ConfigInterrupts(void) {

    RCONbits.IPEN = 0; //no priorities.  This is the default.

    //Configure your interrupts here

    //Interrupt on USART transmit buffer empty
    //PIE1bits.TX1IE = 1; //enable tx interrupt
    INTCONbits.PEIE = 1; //turn on peripheral interrupts

    INTCONbits.GIE = 1; //Turn on interrupts
}

void ConfigPeriph(void) {

    //Configure peripherals here

    //Configure the USART for 19200 baud asynchronous transmission
    SPBRG1 = 520; //19200 baud
    SPBRGH1 = 520 >> 8;
    TXSTA1bits.BRGH = 1;
    BAUDCON1bits.BRG16 = 1;
    TXSTA1bits.SYNC = 0; //asynchronous mode
    RCSTA1bits.SPEN = 1; //Enable the serial port
    RCSTA1bits.CREN = 1; //Enable reception

}

void interrupt HighIsr(void) {
    //Check the source of the interrupt
    if (PIR1bits.TX1IF == 1) {
        //Transmit register is empty
        TXREG1 = txStr[txCount];
        ++txCount;
        if (txStr[txCount] == '\0') {  //Check for the end of the string
            txCount = 0;
        }
    }
}



