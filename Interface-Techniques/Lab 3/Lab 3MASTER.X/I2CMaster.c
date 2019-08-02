#include <xc.h>
#include <stdio.h>
#include "LCD.h"

#pragma config FOSC=HSPLL
#pragma config WDTEN=OFF
#pragma config XINST=OFF

/*
Connections:
        Master RD5 <-> Slave RD5
        Master RD6 <-> Slave RD6
        2.2K pullups on both
 */

void InitPins(void);
void ConfigInterrupts(void);
void ConfigPeriph(void);

void I2CWriteByte(unsigned char byte, unsigned char choice);
unsigned char I2CReadByte(void);
void error(char e);

#define _XTAL_FREQ 40000000L

char line1str[17];
char line2str[17];
char rx;
unsigned char count;
unsigned char toggle = 0;

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
        sprintf(line1str, "%d", count);
        LCDClearLine(0);
        LCDWriteLine(line1str, 0);
        for (i = 0; i < 300000; ++i);
        ++count;
    }
}

void I2CWriteByte(unsigned char byte, unsigned char choice) {
    char data;

    SSP2CON2bits.SEN = 1; //Start condition
    while (SSP2CON2bits.SEN == 1); //Wait for start to finish
    data = SSP2BUF; //Read SSPxBUF to make sure BF is clear
    SSP2BUF = 0xCE; //address with R/W clear for write
    while (SSP2STATbits.BF || SSP2STATbits.R_W); // wait until write cycle is complete
    //Could check for ACK here
    
    
    SSP2BUF = choice; //Send choice, either 1 or 2 for Slave LCD Line
    while (SSP2STATbits.BF || SSP2STATbits.R_W); // wait until write cycle is complete
    //Could check for ACK here
    SSP2BUF = byte; //Send byte
    while (SSP2STATbits.BF || SSP2STATbits.R_W); // wait until write cycle is complete
    //Could check for ACK here
    
    
    
    SSP2CON2bits.PEN = 1; //Stop condition
    while (SSP2CON2bits.PEN == 1); //Wait for stop to finish
}

unsigned char I2CReadByte(void) {
    unsigned char rx;

    SSP2CON2bits.SEN = 1; //Start condition
    while (SSP2CON2bits.SEN == 1); //Wait for start to finish
    rx = SSP2BUF;  //Make sure buffer is clear
    SSP2BUF = 0xCF; //address with R/W set for read
    while (SSP2STATbits.BF || SSP2STATbits.R_W); // wait until write cycle is complete
    //Could check for ACK here
    
    SSP2CON2bits.RCEN = 1; // enable master for 1 byte reception
    while (!SSP2STATbits.BF); // wait until byte received
    rx = SSP2BUF;
    SSP2CON2bits.ACKDT = 0; //NACK for last byte.  Use a 0 here to ACK
    SSP2CON2bits.ACKEN = 1; //Send ACK/NACK
    while (SSP2CON2bits.ACKEN == 1);
    
    SSP2CON2bits.RCEN = 1; // enable master for 1 byte reception
    while (!SSP2STATbits.BF); // wait until byte received
    rx += SSP2BUF;
    SSP2CON2bits.ACKDT = 0; //NACK for last byte.  Use a 0 here to ACK
    SSP2CON2bits.ACKEN = 1; //Send ACK/NACK
    while (SSP2CON2bits.ACKEN == 1);
        
    SSP2CON2bits.RCEN = 1; // enable master for 1 byte reception
    while (!SSP2STATbits.BF); // wait until byte received
    rx += SSP2BUF; // += NOT =+ MASTER STEVEN!!!
    SSP2CON2bits.ACKDT = 0; //NACK for last byte.  Use a 0 here to ACK
    SSP2CON2bits.ACKEN = 1; //Send ACK/NACK
    while (SSP2CON2bits.ACKEN == 1);
    
    SSP2CON2bits.RCEN = 1; // enable master for 1 byte reception
    while (!SSP2STATbits.BF); // wait until byte received
    rx += SSP2BUF;
    SSP2CON2bits.ACKDT = 1; //NACK for last byte.  Use a 0 here to ACK
    SSP2CON2bits.ACKEN = 1; //Send ACK/NACK
    while (SSP2CON2bits.ACKEN == 1);
    
    
    SSP2CON2bits.PEN = 1; //Stop condition
    while (SSP2CON2bits.PEN == 1); //Wait for stop to finish
    return rx;
}

void InitPins(void) {
    LATD = 0; //LED's are outputs
    TRISD = 0; //Turn off all LED's


    //Set TRIS bits for any required peripherals here.
    TRISB = 0b00000001; //Button0 is input;
    INTCON2bits.RBPU = 0; //enable weak pullups on port B
                          //Weak Pullups? Do You Even Lift?  

    TRISD = 0b01100000; //MMSP2 uses RD5 as SDA, RD6 as SCL, both set as inputs

}

void ConfigInterrupts(void) {

    RCONbits.IPEN = 0; //no priorities.  This is the default.

    //Configure your interrupts here

    //set up INT0 to interrupt on falling edge
    INTCON2bits.INTEDG0 = 0; //interrupt on falling edge
    INTCONbits.INT0IE = 1; //Enable the interrupt
    //note that we don't need to set the priority because we disabled priorities (and INT0 is ALWAYS high priority when priorities are enabled.)
    INTCONbits.INT0IF = 0; //Always clear the flag before enabling interrupts

    INTCONbits.GIE = 1; //Turn on interrupts
}

void ConfigPeriph(void) {

    //Configure peripherals here

    SSP2ADD = 0x63; //100kHz
    SSP2CON1bits.SSPM = 0b1000; //I2C Master mode
    SSP2CON1bits.SSPEN = 1; //Enable MSSP
}


void interrupt HighIsr(void) {
    unsigned char rx;
    rx = -1;
    //Check the source of the interrupt
    if (INTCONbits.INT0IF == 1) {
        //source is INT0

        //Write count to slave
//        if (toggle == 0) {
//            I2CWriteByte(count, 1);
//            toggle = 1;
//        } else {
//            I2CWriteByte(count, 2);
//            toggle = 0;
//        }
        
        I2CWriteByte(count, toggle);
        toggle ^= 1;
        
        rx = I2CReadByte();
        sprintf(line2str, "Wrote %d, Got %d", count, rx);
        LCDClearLine(1);
        LCDWriteLine(line2str, 1);
        INTCONbits.INT0IF = 0; //must clear the flag to avoid recursive interrupts
    }
}


