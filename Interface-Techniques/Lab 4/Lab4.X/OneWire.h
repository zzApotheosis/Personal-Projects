/* 
 * File:   OneWire.h
 * Author: Brad McGarvey
 *
 * Created on March 19, 2014, 10:15 AM
 */

#ifndef ONEWIRE_H
#define	ONEWIRE_H

enum {OW_SEARCH_ROM = 0xF0, OW_READ_ROM = 0x33, OW_MATCH_ROM = 0x55,
OW_SKIP_ROM = 0xCC, OW_ALARM_SEARCH = 0xEC};

typedef union {
    unsigned char bytes[8];
    struct {
        unsigned char family;
        unsigned char serial[6];
        unsigned char crc;
    };
} OWRomCode;

extern OWRomCode OWSearchRomCode;

void OWInit(void);
unsigned char OWReset(void);
unsigned char OWReadByte(void);
void OWWriteByte(unsigned char b);
int OWReadBytes(unsigned char *buff, int len);
int OWWriteBytes(unsigned char *buff, int len);
unsigned char OWReadBit(void);
void OWStop(void);
unsigned char OWReadROM(OWRomCode *prc);
unsigned char OWCRC(unsigned char *buff, int len);
void OWStartROMSearch(void);
unsigned char OWNextROM(char alarm);

#define OWPowerOn()     LATJbits.LATJ0 = 1; TRISJbits.TRISJ0 = 0
#define OWPowerOff()    TRISJbits.TRISJ0 = 1; LATJbits.LATJ0 = 0;

#define DQ          PORTJbits.RJ0
#define DQHigh()    TRISJbits.TRISJ0 = 1
#define DQLow()     TRISJbits.TRISJ0 = 0;

#define _XTAL_FREQ  40000000L

#endif	/* ONEWIRE_H */

