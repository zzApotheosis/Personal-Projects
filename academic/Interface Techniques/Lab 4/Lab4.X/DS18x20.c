/*
 * File:   DS18x20.c
 * Author: Brad McGarvey
 *
 * Created on March 20, 2014, 9:48 PM
 */

#include <xc.h>
#include <stdlib.h>
#include "DS18x20.h"
//#include "Oscillator.h"

double ReadDS18S20(OWRomCode *rc) {
    DS18S20Scratchpad sp;

    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0x44); //Convert
    while (OWReadBit() == 0);
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0xBE); //Read scratchpad
    OWReadBytes(sp.bytes, 9);
    if (OWCRC(sp.bytes, 9)) {
        return (sp.temperature / 2.0);
    } else {
        return -999.9;
    }
}

double ReadDS18S20PAR(OWRomCode *rc) {
    DS18S20Scratchpad sp;
    char i;

    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0x44); //Convert
    OWPowerOn();
    for (i = 0; i < 75; ++i) {
        __delay_ms(10);
    }
    OWPowerOff();
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0xBE); //Read scratchpad
    OWReadBytes(sp.bytes, 9);
    if (OWCRC(sp.bytes, 9)) {
        return (sp.temperature / 2.0);
    } else {
        return -999.9;
    }
}

double ReadDS18B20(OWRomCode *rc) {
    DS18B20Scratchpad sp;
    double temp;
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0x44); //Convert
    while (OWReadBit() == 0);
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0xBE); //Read scratchpad
    OWReadBytes(sp.bytes, 9);
    if (OWCRC(sp.bytes, 9)) {
        temp = (sp.temperature & 0b1111) / 16.0;
        temp += sp.temperature >> 4;
    } else {
        return -999.9;
    }
    return temp;
}

double ReadDS18B20PAR(OWRomCode *rc) {
    DS18B20Scratchpad sp;
    char i;

    double temp;
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0x44); //Convert
    OWPowerOn();
    for (i = 0; i < 75; ++i) {
        __delay_ms(10);
    }
    OWPowerOff();
    OWReset();
    if (rc == NULL) {
        OWWriteByte(OW_SKIP_ROM);
    } else {
        OWWriteByte(OW_MATCH_ROM);
        OWWriteBytes(rc->bytes, sizeof(OWRomCode));
    }
    OWWriteByte(0xBE); //Read scratchpad
    OWReadBytes(sp.bytes, 9);
    if (OWCRC(sp.bytes, 9)) {
        temp = (sp.temperature & 0b1111) / 16.0;
        temp += sp.temperature >> 4;
    } else {
        return -999.9;
    }
    return temp;
}