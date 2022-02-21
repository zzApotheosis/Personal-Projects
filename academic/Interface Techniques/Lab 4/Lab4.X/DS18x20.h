/* 
 * File:   DS18x20.h
 * Author: Brad McGarvey
 *
 * Created on March 20, 2014, 9:48 PM
 */

#ifndef DS18X20_H
#define	DS18X20_H

#include "OneWire.h"

typedef union {
    struct {
        int temperature;
        unsigned char TH;
        unsigned char TL;
        int reserved;
        unsigned char countRemain;
        unsigned char countPerC;
        unsigned char crc;
    };
    unsigned char bytes[9];
} DS18S20Scratchpad;

typedef union {
    struct {
        int temperature;
        unsigned char TH;
        unsigned char TL;
        unsigned char configuration;
        char reserved[3];
        unsigned char crc;
    };
    unsigned char bytes[9];
} DS18B20Scratchpad;

double ReadDS18S20(OWRomCode *rc);
double ReadDS18B20(OWRomCode *rc);
double ReadDS18S20PAR(OWRomCode *rc);
double ReadDS18B20PAR(OWRomCode *rc);

#endif	/* DS18X20_H */

