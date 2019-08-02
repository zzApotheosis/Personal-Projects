/*
 * File:   OneWire.c
 * Author: Brad McGarvey
 *
 * Created on March 19, 2014, 10:15 AM
 */

#include <xc.h>
#include "OneWire.h"

unsigned char lastDiscrepancy;
unsigned char lastDevice;
OWRomCode OWSearchRomCode;

void OWInit(void) {
    OWPowerOff();
    DQHigh();
}

unsigned char OWReset(void) {
    char presence;
    DQLow();
    __delay_us(480);
    DQHigh();
    __delay_us(80);
    presence = DQ;
    __delay_us(400);
    return (!presence);
}

unsigned char OWReadByte(void) {
    unsigned char b = 0;
    char i;
    for (i = 0; i < 8; ++i) {
        b >>= 1;
        DQLow();
        __delay_us(1);
        DQHigh();
        __delay_us(12);
        if (DQ) {
            b |= 0b10000000;
        }
        __delay_us(48);
    }
    return b;
}

void OWWriteByte(unsigned char b) {
    char i;
    for (i = 0; i < 8; ++i) {
        if (b & 1) {
            DQLow();
            __delay_us(2);
            DQHigh();
            __delay_us(59);
        } else {
            DQLow();
            __delay_us(61);
            DQHigh();
        }
        b >>= 1;
        __delay_us(1);
    }
}

int OWReadBytes(unsigned char *buff, int len) {
    int i;
    for (i = 0; i < len; ++i) {
        buff[i] = OWReadByte();
    }
    return len;
}

int OWWriteBytes(unsigned char *buff, int len) {
    int i;
    for (i = 0; i < len; ++i) {
        OWWriteByte(buff[i]);
    }
    return len;
}

unsigned char OWReadBit(void) {
    unsigned char b;
    DQLow();
    __delay_us(1);
    DQHigh();
    __delay_us(12);
    b = DQ;
    __delay_us(48);
    return b;
}

void OWwriteBit(unsigned char b) {
    if (b == 1) {
        DQLow();
        __delay_us(2);
        DQHigh();
        __delay_us(59);
    } else {
        DQLow();
        __delay_us(61);
        DQHigh();
    }
    __delay_us(1);
}

void OWStop(void) {
    DQHigh();
    __delay_us(1);
    OWReset();
}

unsigned char OWReadROM(OWRomCode *prc) {
    char i;
    if (OWReset()) {
        OWWriteByte(OW_READ_ROM);
        for (i = 0; i < 8; ++i) {
            prc->bytes[i] = OWReadByte();
        }
        if (OWCRC(prc->bytes, 8)) {
            return 1;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}

const unsigned char OWCRCTable[] = {
    0, 94, 188, 226, 97, 63, 221, 131, 194, 156, 126, 32, 163, 253, 31, 65,
    157, 195, 33, 127, 252, 162, 64, 30, 95, 1, 227, 189, 62, 96, 130, 220,
    35, 125, 159, 193, 66, 28, 254, 160, 225, 191, 93, 3, 128, 222, 60, 98,
    190, 224, 2, 92, 223, 129, 99, 61, 124, 34, 192, 158, 29, 67, 161, 255,
    70, 24, 250, 164, 39, 121, 155, 197, 132, 218, 56, 102, 229, 187, 89, 7,
    219, 133, 103, 57, 186, 228, 6, 88, 25, 71, 165, 251, 120, 38, 196, 154,
    101, 59, 217, 135, 4, 90, 184, 230, 167, 249, 27, 69, 198, 152, 122, 36,
    248, 166, 68, 26, 153, 199, 37, 123, 58, 100, 134, 216, 91, 5, 231, 185,
    140, 210, 48, 110, 237, 179, 81, 15, 78, 16, 242, 172, 47, 113, 147, 205,
    17, 79, 173, 243, 112, 46, 204, 146, 211, 141, 111, 49, 178, 236, 14, 80,
    175, 241, 19, 77, 206, 144, 114, 44, 109, 51, 209, 143, 12, 82, 176, 238,
    50, 108, 142, 208, 83, 13, 239, 177, 240, 174, 76, 18, 145, 207, 45, 115,
    202, 148, 118, 40, 171, 245, 23, 73, 8, 86, 180, 234, 105, 55, 213, 139,
    87, 9, 235, 181, 54, 104, 138, 212, 149, 203, 41, 119, 244, 170, 72, 22,
    233, 183, 85, 11, 136, 214, 52, 106, 43, 117, 151, 201, 74, 20, 246, 168,
    116, 42, 200, 150, 21, 75, 169, 247, 182, 232, 10, 84, 215, 137, 107, 53
};

unsigned char OWCRC(unsigned char *buff, int len) {
    unsigned char crc = 0;
    char i;
    for (i = 0; i < len; ++i) {
        crc = OWCRCTable[crc ^ buff[i]];
    }
    return !crc;
}

void OWStartROMSearch(void) {
    int i;
    lastDiscrepancy = 0;
    lastDevice = 0;
    for (i = 0; i < 8; ++i) {
        OWSearchRomCode.bytes[i] = 0;
    }
}

unsigned char OWNextROM(char alarm) {
    char pos;
    unsigned char bit1, bit2;
    unsigned char *currentByte;
    unsigned char direction;
    unsigned char lastZero;
    unsigned char oneMask = 0b00000001;
    unsigned char zeroMask = 0b11111110;

    if (!OWReset()) {
        return 0;
    }
    if (lastDevice) {
        return 0;
    }
    pos = 1;
    lastZero = 0;
    currentByte = OWSearchRomCode.bytes;
    if (alarm) {
        OWWriteByte(OW_ALARM_SEARCH);
    } else {
        OWWriteByte(OW_SEARCH_ROM);
    }
    do {
        bit1 = OWReadBit();
        bit2 = OWReadBit();
        if (bit1 == 1 && bit2 == 1) {
            return 0;
        }
        if (bit1 == 0 && bit2 == 0) {
            if (lastDiscrepancy == pos) {
                direction = 1;
            } else if (pos > lastDiscrepancy) {
                direction = 0;
            } else {
                direction = (*currentByte >> ((pos - 1) % 8)) & 1;
            }
            if (direction == 0) {
                lastZero = pos;
            }
        } else {
            direction = bit1;
        }
        OWwriteBit(direction);
        if (direction == 0) {
            *currentByte &= zeroMask;
        } else {
            *currentByte |= oneMask;
        }
        zeroMask = (zeroMask << 1) | (zeroMask >> 7);
        oneMask = (oneMask << 1) | (oneMask >> 7);
        if (pos % 8 == 0) {
            ++currentByte;
        }
        ++pos;
    } while (pos <= 64);
    lastDiscrepancy = lastZero;
    if (lastDiscrepancy == 0) {
        lastDevice = 1;
    }
    if (OWCRC(OWSearchRomCode.bytes, sizeof (OWSearchRomCode))) {
        return 1;
    } else {
        return 0;
    }
}