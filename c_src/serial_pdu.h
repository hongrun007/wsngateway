#include <stdint.h>
#include "cantcoap.h"
#define SERIAL_PACKET_LENGTH 100

struct serial_pdu
{
		uint8_t dstip[16];
		uint8_t srcip[16];
		uint16_t dstport;
		uint16_t srcport;
		uint8_t coaplen;
		CoapPDU *coappdu;
};
