#include <erl_nif.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "cantcoap.h"

#define TOKEN_LENGTH 8
#define URI_LENGTH 128

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

BEGIN_C

extern int errno;

/* Type, Method, Token, ID, URI */
static ERL_NIF_TERM make_pdu_5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int type, method, id;
	char uri[URI_LENGTH + 1];
	unsigned char token[TOKEN_LENGTH + 1];
	ErlNifBinary result;
	CoapPDU *pdu;
	CoapPDU::Type typeenum;
	CoapPDU::Code methodenum;
    
	// unwrap all arguments
    if (argc != 5 || !enif_is_number(env, argv[0]) ||
	                        !enif_is_number(env, argv[1]) ||
	                        !enif_is_list(env, argv[2]) ||
	                        !enif_is_number(env, argv[3]) ||
	                        !enif_is_list(env, argv[4])) {
	    return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &type)) {
	    return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[1], &method)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_string(env, argv[2], reinterpret_cast<char *>(token), URI_LENGTH, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_int(env, argv[3], &id)) {
        return enif_make_badarg(env);
    }
	if (!enif_get_string(env, argv[4], uri, URI_LENGTH, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }
	
	// check enums
	if (type != CoapPDU::COAP_CONFIRMABLE &&
	     type != CoapPDU::COAP_NON_CONFIRMABLE &&
	     type != CoapPDU::COAP_ACKNOWLEDGEMENT &&
	     type != CoapPDU::COAP_RESET) {
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid type", ERL_NIF_LATIN1));
	}
	if (!(method >= 0x00 && method <= 0xff)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid method", ERL_NIF_LATIN1));
    }
	
	// make the PDU
	typeenum = static_cast<CoapPDU::Type>(type);
	methodenum = static_cast<CoapPDU::Code>(method);
	pdu = new CoapPDU();
	pdu->setVersion(1);
	pdu->setType(typeenum);
	pdu->setCode(methodenum);
	pdu->setToken(token, strlen(reinterpret_cast<char *>(token)));
	pdu->setMessageID(id);
	pdu->setURI(uri, strlen(uri));
	pdu->addOption(CoapPDU::COAP_OPTION_CONTENT_FORMAT,1,(uint8_t*)")");
	
	// return the binary data
	if (!enif_alloc_binary(pdu->getPDULength(), &result)) {
	    delete pdu;
	    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Unable to allocate the result", ERL_NIF_LATIN1));
	}
	memcpy(result.data, pdu->getPDUPointer(), pdu->getPDULength());
	delete pdu;
	
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
}

/* Buffer */
static ERL_NIF_TERM get_content_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary buffer, result;
    CoapPDU *pdu;
    
    // unwrap all arguments
    if (argc != 1 || !enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_iolist_as_binary(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }
    
    // parse the result
    pdu = new CoapPDU(buffer.data, buffer.size);
    if (!pdu->validate()) {
        delete pdu;
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
    }
    
    // return the payload
    if (!enif_alloc_binary(pdu->getPayloadLength(), &result)) {
        delete pdu;
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Unable to allocate the result", ERL_NIF_LATIN1));
    }
	if(pdu->getPayloadPointer() == NULL || pdu->getPayloadLength() == 0)
	{
			delete pdu;
			return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "no payload", ERL_NIF_LATIN1));
	}
    memcpy(result.data, pdu->getPayloadPointer(), pdu->getPayloadLength());
    delete pdu;
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
}

/*Add option with for parameter:
			 PDU, option number, option length, option value
*/
static ERL_NIF_TERM add_option_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer,result;
		CoapPDU *pdu;
//		CoapPDU::Option optnum;
		int optlen, optnum_int;
		uint8_t *optval;

		if(argc != 4 || !enif_is_binary(env,argv[0]) ||
				!enif_is_number(env,argv[1]) ||
				!enif_is_number(env,argv[2]) ||
				!enif_is_list(env,argv[3]))
		{
				return enif_make_badarg(env);
		}
		if(!enif_get_int(env,argv[1], &optnum_int))
				return enif_make_badarg(env);
//		optnum = static_cast<CoapPDU::Option>(optnum_int);
		if(!enif_get_int(env,argv[2], &optlen))
				return enif_make_badarg(env);
		optval = (uint8_t *)malloc(sizeof(uint8_t) * (optlen+1));
		if(!enif_get_string(env,argv[3], reinterpret_cast<char *>(optval), optlen+1, ERL_NIF_LATIN1))
				return enif_make_badarg(env);
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);

		//parse the result
		pdu = new CoapPDU(buffer.data, 2*buffer.size,  buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				free(optval);
				return enif_make_tuple2(env, enif_make_atom(env,"error"), enif_make_string(env,"Invalid PDU", ERL_NIF_LATIN1));
		}

		if(pdu->addOption(optnum_int, optlen, (uint8_t *)optval))
		{
				delete pdu;
				free(optval);
				return enif_make_tuple2(env, enif_make_atom(env,"error"), enif_make_string(env,"add option failed",ERL_NIF_LATIN1));
		}

		if(!enif_alloc_binary(pdu->getPDULength(), &result))
		{
				delete pdu;
				free(optval);
				return enif_make_tuple2(env, enif_make_atom(env,"error"), enif_make_string(env,"Unable to allocate the result", ERL_NIF_LATIN1));
		}
		memcpy(result.data, pdu->getPDUPointer(), pdu->getPDULength());
		delete pdu;
		free(optval);
		return enif_make_tuple2(env,enif_make_atom(env,"ok"),enif_make_binary(env, &result));
}

/*add payload to the PDU, need parameter: PDU, payload value, payload length(bytes)*/
static ERL_NIF_TERM add_payload_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer, result;
		CoapPDU *pdu;
		int vallen;
		uint8_t *value;
		if(argc !=3 || !enif_is_binary(env,argv[0]) ||
				!enif_is_list(env,argv[1]) ||
				!enif_is_number(env,argv[2]))
		{
				return enif_make_badarg(env);
		}
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);
		if(!enif_get_int(env, argv[2], &vallen))
				return enif_make_badarg(env);
		value = (uint8_t *)malloc(sizeof(uint8_t) * (vallen));
		if(!enif_get_string(env, argv[1], reinterpret_cast<char *>(value), vallen, ERL_NIF_LATIN1))
				return enif_make_badarg(env);
		//parse the result
		pdu = new CoapPDU(buffer.data, 2*buffer.size, buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
		}

		if(pdu->setPayload((uint8_t *)value, vallen))
		{
				delete pdu;
		//		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, pdu->setPayload((uint8_t *)value, vallen)));
		//		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env,(char *) value, ERL_NIF_LATIN1));
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env,"add payload failed", ERL_NIF_LATIN1));
		}

		//return the binary data
		if(!enif_alloc_binary(pdu->getPDULength(), &result))
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env,"error"), enif_make_string(env, "Unable to alloc the result", ERL_NIF_LATIN1));
		}
		memcpy(result.data, pdu->getPDUPointer(), pdu->getPDULength());
		delete pdu;
		free(value);
		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &result));
}

/*need one parameter: PDU, to see if the packet is ACKED*/
static ERL_NIF_TERM get_header_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer;
		int ver, type, tkl, code, messageID;
		CoapPDU *pdu;

		//unwrap all arguments
		if(argc != 1 || !enif_is_binary(env, argv[0]))
				return enif_make_badarg(env);
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);

		//parse the result
		pdu = new CoapPDU(buffer.data, buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"),enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
		}

		//return the Packet header
		ver = (int)pdu->getVersion();
		type = pdu->getType();
		type = type >> 4;
		tkl = pdu->getTokenLength();
		code = pdu->getCode();
		messageID = (unsigned int)pdu->getMessageID();
		delete pdu;
		return enif_make_tuple6(env,enif_make_atom(env, "ok"), enif_make_int(env, ver), enif_make_int(env, type), enif_make_int(env, tkl), enif_make_int(env, code), enif_make_uint(env, messageID));

}

/*need one parameter: PDU */
static ERL_NIF_TERM get_token_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer, token;
		CoapPDU *pdu;
		int tkl;
		//unwrap all arguments
		if(argc != 1 || !enif_is_binary(env, argv[0]))
				return enif_make_badarg(env);
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);
		//parse the result
		pdu = new CoapPDU(buffer.data, buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"),enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
		}
		//return the token
		tkl = pdu->getTokenLength();
		if(!enif_alloc_binary(tkl, &token))
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Unable to allocate the result", ERL_NIF_LATIN1));
		}
		memcpy(token.data, pdu->getTokenPointer(), tkl);
		delete pdu;
		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &token));
}

/*need PDU*/
static ERL_NIF_TERM get_URI_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer;
		char result[12];								//maximum option length is 12 Byte
		int length;
		CoapPDU *pdu;
		//unwrap all arguments
		if(argc != 1 || !enif_is_binary(env, argv[0]))
				return enif_make_badarg(env);
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);
		//parse the result
		pdu = new CoapPDU(buffer.data, buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"),enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
		}
		//return URI
		if(pdu->getURI(result, 14, &length))
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "get URI failed", ERL_NIF_LATIN1));
		}
		delete pdu;
		return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom_len(env, &result[1],(size_t)(length-1)));
}
static ERL_NIF_TERM get_option_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
		ErlNifBinary buffer;
		int get_opt, opt_num, i;
		CoapPDU *pdu;
		struct CoapPDU::CoapOption *opt;
		//unwrap all arguments
		if(argc != 2 || !enif_is_binary(env, argv[0]) || !enif_is_number(env, argv[1]))
				return enif_make_badarg(env);
		if(!enif_inspect_iolist_as_binary(env, argv[0], &buffer))
				return enif_make_badarg(env);
		if(!enif_get_int(env, argv[1], &get_opt))
				return enif_make_badarg(env);
		//parse the result
		pdu = new CoapPDU(buffer.data, 2*buffer.size, buffer.size);
		if(!pdu->validate())
		{
				delete pdu;
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Invalid PDU", ERL_NIF_LATIN1));
		}
		//return option value
		opt = pdu->getOptions();
		opt_num = pdu->getNumOptions();
		for(i=0; i<opt_num; i++)
		{
				if(get_opt == (int)opt[i].optionNumber)
				{
						delete pdu;
						return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_string_len(env, (char *)opt[i].optionValuePointer, opt[i].optionValueLength, ERL_NIF_LATIN1));
				}
		}
		//can not find match option
		delete pdu;
//		return enif_make_tuple5(env, enif_make_atom(env, "error"), enif_make_int(env, (int)opt[0].optionNumber), enif_make_int(env, (int)opt[1].optionNumber), enif_make_int(env, (int)opt[2].optionNumber), enif_make_int(env, (int)opt[3].optionNumber));
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "can't find match option", ERL_NIF_LATIN1));
}





static ErlNifFunc pdu_NIFs[] = {
    {"make_pdu", 5, &make_pdu_5},
    {"get_content", 1, &get_content_1},
	{"add_option", 4, &add_option_4},
	{"add_payload", 3, &add_payload_3},
	{"get_header", 1, &get_header_1},
	{"get_token", 1, &get_token_1},
	{"get_URI", 1, &get_URI_1},
	{"get_option", 2, &get_option_2}	
};

ERL_NIF_INIT(pdu, pdu_NIFs, NULL, NULL, NULL, NULL);

END_C
