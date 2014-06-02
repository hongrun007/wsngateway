-module(pdu).
-export([init/0, make_pdu/5, get_content/1, add_option/4, add_payload/3, get_header/1, get_token/1, get_URI/1, get_option/2]).
-on_load(init/0).

-define(APPNAME, wsngateway).

init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "pdu_drv"]), 0)
    end.

%% @spec make_pdu(Type::integer(), Method::integer(), Token::list(), MID::integer(), URI::list()) ->
%%			{ok, PDU::binary()} | {error, Reason::string()}
make_pdu(_Type, _Method, _Token, _ID, _URI) ->
    erlang:nif_error(nif_not_loaded).

%% @spec get_content(Buffer::binary()) ->
%%			{ok, newpdu::bianry()} | {error, Reason::string()}
get_content(_Buffer) ->
    erlang:nif_error(nif_not_loaded).

%% @spec add_option(PDU::binary(), Optnum::integer(), Optlen::integer(), Optval::list()) ->
%%			{ok ,newpdu::binary()} | {error, Reason::string()}
add_option(_PDU, _Optnum, _Optlen, _Opnval) ->
	erlang:nif_error(nif_not_loaded).

%% @spec add_pauload(PDU::binary(), Payloadvalue::list(), Payloadlen::integer()) ->
%%			{ok, newpdu::binary()} | {error, Reason::string()}
add_payload(_PDU, _Payloadvalue, _Payloadlen) ->
	erlang:nif_error(nif_not_loaded).

%% @spec get_header(PDU::binary()) ->
%% 			{ok, Version::integer(), Type::integer(), Tokenlength::integer(), Code::integer(), message::integer()} |
%%			{error, Reason::string()}
get_header(_PDU) ->
	erlang:nif_error(nif_not_loaded).

%% @spec get_token(PDU::binary()) ->
%%			{ok, Token::binary()} | {error, Reason::string()}
get_token(_PDU) ->
	erlang:nif_error(nif_not_loaded).

%% @spec get_URI(PDU::binary()) ->
%%			{ok, URI::atom()} | {error, Reason::string()}
get_URI(_PUD) ->
	erlang:nif_error(nif_not_loaded).

%% @spec get_option(PDU::binary(), Optnum::integer()) ->
%%			{ok, optionvalue::string()} | {error, Reason::string()}
get_option(_PDU, _Optnum) ->
	erlang:nif_error(nif_not_loaded).
