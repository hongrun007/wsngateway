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

make_pdu(_Type, _Method, _Token, _ID, _URI) ->
    erlang:nif_error(nif_not_loaded).

get_content(_Buffer) ->
    erlang:nif_error(nif_not_loaded).

add_option(_PDU, _Optnum, _Optlen, _Opnval) ->
	erlang:nif_error(nif_not_loaded).

add_payload(_PDU, _Payloadvalue, _Payloadlen) ->
	erlang:nif_error(nif_not_loaded).

get_header(_PDU) ->
	erlang:nif_error(nif_not_loaded).

get_token(_PDU) ->
	erlang:nif_error(nif_not_loaded).

get_URI(_PUD) ->
	erlang:nif_error(nif_not_loaded).

get_option(_PDU, _Optnum) ->
	erlang:nif_error(nif_not_loaded).
