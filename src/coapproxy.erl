-module(coapproxy).
-export([main/0, main/1]).
-include("coap.hel").
%-define(MAX_ID, 65536).
%-define(TOKEN_LENGTH, 8).
%-define(PORT, 5683).
%-define(TMP_PORT, 6666).
%-define(COAP_GET, 1).
%-define(COAP_POST, 2).
%-define(COAP_PUT, 3).
%-define(COAP_DELETE, 4).

%% @spec main()
%% this is a proxy demon
main() ->
	Pid0 = spawn(fun openport/0),
	register(proxy, Pid0).

%% @spec main(P::integer())
%% this is a proxy demon can be assign a special udp prot
main(P) ->
	Pid2 = spawn(fun() -> openport(P) end),
	register(proxy1, Pid2).

openport() ->
	case gen_udp:open(?PORT, [binary, inet, {active, true}]) of
		{ok, Socket} ->
			io:format("Port:~p opened, wait for messages.~n",[?PORT]),
			proxy_recv(Socket);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.
openport(Port) ->
	case gen_udp:open(Port, [binary, inet, {active, true}]) of
		{ok, Socket} ->
			io:format("Port:~p opened, wait for messages.~n",[Port]),
			proxy_recv(Socket);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.

proxy_recv(S) ->
	receive
		{udp, S, FromIP, FromPort, Bin} ->
			spawn(fun() -> wait_response(FromIP, FromPort, S, Bin) end),
			proxy_recv(S);
		{error, Reason} ->
			io:format("{get error message, ~p}~n", [Reason]),
			proxy_recv(S);
		stop ->
		gen_udp:close(S),
		io:format("UDP port closed~n")
	end.

wait_response(FromIP, FromPort, Socket, Bin) ->
	case pdu:get_URI(Bin) of
        {ok, URI} ->
            io:format("Get request message from:~p:~p with URI: ~p~n",[FromIP, FromPort, URI]);
        {error, URI} ->
            io:format("get_URI error:~p~n",[{error, URI}]),
			exit(no_URI)
    end,
    case pdu:get_option(Bin, ?COAP_OPTION_URI_HOST) of
        {ok, URI_HOST} ->
            io:format("Get URI_HOST: ~p~n",[URI_HOST]);
        {error, URI_HOST} ->
            io:format("get_option error ~p~n", [{error, URI_HOST}])
%			exit(no_URI_HOST)
    end,
	{ok, RemoteIP} = inet_parse:address(URI_HOST),
    case pdu:get_option(Bin, ?COAP_OPTION_URI_PORT) of
        {ok, URI_PORT} ->
            io:format("Get URI_PORT: ~p~n",[URI_PORT]);
        {error, URI_PORT} ->
            io:format("get_option error ~p~n", [{error, URI_PORT}])
%			exit(no_URI_PORT)
    end,
	RemotePort = list_to_integer(URI_PORT),
    case pdu:get_header(Bin) of
        {ok, _Version, _Type, _Tkl, _Code, _MessageID} ->
            io:format("Get Version: ~p, Type: ~p, Tkl: ~p, Code: ~p, MessageID: ~p~n",[_Version, _Type, _Tkl, _Code, _MessageID]);
        {error, H_reason} ->
            io:format("get_header error ~p~n", [{error, H_reason}]),
			{_Type, _Code, _MessageID} = {0, 0, 0},							%Unsafe case
			exit(no_Header)
    end,
    case pdu:get_token(Bin) of
        {ok, Token} ->
            io:format("Get Token: ~p~n",[Token]);
        {error, Token} ->
            io:format("get_token error ~p~n", [{error, Token}]),
			exit(no_Token)
    end,
%	{ok, PDU} = pdu:make_pdu(_Type, _Code, binary_to_list(Token), _MessageID, atom_to_list(URI)),
	{ok, Socket_to} = serial:slipinit(),
%	case pdu:get_content(Bin) of
%		{ok, Value} ->
%			NewPDU = pdu:add_payload(PDU, binary_to_list(Value), length(binary_to_list(Value))),
			%send to serial prot
%			io:format("Everything is ok, prepare to send to serial port~n"),
%			gen_udp:send(Socket_to, RemoteIP, RemotePort, NewPDU);
%		{error, _Reason} ->
			%send to serial prot
%			io:format("Everything is ok, but without payload, prepare to send to serial port~n"),
			gen_udp:send(Socket_to, RemoteIP, RemotePort, Bin),
%	end,
	receive
		{udp, Socket_to, RemoteIP, RemotePort, B} ->
			gen_udp:send(Socket, FromIP, FromPort, B)
		after 5000 ->
			case pdu:make_pdu(?COAP_ACKNOWLEDGEMENT, ?COAP_GATEWAY_TIMEOUT, binary_to_list(Token), _MessageID, atom_to_list(URI)) of
				{ok, TimeoutResponse} ->
					gen_udp:send(Socket, FromIP, FromPort, TimeoutResponse),
					io:format("Not received response in 5 seconds.~n");
				{error, Make_pdu} ->
					io:format("Make pdu failed with reason: ~p~n",[Make_pdu])
			end
	end.
