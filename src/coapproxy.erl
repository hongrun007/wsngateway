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
	register(proxy, Pid0),
	Pid1 = spawn(fun openport6/0),
	register(proxy6, Pid1).

%% @spec main(P::integer())
%% this is a proxy demon can be assign a special udp prot
main(P) ->
	Pid2 = spawn(fun() -> openport(P) end),
	register(proxy, Pid2),
	Pid3 = spawn(fun() -> openport6(P) end),
	register(proxy6, Pid3).

openport() ->
	case gen_udp:open(?PORT, [binary, inet, {active, true}]) of
		{ok, Socket} ->
			io:format("IPv4 Port:~p opened, wait for messages.~n",[?PORT]),
			% 4 means IPv4
			proxy_recv(Socket, 4);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.
openport(Port) ->
	case gen_udp:open(Port, [binary, inet, {active, true}]) of
		{ok, Socket} ->
			io:format("IPv4 Port:~p opened, wait for messages.~n",[Port]),
			proxy_recv(Socket, 4);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.

openport6() ->
	case gen_udp:open(?PORT + 1, [binary, inet6, {active, true}]) of
		{ok, Socket} ->
			io:format("IPv6 Port:~p opened, wait for messages.~n",[?PORT + 1]),
			% 6 means IPv6
			proxy_recv(Socket, 6);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.
openport6(Port) ->
	case gen_udp:open(Port + 1, [binary, inet6, {active, true}]) of
		{ok, Socket} ->
			io:format("IPv6 Port:~p opened, wait for messages.~n",[Port+1]),
			proxy_recv(Socket, 6);
		{error, Reason} ->
			io:format("{error, ~p}~n", [Reason])
	end.
proxy_recv(S, FromIPvN) ->
	receive
		{udp, S, FromIP, FromPort, Bin} ->
			spawn(fun() -> wait_response(FromIP, FromPort, S, Bin, FromIPvN) end),
			proxy_recv(S, FromIPvN);
		{error, Reason} ->
			io:format("{get error message, ~p}~n", [Reason]),
			proxy_recv(S, FromIPvN);
		stop ->
		gen_udp:close(S),
		io:format("UDP port closed~n")
	end.

wait_response(FromIP, FromPort, Socket, Bin, FromIPvN) ->
	{_, A1, A2} = now(),
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
	case FromIPvN of
		4->
			RemotePort = list_to_integer(URI_PORT);
		6 ->
			Temp = [0|URI_PORT],
			[X1,X2,X3] = Temp,
			RemotePort = X2 *256 + X3
	end,
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
	{ok, Socket_to} = case FromIPvN of
		4 ->
			gen_udp:open(0, [binary, inet6, {active, true}]);
		6 ->
			gen_udp:open(0, [binary, inet, {active, true}])
	end,
	gen_udp:send(Socket_to, RemoteIP, RemotePort, Bin),
	%% send the packet and wait for the response
	{_, A3, A4} = now(),
	io:format("From client at: ~p seconds, ~p microseconds\n", [A1, A2]),
	io:format("Leave proxy at: ~p seconds, ~p microseconds\n", [A3, A4]),
	io:format("Process in proxy use: ~p seconds, ~p microseconds\n",[A3-A1, A4-A2]),
	receive
		{udp, Socket_to, RemoteIP, RemotePort, B} ->
			{_, A5, A6} = now(),
			io:formamt("Round trip time from proxy to sensor: ~p seconds, ~p microseconds\n", [A5-A3, A6-A4]),
			gen_udp:send(Socket, FromIP, FromPort, B),
			{_, A7, A8} = now(),
			io:format("Process time to forward packet: ~p seconds, ~p microseconds\n", [A7-A5, A8-A6])
		%% after 5 seconds the gateway send back timeout information
		after 5000 ->
			case pdu:make_pdu(?COAP_ACKNOWLEDGEMENT, ?COAP_GATEWAY_TIMEOUT, binary_to_list(Token), _MessageID, atom_to_list(URI)) of
				{ok, TimeoutResponse} ->
					gen_udp:send(Socket, FromIP, FromPort, TimeoutResponse),
					io:format("Not received response in 5 seconds.~n");
				{error, Make_pdu} ->
					io:format("Make pdu failed with reason: ~p~n",[Make_pdu])
			end
	end.
