-module(serial).
-export([slipinit/0]).

%% @spec slipinit() ->
%%		{ok, Socket::socket()} | {error, Reason}
%% This function initialize the SLIP port
slipinit() ->
	{ok, LocalIP} = inet_parse:address("2001::a"),
	gen_udp:open(0, [binary, inet6, {active, true}, {ifaddr, LocalIP}]).

serialloop(Socket) ->
	{ok, Local} = inet_parse:address("127.0.0.1"),
	receive
		{udp, Socket, Local, 12345, Packet} ->
			{ok, _, _, _, _, Mid} = pdu:get_header(Packet),
%			IntMid = binary_to_integer(BinMid),
			io:format("Mid is: ~p~n",[Mid]),
			case get_keys(Mid) of
				undefined ->
					io:format("undefined MessageID\n"),
					undefined;
				[PID] ->
					erase(PID),
					PID ! {back, self(), Packet}
			end,
			serialloop(Socket);
		{delete, PID} ->
			erase(PID),
			serialloop(Socket);
		{FromPid, Mid, URI_HOST, URI_PORT, Coap} ->
			put(FromPid, Mid),
			Host_bin = list_to_binary(URI_HOST),
			Port_int = list_to_integer(URI_PORT),
			Packet = <<Host_bin/binary, 255:8, Port_int:16, 255:8, Coap/binary>>,
			gen_udp:send(Socket, Local, 12345, Packet),
			serialloop(Socket)	
	end.
