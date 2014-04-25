-module(serial).
-export([serialinit/0]).
-define(SERIAL_WAIT_PORT, 12345).
-define(SERIAL_C_PORT, 54321).

serialinit() ->
	{ok, Socket} = gen_udp:open(?SERIAL_WAIT_PORT, [binary, inet, {active, true}]),
	serialloop(Socket).

serialloop(Socket) ->
	{ok, Local} = inet_parse:address("127.0.0.1"),
	receive
		{udp, Socket, Local, ?SERIAL_C_PORT, Packet} ->
			<<_:16, BinMid:16,_>> = Packet,
			IntMid = binary_to_integer(BinMid),
			case get_keys(IntMid) of
				undefined ->
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
			gen_udp:send(Socket, Local, ?SERIAL_C_PORT, Packet),
			serialloop(Socket)	
	end.
