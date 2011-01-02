-define(Now2Timestamp(X), encode_Timestamp(X)). 
-define(Trace(IoDevice,X),trace_line(IoDevice,X)).

open_log_file (LogFileName) ->
    case file:open(LogFileName, [write,{encoding, utf8}]) of 
	{ok,IoDevice} -> IoDevice;
	{error,Reason} -> 
	    io:format("File Open Error: ~w ~n",[file:format_error(Reason)])
    end.	



string_format(Pattern, Values, LineNo) -> lists:flatten(io_lib:format("L# ~w : "++Pattern, [LineNo|Values])).

trace_line(IoDevice,X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
			  io:format(IoDevice,"~p/~p/~p - ~p:~p:~p:~p [~p * ~p * ~p] --> ~p~n",
				    [Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE, self(), X]).



encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
    {{Year,Month,Day},{HH,MM,SS}}=
	calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
    {Year,Month,Day,HH,MM,SS,MicroSeconds}.


