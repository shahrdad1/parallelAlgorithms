-define(Now2Timestamp(X), encode_Timestamp(X)). 
-define(Trace(IoDevice,X),trace_line(IoDevice,X)).
-define(Log_Line(X,Y,Z),log_line(X,Y,Z)).

open_log_file (LogFileName) ->
    case file:open(LogFileName, [write,{encoding, utf8}]) of 
	{ok,IoDevice} -> IoDevice;
	{error,Reason} -> 
	    io:format("File Open Error: ~w ~n",[file:format_error(Reason)])
    end.	

%% This is to print a log line in a file by a process
trace_line(IoDevice,X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
			  io:format(IoDevice,"~p/~p/~p - ~p:~p:~p:~p [~p * ~p * ~p] --> ~p~n",
				    [Year,Month,Day,HH,MM,SS,MicroSeconds, self(), node(), ?MODULE, X]).


%% This is to prepare a log line and sending to remote log server
log_line(Pattern, Values, LineNo)->
    {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
    lists:flatten(io_lib:format("~w/~w/~w - ~w:~w:~w:~w [~w * ~w * ~w] --> L# ~w : "++Pattern,
 				[Year,Month,Day,HH,MM,SS,MicroSeconds, self(),node(), ?MODULE, LineNo|Values])).


encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
    {{Year,Month,Day},{HH,MM,SS}}=
	calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
    {Year,Month,Day,HH,MM,SS,MicroSeconds}.


string_format(Pattern, Values, LineNo) -> lists:flatten(io_lib:format("L# ~w : "++Pattern, [LineNo|Values])).
