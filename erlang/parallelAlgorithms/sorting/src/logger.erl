-module(logger).
-export([log/2]).
-include("trace.hrl").

log(LogFileName,ProcessName)->
    register(ProcessName,self()),
    io:format("Log File Name~p~n",[LogFileName]),
    IODevice =open_log_file("./log/"++LogFileName++".log"),
    ?Trace(IODevice,string_format("Hello, Logger process ~w is starting on node ~w",[self(), node()],?LINE)),
    loop(IODevice).


loop(IODevice)->
    receive 
	{_info,Message}->
	    io:format(IODevice,"~p~n",[Message]),
	    loop(IODevice);

	_AnyOtherMessage ->
	    io:format(IODevice,"This is an *UN-HANDLED*  Message!!! ==> ~p~n",[_AnyOtherMessage]),
	    loop(IODevice)

    end.
	    
	    
	    
	    
    
