-module(master).
-export([run/0,start_slave_nodes/2]).
-include("trace.hrl").


run() ->

    IODevice =open_log_file("./log/master.log"),

    %% Register master(current) process within current node running on local machine
    MasterNode = node(),
    MasterProcessName=convert_node_name_2_process_name(MasterNode),
    register(MasterProcessName,self()),

    %% Build common arguments which is used to start logger node, slave nodes ,...
    CommonArgs = build_common_argumens(),

    %% create and start logger node on given remote machine and start logger process within the node
    [Logger|_T]=create_logger(IODevice,CommonArgs,log_machine,log_node_name),

    %% create and start slave nodes on given remote machines
    RemoteSlaveNodes=build_and_start_slave_nodes(Logger,CommonArgs,slave_machines,slave_node_name),
      
    %% Create the topology of interest by starting processes on remote nodes and link them 
    %% together. How this is done depends on the problem (Systolic array,  hypercube ,...)
    ProcessNames = create_topology(Logger,RemoteSlaveNodes,{MasterProcessName,MasterNode}),

    sleep(1000),
    start_sorting_algorithm(ProcessNames,{MasterProcessName,MasterNode}).



create_logger(IODevice,CommonArgs,Log_Machine,Log_Node_Name)->
    RemoteLoggerMachines=get_commandline_arg_value(Log_Machine),
    [LoggerNodeName|[]]=get_commandline_arg_value(Log_Node_Name),

    LoggerNode_Start_Link_Arg_List=build_start_link_arguments(RemoteLoggerMachines,LoggerNodeName,CommonArgs),

    RemoteLoggerNodes = lists:foldl (fun start_slave_nodes/2 ,[],LoggerNode_Start_Link_Arg_List),
    ?Trace(IODevice,string_format("Logger Node List:~w",[RemoteLoggerNodes],?LINE)),

    %% Start logger remote process
    lists:foldl(fun (LoggerNode,Acc)->
				   LoggerProcessName=convert_node_name_2_process_name(LoggerNode),
				   spawn_link(LoggerNode,logger,log,["systolic_array_log",LoggerProcessName]),
				   ?Trace(IODevice, string_format("Logger process starts on remote node ~w and is ready to log",
								  [LoggerNode],?LINE)),

				   lists:append(Acc,[{LoggerProcessName,LoggerNode}])

			   end ,[],RemoteLoggerNodes).
    


build_and_start_slave_nodes(Logger,CommonArgs,Slave_Machines,Slave_Node_Name)->

    %% Get the list of slave remote machines (look at the make file)
    RemoteSlaveMachines=get_commandline_arg_value(Slave_Machines),
    [SlaveNodeName|[]]=get_commandline_arg_value(Slave_Node_Name),

    SlaveNode_Start_Link_Arg_List=build_start_link_arguments(RemoteSlaveMachines,SlaveNodeName,CommonArgs),

    RemoteSlaveNodes = lists:foldl (fun start_slave_nodes/2 ,[], SlaveNode_Start_Link_Arg_List),

    Logger ! {info,?Log_Line("Slave Node List:~w",[RemoteSlaveNodes],?LINE)},
    RemoteSlaveNodes.

    

%% Create topology of interest by starting processes on remote nodes and link them together.
create_topology(Logger,RemoteSlaveNodes,Master)->
    ProcessNames=create_systolic_array_topology:create_topology(Logger, array:from_list(RemoteSlaveNodes),Master),
    Logger ! {info,?Log_Line("Process Names:~w",[ProcessNames],?LINE)},
    ProcessNames.
  


%% These are the arguments that are used to start all remote nodes
build_common_argumens()->

    %% Get output directory that slave node needs to 
    %% have in its path when starting. I assumed it would be exactly 
    %% the same as what cluster node started with (look at the make file)

    DirList = get_commandline_arg_value(pa),


    %% Build the right argument for -pa option which is 
    %% to be used when starting the slave on remote hosts

    Binary_Path=lists:foldl(fun (Dir,Acc)->lists:append(Acc,Dir) end,"-pa ",
			    lists:map (fun (Dir)->Dir++" " end, DirList)),


    %% Now get cookie. However the cookie on master and all the slave must be the 
    %% same. Right now I hard code it to 'nocookie' which is on the make file.
    %% However when you are sure what erlang:get_cookie() returns for every 
    %% remote node is the same as what you set in the Makefile, then you can 
    %% call the function: erlang:get_cookie()

    Cookie = atom_to_list(erlang:get_cookie()),
    %% Cookie = atom_to_list(nocookie),


    %% Build the rest of the argument for starting each slaves on remote hosts
    Binary_Path++"-rsh ssh -setcookie "++Cookie.
    

%% pick SlaveName#N as node name for slave nodes that runs on remote Machine
build_start_link_arguments(RemoteSlaveMachines,SlaveNodeName,CommonArgs)->

    {[Start_Link_Arg|T]=Start_Link_Arg_List,_Counter}=
	lists:mapfoldl(fun(RemoteSlaveMachine, Index)-> 
			       {{RemoteSlaveMachine,SlaveNodeName++integer_to_list(Index),CommonArgs}, Index+1} 
		       end,1,RemoteSlaveMachines),

    Start_Link_Arg_List.


start_slave_nodes({RemoteMachine, NodeName, Args},Acc) -> 

    case slave:start_link(RemoteMachine, list_to_atom(NodeName), Args) of
	{ok,ErlangNodeName} -> 
	    lists:append(Acc,[ErlangNodeName]);

	{error,Reason} -> io:format("Error: ~p on starting Node: ~p on Remote Machine ~p with args: ~p~n",
				   [Reason,NodeName,RemoteMachine,Args])
    end.




start_sorting_algorithm([RemoteProcess|_T]=RemoteProcesses,{MasterProcessName,MasterNode}=Master) ->
    
    io:format("2 --> ~p~n",[RemoteProcess]),
    RemoteProcess ! {Master,2},

    io:format("1 --> ~p~n",[RemoteProcess]),
    RemoteProcess ! {Master,1},

    io:format("5 --> ~p~n",[RemoteProcess]),
    RemoteProcess ! {Master,5},

    io:format("0 --> ~p~n",[RemoteProcess]),
    RemoteProcess ! {Master,0},

    io:format("3 --> ~p~n",[RemoteProcess]),
    RemoteProcess ! {Master,3},

    %% Wait a little bit for all processes to register and do their own personal stuff
    sleep(100),

    lists:foreach(fun (Process) -> 
			  io:format ("{~p,start} -> ~p~n",[MasterProcessName,Process]),
			  Process ! {Master,start} 
		  end,
		  RemoteProcesses),

    wait_for_exit().



get_commandline_arg_value(ArgName)->
    case init:get_argument(ArgName) of
	error ->
	    " ";
	{ok,[ArgValue]} ->
	    ArgValue
    end.
    

convert_node_name_2_process_name(Node)->
    NodeString = atom_to_list(Node),
    list_to_atom(lists:map(fun (Alphabet) ->
				   case Alphabet of
				       $@ ->
					   $_;
				       _Any -> _Any
				   end
			   end,NodeString)).


wait_for_exit()->
    receive
	exit->
	    void;
	_ ->wait_for_exit()
    end.


sleep(T)->
    receive
    after T -> void
    end.



