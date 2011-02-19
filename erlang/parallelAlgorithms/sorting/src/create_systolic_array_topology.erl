-module(create_systolic_array_topology).
-export([create_topology/3]).
-include("trace.hrl").


create_topology(Logger, NodeArray, {MasterProcessName,MasterNode}=Master)->

    Logger ! {info,?Log_Line("create_topology() is called with ~w,~w,{~w,~w}",[Logger, NodeArray, MasterProcessName,MasterNode],?LINE)},

    TimeInterval=100,

    Size=array:size(NodeArray),

    case Size of

	1->Logger ! {info,?Log_Line("Program Terminates : Number of nodes are not enough!",[],?LINE)};

	2->
	    Logger ! {info,?Log_Line("Number of nodes are 2",[],?LINE)},

	    PreviousNodeProcessName=MasterProcessName,
	    FirstNode = array:get(0,NodeArray),
	    FirstNodeProcessName = convert_node_name_2_process_name(FirstNode),

	    Logger ! {info,?Log_Line("First Node: ~w",[FirstNode],?LINE)},
	    Logger ! {info,?Log_Line("First Node Process Name: ~w",[FirstNodeProcessName],?LINE)},

	    SecondNode=array:get(1,NodeArray),
	    SecondNodeProcessName = convert_node_name_2_process_name(SecondNode),

	    Logger ! {info,?Log_Line("Second Node: ~w",[SecondNode],?LINE)},
	    Logger ! {info,?Log_Line("Second Node Process Name: ~w",[SecondNodeProcessName],?LINE)},

	    spawn_link(FirstNode,systolic_array,  sort, 
		       [Logger,{FirstNodeProcessName,FirstNode}, 
			{{PreviousNodeProcessName,MasterNode},
			 {SecondNodeProcessName,SecondNode}},-999,TimeInterval,PreviousNodeProcessName]),

	    Logger ! {info,?Log_Line("Spawn Link is called with following arguments: ",[],?LINE)},
	    Logger ! {info,?Log_Line("First arg: ~w ",[FirstNode],?LINE)},
	    Logger ! {info,?Log_Line("Last arg: [{~w,~w},{{~w,~w},{~w,~w}}, -999, ~w, ~w ]",
				     [FirstNodeProcessName,FirstNode,PreviousNodeProcessName,MasterNode,
				      SecondNodeProcessName,SecondNode,TimeInterval,PreviousNodeProcessName],?LINE)},

	    spawn_link(SecondNode,systolic_array,  sort, 
		       [Logger,{SecondNodeProcessName,SecondNode}, {{FirstNodeProcessName,FirstNode},nil}
			,-999,TimeInterval,PreviousNodeProcessName]),

	    Logger ! {info,?Log_Line("Spawn Link is called with following arguments: ",[],?LINE)},
	    Logger ! {info,?Log_Line("First arg: ~w ",[SecondNode],?LINE)},
	    Logger ! {info,?Log_Line("Last arg: [{~w,~w},{{~w,~w},nil}, -999, ~w, ~w ]",
				     [SecondNodeProcessName,SecondNode,FirstNodeProcessName,FirstNode,
				      TimeInterval,PreviousNodeProcessName],?LINE)},

	    Logger ! {info,?Log_Line("create_topology() returns: [{~w,~w},{~w,~w}] ",
				     [FirstNodeProcessName,FirstNode,SecondNodeProcessName,SecondNode],?LINE)},
	    
	    [{FirstNodeProcessName,FirstNode},{SecondNodeProcessName,SecondNode}];


	_Number ->

	    Logger ! {info,?Log_Line("Number of nodes are ~w ",[_Number],?LINE)},

	    create_and_link_processes(Logger,0,Size,NodeArray, [],Master)
    end. 


create_and_link_processes(Logger,0,Size, NodeArray, Acc,{MasterProcessName,MasterNode}=Master)->

    Logger ! {info,?Log_Line("create_and_link_processes() is called with: [~w,0,~w,~w,~w,{~w,~w}] ",
				     [Logger,Size, NodeArray, Acc,MasterProcessName,MasterNode],?LINE)},

    TimeInterval=100,

    PreviousNode=MasterNode,
    PreviousNodeProcessName=MasterProcessName,

    CurrentNode=array:get(0,NodeArray),
    CurrentNodeProcessName = convert_node_name_2_process_name(CurrentNode),

    NextNode=array:get(1,NodeArray),
    NextNodeProcessName = convert_node_name_2_process_name(NextNode),

    Logger ! {info,?Log_Line("Current Node: ~w",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Current Node Process Name: ~w",[CurrentNodeProcessName ],?LINE)},
    Logger ! {info,?Log_Line("Next Node: ~w",[NextNode],?LINE)},
    Logger ! {info,?Log_Line("Next Node Process Name: ~w",[NextNodeProcessName ],?LINE)},
    Logger ! {info,?Log_Line("Previous Node: ~w",[PreviousNode],?LINE)},
    Logger ! {info,?Log_Line("Previous Node Process Name: ~w",[PreviousNodeProcessName],?LINE)},


    Logger ! {info,?Log_Line("Spawn Link is called with following arguments: ",[],?LINE)},
    Logger ! {info,?Log_Line("First arg: ~w ",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Last arg: [{~w,~w},{{~w,~w},{~w,~w}}, -999, ~w, ~w ]",
				     [CurrentNodeProcessName,CurrentNode,PreviousNodeProcessName,PreviousNode,
				      NextNodeProcessName,NextNode,TimeInterval,PreviousNodeProcessName],?LINE)},


    spawn_link(CurrentNode,systolic_array,  sort, [Logger,{CurrentNodeProcessName,CurrentNode}, 
							 {{PreviousNodeProcessName,PreviousNode},{NextNodeProcessName,NextNode}},
							 -999,TimeInterval,PreviousNodeProcessName]),


    create_and_link_processes(Logger,1,Size,NodeArray,lists:append(Acc,[{CurrentNodeProcessName,CurrentNode}]),Master);



create_and_link_processes(Logger,Index,Size, NodeArray, Acc,{MasterProcessName,MasterNode}=Master) when Index=:=(Size-1)->

    Logger ! {info,?Log_Line("create_and_link_processes() is called with: [~w,~w,~w,~w,~w,{~w,~w}] ",
				     [Logger,Index,Size, NodeArray, Acc,MasterProcessName,MasterNode],?LINE)},

    TimeInterval=100,

    PreviousNode=array:get(Index-1,NodeArray),
    PreviousNodeProcessName = convert_node_name_2_process_name(PreviousNode),

    CurrentNode=array:get(Index,NodeArray),
    CurrentNodeProcessName = convert_node_name_2_process_name(CurrentNode),


    Logger ! {info,?Log_Line("Current Node: ~w",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Current Node Process Name: ~w",[CurrentNodeProcessName ],?LINE)},
    Logger ! {info,?Log_Line("Previous Node: ~w",[PreviousNode],?LINE)},
    Logger ! {info,?Log_Line("Previous Node Process Name: ~w",[PreviousNodeProcessName],?LINE)},


    Logger ! {info,?Log_Line("Spawn Link is called with following arguments: ",[],?LINE)},
    Logger ! {info,?Log_Line("First arg: ~w ",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Last arg: [{~w,~w},{{~w,~w},nil}, -999, ~w, ~w ]",
				     [CurrentNodeProcessName,CurrentNode,PreviousNodeProcessName,PreviousNode,
				      TimeInterval,MasterProcessName],?LINE)},

    spawn_link(CurrentNode,systolic_array,  sort, [Logger,{CurrentNodeProcessName,CurrentNode}, 
							 {{PreviousNodeProcessName,PreviousNode},nil},
							 -999,TimeInterval,MasterProcessName]),

    Result=lists:append(Acc,[{CurrentNodeProcessName,CurrentNode}]),

    Logger ! {info,?Log_Line("create_and_link_processes() returns: ~w ",[Result],?LINE)},

    Result;


create_and_link_processes(Logger,Index, Size, NodeArray, Acc, {MasterProcessName,MasterNode}=Master)->

    Logger ! {info,?Log_Line("create_and_link_processes() is called with: [~w,~w,~w,~w,~w,{~w,~w}] ",
				     [Logger,Index,Size, NodeArray, Acc,MasterProcessName,MasterNode],?LINE)},

    TimeInterval=100,

    PreviousNode=array:get(Index-1,NodeArray),
    PreviousNodeProcessName = convert_node_name_2_process_name(PreviousNode),

    CurrentNode=array:get(Index,NodeArray),
    CurrentNodeProcessName = convert_node_name_2_process_name(CurrentNode),

    NextNode=array:get(Index+1,NodeArray),
    NextNodeProcessName = convert_node_name_2_process_name(NextNode),


    Logger ! {info,?Log_Line("Current Node: ~w",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Current Node Process Name: ~w",[CurrentNodeProcessName ],?LINE)},
    Logger ! {info,?Log_Line("Next Node: ~w",[NextNode],?LINE)},
    Logger ! {info,?Log_Line("Next Node Process Name: ~w",[NextNodeProcessName ],?LINE)},
    Logger ! {info,?Log_Line("Previous Node: ~w",[PreviousNode],?LINE)},
    Logger ! {info,?Log_Line("Previous Node Process Name: ~w",[PreviousNodeProcessName],?LINE)},

    Logger ! {info,?Log_Line("Spawn Link is called with following arguments: ",[],?LINE)},
    Logger ! {info,?Log_Line("First arg: ~w ",[CurrentNode],?LINE)},
    Logger ! {info,?Log_Line("Last arg: [{~w,~w},{{~w,~w},{~w,~w}}, -999, ~w, ~w ]",
				     [CurrentNodeProcessName,CurrentNode,PreviousNodeProcessName,PreviousNode,
				      NextNodeProcessName,NextNode,TimeInterval,MasterProcessName],?LINE)},


    spawn_link(CurrentNode,systolic_array,  sort, [Logger,{CurrentNodeProcessName,CurrentNode}, 
							 {{PreviousNodeProcessName,PreviousNode},{NextNodeProcessName,NextNode}},
							 -999,TimeInterval,MasterProcessName]),

    create_and_link_processes(Logger,Index+1,Size, NodeArray,lists:append(Acc,[{CurrentNodeProcessName,CurrentNode}]),Master).


convert_node_name_2_process_name(Node)->
    NodeString = atom_to_list(Node),
    list_to_atom(lists:map(fun (Alphabet) ->
				   case Alphabet of
				       $@ ->
					   $_;
				       _Any -> _Any
				   end
			   end,NodeString)).

