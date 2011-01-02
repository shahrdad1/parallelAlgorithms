
%% Leftist Heap structure => {Value , Left subtree, right subtree}
%% See The Art of Computer programming Vol. 3, p:150 for algorithm

-module(leftist_heap).

%%
%% Include files
%%




%%
%% Exported Functions
%%
-export([createHeap/0,readValues/0, insert/2,find_min/1, delete_min/1,encode_Timestamp/1,trace_line/2]).

-define(Now2Timestamp(X), encode_Timestamp(X)). 
-define(Trace(IoDevice,X),trace_line(IoDevice,X)).


%%
%% API Functions
%%


insert (Val, Heap) ->case file:open("./log/Leftist_Heap_Input.log", [write,{encoding, utf8}]) of 
												 {ok,IoDevice} -> merge_nodes({Val, nil, nil}, Heap, IoDevice); 
												 {error,Reason} -> 
													io:format("File Open Error: ~p ",[file:format_error(Reason)])
				  	end.
	

find_min ({Val,_,_}) -> Val.

delete_min ({_,L,R}) -> case file:open("./log/Leftist_Heap_Input.log", [write,{encoding, utf8}]) of 
												 {ok,IoDevice} -> merge_nodes(L,R,IoDevice); 
												 {error,Reason} -> 
													io:format("File Open Error: ~p ",[file:format_error(Reason)])
				  		end. 
	
%%
%% Local Functions
%%


readValues () -> case file:read_file("./data/Leftist_Heap_Input.txt") of
							{ok, DataL} -> 	Text = binary_to_list(DataL),
										   	Tokenized = string:tokens(Text, ","),
											lists:foldl(fun (Element, Queue) -> queue:in({list_to_integer(Element),nil,nil}, Queue) end, 
														queue:new(), Tokenized);
%% 											lists:map(fun (Entry) -> io:format("~p ",[Entry]) end,
%% 														 queue:to_list(InputQueue));
									
							{error, Reason} -> file:format_error(Reason)
				end.


createHeap ()  -> case file:open("./log/Leftist_Heap_Input.log", [write,{encoding, utf8}]) of 
												 {ok,IoDevice} -> M = merge (readValues(),IoDevice),
																  ?Trace(IoDevice,string_format("Result --> ~w",[M],?LINE)),
																  M;
					  
												 {error,Reason} -> 
													io:format("File Open Error: ~p ",[file:format_error(Reason)])
				  end.

merge (Queue1,IoDevice) -> 	?Trace(IoDevice,string_format("merge() Invoked with: ",[],?LINE)),
							print_queue(IoDevice,Queue1,49),
							case queue:out(Queue1) of
								
								{{value, Node1}, Queue2} ->  
									
									?Trace(IoDevice,string_format("Node ~w popped from the queue.",[Node1],?LINE)),
									
									case queue:out(Queue2) of
										
								 		{{value, Node2}, Queue3} ->
											?Trace(IoDevice,string_format("Node ~w popped from the queue.",[Node2],?LINE)),
									 		Merged_Node = merge_nodes(Node1,Node2,IoDevice),
 									 		Queue4 = queue:in(Merged_Node, Queue3),
											print_queue(IoDevice,Queue4, 63),
 									 		merge(Queue4,IoDevice);
								
								 		{empty, _} -> ?Trace(IoDevice,string_format("~w ",[Node1],?LINE)),
													  Node1
									end;
							
							
								{empty, _} -> ?Trace(IoDevice,string_format("Done!",[],?LINE))
							end.

merge_nodes({Val1,L1,nil}=SubTree1,{Val2,L2,nil}=SubTree2,IoDevice) -> 
	case Val1 > Val2 of 
		
	  	true 	->  ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,nil,Val2,L2,nil],?LINE)),
					re_arrange_children({Val2,L2,SubTree1});
		
		false 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,nil,Val2,L2,nil],?LINE)),
					re_arrange_children({Val1,L1,SubTree2})

	end;

merge_nodes({Val1,L1,R1}=SubTree1,{Val2,L2,nil}=SubTree2,IoDevice) -> 
	case Val1 > Val2 of 
	  	true 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,R1,Val2,L2,nil],?LINE)),
					re_arrange_children({Val2,L2,SubTree1});
		
		false 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,R1,Val2,L2,nil],?LINE)),
					re_arrange_children({Val1,L1,re_arrange_children(merge_nodes(SubTree2,R1,IoDevice))})
				  
	end;



merge_nodes({Val1,L1,R1}=SubTree1,{Val2,L2,R2}=SubTree2,IoDevice) -> 
	case Val1 > Val2 of 
		
	  	true 	->  ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,R1,Val2,L2,R2],?LINE)),
					re_arrange_children({Val2,L2,re_arrange_children(merge_nodes(SubTree1,R2,IoDevice))});
		
		false 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})",[Val1,L1,R1,Val2,L2,R2],?LINE)),
					re_arrange_children({Val1,L1,re_arrange_children(merge_nodes(SubTree2,R1,IoDevice))})

	end.



  

re_arrange_children ({Val,L,R}=SubTree) -> case dist(L) < dist(R) of 
									 true -> {Val,R,L};
									 false -> SubTree
								 end.

dist(nil) -> 0;
dist ({_,L,R}) -> 1 + min(dist(L),dist(R)).

print_queue(IoDevice, Queue, Seq) ->lists:map(fun (Entry) -> ?Trace(IoDevice,string_format("~w:~w ",[Seq,Entry],?LINE)) end,
 														 queue:to_list(Queue)),
								?Trace(IoDevice,string_format("~n",[],?LINE)),
								Queue.


string_format(Pattern, Values, LineNo) -> lists:flatten(io_lib:format("Line# ~w : "++Pattern, [LineNo|Values])).


trace_line(IoDevice,X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
							io:format(IoDevice,"~p/~p/~p - ~p:~p:~p:~p [Machine:~p * Module:~p ] --> ~p~n",
								[Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE, X]).



encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
					{{Year,Month,Day},{HH,MM,SS}}=
						calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
					{Year,Month,Day,HH,MM,SS,MicroSeconds}.



%% merge_nodes({Val1,L1,nil},{Val2,L2,R2},Queue) -> 
%% 	case Val1 > Val2 of 
%% 	  	true 	-> queue:in({Val2,L2,merge_nodes({Val1,L1,nil},R2,Queue)}, Queue);
%% 		false 	-> queue:in({Val1,L1,{Val2,L2,R2}},Queue)
%% 	end;


%% merge_nodes({Val1,L1,nil},{Val2,L2,{ValR2,RL2,nil}},Queue) -> 
%% 	case Val1 > Val2 of 
%% 	  	true 	-> case Val1 < ValR2 of 
%% 					   true  -> queue:in({Val2,L2,{Val1,L1,{ValR2,RL2,nil}}}, Queue);
%% 					   false -> queue:in({Val2,L2,{ValR2,RL2,{Val1,L1,nil}}}, Queue)
%% 				   end;
%% 		false 	-> queue:in({Val1,L1,{Val2,L2,{ValR2,RL2,nil}}}, Queue)
%% 	end;







  