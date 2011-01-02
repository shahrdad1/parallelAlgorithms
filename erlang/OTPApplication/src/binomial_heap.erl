-module(binomial_heap).

%% The node of binomial tree has the following structure:
%% 	{Val, Rank, List of children whose rank is decreasing from left to right }

-define(Now2Timestamp(X), encode_Timestamp(X)). 
-define(Trace(IoDevice,X),trace_line(IoDevice,X)).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([encode_Timestamp/1,trace_line/2]).
-export([createBinomialTree/2, binary_rep/2,create_binomial_heap/2,
		 pad_binomial_heap/3, pad_with_empty_nodes/2,pad_with_empty_nodes/1,
		 create_binomial_heap_padded_with_empty_nodes/2,merge_tree_to_heap/3,
		 merge_heaps/0,open_log_file/1,create_binomial_heap/0,
		 insert_element/0, remove_min_tree/1,remove_min/1]).

insert_element() -> merge_heaps("./data/singleton.txt","./data/Binomial_Heap_Input2.txt").

remove_min(Binomial_Heap) -> 
	IoDevice = open_log_file("./log/Binomial_Heap_merge.log"),
	{{_,_,Children}, New_Heap} = remove_min_tree(Binomial_Heap),
	?Trace(IoDevice,string_format("Reverse Children: ~w, New_Heap: ~w ~n",[lists:reverse(Children),New_Heap],?LINE)),
	PaddedHeap1 = pad_with_empty_nodes(lists:reverse(Children)),
	PaddedHeap2 = pad_with_empty_nodes(New_Heap),
	?Trace(IoDevice,string_format("PaddedHeap1:~w PaddedHeap2:~w ~n", [PaddedHeap1,PaddedHeap2],?LINE)),
	merge_heap_elements(PaddedHeap2,PaddedHeap1,[],[],IoDevice).
	

remove_min_tree(Binomial_Heap) -> Min_Tree = find_min_tree(Binomial_Heap,nil),
								  remove_tree (Min_Tree, Binomial_Heap,[]).
find_min_tree([], Min_Tree) -> Min_Tree;

find_min_tree([Tree|Rest], Min_Tree) -> 

	case Min_Tree of
		nil -> find_min_tree(Rest,Tree);

		_ ->{Val,_,_} = Tree,
			{MinVal,_,_} = Min_Tree,
			 case MinVal > Val of 
				 true -> find_min_tree (Rest,Tree);
				 false -> find_min_tree (Rest,Min_Tree)
			 end
	end.

remove_tree (Tree, [],_) -> throw (['Four',{not_found, [{exception,return_error,1},{binomial_heap,remove_tree,Tree}]}]);
remove_tree (Tree, [Tree|Rest], Acc) -> {Tree,lists:append(Acc,Rest)};
remove_tree (Tree, [Tree1|Rest], Acc) -> remove_tree(Tree,Rest,lists:append(Acc, [Tree1])).
	

  
merge_heaps(File1,File2) -> 
	IoDevice = open_log_file("./log/Binomial_Heap_merge.log"),
	BinomialHeap1 = create_binomial_heap_padded_with_empty_nodes(File1,IoDevice),
	BinomialHeap2 = create_binomial_heap_padded_with_empty_nodes(File2,IoDevice),
	merge_heap_elements(BinomialHeap1,BinomialHeap2,[],[],IoDevice).

merge_heaps() -> 
	IoDevice = open_log_file("./log/Binomial_Heap_merge.log"),
	BinomialHeap1 = create_binomial_heap_padded_with_empty_nodes("./data/Binomial_Heap_Input1.txt",IoDevice),
	
	BinomialHeap2 = create_binomial_heap_padded_with_empty_nodes( "./data/Binomial_Heap_Input2.txt",IoDevice),
	
	merge_heap_elements(BinomialHeap1,BinomialHeap2,[],[],IoDevice).

merge_tree_to_heap(BinomialTree,Heap,IoDevice) ->
		BinomialHeap1 = pad_with_empty_nodes([BinomialTree]),
		merge_heap_elements(BinomialHeap1,Heap,[],[],IoDevice).


%%
%% API Functions
%%

%% ----- Empty cases For both Args------

merge_heap_elements ([],[],[],Merged_Heaps,_)->Merged_Heaps;
	
merge_heap_elements ([],[],Carry,Merged_Heaps,_)->lists:append(Merged_Heaps,[Carry]);

%% ----- Empty cases For first Arg------
	
merge_heap_elements ([],[{nil}|Rest2],[],Merged_Heaps,IoDevice)->
	case Rest2 of
	  [] -> Merged_Heaps;
				  
	  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
		  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
		  lists:append(Merged_Heaps, Rest3)
	end;
	
merge_heap_elements ([],[{nil}|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Carry]),
	case Rest2 of
	  [] -> New_Merged_Heaps;
				  
	  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
		  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
		  lists:append(New_Merged_Heaps, Rest3)
    end;

%% ----- Empty cases For 2nd Arg------

merge_heap_elements ([{nil}|Rest1],[],[],Merged_Heaps,IoDevice)->
	case Rest1 of
		[] -> Merged_Heaps;
				  
		_ ->  Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
			  ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
			  lists:append(Merged_Heaps, Rest3)
	end;

merge_heap_elements ([{nil}|Rest1],[],Carry,Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Carry]),
	case Rest1 of
		[] -> New_Merged_Heaps;
				  
		_ -> Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
			 ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
			 lists:append(New_Merged_Heaps, Rest3)
	end;

%% ----- nil cases ------

merge_heap_elements ([{nil}|Rest1],[{nil}|Rest2],[],Merged_Heaps,IoDevice)->
	case Rest1 of
		[] -> case Rest2 of
				  [] -> Merged_Heaps;
				  
				  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
					  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
					  lists:append(Merged_Heaps, Rest3)
			  end;
		_ -> case Rest2 of
				  [] ->
					  Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
					  ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
					  lists:append(Merged_Heaps, Rest3);
				 
				  _ -> merge_heap_elements(Rest1,Rest2,[],Merged_Heaps,IoDevice)
			  end
	end;

merge_heap_elements ([{nil}|Rest1],[{nil}|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Carry]),
	case Rest1 of
		[] -> case Rest2 of
				  [] -> New_Merged_Heaps;
				  
				  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
					  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
					  lists:append(New_Merged_Heaps, Rest3)
			  end;
		_ -> case Rest2 of
				  [] ->
					  Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
					  ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
					  lists:append(New_Merged_Heaps, Rest3);
				 
				  _ -> merge_heap_elements(Rest1,Rest2,[],New_Merged_Heaps,IoDevice)
			  end
	end;

%% ---------- Empty case for 2nd arg-----------
merge_heap_elements ([Binomial_Tree1|Rest1],[],[],Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Binomial_Tree1]),
	case Rest1 of
		[] -> New_Merged_Heaps;
				  
		_ -> Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
			 ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
			 lists:append(New_Merged_Heaps, Rest3)
	end;
	
	

merge_heap_elements ([Binomial_Tree1|Rest1],[],Carry,Merged_Heaps,IoDevice)->
	New_Carry = merge_BinomialTrees(Binomial_Tree1,Carry,IoDevice),
	case Rest1 of
		[] -> lists:append(Merged_Heaps,[New_Carry]);

		_ -> New_Merged_Heaps = lists:append(Merged_Heaps, [New_Carry]),
			Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
			?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
			lists:append(New_Merged_Heaps, Rest3)
	end;

%% ---------- nil cases  for 2nd arg-----------

merge_heap_elements ([Binomial_Tree1|Rest1],[{nil}|Rest2],[],Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Binomial_Tree1]),
	case Rest1 of
		[] -> case Rest2 of
				  [] -> New_Merged_Heaps;
				  
				  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
					  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
					  lists:append(New_Merged_Heaps, Rest3)
			  end;
		_ -> case Rest2 of
				  [] ->
					  Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
					  ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
					  lists:append(New_Merged_Heaps, Rest3);
				 
				  _ -> merge_heap_elements(Rest1,Rest2,[],New_Merged_Heaps,IoDevice)
			  end
	end;
	

merge_heap_elements ([Binomial_Tree1|Rest1],[{nil}|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Carry = merge_BinomialTrees(Binomial_Tree1,Carry,IoDevice),
	case Rest1 of
		[] -> case Rest2 of
				  [] -> lists:append(Merged_Heaps,[New_Carry]);
				  
				  _ ->  New_Merged_Heaps = lists:append(Merged_Heaps,[New_Carry]),
						Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
						?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)),
						lists:append(New_Merged_Heaps, Rest3)
			  end;

		_ -> case Rest2 of
				  [] -> New_Merged_Heaps = lists:append(Merged_Heaps, [New_Carry]),
						Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
						?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
						lists:append(New_Merged_Heaps, Rest3);
				 
				  _ -> merge_heap_elements (Rest1,Rest2,New_Carry,Merged_Heaps,IoDevice)
			  end
	end;

%% ---------- Empty case for 1st arg-----------	

merge_heap_elements ([],[Binomial_Tree2|Rest2],[],Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Binomial_Tree2]),
	case Rest2 of
	  [] -> New_Merged_Heaps;
	  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
		  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
		  lists:append(New_Merged_Heaps, Rest3)
	end;


merge_heap_elements ([],[Binomial_Tree2|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Carry = merge_BinomialTrees(Binomial_Tree2,Carry,IoDevice),
	
	case Rest2 of
	  [] -> lists:append(Merged_Heaps,[New_Carry]);
				  
	  _ ->  New_Merged_Heaps = lists:append(Merged_Heaps,[New_Carry]),
			Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
			?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)),
			lists:append(New_Merged_Heaps, Rest3)
	end;

%% ---------- nil case for 1st arg-----------	

merge_heap_elements ([{nil}|Rest1],[Binomial_Tree2|Rest2],[],Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Binomial_Tree2]),
	case Rest1 of
		[] -> case Rest2 of
				  [] -> New_Merged_Heaps;
				  _ ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
					  ?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)), 
					  lists:append(New_Merged_Heaps, Rest3)
			  end;
		_ -> case Rest2 of
				  [] ->Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1), 
					  ?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
					  lists:append(New_Merged_Heaps, Rest3);
				  _ -> merge_heap_elements(Rest1,Rest2,[],New_Merged_Heaps,IoDevice)
			  end
	end;

merge_heap_elements ([{nil}|Rest1],[Binomial_Tree2|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Carry = merge_BinomialTrees(Binomial_Tree2,Carry,IoDevice),
	
	case Rest1 of
		[] -> case Rest2 of
				  [] -> lists:append(Merged_Heaps,[New_Carry]);
				  
				  _ ->  New_Merged_Heaps = lists:append(Merged_Heaps,[New_Carry]),
						Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest2),
						?Trace(IoDevice,string_format( "Append Rest2: ~w ~n", [Rest3],?LINE)),
						lists:append(New_Merged_Heaps, Rest3)
			  end;
		_ -> case Rest2 of
				  [] -> New_Merged_Heaps = lists:append(Merged_Heaps, [New_Carry]),
						Rest3 = lists:filter(fun (X) -> filter_nil_element(X) end, Rest1),
						?Trace(IoDevice,string_format( "Append Rest1: ~w ~n", [Rest3],?LINE)),
						lists:append(New_Merged_Heaps, Rest3);
				 
				  _ -> merge_heap_elements(Rest1,Rest2,New_Carry,Merged_Heaps,IoDevice)
			  end
	end;
	

merge_heap_elements ([Binomial_Tree1|Rest1],[Binomial_Tree2|Rest2],[],Merged_Heaps,IoDevice)->
	Carry = merge_BinomialTrees(Binomial_Tree1,Binomial_Tree2,IoDevice),
	merge_heap_elements (Rest1,Rest2,Carry,Merged_Heaps,IoDevice);

merge_heap_elements ([Binomial_Tree1|Rest1],[Binomial_Tree2|Rest2],Carry,Merged_Heaps,IoDevice)->
	New_Merged_Heaps = lists:append(Merged_Heaps,[Carry]),
	New_Carry = merge_BinomialTrees(Binomial_Tree1,Binomial_Tree2,IoDevice),
	merge_heap_elements (Rest1,Rest2,New_Carry,New_Merged_Heaps,IoDevice).





%% Create Binomial Heap

create_binomial_heap_padded_with_empty_nodes(IoDevice)->
	InputFileName = "./data/Binomial_Heap_Input.txt",
	BinomialHeap = create_binomial_heap(InputFileName,IoDevice),
	pad_with_empty_nodes(BinomialHeap,IoDevice).
	
create_binomial_heap_padded_with_empty_nodes(InputFileName,IoDevice)->
	BinomialHeap= create_binomial_heap(InputFileName,IoDevice),
	pad_with_empty_nodes(BinomialHeap,IoDevice).
	
create_binomial_heap () -> 
	InputFileName = "./data/Binomial_Heap_Input.txt",
	IoDevice = open_log_file("./log/Binomial_Heap_Create.log"),
	InputListInInteger = read_data_from_file(InputFileName),
	Queues = prepare_to_build_heap(IoDevice,InputListInInteger),
	?Trace(IoDevice,string_format("66 ~n",[],?LINE)),
	Binomial_Heap = createBinomialTrees(IoDevice,Queues,[]),
	?Trace(IoDevice,string_format("Result --> ~w~n",[Binomial_Heap],?LINE)),
	Binomial_Heap.
					  
create_binomial_heap (IoDevice) -> 
	InputFileName = "./data/Binomial_Heap_Input.txt",
	
	InputListInInteger = read_data_from_file(InputFileName),
	Queues = prepare_to_build_heap(IoDevice,InputListInInteger),
	?Trace(IoDevice,string_format("66 ~n",[],?LINE)),
	Binomial_Heap = createBinomialTrees(IoDevice,Queues,[]),
	?Trace(IoDevice,string_format("Result --> ~w~n",[Binomial_Heap],?LINE)),
	Binomial_Heap.
					  
create_binomial_heap (InputFileName,IoDevice) -> 
	InputListInInteger = read_data_from_file(InputFileName),
	Queues = prepare_to_build_heap(IoDevice,InputListInInteger),
	?Trace(IoDevice,string_format("66 ~n",[],?LINE)),
	Binomial_Heap = createBinomialTrees(IoDevice,Queues,[]),
	?Trace(IoDevice,string_format("Result --> ~w~n",[Binomial_Heap],?LINE)),
	Binomial_Heap.					  

%%
%% Local Functions
%%


read_data_from_file(InputFileName) -> 
				case file:read_file(InputFileName) of
					{ok, DataL} -> 	
						Text = binary_to_list(DataL),
						InputListInString = string:tokens(Text, ","),
						lists:foldl(fun (InputValue, Acc) -> 
						 				lists:append(Acc,[list_to_integer(InputValue)]) 
									end,[],InputListInString);
					{error, Reason} -> file:format_error(Reason)
				end.
					

pad_with_empty_nodes(BinomialHeap) -> 
					IoDevice = open_log_file("./log/Binomial_Heap_Pad.log"),
					BinomialHeap1 = pad_binomial_heap(BinomialHeap, [],IoDevice),
					[{_,Rank,_}|_] = BinomialHeap1,
					prepend_binomial_heap_with_Empty_nodes(BinomialHeap1,Rank).

pad_with_empty_nodes(BinomialHeap,IoDevice) -> 
					BinomialHeap1 = pad_binomial_heap(BinomialHeap, [],IoDevice),
					[{_,Rank,_}|_] = BinomialHeap1,
					prepend_binomial_heap_with_Empty_nodes(BinomialHeap1,Rank).


prepare_to_build_heap(IoDevice,InputList) ->
					Length = length(InputList),
					BinaryRep = binary_rep(IoDevice,Length),
					?Trace(IoDevice,string_format("Binary Repesentation of digit which is the number of input values.~n",[],?LINE)),
					lists:map(fun (X) -> ?Trace(IoDevice,string_format("~w ~n", [X],?LINE)) end, BinaryRep),
					SignificantValues = calculate_significant_values (IoDevice,BinaryRep),
					?Trace(IoDevice,string_format("List of significant values of each bit in Binary Repesentation of the number of input values.~n",[],?LINE)),
					lists:map(fun (Element) -> ?Trace(IoDevice,string_format("~w ~n",[Element],?LINE)) end, SignificantValues),
					Non_zero_values = lists:filter(fun (Value) -> filter_zero_Values(Value) end , SignificantValues),
					?Trace(IoDevice,string_format("List of Non_zero significant values of each bit in Binary Repesentation of the number of input values.~n",[],?LINE)),
					lists:map(fun (Element) -> ?Trace(IoDevice,string_format("~w ~n",[Element],?LINE)) end, Non_zero_values),
					Queues = enqeue_input_values(IoDevice,InputList,Non_zero_values,[]),
					lists:map(fun (Queue) -> ?Trace(IoDevice,string_format("Queue -> ~w ~n",[Queue],?LINE)) end,Queues),
					Queues.


createBinomialTrees (_,[],BinimialHeap) -> BinimialHeap;

createBinomialTrees (IoDevice,[Queue|T],BinimialHeap) ->
				io:format("73 ~n",[]),
				M = createBinomialTree (IoDevice,Queue),
				createBinomialTrees (IoDevice,T,lists:append(BinimialHeap, [M])).

createBinomialTree (IoDevice,Queue)  -> M = merge (Queue,IoDevice),
										?Trace(IoDevice,string_format("Result --> ~w~n",[M],?LINE)),
										M.



merge (Queue1,IoDevice) -> 	?Trace(IoDevice,string_format("merge() Invoked with: ",[],?LINE)),
							print_queue(IoDevice,Queue1,49),
							case queue:out(Queue1) of
								
								{{value, Node1}, Queue2} ->  
									
									?Trace(IoDevice,string_format("Node ~w popped from the queue.~n",[Node1],?LINE)),
									
									case queue:out(Queue2) of
										
								 		{{value, Node2}, Queue3} ->
											?Trace(IoDevice,string_format("Node ~w popped from the queue.~n",[Node2],?LINE)),
									 		Merged_Trees = merge_BinomialTrees(Node1,Node2,IoDevice),
 									 		Queue4 = queue:in(Merged_Trees, Queue3),
											print_queue(IoDevice,Queue4, 63),
 									 		merge(Queue4,IoDevice);
								
								 		{empty, _} -> ?Trace(IoDevice,string_format("~w ~n",[Node1],?LINE)),
													  Node1
									end;
							
							
								{empty, _} -> ?Trace(IoDevice,string_format("Done!",[],?LINE))
							end.


merge_BinomialTrees({Val1,Rank1,Children1}=SubTree1,{Val2,Rank2,Children2}=SubTree2,IoDevice) -> 
	case Val1 > Val2 of 
		
	  	true 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})~n",[Val1,Rank1,Children1,Val2,Rank2,Children2],?LINE)),
					{Val2,Rank2+1,[SubTree1|Children2]};
		
		false 	-> ?Trace(IoDevice,string_format("merge_nodes({~w,~w,~w},{~w,~w,~w})~n",[Val1,Rank1,Children1,Val2,Rank2,Children2],?LINE)),
					{Val1,Rank1+1,[SubTree2|Children1]}

	end.



enqeue_input_values(_,[],[],ListOfQueues) -> ListOfQueues;
enqeue_input_values(IoDevice,InputList,[{{counter,_},{bitSignificantValue,NumberOfInputValuesToRead}}|T],ListOfQueues) -> 
	{First_N_Element, Rest}=retrieve_first_N_elements(IoDevice,InputList,NumberOfInputValuesToRead),
	NthQueue = lists:foldl(fun (Element, Queue) -> 
						 queue:in({Element,0,[]}, Queue) 
				end, queue:new(), First_N_Element),
	UpdatedQueueList = lists:append(ListOfQueues,[NthQueue]),
	enqeue_input_values(IoDevice,Rest,T,UpdatedQueueList).
	

retrieve_first_N_elements (_,[],N) -> throw (['One',{empyList, [{exception,return_error,1},{binomial_heap,retrieve_first_N_elements,N}]}]);
retrieve_first_N_elements (IoDevice,List,N) when (N =< length(List)) and (N > 0) -> 
										?Trace(IoDevice,string_format("retrieve_first_N_elements : ~w ~n",[List],?LINE)),
										{lists:reverse(lists:nthtail(length(List)-N,lists:reverse(List))),lists:nthtail(N, List)};
retrieve_first_N_elements (_,List,N) -> throw (['Two',{badarg, [{exception,return_error,1},{binomial_heap,retrieve_first_N_elements,{List,N}}]}]).




%%  Note that N must be less than or equal to 255,
%% This is for convinienece only. for N > 255 we just 
%% need to extend the following (with regard to Endianess)

binary_rep (IoDevice,N) -> 
	<<A8:1/bitstring-unit:1,A7:1/bitstring-unit:1,A6:1/bitstring-unit:1,
	  A5:1/bitstring-unit:1,A4:1/bitstring-unit:1,A3:1/bitstring-unit:1,
	  A2:1/bitstring-unit:1,A1:1/bitstring-unit:1>> = <<N>>,
	[A1,A2,A3,A4,A5,A6,A7,A8].


calculate_significant_values (IoDevice,BitValuesList) -> 
	lists:reverse(lists:foldl(fun (BitValue,Acc) -> 
						 prepare_bit_index (BitValue,Acc) 
				end, [], BitValuesList)). 


%% Match a bit of value 1
prepare_bit_index(<<1:1>>, []) -> [{{counter,1},{bitSignificantValue,1}}];

prepare_bit_index(<<1:1>>, [{{counter,Counter},{bitSignificantValue,BitSignificantValue}}|T]) ->
	
	[{{counter,Counter+1},{bitSignificantValue,trunc(math:pow(2, Counter))}}|[{{counter,Counter},{bitSignificantValue,BitSignificantValue}}|T]];

%% Match a bit of value 0
prepare_bit_index(<<0:1>>, []) -> [{{counter,1},{bitSignificantValue,0}}];

prepare_bit_index(<<0:1>>, [{{counter,Counter},{bitSignificantValue,BitSignificantValue}}|T]) ->
	
	[{{counter,Counter+1},{bitSignificantValue,0}}|[{{counter,Counter},{bitSignificantValue,BitSignificantValue}}|T]]. 


filter_zero_Values ({{counter,_},{bitSignificantValue,BitSignificantValue}}) ->
	case BitSignificantValue of
		0 -> false;
		_ -> true
	end.



prepend_binomial_heap_with_Empty_nodes(BinomialHeap,FirstTreeRank)->

	case FirstTreeRank of
			0 -> BinomialHeap;
			_ -> NewHeap = lists:append([{nil}], BinomialHeap),
				 prepend_binomial_heap_with_Empty_nodes(NewHeap,FirstTreeRank-1)
	end.


pad_binomial_heap ([],PaddedBinomialHeap,_) ->PaddedBinomialHeap;

pad_binomial_heap ([Binomial_Tree],PaddedBinomialHeap,_) ->
	lists:append(PaddedBinomialHeap, [Binomial_Tree]);
	
pad_binomial_heap ([Binomial_Tree1,Binomial_Tree2],PaddedBinomialHeap,IoDevice) ->
	{_,Rank1,_}=Binomial_Tree1,
	{_,Rank2,_}=Binomial_Tree2,
	PaddedBinomialHeap1=lists:append(PaddedBinomialHeap, [Binomial_Tree1]),
	PaddedBinomialHeap2=fill_with_empty_nodes(Rank1,Rank2,PaddedBinomialHeap1,IoDevice),
	pad_binomial_heap ([Binomial_Tree2],PaddedBinomialHeap2,IoDevice);

	
pad_binomial_heap ([Binomial_Tree1|[Binomial_Tree2|Rest]],PaddedBinomialHeap,IoDevice) ->
	{_,Rank1,_}=Binomial_Tree1,
	{_,Rank2,_}=Binomial_Tree2,
	PaddedBinomialHeap1=lists:append(PaddedBinomialHeap, [Binomial_Tree1]),
	PaddedBinomialHeap2=fill_with_empty_nodes(Rank1,Rank2,PaddedBinomialHeap1,IoDevice),
	pad_binomial_heap ([Binomial_Tree2|Rest],PaddedBinomialHeap2,IoDevice).



fill_with_empty_nodes (Rank1, Rank2 , List,IoDevice) ->
	case Rank1 == (Rank2-1) of
		true -> List;
		_ ->
			NewList = lists:append(List, [{nil}]),
			fill_with_empty_nodes(Rank1+1,Rank2,NewList,IoDevice)
	end.
	
filter_nil_element({nil}) -> false;
filter_nil_element(_) -> true.

open_log_file (LogFileName) ->
				case file:open(LogFileName, [write,{encoding, utf8}]) of 
					{ok,IoDevice} -> IoDevice;
					{error,Reason} -> 
								io:format("File Open Error: ~w ~n",[file:format_error(Reason)])
				end.	

print_queue(IoDevice, Queue, Seq) ->
								lists:map(fun (Entry) -> 
								?Trace(IoDevice,string_format( "~w:~w ",[Seq,Entry],?LINE)) end,
 								queue:to_list(Queue)),
								?Trace(IoDevice,string_format( "~n",[],?LINE)),
								Queue.


string_format(Pattern, Values, LineNo) -> lists:flatten(io_lib:format("Line# ~w : "++Pattern, [LineNo|Values])).


trace_line(IoDevice,X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
							io:format(IoDevice,"~p/~p/~p - ~p:~p:~p:~p [Machine:~p * Module:~p ] --> ~p~n",
								[Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE, X]).



encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
					{{Year,Month,Day},{HH,MM,SS}}=
						calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
					{Year,Month,Day,HH,MM,SS,MicroSeconds}.


