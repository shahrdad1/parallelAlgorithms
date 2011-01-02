
-module(binomial_heap_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([]).

remove_min_test() ->
	Binomial_Heap =	binomial_heap:create_binomial_heap(),
	IoDevice = open_log_file("./log/Binomial_Heap_unittest5.log"),
	New_Binomial_Heap = binomial_heap:remove_min(Binomial_Heap),
	validate(New_Binomial_Heap,IoDevice).

remove_min_tree_test() ->
	Binomial_Heap =	binomial_heap:create_binomial_heap(),
	IoDevice = open_log_file("./log/Binomial_Heap_unittest4.log"),
	{{MinVal,Rank,Children},New_Binomial_Heap}=binomial_heap:remove_min_tree(Binomial_Heap),
	lists:map(fun ({Val,_,_}) -> ?assert (MinVal =< Val) end, Binomial_Heap),
	validate(New_Binomial_Heap,IoDevice),
	?assertEqual(length(Binomial_Heap),length(New_Binomial_Heap)+1),
	?assertEqual(Binomial_Heap,binomial_heap:merge_tree_to_heap({MinVal,Rank,Children}, New_Binomial_Heap,IoDevice)).

insert_element_test() -> Updated_Heap = binomial_heap:insert_element(),
						 IoDevice = open_log_file("./log/Binomial_Heap_unittest3.log"),
						 validate (Updated_Heap,IoDevice).

merge_binomial_heap_test() -> 
								Merged_Binomial_Heap = binomial_heap:merge_heaps(),
							   	IoDevice = open_log_file("./log/Binomial_Heap_unittest2.log"),	
							   	validate (Merged_Binomial_Heap,IoDevice).


create_binomial_heap_test() -> 
								Binomial_Heap = 
								   	binomial_heap:create_binomial_heap(),
							   	IoDevice = open_log_file("./log/Binomial_Heap_unittest1.log"),	
							   	validate (Binomial_Heap,IoDevice).


pad_binomial_heap_test() -> 
%% 	IoDevice = open_log_file("./log/Binomial_Heap_unittest2.log"),
	BinomialHeap = [{1,4,[]},{2,6,[]},{3,7,[]},{4,12,[]},{5,13,[]},{6,14,[]},{7,15,[]},{8,21,[]}],
	io:format("Line 24 in unit test",[]),
	PaddedHeap = binomial_heap:pad_with_empty_nodes(BinomialHeap),
	
	Expected=[{nil},{nil},{nil},{nil},{1,4,[]},{nil},
			  {2,6,[]},{3,7,[]},{nil},{nil},{nil},{nil},
			  {4,12,[]},{5,13,[]},{6,14,[]},{7,15,[]},
			  {nil},{nil},{nil},{nil},{nil},{8,21,[]}],
	?assertEqual(PaddedHeap,Expected).



validate([],IoDevice) -> io:format(IoDevice,"Validation is done.",[]);

validate(Binomial_Heap,IoDevice) ->
	lists:map(fun (Binomial_Tree) -> validate_binomial_tree(Binomial_Tree,IoDevice) end, 
			  Binomial_Heap).

validate_binomial_tree (BinomialTree,IoDevice) -> 
	level_order_traverse(queue:in(BinomialTree, queue:new()),IoDevice).



%%
%% Local Functions
%%
	
level_order_traverse(Queue,IoDevice)	->
		case queue:out(Queue) of
								
			{{value, {Val, Rank,Children}}, Queue1} ->  
									
				io:format(IoDevice,"Node ~p popped from the queue.~n",[{Val, Rank,Children}]),
				assert_Parent_is_less_than_all_children(Val,Children,IoDevice),
				io:format(IoDevice,"Node ~p is less than all its children.~n",[{Val, Rank,Children}]),
				assert_children_rank_decreasing(Children,IoDevice),
				io:format(IoDevice,"Children of node ~p have decreasing ranks.~n",[{Val, Rank,Children}]),
				Queue2 = enqueue_children(Children,Queue1),
				level_order_traverse(Queue2,IoDevice);
									
							
							
			{empty, _} -> io:format("Validation Succesfull!")
		end.

assert_Parent_is_less_than_all_children(ParentVal,[],IoDevice) -> 
	io:format(IoDevice,"Parent ~p is less than all its chldren []~n",[ParentVal]);

assert_Parent_is_less_than_all_children(ParentVal,Children,_) ->
	lists:map(fun ({ChildVal,_,_}) -> 
				?assert (ParentVal =< ChildVal) 
			end,Children).

assert_children_rank_decreasing ([],IoDevice) -> 
	io:format(IoDevice,"chldren [] has decreasing ranks. ~n",[]),
										0;

assert_children_rank_decreasing ([HighestRankChild|Rest],IoDevice) ->
	{_,HighestRank,_}= HighestRankChild,
	lists:foldl(fun ({_,Rank,_},Acc) ->
						 io:format(IoDevice,"?assertEqual(~p,~p)~n", [Rank,Acc]),
						 ?assertEqual(Rank,Acc-1),
						 Acc-1

				end,HighestRank, Rest).
	
								
enqueue_children(Children,Queue) -> 
		lists:foldl(fun (BinomialTree,QueueAcc) -> queue:in(BinomialTree, QueueAcc) end, 
					Queue, Children).

						
open_log_file (FileName) ->
				case file:open(FileName, [write,{encoding, utf8}]) of 
					{ok,IoDevice} -> IoDevice;
					{error,Reason} -> 
								io:format("File Open Error: ~p ~n",[file:format_error(Reason)])
				end.								
						
						
	

