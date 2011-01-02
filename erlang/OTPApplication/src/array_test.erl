%% Author: Shahrdad
%% Created: May 1, 2010
%% Description: TODO: Add description to array_test
-module(array_test).
%%
%% Include files
%%



%%
%% Exported Functions
%%
-export([acc_logic/3,map_logic/2,merge_logic/3]).
-export([fold_example/1,from_orddict_test/0,map_test/1,create_array/3,catch_test/3,sparse_foldl_example/1]).
-export([sparse_fold_example/1,sparse_map_test/0,sparse_size_test/2, sparse_to_orddict_test/2,merge_element_test/1]).
-export([merge_arrays_test/0]).
%%
%% API Functions
%%

fold_example(r) -> foldr_example([1,2,3,4,5]);
fold_example(l) -> foldl_example([1,2,3,4,5]);
fold_example(Other) -> throw ({badArgument,Other}).

foldl_example(List) when is_list(List)-> 
	Array=array:from_list(List),
	array:foldl(fun acc_logic/3, 0, Array);	
foldl_example(_) -> throw ("The arg must be a list of integers!").

foldr_example(List) when is_list(List)-> 
	Array=array:from_list(List),
	array:foldr(fun acc_logic/3, 0, Array);
foldr_example(_) -> throw ("The arg must be a list of integers!").


						
%% ********************* Sparse stuff ****************************
	
sparse_fold_example(r) -> sparse_foldr_example(array:set(17, 1, array:new()));
sparse_fold_example(l) -> sparse_foldl_example(array:set(11, 1, array:new()));
sparse_fold_example(Other) -> throw ({badArgument,Other}).

sparse_foldl_example(Array) -> array:sparse_foldl(fun acc_logic/3, 0, Array).


sparse_foldr_example(Array) -> array:sparse_foldr(fun acc_logic/3, 0, Array).



acc_logic (Index, ArrayElement, Accumulator) when is_integer(ArrayElement)-> 
								io:format("Index: ~p, Accumulator: ~p, ArrayElement: ~p ~n",
										  [Index,Accumulator,ArrayElement]),
								Accumulator + ArrayElement;

acc_logic (_,_,_) -> throw ("array elemets must be all integers! ").



sparse_map_test() ->
	Array=array:set(12, 23, array:new()),
	array:sparse_map(fun map_logic/2,Array).

sparse_size_test (Index, Value) when is_integer(Index) ->
	Array = array:set(Index,Value,array:new()),
	io:format("Here is the Array: ~p ~n", [Array]),
	io:format("Here is sparse Array size: ~p ~n",[array:sparse_size(Array)]),
	io:format("Here is Array size: ~p ~n",[array:size(Array)]),
	io:format("Here is sparse-to-list -view of the sparse array: ~p ~n",[array:sparse_to_list(Array)]),
	io:format("Here is to-list view of the sparse array: ~p ~n ~n ~n",[array:to_list(Array)]),
	ResizedArray = array:resize(1,Array),
	io:format("Here is the resized array to 1: ~p ~n",[ResizedArray]),
	io:format("Here is list view of the resized array: ~p ~n",[array:to_list(ResizedArray)]).
	

sparse_to_orddict_test(Index, Value) when is_integer(Index) ->
	Array = array:set(Index, Value, array:new()),
	io:format("Here is the sparse-to-orddict() result: ~p ~n",[array:sparse_to_orddict(Array)]),
	io:format("Here is the to-orddict() result: ~p ~n",[array:to_orddict(Array)]).


map_logic (Index, Value) when Index rem 2 == 0 -> 
		 	io:format("entry ~p at Index : ~p of existing array is mapped to entry ~p at index ~p of new array ~n", [Value,Index,Value * Value,Index]), 
			Value * Value;

map_logic (Index, Value) -> 
		 	io:format("entry ~p at Index : ~p of existing array is mapped to entry ~p at index ~p of new array ~n", [Value,Index,0,Index]), 
			0.


%% ***************************************************************

from_orddict_test() -> Input = [{0,1},{2,2},{4,5},{12,-4}],
					   Array = array:from_orddict(Input,-99999),
					   array:size(Array).

map_test(List) when is_list(List)->
	Array=array:from_list(List),
	array:map(fun map_logic/2,Array).

create_array(Size, FixedFlag, DefaultVal) when is_integer(Size), (FixedFlag==false);(FixedFlag==true) ->
	array:new([{size,Size},{fixed,FixedFlag},{default,DefaultVal}]).

catch_test(Size, FixedFlag, DefaultVal)->
	A1=create_array(Size, FixedFlag, DefaultVal),
	try array:set(10,"value",A1)
	catch Class:Pattern ->
			  case Class of 
				  error -> case Pattern of
							   badarg -> io:format("Error: Fixed size array cannot be expanded~n");
							   Other -> io:format("~p : ~p ~n",[Other,Pattern])
						   end;
				  Other -> io:format("~p : ~p ~n",[Other,Pattern])
			  end
			  
	end.

%% Merging two sorted arrays algorithem:
%% Assume two sorted arrays Array1 and Array2 are given:
%% 
%% 1_Pick Array1 as source and Array2 as Initial Accumulator (must be resizable)
%% 
%% 2_Pick the entry@0 of Array1 and merge it to Initial Accumulator by running a
%% Merge Logic. As a result "1st Updated Accumulator" is created.
%% 
%% 3_Assume the entry@(N-1) of Array1 is successfully merged to Nth Updated Accumulator (by running the
%% Merge Logic) and a new (N+1)th Updated Accumulator is created.
%% 
%% 4_Pick the entry@N of Array1 and merge it to (N+1)th Updated Accumulator by running the
%% Merge Logic. As a result "(N+2)th Updated Accumulator" is created.
%%


merge_arrays_test() -> 	Array1 = array:relax(array:from_list([1,3,4,7,9,11])),
						Array2 = array:relax(array:from_list([0,2,5,6,8,10,12])),
						io:format("Format ~p",[Array1]),
						array:to_list(merge_arrays(Array1,Array2)).


merge_arrays (Array1, Array2) -> 
	array:foldl(fun merge_logic/3, Array2, Array1).


merge_logic (Array1Index, Array1Value, Accumulator_Array)->
	merge_element (Array1Value, Accumulator_Array).



%%Now that skeleton is in place we can focus on merge logic:

%% The requierment is to insert Value into a given sorted array so that
%% it remains sorted and doesn't create duplicates.
%% 
%% The algorithem would be:
%% 1_Given Value is compared with entry@N.
%% 2_If Value < entry@N all the elements positioned @N,N+1,... are shifted one position to the right
%% and Value is inserted at position N.
%% 3_If Value > entry@N repeat step 2 for entry positioned at N+1. 
%% If we hit end of the Array, just append it to the end.
%% 4_Value = entry@N, do nothing.
%% 


%%
%% Local Functions
%%


merge_element (NewValue, Array) when is_integer(NewValue)-> 
	%% Addresses the scenario in which a new element is added
	Insert_Logic = fun (CurrentIndex, CurrentValue, Accumulator_Array)->
							io:format("CurrentIndex: ~p, CurrentValue:~p, Acc:~p ~n",[CurrentIndex,CurrentValue,array:to_list(Accumulator_Array)]),
							LastElementIndex = array:size(Array)-1,
							case (CurrentIndex) of
								LastElementIndex when NewValue > CurrentValue -> New_Accumulator_Array = array:set(LastElementIndex, CurrentValue, Accumulator_Array),
																				 array:set(LastElementIndex+1, NewValue, New_Accumulator_Array);
								_ -> 
									case (NewValue <  CurrentValue) and (CurrentIndex == 0) of 
										true -> New_Accumulator_Array = array:set(CurrentIndex, NewValue, Accumulator_Array),
												array:set(CurrentIndex+1, CurrentValue, New_Accumulator_Array);
										
										false -> 
											case (NewValue >  CurrentValue) and (CurrentIndex == 0) of
												true -> array:set(CurrentIndex, CurrentValue, Accumulator_Array);
												
												false -> 								
													case (NewValue <  CurrentValue)  and (NewValue>array:get(CurrentIndex-1, Array)) of 
														true -> New_Accumulator_Array = array:set(CurrentIndex,NewValue,Accumulator_Array),
																array:set(CurrentIndex+1, CurrentValue, New_Accumulator_Array);
														
														false -> 
															case (NewValue <  CurrentValue) of 
																true -> 														 
																	array:set(CurrentIndex+1, CurrentValue,Accumulator_Array);
																false ->
																	array:set(CurrentIndex, CurrentValue,Accumulator_Array)
															end
													end
											end
									end
									
							end
				   end,
	
%% 	Addresses the scenario in which an existing element is added to the array
	ElementsOfExistingArray = array:to_list(Array),
	case lists:member(NewValue, ElementsOfExistingArray) of 
		true -> array:from_list(ElementsOfExistingArray);
		false -> array:foldl(Insert_Logic, array:new(), Array)
	end.



	
merge_element_test(NewElement) -> Array = array:from_list([1,3,4,7,9,11]),
						   array:to_list(merge_element(NewElement,Array)).
						   


























												  
	

