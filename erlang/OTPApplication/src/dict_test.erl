%% Author: sshadab
%% Created: 2010-05-07
%% Description: TODO: Add description to dict_test

%% this module considers two keys as different if they do not match (=:=), 
%% orddict considers two keys as different if and only if they do not compare equal (==).

-module(dict_test).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([append/0,append_list/0,print_dict/1, fetch/2,from_list/1,filter_negative_test/0,filter_positive_test/0]).
-export([find_test/1,fold_Positive_test/0,fold_negative_test/0,is_key_test_positive/0,is_key_test_negative/0]).
-export([map_test/0, merge_test/0,store_test/2,to_list_test/0, update_test/1,update_Test/2,fold_test/0,update_counter_test/0]).

%%
%% API Functions
%%

append () -> D1=dict:append(1, "One", dict:new()),
			 D2=dict:append(2, "Two", D1),
			 D3=dict:append(3, "Three", D2),
			 D4=dict:append(3, "Three", D3),
			 D5=dict:append(3, "Three", D4),
			 print_dict(D5),
			 D5.


append_list() -> dict:append_list(1, ["Two","Three","Four"], dict:append(1, "One", dict:new())).

fetch(Key, Dict) -> try dict:fetch(Key, Dict) of
						Value -> io:format("~p --> ~p ~n", [Key, Value]),
								 Value
					catch Class1:Pattern1 ->
						case Class1 of 
			  				error -> io:format("Key ~p not found in dictionary ~p ~n",[Key, Dict]),
			   				key_not_found; %%client code matches this value to see if the lookup was successfull

			  			Other -> io:format("~p:~p" ,[Other,Pattern1])
						end
					end.

from_list([]) -> dict:new();
from_list(KeyValueList) -> dict:from_list(KeyValueList).

filter_positive_test () -> D1=dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4},{"5",5},{"6",6}]),
			 FilterFunction = fun (Key,Value) when is_integer(Value) -> 
									   case Value rem 2 of
										   0 -> true;
										   _ -> false
									   end;
								 (Key,BadArg) -> io:format("Un-applicable for Key:~p Val:~p ~n", [Key ,BadArg]),
												 throw("Bad Argument")
												 
							  end,
			 D2=dict:filter(FilterFunction, D1),
			 print_dict (D2).

filter_negative_test () -> D1=dict:from_list([{"1",1},{"2",atom},{"3",3},{"4",4},{"5",5},{"6",6}]),
			 FilterFunction = fun (Key,Value) when is_integer(Value) -> 
									   case Value rem 2 of
										   0 -> true;
										   _ -> false
									   end;
								 (Key,BadArg) -> io:format("Un-applicable for Key:~p Val:~p ~n", [Key ,BadArg]),
												 throw("Bad Argument")
												 
							  end,
			 D2=dict:filter(FilterFunction, D1),
			 print_dict (D2).


find_test(Key) -> D1= dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4},{"5",5},{"6",6}]),
				dict:find(Key, D1).


%% The fold_test does a Merge: it merges two dictionaries, Dict1 and Dict2, to create a new dictionary. 
%% All the <Key,Value> pairs from both dictionaries are included in the new dictionary. 
%% If a key occurs in both dictionaries then Fun is called with the key and both values 
%% to return a new value.

%% Algorithem of the merge includes the concept of "fold" (and "accumulation"):
%% 1_ pick one dict as source and the other as initial accumulator
%% 2_ pick FIRST pair of <Key,Value> from source and add it to initial accumulator by 
%% running some sort of merge logic (new updated accumulator Dict is created)

%% 3_ pick NEXT pair of <Key,Value> from source and add it to new updated accumulator we got from previous step
%% 4_ repeat step 3 for each element in source dictionary

%%Now that skeleton is in place we can focus on merge logic:

%% Given a pair of <Key,Value> and a dictionary:
%% 1_ if Key doesn't exist in dictionary, add <Key,Value>.
%% 2_ if it exists, call a given logic to decide what value has to be stored in dictionary.
%% This logic (SharedKeyHandling) takes the "Key", "Value" and "existing-Value" and it returns a "new-Value".
%% Then the <Key,new-Value> is added to dictionary.

%% This is very similar to what dict:update(Key, Fun, Initial, Dict1) -> Dict2 does:
%% 1_If "Key" is not present in Dict1 then Initial will be stored as the first value (<Key, Initial> will be added to Dict1)
%% 2_If "Key" exists in Dict1 (say: <Key,Value>) then it updates the value in a dictionary by calling Fun
%% on the "Value" to get a new value (I.e. it modifies <Key,Value> to <Key, Fun(Value)> 
%% which apperantly results in new Dictionary).

%% In order to use dict:update/4 for our merge logic, we need to implement the Fun.
%% Fun takes the Value in <Key,Value> and returns a New-value. The question is what is the logic
%% to map "Value ==> New-Value". Remeber in order to merge a given <Key,Value> to a dictionary when 
%% Key already exists we are given the logic to decide WHAT VALUE HAS TO BE STORED IN DICTIONARY ("SharedKeyHandling"): 
%% I.e. It maps ("Key", "Value" , "existing-Value") ==> New-Value. Therefore all Fun has to do is calling
%% SharedKeyHandling("Key", "Value" , "existing-Value").


fold_test() ->  
			%Initialization :
				{AccumulatorDict, SourceDict, SharedKeyHandling}=init(),

			% function definition:
				MergeLogic = fun (SourceDict_Key, SourceDict_Value, UpdatedAccumulatorDict) ->
									  
									  UpdateLogic=	fun(UpdatedAccumulatorDict_Value) ->
														SharedKeyHandling(SourceDict_Key,SourceDict_Value,UpdatedAccumulatorDict_Value)
											  		end,
									  dict:update(SourceDict_Key, UpdateLogic, SourceDict_Value, UpdatedAccumulatorDict)
									  
							 end,
				Merged = dict:fold(MergeLogic, AccumulatorDict, SourceDict),
				print_dict(Merged),
				Merged.


				
				
				
fold_Positive_test() -> D1= dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4},{"5",5},{"6",6}]),
			   CalculateSum = fun(Key, Value, AccIn) when is_integer(Value) ->
								 case Value rem 2 of
									 0-> Value+AccIn;
									 _ -> 0+AccIn
								 end;
								(Key, BadArg,_) -> io:format("Bad Argument ~p for Key ~p ~n",[Key, BadArg]),
												   throw ("Bad Argument ibn Dictionary")
							  end,
			   dict:fold(CalculateSum, 0, D1).
			   

fold_negative_test() -> D1= dict:from_list([{"1",1},{"2",doe},{"3",3},{"4",4},{"5",5},{"6",6}]),
			   CalculateSum = fun(Key, Value, AccIn) when is_integer(Value) ->
								 case Value rem 2 of
									 0-> Value+AccIn;
									 _ -> 0+AccIn
								 end;
								(Key, BadArg,_) -> io:format("Bad Argument ~p for Key ~p ~n",[BadArg,Key]),
												   throw ("Bad Argument ibn Dictionary")
							  end,
			   dict:fold(CalculateSum, 0, D1).
			   

is_key_test_positive() -> D1=dict:from_list([{"1",1},{"2",2}]),
						  dict:is_key("2", D1).

is_key_test_negative() -> D1=dict:from_list([{"1",1},{"2",2}]),
						  dict:is_key(2, D1).

								   
map_test() -> D1=dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4},{"5",5},{"6",6}]),
			  Make_double = fun(Key, Value) when is_integer(Value) -> Value*Value;
							   (Key, BadArg) -> io:format("Bad argument: ~p for key ~p: ~n", [BadArg,Key]),
												throw("Bad Argument.")
							end,
			  Print_Dict = fun(Key, Value) -> io:format("<~p, ~p> ~n", [Key,Value]) end,
			  D2=dict:map(Make_double, D1),
			  
			  %% Now print the result
			  io:format("Now print the result: ~n"),
			  dict:map(Print_Dict, D2),
			  io:format("~n ~n ~n"),
			  D2.

merge_test() -> D1=dict:from_list([{"1",1},{"3",3},{"5",5},{"7",7},{"8",7},{"10",10},{"12",12}]),
				D2=dict:from_list([{"2",2},{"4",4},{"8",6},{"8",8},{"10",10},{"12",12},{"14",14}]),
				Repetition_Calc = fun(Key, Valu1, Valu2) -> io:format("Key ~p, with the same value ~p exists in both dictionaries: ~n",[Key, Valu1]),
														   Valu1*Valu1
								end,
				MergedDict = dict:merge(Repetition_Calc, D1, D2),
				print_dict(MergedDict),
				io:format("~n ~n ~n"),
				MergedDict.

								  
store_test(Key, Value) -> D1=dict:from_list([{"1",1},{"3",3},{"5",5},{"7",7}]),
				io:format("Before storing {~p,~p}:~n",[Key,Value]),
				print_dict(D1),
				D2=dict:store(Key, Value, D1),
				io:format("After storing {~p,~p}:~n",[Key,Value]),
				print_dict(D2).
						  
to_list_test() -> D1=dict:from_list([{"1",1},{"3",3},{"5",5},{"7",7}]),
				  dict:to_list(D1).	


update_test(Key) -> D1=dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4}]),
					case dict:is_key(Key, D1) of
						true ->
							Calculate = fun (Value) when is_integer(Value) ->
									  		case Value rem 2 of
										  		0 -> Value/2;
										  		_ -> 0
									  		end;
											(BadArg) -> throw (" Expected Integer argument!!")
							 			end,
				 			D2 = dict:update(Key, Calculate, D1),
				 			print_dict(D2),
				 			D2;
						false -> throw ("Key not found!")
					end.
					

%% This method appends a value at the end of the list corresponding 
%% with the given key. If the given key doesn't exist in the dictionary, 
%% a new entry Key,[Value] will be added to the dictionary.

%% This is what dict:update(Key, Fun, Initial, Dict1) -> Dict2 does:

%% 1_If "Key" is not present in Dict1 then Initial will be stored as the first value 
%%(i.e. <Key, Initial> will be added to Dict1)

%% 2_If "Key" exists in Dict1 (say: <Key,Value>) then it updates the value in a dictionary by calling Fun
%% on the "Value" to get a new value (I.e. it modifies <Key,Value> to <Key, Fun(Value)> 
%% which apperantly results in new Dictionary).

update_Test(Key, VlaueToBeAppended) ->D1=dict:from_list([{"1",1},{"2",2},{"3",3},{"4",4}]),
							io:format("The original dictionary:~n"),
							print_dict(D1),	  
									  
							% The function accepts Value associated with the Key (here is a list)
							% and returns a new Value (list). 
							% The logic is appending given list(VlaueToBeAppended) to the 
							% list associated with the given key in the Dictionary 
							UpdateLogic = fun (ValueAssociatedWithKey)  -> 
											  lists:append([ValueAssociatedWithKey],
														   [VlaueToBeAppended]) 
									 end,

							% If the given key doesn't exist in the dictionary 
							% <Key,[VlaueToBeAppended]> will be added to dictionary
				 			D2 = dict:update(Key, UpdateLogic,[VlaueToBeAppended], D1),
							io:format("The new dictionary:~n"),
				 			print_dict(D2),
				 			D2.

update_counter_test() -> Dict1 = dict:update_counter(1, 1, dict:new()),
						Dict2 = dict:update_counter(1, 1,Dict1),
						Dict3 = dict:update_counter(2, 3,Dict2),
						Dict4 = dict:update_counter(2, 3,Dict3),
						Dict5 = dict:update_counter(4, 1,Dict4),
						print_dict(Dict5),
						Dict5.



%%
%% Local Functions
%%

print_dict (Dict) -> try print_values(dict:fetch_keys(Dict),Dict)
					 catch Class:Exp ->
							   case Class of
								   error -> io:format("Unable to print: The argument is not of type Dictionary ~n");
								   Other -> io:format("~p,~p",[Other,Exp])
							   
							   end
					end.
										 
print_values([],_) -> io:format("~n");
print_values(Keys,Dict) -> [FirstKey|TheRest]= Keys,
						   fetch(FirstKey, Dict),
						   print_values(TheRest,Dict).
						   
init()->D1=dict:from_list([{"1",1},{"3",3},{"5",5},{"7",7},{"8",7},{"10",10},{"12",12}]),
		D2=dict:from_list([{"2",2},{"4",4},{"8",6},{"8",8},{"10",10},{"12",12},{"14",14}]),
		Fun = fun(Key, SourceDictValue, UpdatedAccumulatorDictValue) -> 
			io:format("Key: ~p exists in both dictionaries: ~n",[Key]),
			SourceDictValue*UpdatedAccumulatorDictValue
		end,
		{D1,D2,Fun}.
					  

										   




