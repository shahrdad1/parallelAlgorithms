%% Author: sshadab
%% Created: 2010-06-09

%% Unless otherwise stated, all functions assume that position numbering starts at 1. 
%% i.e. the first element of a list is at position 1.

%% Two terms T1 and T2 compare equal if T1 == T2 evaluates to true. 
%% They match if T1 =:= T2 evaluates to true.


-module(lists_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([all_test/1, any_test/ 1, append_test1/1, append_test2/2, concat_test/0]).
-export([delete_test1/0,delete_test2/0,delete_test3/0,delete_test4/0,delete_test5/0,delete_test6/0,delete_test7/0]).
-export([dropwhile_test1/2,dropwhile_test2/0,duplicate_test/2, filter_test/2, flatmap_test/1, foldl_test/2]).
-export([foreach_test/1,keydelete_test/3, keyfind_test/3, keymap_test/0, keymember_test/3]).
-export([keymerge_test/1, keyreplace_test/3, keysearch_test/3, keysort_test/1, keystore_test/3]).
-export([keytake_test/2, last_test/1, map_test/1, mapfoldl_test/2, merge_test1/0, merge_test2/0,merge_test3/0]).
-export([merge_test4/0, nth_test/2, nthtail_test/2, partition_test1/1, partition_test2/1]).
-export([splitwith_test/2, prefix_test/2,reverse_test/2, seq_test/3, sort_test/0, split_test/2]).
-export([sublist_test/2, sublist_test1/3, subtract_test/2, suffix_test/2,sum_test/1,takewhile_test/0]).
-export([ukeymerge_test/0, ukeysort_test/1, umerge_test1/0, umerge_test2/0, umerge_test3/0, unzip_test/0, unzip3_test/0]).
-export([usort_test/0, usort_test1/0,zipwith3_test/0,zip_test1/2,zip3_test/3,zipwith_test/2]).
%%
%% API Functions
%%


all_test (List) -> lists:all(fun (X) -> case X rem 2  of 0 -> true; _ -> false end end, List). 

any_test (List) -> lists:any(fun (X) -> case X rem 2  of 0 -> true; _ -> false end end, List). 


%% Note that ListOfLists must only contain lists: [[1,2,3],4,5] is invalid. 
append_test1(ListOfLists) -> lists:append(ListOfLists).

append_test2(List1, List2) -> lists:append(List1, List2).


%% concat(Things) -> string()
%% 
%% Concatenates the text representation of the elements of 'Things'.
%% 
%% Things = [Thing]
%% 
%%  Thing = atom() | integer() | float() | string()
%% 

concat_test() -> lists:concat([doc, '/', file, '.', 3]).


delete_test1() -> lists:delete(1, [1,2.0, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test2()  -> lists:delete("String with space", [1,2.4, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test3() -> lists:delete("String with space  	", [1,2.4, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test4() -> lists:delete("caps matters!", [1,2.4, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test5() -> lists:delete("CAPS Matters!", [1,2.4, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test6() -> lists:delete({a,b,{c}}, [1,2.4, "String with space  	", "CAPS Matters!", {a,b,{c}}]).

delete_test7() -> lists:delete(2, [1,2.0, "String with space  	", "CAPS Matters!", {a,b,{c}}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 							dropwhile(Pred, List1) -> List2										%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% As long as the Pred(X) returns true, the dropwhile function drops the X from the list and continue 
%% with the next  element in the list. When it got to the an element for which Pred(X) returns false
%% function stops and returns whatever is left from the list.

%% This function can be used to remove the repetitions starting from the FIRST POSITION in a list:

%% 					lists_test:dropwhile_test1(16#20,"     shahrdad").

dropwhile_test1(ElementToRemove,List) -> lists:dropwhile(fun (X) ->io:format("X: ~p ~n", [X]),
											  case X of 
												  ElementToRemove -> true; 
												 
												  _->false 
											  end 
										end, List).


dropwhile_test2() -> lists:dropwhile(fun (X) ->io:format("X: ~p ~n", [X]),
											   true
									end, 	[2,2,2,2,2.0,2.0,2.0,3,4]).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 							takewhile(Pred, List1) -> List2										%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The function returns the longest prefix of the list for which all elements satisfy the predicate



takewhile_test() -> lists:takewhile(fun (X) ->
										X==2	 
									end, 	
									[2,2,2,2,2.0,2.0,2.0,3,6,4]).












duplicate_test(N,Elem) -> lists:duplicate(N, Elem).















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 							filter(Pred, List1) -> List2										%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Pred = fun(Elem) -> bool()
%% 
%%  Elem = term()
%% 
%% List1 = List2 = [term()]
%% 
%% 
%% List2 is a list of all elements Elem in List1 for which Pred(Elem) returns true.

%% lists_test:filter_test("shahrdad",$h).

filter_test(List1, Elem) -> lists:filter(fun (X) -> case X of Elem -> true; _ -> false end end, List1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 							flatmap(Fun, List1) -> List2										%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Fun = fun(A) -> [B]
%% 
%% List1 = [A]
%% 
%% List2 = [B]
%% 
%%  A = B = term()
%% 
%% 
%% Takes a list (List1) and a function that takes each element of List1 to a new list. 
%% flatmap appends these new lists and return the result.

%% 						lists_test:flatmap_test("shahrdad").

flatmap_test(List1) -> lists:flatmap(fun (X) -> [X,X] end, List1). 


%% 				lists_test:foldl_test("Shahrdad", $h).

foldl_test (List , ElemToBeFiltered) -> 
										lists:foldl(fun (Elem, Acc) when is_atom(Elem) -> 
																		case Elem of 
						  												 ElemToBeFiltered -> Acc;
						  												 _ -> lists:append(Acc, [Elem])
					  													end
													end,
													[], List).




%% Takes a function from As to Bs, and a list of As and produces a list of Bs by applying the 
%% function to every element in the list. 
%% This function is used to obtain the return values. 
%% The evaluation order is implementation dependent.

map_test(List) -> lists:map(fun (X) when is_atom(X)-> atom_to_list(X) end, List).




%% mapfold combines the operations of map/2 and foldl/3 into one pass. 
mapfoldl_test (List , ElemToBeFiltered) -> 
		 lists:mapfoldl(fun (Elem, AccIn) when is_atom(Elem) -> 
								case Elem of 
									 ElemToBeFiltered ->{atom_to_list(Elem)++" is filtered ", AccIn};
									 _ -> {atom_to_list(Elem),lists:append(AccIn, [Elem])}
								end
						end,
						 [],List).


%% This function is used for its side effects and the evaluation order is defined to be 
%% the same as the order of the elements in the list.

foreach_test(List) -> lists:foreach(fun (X) -> io:format("~p ~n",[X]) end, List).



%% Returns a copy of TupleList1 where the first occurrence of a tuple whose Nth element compares 
%% equal to Key is deleted, if there is such a tuple.

%% TupleList1 = [...,{ ..., val# N-1, val# N , val# N+1,...}, ...]
%% IndexInTuple : N
%% N = 1..tuple_size(Tuple)

keydelete_test(Key, IndexInTuple, TupleList1) -> 
	lists:keydelete(Key, IndexInTuple, TupleList1).



%% Searches the list of tuples TupleList for a tuple whose Nth element compares equal to Key. 
%% Returns Tuple if such a tuple is found,otherwise false.

%% TupleList1 = [...,{ ..., val# N-1, val# N , val# N+1,...}, ...]
%% IndexInTuple : N
%% N = 1..tuple_size(Tuple)

keyfind_test(Key, IndexInTuple, TupleList1) -> 
	case lists:keyfind(Key, IndexInTuple, TupleList1) of
		false -> {};
		FoundTuple -> FoundTuple
	end.



%% keymap(Fun, N, [...,{ ..., val# N-1, val# N , val# N+1,...}, ...]) -> [...,{ ..., val# N-1, Fun(val# N) , val# N+1,...}, ...]
%% N = 1..tuple_size(Tuple)

keymap_test ()-> Fun = fun (X) when is_atom(X)-> atom_to_list(X) end,
				 lists:keymap(Fun, 2, [{a,shahrdad,shadab}, {b,tahereh,danesh}, 
									   {c,bibi,safieh}, {d,bibi,sajedeh}]).




%% keymember(Key, N, TupleList) Returns true if there is a tuple in TupleList  
%% whose Nth element compares equal to Key, otherwise false.

%% TupleList: [...,{ ..., val# N-1, val# N , val# N+1,...}, ...]
%% N = 1..tuple_size(Tuple)
keymember_test(Key, N, TupleList) -> lists:keymember(Key, N, TupleList).




%% keymerge(N, TupleList1, TupleList2) -> TupleList3
%% Returns the sorted list formed by merging TupleList1  and TupleList2. 
%% The merge is performed on the Nth element of each tuple. 
%% Both TupleList1 and TupleList2 must be key-sorted prior to evaluating this function. 
%% When two tuples compare equal, the tuple from TupleList1 is picked before the tuple from TupleList2.

%% N = 1..tuple_size(Tuple)

keymerge_test(Index) -> lists:keymerge(Index,
								   [{a,val1,"Ted"},
									  {c,val2,"yanke"},
									  {e,val3,"Zina"}
									],
								   [
									{b,val4,"bob"},
									{d,val5,"Jane"}
								   ]).






%% Returns the sorted list formed by merging all the sub-lists of ListOfLists. 
%% All sub-lists must be sorted prior to evaluating this function. 
%% When two elements compare equal, the element from the sub-list with the lowest 
%% position in ListOfLists is picked before the other element.

merge_test1() -> lists:merge([[1,3,5,7,9],[5,a,c,g,h],"bdfhj"]).






%% Returns the sorted list formed by merging all the sub-lists of ListOfLists. 
%% All sub-lists must be sorted and contain no duplicates prior to evaluating this function. 
%% When two elements compare equal, the element from the sub-list with the lowest position in 
%% ListOfLists is picked and the other one deleted.

umerge_test1 () -> lists:umerge([1,3,5,5,7,9,c,f,q],[2,4,5,5,6,8,a,b,z]).







%% Returns the sorted list formed by merging List1 and List2. Both List1 and List2 must be 
%% sorted prior to evaluating this function. 
%% When two elements compare equal, the element from List1 is picked before the element from List2.
merge_test2() -> lists:merge([1,3,5,7,9],[2,4,6,9,11]).







%% Returns the sorted list formed by merging List1 and List2. 
%% Both List1 and List2 must be sorted according to the ordering function  Fun and contain no 
%% duplicates prior to evaluating this function. Fun(A, B) should return true if A compares 
%% less than or equal to B in the ordering, false otherwise. 
%% When two elements compare equal, the element from List1 is picked and the one from List2  deleted.

umerge_test2() -> 	OrderingFunc = fun ({X1,Y1},{X2,Y2}) 
						when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
						
						   case (X1 < X2) of
							   true -> true;
							   false -> case (X1 > X2) of
											true -> false;
											false -> (Y1 < Y2)
										end
						   end
				   end,
									   
					lists:umerge(OrderingFunc, [{2,4},{2,6},{4,1},{6,7}], [{2,5},{3,0},{3,7},{5,3},{6,7}]).







%% Returns the sorted list formed by merging List1 and List2. 
%% Both List1 and List2 must be sorted according to the ordering function  Fun prior to evaluating 
%% this function. 
%% Fun(A, B) should return true if A compares less than or equal to B in the ordering, 
%% false  otherwise. 
%% 
%% When two elements compare equal, the element from List1 is picked before the element from List2.

%% Whenever an ordering function  F is expected as argument, it is assumed that the following 
%% properties hold of F for all x, y and z:
%% 
%%     *
%% 
%%       if x F y and y F x then x = y (F is antisymmetric);
%%     *
%% 
%%       if x F y and and y F z then x F z (F is transitive);
%%     *
%% 
%%       x F y or y F x (F is total).
%% 

merge_test3 () ->
	OrderingFunc = fun ({X1,Y1},{X2,Y2}) 
						when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
						
						   case (X1 < X2) of
							   true -> true;
							   false -> case (X1 > X2) of
											true -> false;
											false -> (Y1 < Y2)
										end
						   end
				   end,
									   
	lists:merge(OrderingFunc, [{2,4},{2,6},{4,1},{6,7}], [{2,5},{3,0},{3,7},{5,3},{6,7}]).







%% Returns the sorted list formed by merging List1, List2 and List3. All of List1, List2 and 
%% List3 must be sorted and contain no duplicates prior to evaluating this function. 
%% When two elements compare equal, the element from List1 is picked if there is such an element, 
%% otherwise the element from List2 is picked, and the other one deleted.


umerge_test3 () -> lists:umerge3([1,3,5,7,9],[5, a,c,g,h],"bdfhj").




%% Returns the sorted list formed by merging List1, List2 and List3. 
%% All of List1, List2 and List3 must be sorted prior to evaluating this function. 
%% When two elements compare equal, the element from List1, if there is such an element, 
%% is picked before the other element, otherwise the element from List2 is picked before 
%% the element from List3.

merge_test4 () -> lists:merge3([1,3,5,7,9],[5,a,c,g,h],"bdfhj").






%% Returns the sorted list formed by merging TupleList1  and TupleList2. 
%% The merge is performed on the Nth element of each tuple. 
%% Both TupleList1 and TupleList2 must be key-sorted without duplicates prior to 
%% evaluating this function. When two tuples compare equal, the tuple from 
%% TupleList1 is picked and the one from TupleList2 deleted.

ukeymerge_test() -> lists:ukeymerge(2,[{a,val1,"Ted"},
										{c,val3,"yanke"},
										{e,val5,"Zina"},
										{b,val7,"bob"},
										{d,val9,"Jane"}
							   		   ],
							   			[{g,val1,"Jake"},
							    		 {r,val2,"George"},
										 {a,val4,"Zipta"},
										 {p,val6,"Zorba"},
										 {q,val9,"Welmaz"}
							   			]).




%% "Zips" two lists of equal length into one list of two-tuples, where the first element of 
%% each tuple is taken from the first list and the second element is taken from corresponding 
%% element in the second list.

zip_test1(List1, List2) -> lists:zip(List1, List2).




%% "Zips" three lists of equal length into one list of three-tuples, where the first 
%% element of each tuple is taken from the first list, the second element is taken from 
%% corresponding element in the second list, and the third element is taken from the 
%% corresponding element in the third list.

zip3_test(List1, List2, List3) -> lists:zip3(List1, List2, List3).




%% Combine the elements of two lists of equal length into one list. 
%% For each pair X, Y of list elements from the two lists, the element in the result list will be 
%% Combine(X, Y).

zipwith_test(List1,List2) -> Combine = fun (X,Y) when is_atom(X), is_atom(Y) -> {atom_to_list(X), atom_to_list(Y)} end, 
							lists:zipwith(Combine, List1, List2).




%% Combine the elements of three lists of equal length into one list. For each triple X, Y, Z of list elements from the three lists, 
%% the element in the result list will be Combine(X, Y, Z).
%% 
%% zipwith3(fun(X, Y, Z) -> {X,Y,Z} end, List1, List2, List3) is equivalent to zip3(List1, List2, List3).

zipwith3_test() ->  lists:zipwith3(fun(X, Y, Z) -> [X,Y,Z] end, [a,b,c], [x,y,z], [1,2,3]).





%% "Unzips" a list of two-tuples into two lists, where the first list contains the first element of 
%% each tuple, and the second list contains the second element of each tuple.

unzip_test () -> lists:unzip([{1,a},{2,b},{3,c},{4,e},{5,f}]).






%% "Unzips" a list of three-tuples into three lists, where the first list contains the first 
%% element of each tuple, the second list contains the second element of each tuple, and 
%% the third list contains the third element of each tuple.

unzip3_test() -> lists:unzip3([{a,val1,"Ted"},{c,val3,"yanke"},{e,val5,"Zina"},{b,val7,"bob"},{d,val9,"Jane"}]).






%% Returns a list containing the sorted elements of the list TupleList1.
%% In the case that there are multple tuples whose Nth element are comparing equal, 
%% all but the first tuple of the tuples  have been deleted. 
%% Sorting is performed on the Nth element of the tuples.



ukeysort_test(N) -> lists:ukeysort(N, [{a,val1,"Ted"},
										{c,val3,"yanke"},
										{e,val5,"Zina"},
										{b,val7,"bob"},
										{d,val9,"Jane"},
							   			{g,val1,"Jake"},
							    		{r,val2,"George"},
										{a,val4,"Zipta"},
										{p,val6,"Zorba"},
										{q,val9,"Welmaz"}
							   			]).




%% Returns a copy of TupleList1 where the first occurrence of a T tuple whose Nth element compares 
%% equal to Key is replaced with NewTuple, if there is such a tuple T. if not, it returns the 
%% original list.

%% lists_test:keyreplace_test(val3,2,{z,shahrdad,"Shahrdad"}).
%% N = 1..tuple_size(Tuple)

keyreplace_test(Key, N, NewTuple) -> 
			lists:keyreplace(Key, N, 
									[{a,val1,"Ted"},
									 {c,val2,"yanke"},
									 {e,val3,"Zina"},
									 {b,val4,"bob"},
									 {d,val5,"Jane"}
								   ]
							, NewTuple). 





%% Returns a copy of TupleList1 where the first occurrence of a tuple T whose Nth element compares 
%% equal to Key is replaced with NewTuple, if there is such a tuple T. 
%% If there is no such tuple T a copy of TupleList1 where [NewTuple] has been appended to the end 
%% is returned.
%% N = 1..tuple_size(Tuple)

keystore_test(Key, N, NewTuple) ->
	lists:keystore(Key, N, 
					[{a,val1,"Ted"},
					 {c,val2,"yanke"},
					 {e,val3,"Zina"},
					 {b,val4,"bob"},
					 {d,val5,"Jane"}
				   ]
				, NewTuple). 






%% Searches the list of tuples TupleList for a tuple whose Nth element compares equal to Key. 
%% Returns {value, Tuple} if such a tuple is found, otherwise false.
%% N = 1..tuple_size(Tuple)

keysearch_test(Key, IndexInTuple, TupleList1) -> 
	case lists:keysearch(Key, IndexInTuple, TupleList1) of
		false -> {};
		{value, FoundTuple} -> FoundTuple
	end.






%% Searches the list of tuples TupleList1 for a tuple whose Nth element compares equal to Key. 
%% Returns {value, Tuple, TupleList2} if such a tuple is found, otherwise false. 
%% TupleList2 is a copy of TupleList1 where the first occurrence of Tuple has been removed.

keytake_test(Key, N) -> case lists:keytake(Key, N, 
									[{a,val1,"Ted"},
									 {c,val2,"yanke"},
									 {e,val3,"Zina"},
									 {b,val4,"bob"},
									 {d,val5,"Jane"}
								   ]) of
							false -> notFound;
							{value, Tuple, TupleList} ->
								io:format("Tuple: ~p is removed~n Here is the new list:~p ~n",[Tuple,TupleList])
						end.





%% Returns a list containing the sorted elements of the list TupleList1. 
%% Sorting is performed on the Nth element of the tuples.
%% N = 1..tuple_size(Tuple)

keysort_test(N) -> lists:keysort(N, [{a,val1,"Ted"},
									 {c,val2,"yanke"},
									 {e,val3,"Zina"},
									 {b,val4,"bob"},
									 {d,val5,"Jane"}
								   ]).



%% Returns the last element in List.

last_test(List) -> lists:last(List).



%% Returns the Nth element of List. 
%% N = 1..list_size
nth_test(N, List) -> lists:nth(N, List).




%% Returns the Nth tail of List, that is, the sublist of List starting at N+1 and 
%% continuing up to the end of the list.
%% N = 0..list_size (because it is starting at N+1)
nthtail_test (N,List) -> lists:nthtail(N, List).






%% Returns the sub-list of List1 starting at position 1 and with (max) Len elements. 
%% It is not an error for Len to exceed the length of the list -- in that case the whole 
%% list is returned.

sublist_test(List1, Len) -> lists:sublist(List1, Len).





%% Returns the sub-list of List1 starting at Start  and with (max) Len elements. 
%% It is not an error for Start+Len to exceed the length of the list.

sublist_test1(List1, Start, Len) -> lists:sublist(List1, Start, Len).





%% Partitions List into two lists, where the first list contains all elements for which 
%% Pred(Elem) returns true, and the second list contains all elements for which Pred(Elem) returns false.

%%  lists_test:partition_test([1,2,3,4,5,6,7,8,9]).

partition_test1 (List) ->
	Pred = fun (X) when is_integer(X) -> (X rem 2)==0 end,
	lists:partition(Pred, List).



partition_test2(List) -> lists:partition(fun (X) -> is_integer(X) end, List).



%% 
%% Partitions List into two lists according to Pred. splitwith/2 behaves as if it is defined as follows:
%% splitwith(Pred, List) -> 
%%     {takewhile(Pred, List), dropwhile(Pred, List)}.
%% 
%%  lists_test:splitwith_test([1,2,3,4,5,6,7,8,9],5).

splitwith_test (List, Mid) -> 
	Pred = fun (X) when is_integer(X), is_integer(Mid) -> (X =< Mid) end,
	lists:splitwith(Pred, List).



%% Splits List1 into List2 and List3. List2 contains the first N elements and List3 the rest of 
%% the elements (the Nth tail).

%% N = 0..length(List1)

split_test(N,List) -> lists:split(N, List).




%% Returns true if List1 is a prefix of List2, otherwise false.
%%  lists_test:prefix_test("_-","_-Shahrdad").
prefix_test(List1, List2) -> lists:prefix(List1, List2).



%% Returns true if List1 is a suffix of List2, otherwise false.
%% lists_test:suffix_test("  shadab ","Shahrdad shadab ")
suffix_test(List1, List2) -> lists:suffix(List1, List2).



%% Returns a list with the top level elements in List1  in reverse order, with the tail Tail appended.
reverse_test(List, Tail) -> lists:reverse(List, Tail).



%% Returns the sum of the elements in List.
sum_test(List) -> lists:sum(List).


%% Returns a sequence of integers which starts with From  and contains the successive 
%% results of adding Incr to the previous element, until To has been reached or passed 
%% (in the latter case, To is not an element of the sequence). Incr defaults to 1.
%% Failure: If To<From-Incr and Incr is positive, or if To>From-Incr and Incr is negative, or 
%% if Incr==0 and From/=To.
%% 
%% The following equalities hold for all sequences:
%% 
%% length(lists:seq(From, To)) == To-From+1
%% length(lists:seq(From, To, Incr)) == (To-From+Incr) div Incr

seq_test(From, To, Incr) -> lists:seq(From, To, Incr).






%% Returns a list containing the sorted elements of List1, according to the ordering function  Fun. 
%% OrderingFunc(A, B) should return true if A compares less than or equal to B in the ordering, false otherwise.

sort_test () -> 
					OrderingFunc = fun ({X1,Y1},{X2,Y2}) 
						when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
						
						   case (X1 < X2) of
							   true -> true;
							   false -> case (X1 > X2) of
											true -> false;
											false -> (Y1 < Y2)
										end
						   end
				   end,
									   
				   lists:sort(OrderingFunc, [{3,7},{2,4},{0,7},{5,3},{5,6},{4,1},{12,5},{-1,0},{16,7}]).





%% Returns a list which contains the sorted elements of List1.
%% In the case that there are elements that are comparing equal (according to the ordering function) 
%% all but the first element of the elements comparing equal have been deleted. 
%% OrderingFunc(A, B) should return true if A compares less than or equal to B in the ordering, false otherwise.

usort_test1() -> 
				OrderingFunc = fun ({X1,Y1},{X2,Y2}) 
						when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
						
						   case (X1 =< X2) of
							   true -> true;
							   false -> (Y1 =< Y2)
						   end
				   end,
				lists:usort(OrderingFunc, [{3,7},{2,4},{3,7},{5,3},{2,4},{4,1},{3,7},{-1,0},{4,1}]).






 
 
%%  Returns a list containing the sorted elements of List1 
%%  In the case that there are elements that are comparing equal, 
%%  all but the first element of the elements comparing equal have been deleted. 

usort_test() -> lists:usort([5,2,78,8,2,3,5,78,2,1,8,6,9,56]).








%% subtract(List1, List2) -> List3

%% Returns a new list List3 which is a copy of List1, subjected to the following procedure: 
%% for each element in List2, its first occurrence in List1  is deleted.

%% The complexity of lists:subtract(A, B) is proportional to length(A)*length(B), 
%% meaning that it will be very slow if both A and B are long lists. 
%% (Using ordered lists and ordsets:subtract/2  is a much better choice if both lists are long.)

%% lists_test:subtract_test("Shahrdad","shr").

subtract_test(List1, List2) -> lists:subtract(List1, List2).









%%
%% Local Functions
%%


  