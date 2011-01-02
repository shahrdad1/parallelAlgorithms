%% Author: Shahrdad
%% Created: Feb 18, 2010
%% Description: TODO: Add description to ch2
-module(ch2).

%%
%% Include files
%%


%%
%% Exported Functions
%%

-export([isEmpty/1,cons/2, head/1, tail/1,appendElement/2, append/2, find/2]).
-export([update/3,suffix/1, insert/3, createBinaryTree/1, test/0,testWorst/0, lookup/2, completeTree/2]).
%%
%% API Functions
%%
isEmpty([_|_]) 	-> false;
isEmpty([])		-> true.

cons(X,T) -> case is_list(X) of 
				false -> case is_list(T) of
							true ->[X|T];
							false -> throw("Second arg must be a list")
						 end;
				true -> throw("First Arg must not be a list")
			 end.

%%  The appendElement here costs O(n) copying.
appendElement(X,[])	->	[X];
appendElement(X,[H|T])	->	[H|appendElement(X,T)].

						
head([])	-> throw ("Undefined!");
head([H|_])	->H.

tail([])	-> throw ("Undefined!");
tail([_|T])	-> T.


%%  The append here costs O(n) copying.
%% 	Note that here we are creating a new list whose first n elements 
%%	(if we assume L1 has n elements) are copied from L1 but we don't
%%  copy elements of L2. I.e. Elements of L2 are shared between L2 and our new List.
append([],L2)	-> 	L2;
append([H|T],L2)->	[H|append(T,L2)].

find(X,T)		-> find(X,T,0,lists:flatlength(T)).

find(_,[],_,_)	-> -1;
find(X,[H|T],I,K) -> case X=:=H of
						 true 	-> I+1;
						 false	-> case I < K of 
									   true	->	find(X,T,I+1,K);
									   false ->	-1
								   end
				   	end.


%%
%% Update an element at a given index of the list
%%
%% Here we copy all the elements on the path from the beginning
%% of the list 'L' upto the element we want to update (element 'I')
%% into the new list we are creating.However the rest of the elemets
%% of L which are not in the path will be shared betwen new list and 
%% L.

update(L,I,X)		-> update(L,I,X,1,lists:flatlength(L)).

update([_|T],I,X,I,_)	-> [X|T];			
update([H|T],I,X,C,K) -> case C < K of 		 
						   true -> [H|update(T,I,X,C+1,K)];
						   false-> throw("Index is out of range")
					   	end.


suffix(T)->[T|suffix1(T)].

suffix1([])		->[];
suffix1([_|T])	->[T|suffix1(T)].

%% 
%% Unbalanced BinaryTree => {key, Value , Left subtree, right subtree}
%% 
createBinaryTree(L)	-> io:format("Here is the Input ~p~n:",[L]),
						Tree = insert(L,nil),
					   io:format("Here is the tree ~p~n:",[Tree]).

insert([],Tree) ->Tree;
insert([{Key,Value}|T], Tree) ->  insert(T, insert (Key,Value,Tree)).

insert (Key, Value, nil) -> {Key, Value, nil, nil};

insert (Key, Value,{Key,_,LSubtree1, RSubtree1})	-> {Key,Value,LSubtree1, RSubtree1};

insert (Key, Value, {Key1,Value1,LSubtree1, RSubtree1}) -> case Key < Key1 of 
				true -> { Key1, Value1, insert(Key,Value,LSubtree1),RSubtree1};
				false ->{ Key1, Value1,LSubtree1, insert (Key,Value,RSubtree1)}
			 end.



%% 
%% Exercise 2.3:  Another insert to Binary tree, it doesn't copy any node in the search path 
%% if the new node already exists in the tree
%% 


%% I copy only whan I insert a new value
insert1 (Key, Value, nil) -> {Key, Value, nil, nil};

%% When the value to be inserted already exists in the tree don't do anything, just return
insert1 (Key, _,{Key,_,_,_}) -> nil;

%% Does Not copy when iterating
insert1 (Key, Value, {Key1,Value1,LSubtree1, RSubtree1}) -> case Key < Key1 of 
				true -> Subtree = insert1(Key,Value,LSubtree1), case Subtree /= nil of
																	true -> { Key1, Value1,Subtree,RSubtree1};
																	false -> nil %% If we haven't inserted any thing, just return
																end;
																
				false -> Subtree= insert1(Key,Value,RSubtree1), case Subtree/=nil of 
																	true -> { Key1, Value1,LSubtree1,Subtree};
																	false -> nil %% If we haven't inserted any thing, just return
						 										end
			 end.

%% 
%% Exercise 2.4 part a: function complete(d,x) creates a complete binary tree of depth d and 
%% x is in each of its node. structure of the tree is {Value, LSubtree, RSubtree}
%% 

completeTree(D,X) -> complete(D,X,nil).
complete(D,X,nil) -> {X,complete(D-1,X,nil), case D>=1 of 
													 true -> complete(D-2,X,nil);
													 false ->  {X,nil,nil}
					  end}.

%% complete(D,X,{V,L,R}) -> {V,complete(D-1,X,L), complete(D-1,X,R)}.


%% 
%% Look up an element within an unbalance binary tree
%% 

lookup(_, nil)->false;
lookup(Key, {Key,_,_,_}) ->	true;
lookup(Key, {Key1,_,L,_}) when Key < Key1 -> lookup (Key, L);
lookup(Key, {Key1,_,_,R}) when Key > Key1 -> lookup (Key, R).


%% 
%% Print a Binary Tree
%%  
  
write_tree (T) 		->		write_tree(0,T).
write_tree (D,nil)	-> 	io:format('nil', []);
write_tree (D,{Key,Value,LSubtree, RSubtree}) -> D1=D+4,
										write_tree(D1,RSubtree),
										io:format('~n',[]),
%% 									tab(D),
										io:format('~w ===> ~w~n', [Key,Value]),
										write_tree(D1,LSubtree).

%%   
%%  A Test function to insert data in to the tree and print it  
%% 

test()	->		
	S1=nil,
	S2=insert1(4,joe,S1),
	S3=insert1(12,fred,S2),
	S4=insert1(3,jane,S3),
	S5=insert1(7,kalle,S4),
	S6=insert1(6,thomas,S5),
	S7=insert1(5,rickard,S6),
	S8=insert1(9,susan,S7),
	S9=insert1(2,tobbe,S8),
	S10=insert1(8,dan,S9),
	S11=insert1(5,dan,S10).

testWorst()	->		
	S1=nil,
	S2=insert(1,joe,S1),
	S3=insert(2,fred,S2),
	S4=insert(3,jane,S3),
	S5=insert(4,kalle,S4),
	S6=insert(5,thomas,S5),
	S7=insert(6,rickard,S6),
	S8=insert(7,susan,S7),
	S9=insert(8,tobbe,S8),
	S10=insert(9,dan,S9),
	lookup(10,S10).





