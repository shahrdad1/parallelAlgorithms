
-module(exercises).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([concatenate/1,flatten/1,sort/1, convert_to_term/2,build_term/2, parse/1, push/2,pop/1]).





%% Evaluating and Compiling Expressions
%% infix_to_prefix ({Left,OP,Right}) 


parse (Exp) when is_list(Exp) -> {[],StringRep,no_more_element}=convert_to_term(Exp, []),
								 StringRep. 

convert_to_term([], Stack) -> pop(Stack);

	
convert_to_term([H|T], Stack) -> case H of 
					   				$( -> convert_to_term(T, push(H, Stack));
									 
									$) -> Rest = build_term(Stack,[$}]),
										convert_to_term(T, Rest);
									 
									 _  -> 
										 convert_to_term(T, push(H,Stack))   
						  		end.	  

						  
build_term([], Acc) -> Acc;			  
build_term(Stack, Acc) -> io:format("Pop stack: ~p ~n",[Stack]),
							{Rest,Token,Next} = pop(Stack), %% It is neceessory to know what is next to pop up
							case Token of 
							  	$+ -> build_term(Rest,lists:append([[$,,$p,$l,$u,$s,$,],Acc]));
							  	$- -> build_term(Rest,lists:append([[$,,$m,$i,$n,$u,$s,$,],Acc]));
								
								$( -> io:format("Currently popped: ~p Rest:~p ~n", [Token, Rest]),
									case Next of
										  
										$~ -> 
											%% Remove the '~'
											{Rest1,$~,_} = pop(Rest),
											io:format("Tilded is Removed: ~p ~n",[Rest1]),											
											New = lists:append([[${], [$m,$i,$n,$u,$s,$,],[${],Acc,[$}] ]),
											io:format("Here is the one with tilde: ~p~n",[New]),
											push(New, Rest1);
									  
										_ -> push([${|Acc],Rest)
									  
									  end;
									
									
									
%%								what if it starts with '{'
								_ ->io:format("Token ~p ~n",[Token]), 
									case ((is_list(Token)) andalso (lists:prefix("{",Token))) of
										
										 true  -> io:format("append ~p To ~p ~n",[Token, Acc]),
											 		build_term(Rest,lists:append([Token,Acc]));
										
										 false -> io:format("prepend ~p ~p ~n",[Token, Acc]),
											 build_term(Rest,[${,$n,$u,$m,$,,Token,$}|Acc])
									 end
							end.
						  

push (Element, List) -> io:format("Push element ~p to Stack: ~p ~n", [Element, List] ),
						[Element|List].

pop ([]) -> throw ("Stack is empty!!"); 
pop (List) -> case length(List) >= 2 of
				  true -> {lists:sublist(List, 2, length(List)),lists:nth(1, List),lists:nth(2, List)};
				  false -> {lists:sublist(List, 2, length(List)),lists:nth(1, List),no_more_element}
			  end.



sort ([]) -> [];
sort ([H]) -> [H];

sort ([H|[H1|[]]]) when not((is_list(H)) and (is_list(H1)))-> 
						case (H < H1) of
						  true -> [H,H1];
						  false -> [H1,H]
						end;

sort (L) -> {L1, L2} = lists:split(length(L) div 2, L),
			 lists:merge(sort (L1), sort (L2)).
			 
			








flatten(L) -> flatten1(L,[]).

flatten1([],Acc) -> reverse(Acc);
flatten1([H|T],Acc) -> case is_list(H) of
						   true ->  flatten1(concatenate([H|T]),Acc);
						   false -> flatten1(T,[H|Acc])
					   end.
									


concatenate(L) -> concatenate1(L,[]).

concatenate1([],Acc) -> Acc;
concatenate1([H|T],Acc) -> concatenate1(T,reverse(concat(H,reverse(Acc)))).



	
%%
%% Local Functions
%%

reverse(L) -> reverse1(L,[]).

reverse1([], Acc) -> Acc;
reverse1([H|T], Acc) -> reverse1(T,[H|Acc]).

concat([],L) -> L;
concat(S,L) when not(is_list(S))-> concat([S],L);	
concat([H|T],L) -> concat(T,[H|L]).



