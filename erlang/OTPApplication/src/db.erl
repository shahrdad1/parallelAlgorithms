
-module(db).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([new/0, read/2, write/3, match/2, delete/2]).

%%
%% API Functions
%%

new() -> [].

write(Atom1,Atom2,Db) -> [{Atom1,Atom2}|Db].

read(_, []) -> {error,instance};
read(Atom, [{Atom1,Atom2}|T]) -> case Atom of
									 Atom1  -> {Atom1,Atom2};
									 _ -> read(Atom,T)
								 end.



									
match(Atom, Db)-> match1(Atom,Db,[]).

match1(_,[],Acc) -> reverse(Acc);
match1(Atom, [{Atom1,Atom2}|T], Acc) -> case Atom of
									  		Atom2 -> match1(Atom,T,[Atom1|Acc]);
										   _ -> match1(Atom,T,Acc)
									   end.


									
delete(Atom, Db)-> delete1(Atom,Db,[]).

delete1(_,[],Acc) -> reverse(Acc);
delete1(Atom, [{Atom1,Atom2}|T], Acc) -> case Atom=/= Atom1 of
									  		true -> delete1(Atom,T,[{Atom1,Atom2}|Acc]);
										    false -> delete1(Atom,T,Acc)
									   end.




%%
%% Local Functions
%%

reverse(List) -> reverse_list(List,[]).

reverse_list ([], Acc) -> Acc;
reverse_list ([H|T], Acc) -> reverse_list (T, [H|Acc]).

