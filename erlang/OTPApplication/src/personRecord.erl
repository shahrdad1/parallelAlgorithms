%% file_comment
-module(personRecord).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all). % For test purposes only.

-include("personRecord.hrl").

%%
%% API Functions
%%
demo ()	->	P = create_record(),
			print_record(P),
			io:format("record size: ~p~n",[record_info(size,person)]),
			io:format("record fields: ~p~n",[record_info(fields,person)]),
		   	P1 = update_record(P),
		   	print_record1(P1).
		   		
		

%% Create a Reecord
create_record() -> 
	#person{name = "Sahahrdad",age = 44, 
           	dict = [{computer_knowledge, excellent},{drinks, coke}],
			address = #address{street="Mesonove",postalCode="Q3W4E1"}
			}.


update_record(Record) -> 
	Record#person{	name = "Parivash",
					age = 45,
					phone = ["416-614-2345","905-808-9090"],
					dict=[{job, hair_dresser},{cookingSkills, good}],
					address = #address{street="St. Cathrine",postalCode="R3W3E3"}
				}.	

			



print_record(#person{name=Name, age=Age, phone=PhoneList , dict=DictList, address=AddressRecord}) -> 
					io:format("Name: ~p Age: ~p phone: ~p Dict: ~p Address: ~p ~n", 
							  [Name, Age, PhoneList , DictList, AddressRecord]).


print_record1(Record) when is_record(Record, person) -> 
						io:format("Name: ~p ~n", [Record#person.name]),
						io:format("Age: ~p ~n", [Record#person.age]),
						io:format("phone: ~p ~n", [Record#person.phone]),
						io:format("Dict: ~p ~n", [Record#person.dict]),
						io:format("Address: ~p ~n", [Record#person.address]).








%%
%% Local Functions
%%

print_path() ->
	io:format("Current path:~p~n",[c:pwd()]).
