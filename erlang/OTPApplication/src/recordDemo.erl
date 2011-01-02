%% Author: Shahrdad
%% Created: Feb 7, 2010
%% Description: TODO: Add description to recordDemo
-module(recordDemo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([copyEmployee1/2,copyEmployee/2, addEmployee/1]).

-record(employee,	{
					name="shahrdad", 
					address="#44-8T Dr.", 
					phone=[
							{home,"905-737-7273"},
							{cell, 905-905-9090}
						  ]
					}
		).
			
-record(product,	{
					id="234-232435455",
					name="Baby Killer",
					introductionDate="12/12/2010"
					}
		).
%%
%% API Functions
%%

%%
%% TODO: Add description of extractRecord/function_arity
%%

%%
%% TODO: Add description of copyEmployee/function_arity
%%
copyEmployee(Name, Record) when is_record(Record, employee) -> Record#employee{name=Name}.

copyEmployee1(NewName, #employee{name=Name,address=Address, phone=PhoneList}=Record) when is_record(Record, employee) -> 
	Record#employee{name=NewName, address=Address,phone=PhoneList}.
%%
%% TODO: Add description of addRecord/function_arity
%%
addEmployee({Name, Address , PhoneList}) when is_list(PhoneList)-> #employee{name=Name,address=Address,phone=PhoneList}.



%%
%% Local Functions
%%

