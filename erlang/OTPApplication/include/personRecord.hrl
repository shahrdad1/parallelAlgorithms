%% File: personRecord.hrl

-record(address, {street, postalCode}).

-record(person, {name, age, phone = [], dict = [], 
				 address=#address{street="Rose Trail", postalCode="L4p3y1"}}).
