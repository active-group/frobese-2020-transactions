-type account_number() :: integer().
-type money() :: number().

-record(transaction, 
    {timestamp :: erlang:timestamp(), 
     sender :: account_number(), 
     receiver :: account_number(), 
     amount :: money()}).

-record(create, {transaction  :: #transaction{}}).

-record(register, {since  :: erlang:timestamp() | nil }).