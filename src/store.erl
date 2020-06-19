-module(store).

-include("data.hrl").

-export([get_all_transactions/0, get_transactions/1,
	 init_database/0, put_transaction/1]).

init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    populate_tables(),
    ok.

destroy_tables() ->
    mnesia:del_table_copy(transaction, node()).

create_tables() ->
    mnesia:create_table(transaction,
			[{attributes, record_info(fields, transaction)}]).

populate_tables() ->
    %% 1=>1000  2=>1000 3=>1000
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 1, receiver = 2, amount=100}),
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 1, receiver = 2, amount=200}),
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 1, receiver = 2, amount=400}),
    %% 1=>300   2=>1700 3=>1000
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 2, receiver = 1, amount=300}),
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 2, receiver = 1, amount=200}),
    %% 1=>800   2=>1200 3=>1000
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 3, receiver = 1, amount=600}),
    put_transaction(#transaction{timestamp = erlang:timestamp(), sender = 3, receiver = 2, amount=400}).
    %% 1=>400  2=>1600  3=>0

-spec put_transaction(#transaction{}) -> ok | error.

put_transaction(Transaction) ->
    Fun = fun () -> mnesia:write(Transaction) end,
    mnesia_transaction(Fun).

-spec
     get_transactions(account_number()) -> [#transaction{}] |
					   error.

get_transactions(AccountNr) ->
    Fun = fun () ->
		  mnesia:select(transaction,
				[{'$1',
				  [{'orelse',
				    {'==', {element, #transaction.sender, '$1'},
				     AccountNr},
				    {'==',
				     {element, #transaction.receiver, '$1'},
				     AccountNr}}],
				  ['$_']}])
	  end,
    mnesia_transaction(Fun).

-spec get_all_transactions() -> [#transaction{}] |
				error.

get_all_transactions() ->
    Fun = fun () ->
		  mnesia:select(transaction, [{'_', [], ['$_']}])
	  end,
    mnesia_transaction(Fun).

mnesia_transaction(Fun) ->
    case mnesia:transaction(Fun) of
      {atomic, Res} -> Res;
      _ -> error
    end.
