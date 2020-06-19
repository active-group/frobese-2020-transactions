-module(store).

-include("data.hrl").

-export([find_transactions/1, init_database/0,
	 put_transaction/1]).

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
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = nil, receiver = 2, amount = 100}),
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 1, receiver = 2, amount = 200}),
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 1, receiver = 2, amount = 400}),
    %% 1=>300   2=>1700 3=>1000
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 2, receiver = 1, amount = 300}),
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 2, receiver = 1, amount = 200}),
    %% 1=>800   2=>1200 3=>1000
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 3, receiver = 1, amount = 600}),
    put_transaction(#transaction{timestamp =
				     erlang:timestamp(),
				 sender = 3, receiver = 2, amount = 400}).

    %% 1=>400  2=>1600  3=>0

-spec put_transaction(#transaction{}) -> ok | error.

put_transaction(Transaction) ->
    Fun = fun () -> mnesia:write(Transaction) end,
    mnesia_transaction(Fun).

-spec find_transactions({account, account_number()} |
			{since, erlang:timestamp() | nill} |
			{create, account_number() | nil}) -> [#transaction{}] |
							     error.

find_transactions({account, AccountNr}) ->
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
    mnesia_transaction_filtered(Fun);
find_transactions({since, nil}) ->
    Fun = fun () ->
		  mnesia:select(transaction, [{'_', [], ['$_']}])
	  end,
    mnesia_transaction_filtered(Fun);
find_transactions({since, Since}) ->
    lists:filter(fun (#transaction{timestamp =
				       Timestamp}) ->
			 Since < Timestamp
		 end,
		 find_transactions({since, nil}));
find_transactions({create, nil}) ->
    Fun = fun () ->
		  mnesia:match_object({transaction, '_', nil, '_', '_'})
	  end,
    mnesia_transaction(Fun);
find_transactions({create, AccountNr}) ->
    Fun = fun () ->
		  mnesia:match_object({transaction, '_', nil, AccountNr,
				       '_'})
	  end,
    mnesia_transaction(Fun).

mnesia_transaction_filtered(Fun) ->
    case mnesia:transaction(Fun) of
      {atomic, Res} ->
	  lists:filter(fun (#transaction{sender = A,
					 receiver = B}) ->
			       A == nil orelse B == nil
		       end,
		       Res);
      _ -> error
    end.

mnesia_transaction(Fun) ->
    case mnesia:transaction(Fun) of
      {atomic, Res} -> Res;
      _ -> error
    end.
