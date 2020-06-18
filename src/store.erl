-module(store).

-include("data.hrl").

-export([get_all_transactions/0, get_transactions/1,
	 init_database/0, put_transaction/1]).

init_database() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    destroy_tables(),
    create_tables(),
    ok.

destroy_tables() ->
    mnesia:del_table_copy(transaction, node()).

create_tables() ->
    mnesia:create_table(transaction,
			[{attributes, record_info(fields, transaction)}]).

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
