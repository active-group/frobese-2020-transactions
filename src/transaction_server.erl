-module(transaction_server).

-behavior(gen_server).

-include("data.hrl").

-record(state,
	{accounts  :: #{}, pids  :: sets:set(pid())}).

%%
%% gen_server callbacks
%%

-export([add_account/2, handle_call/3, handle_cast/2,
	 handle_continue/2, handle_info/2, init/1,
	 start_link/0]).

init([]) ->
    lager:info("Initializing transaction_server: ~p~n",
	       [node()]),
    {ok, #state{accounts = maps:new(), pids = sets:new()},
     {continue, init}}.

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [],
			  []).

handle_continue(init, State) ->
    lager:info("Started transaction_server: ~p:~p~n",
	       [node(), self()]),
    Accounts = init_accounts(),
    {noreply, State#state{accounts = Accounts},
     {continue, accounts}};
handle_continue(accounts, State) ->
    lager:info("Refreshing accounts: ~p:~p~n",
	       [node(), self()]),
    gen_server:cast({global, accounts},
		    {register, node(), self()}),
    gen_server:cast({global, accounts},
		    {replay, node(), self()}),
    erlang:send_after(10000, self(), {refresh, accounts}),
    {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_call(#transfer{transaction = Transaction}, _From,
	    State) ->
    try transfer(Transaction, State) of
      Res ->
	  NewState = state_add_transaction(Transaction, State),
	  lager:info("State after transfer: ~p~n", [NewState]),
	  {reply, Res, NewState}
    catch
      Error -> {reply, {error, Error}, State}
    end;
handle_call(#register{since = Since}, _From,
	    #state{pids = Pids} = State) ->
    %TODO monitor and deregister processes
    Transactions = store:find_transactions({since, Since}),
    publish_transaction(_From, Transactions),
    {reply, ok,
     State#state{pids = sets:add_element(_From, Pids)}};
handle_call(_, _From, State) ->
    {reply, {error, wrong_payload}, State}.

handle_info({new,
	     #{account_number := Account, amount := Amount}},
	    #state{accounts = Accounts} = State) ->
    NewAccounts = add_account(#transaction{sender = nil,
					   receiver = Account, amount = Amount},
			      Accounts),
    {noreply, State#state{accounts = NewAccounts}};
handle_info({replay, List},
	    #state{accounts = Accounts} = State) ->
    NewAccounts = lists:foldl(fun (#{account_number :=
					 Account,
				     amount := Amount},
				   Acc) ->
				      add_account(#transaction{sender = nil,
							       receiver =
								   Account,
							       amount = Amount},
						  Acc)
			      end,
			      Accounts, List),
    {noreply, State#state{accounts = NewAccounts}};
handle_info({refresh, accounts}, State) ->
    {noreply, State, {continue, accounts}};
handle_info(_, State) -> {noreply, State}.

%%
%% internal functions
%%

init_accounts() ->
    Initial = store:find_transactions({create, nil}),
    lager:info("Initial: ~p~n", [Initial]),
    lists:foldl(fun (#transaction{} = Transaction, Acc) ->
			add_account(Transaction, Acc)
		end,
		maps:new(), Initial).

add_account(#transaction{sender = nil,
			 receiver = Receiver, amount = Amount} =
		Transaction,
	    Accounts) ->
    case maps:is_key(Receiver, Accounts) of
      true -> Accounts;
      false ->
	  List = store:find_transactions({account, Receiver}),
	  NewAccounts = maps:put(Receiver, 0, Accounts),
	  update_account([Transaction | List], Receiver,
			 NewAccounts)
    end.

update_account([#transaction{sender = Sender,
			     amount = Amount}
		| Tail],
	       Account, Accounts)
    when Account == Sender ->
    NewAccounts = maps:update_with(Account,
				   fun (Old) -> Old - Amount end, Accounts),
    update_account(Tail, Account, NewAccounts);
update_account([#transaction{receiver = Receiver,
			     amount = Amount}
		| Tail],
	       Account, Accounts)
    when Account == Receiver ->
    NewAccounts = maps:update_with(Account,
				   fun (Old) -> Old + Amount end, Accounts),
    update_account(Tail, Account, NewAccounts);
update_account([], Account, Accounts) -> Accounts.

transfer(Transaction,
	 #state{accounts = Accounts, pids = Pids}) ->
    Timestamp = erlang:timestamp(),
    validate_transaction(Transaction, Accounts),
    save_transaction(Transaction, Timestamp),
    publish_transaction(sets:to_list(Pids), Transaction).

state_add_account(Account, Amount,
		  #state{accounts = Accounts} = State) ->
    Updated = maps:put(Account, Amount, Accounts),
    % store:get_transactions(Account)
    {noreply, State#state{accounts = Updated}}.

state_add_transaction(#transaction{sender = Sender,
				   receiver = Receiver, amount = Amount},
		      #state{accounts = Accounts} = State) ->
    Map1 = maps:update_with(Sender,
			    fun (Old) -> Old - Amount end, Accounts),
    Map2 = maps:update_with(Receiver,
			    fun (Old) -> Old + Amount end, Map1),
    State#state{accounts = Map2}.

% init_state(Transactions) ->
%     BlankState = lists:foldl(fun (#transaction{sender =
% 						   Sender,
% 					       receiver = Receiver},
% 				  Map) ->
% 				     Map1 = maps:put(Sender, 0, Map),
% 				     maps:put(Receiver, 0, Map1)
% 			     end,
% 			     maps:new(), Transactions),
%     update_state(Transactions, BlankState);
% init_state(_) -> maps:new().

publish_transaction(Pids,
		    #transaction{} = Transaction) ->
    publish_transaction(Pids, [Transaction]);
publish_transaction(Pids, [Head | Tail]) ->
    publish:as_cast(Pids, {transaction, Head}),
    publish_transaction(Pids, Tail);
publish_transaction(_Pids, []) -> ok.

save_transaction(Transaction, Timestamp) ->
    case
      store:put_transaction(Transaction#transaction{timestamp
							= Timestamp})
	of
      error -> throw(service_unavailable);
      _ -> {ok, Timestamp}
    end.

validate_transaction(#transaction{amount = nil},
		     _Accounts) ->
    throw(amount_nil);
validate_transaction(#transaction{sender = nil},
		     _Accounts) ->
    throw(sender_nil);
validate_transaction(#transaction{receiver = nil},
		     _Accounts) ->
    throw(receiver_nil);
validate_transaction(#transaction{amount = Amount},
		     _Accounts)
    when Amount < 0 ->
    throw(negative_amount);
validate_transaction(#transaction{sender = Sender,
				  receiver = Receiver, amount = Amount},
		     Accounts) ->
    case {maps:find(Sender, Accounts),
	  maps:find(Receiver, Accounts)}
	of
      {{ok, SenderAmount}, {ok, _ReceiverAmount}} ->
	  if SenderAmount < Amount -> throw(insufficient_funds);
	     true -> ok
	  end;
      {error, _} -> throw(sender_account_not_found);
      {_, error} -> throw(receiver_account_not_found)
    end;
validate_transaction(_, _) -> ok.
