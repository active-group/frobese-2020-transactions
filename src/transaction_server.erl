-module(transaction_server).

-behavior(gen_server).

-include("data.hrl").

-record(state,
	{accounts  :: #{}, pids  :: sets:set(pid())}).

%%
%% gen_server callbacks
%%

-export([handle_call/3, handle_cast/2,
	 handle_continue/2, init/1, start_link/0]).

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
    {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_call(#transfer{transaction = Transaction}, _From,
	    State) ->
    Return = try handle_transfer(Transaction, State) of
	       Res -> Res
	     catch
	       Error -> {error, Error}
	     end,
    NewState = state_add_transaction(Transaction, State),
    {reply, Return, NewState};
handle_call(#register{since = Since}, _From,
	    #state{pids = Pids} = State) ->
    %TODO monitor and deregister processes
    Transactions = store:get_all_transactions(Since),
    publish_transaction(_From, Transactions),
    {reply, ok,
     State#state{pids = sets:add_element(_From, Pids)}};
handle_call(_, _From, State) ->
    {reply, {error, wrong_payload}, State}.

%%
%% internal functions
%%

handle_transfer(Transaction,
		#state{accounts = Accounts, pids = Pids}) ->
    Timestamp = erlang:timestamp(),
    validate_transaction(Transaction, Accounts),
    save_transaction(Transaction, Timestamp),
    publish_transaction(sets:to_list(Pids), Transaction).

% handle_register ( Pid , State ) -> state_add_transactions ( [ ] , State ) -> State ; state_add_transactions ( [ Head | Tail ] , State ) -> NewState = state_add_transaction ( Head , State ) , state_add_transactions ( Tail , NewState ) .

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

state_add_transaction(#transaction{sender = Sender,
				   receiver = Receiver, amount = Amount},
		      #state{accounts = Accounts} = State) ->
    Map1 = maps:update_with(Sender,
			    fun (Old) -> Old - Amount end, Accounts),
    Map2 = maps:update_with(Receiver,
			    fun (Old) -> Old + Amount end, Map1),
    State#state{accounts = Map2}.

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
    case {maps:find(Sender, Accounts), maps:find(Receiver, Accounts)} of
        {{ok,SenderAmount}, {ok, _ReceiverAmount}} -> 
            if SenderAmount < Amount -> throw(insufficient_funds);
                true -> ok
            end;
        {error, _} -> throw(sender_account_not_found);
        {_, error} -> throw(receiver_account_not_found)
    end.
validate_transaction(_, _) -> ok.
