-module(transaction_server).

-behavior(gen_server).

-include("data.hrl").

-record(state, {accounts  :: #{}, pids  :: [pid()]}).

%%
%% gen_server callbacks
%%

-export([handle_call/3, handle_cast/2,
	 handle_continue/2, init/1, start_link/0]).

init([]) ->
    lager:info("Initializing transaction_server: ~p~n",
	       [node()]),
    {ok, #state{accounts = maps:new(), pids = []},
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
    Return = try handle_transfer(Transaction) of
	       Res -> Res
	     catch
	       Error -> {error, Error}
	     end,
    NewState = add_transaction_to_state(Transaction, State),
    {reply, Return, NewState};
handle_call(_, _From, State) ->
    {reply, {error, wrong_payload}, State}.

%%
%% internal functions
%%

handle_transfer(Transaction) ->
    Timestamp = erlang:timestamp(),
    validate_transaction(Transaction),
    save_transaction(Transaction, Timestamp).

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

update_state([], State) -> State;
update_state([Head | Tail], State) ->
    NewState = add_transaction_to_state(Head, State),
    update_state(Tail, NewState).

add_transaction_to_state(#transaction{sender = Sender,
				      receiver = Receiver, amount = Amount},
			 State) ->
    Map1 = maps:update_with(Sender,
			    fun (Old) -> Old - Amount end, State),
    maps:update_with(Receiver,
		     fun (Old) -> Old + Amount end, Map1).

% add_account_to_state(#transaction{sender})

save_transaction(Transaction, Timestamp) ->
    case
      store:put_transaction(Transaction#transaction{timestamp
							= Timestamp})
	of
      error -> throw(service_unavailable);
      _ -> {ok, Timestamp}
    end.

validate_transaction(#transaction{amount = nil}) ->
    throw(amount_nil);
validate_transaction(#transaction{sender = nil}) ->
    throw(sender_nil);
validate_transaction(#transaction{receiver = nil}) ->
    throw(receiver_nil);
validate_transaction(#transaction{amount = Amount})
    when Amount < 0 ->
    throw(negative_amount);
validate_transaction(_) -> ok.
