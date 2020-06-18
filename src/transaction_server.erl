-module(transaction_server).

-behavior(gen_server).

-include("data.hrl").

-export([transfer/3]).

-export([handle_call/3, handle_cast/2,
	 handle_continue/2, init/1, start_link/0]).

-spec transfer(account_number(), account_number(),
	       money()) -> {error,
			    amount_nil | sender_nil | receiver_nil |
			    sender_account_not_found |
			    receiver_account_not_found | insufficient_funds |
			    negative_amount | service_unavailable} |
			   {ok, erlang:timestamp()}.

transfer(Sender_account, Receiver_account, Amount) ->
    case gen_server:call({global, ?MODULE},
			 #create{transaction =
				     #transaction{sender = Sender_account,
						  receiver = Receiver_account,
						  amount = Amount}})
	of
      {reply, Reply} -> Reply;
      Error -> Error
    end.

init([]) -> {ok, maps:new(), {continue, init}}.

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [],
			  []).

handle_continue({continue, init}, State) ->
    {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_call(#create{transaction = Transaction}, _From,
	    State) ->
    Return = try handle_put(Transaction) of
	       Res -> Res
	     catch
	       Error -> {error, Error}
	     end,
    NewState = add_transaction_to_state(Transaction, State),
    {reply, Return, NewState};
handle_call(_, _From, State) ->
    {reply, {error, wrong_payload}, State}.

handle_put(Transaction) ->
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
