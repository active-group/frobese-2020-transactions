-module(transaction_client).

-include("data.hrl").

-export([register/1, transfer/3]).

-spec transfer(account_number(), account_number(),
	       money()) -> {error,
			    amount_nil | sender_nil | receiver_nil |
			    sender_account_not_found |
			    receiver_account_not_found | insufficient_funds |
			    negative_amount | service_unavailable} |
			   {ok, erlang:timestamp()}.

transfer(Sender_account, Receiver_account, Amount) ->
    case gen_server:call({global, transaction_server},
			 #transfer{transaction =
				       #transaction{sender = Sender_account,
						    receiver = Receiver_account,
						    amount = Amount}})
	of
      {reply, Reply} -> Reply;
      Error -> Error
    end.

-spec register(erlang:timestamp() | nil) -> ok |
					    {error, reason}.

register(Timestamp) ->
    case gen_server:call({global, transaction_server},
			 #register{since = Timestamp})
	of
      {reply, Reply} -> Reply;
      Error -> Error
    end.
