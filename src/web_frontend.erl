-module(web_frontend).

-export([init/2]).

-spec bin_to_int(binary()) -> integer().

bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

-spec error() -> binary().

error() ->
    <<"\n      <p> An error occured: ~p </p> "
      "~n\n       <a href=\"/\"> Back </a>\n "
      "   ">>.

-spec success() -> binary().

success() ->
    <<"\n      <p> Transaction with id ~p successful"
      "ly created </p> ~n\n               <a "
      "href=\"/\"> Back </a>\n    ">>.

-spec form() -> binary().

form() ->
    <<"\n<h3> Create transaction </h3>\n   "
      "                    <form method=\"post\" "
      "action=\"/transactions/create\">\n  "
      "<label for=\"transactions_from\"> From "
      "(account number) </label>\n  <input "
      "type=\"text\" id=\"transactions_from\" "
      "name=\"transactions_from\" />\n\n  <label "
      "for=\"transactions_to\"> To (account "
      "number) </label>\n  <input type=\"text\" "
      "id=\"transactions_to\" name=\"transactions_to\" "
      "/>\n\n  <label for=\"transactions_amount\"> "
      "Amount </label>\n  <input type=\"text\" "
      "id=\"transactions_amount\" name=\"transaction"
      "s_amount\" />\n\n  <input type=\"submit\" "
      "value=\"Create transaction\" />\n</form>">>.

init(Req, create) ->
    {ok, KeyValuesL, _} =
	cowboy_req:read_urlencoded_body(Req),
    KeyValues = maps:from_list(KeyValuesL),
    SenderAccountNumber =
	bin_to_int(maps:get(<<"transactions_from">>,
			    KeyValues)),
    ReceiverAccountNumber =
	bin_to_int(maps:get(<<"transactions_to">>, KeyValues)),
    Amount = bin_to_int(maps:get(<<"transactions_amount">>,
				 KeyValues)),
    Body = case
	     transaction_client:transfer(SenderAccountNumber,
					 ReceiverAccountNumber, Amount)
	       of
	     {ok, TxId} -> io_lib:format(success(), [TxId]);
	     {error, Err} -> io_lib:format(error(), [Err])
	   end,
    Req2 = cowboy_req:reply(200,
			    #{<<"content-type">> => <<"text/html">>}, Body,
			    Req),
    {ok, Req2, []};
init(Req0, index) ->
    ResponseBody = form(),
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			   ResponseBody, Req0),
    {ok, Req, []}.
