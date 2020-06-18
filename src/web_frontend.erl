-module(web_frontend).
-export([init/2]).


success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
      <p> It could take several minutes until the transaction is ready for transactions </p>
      <a href=\"/\"> Back </a>
    " >>.


form() ->
    << "
<h3> Open Account </h3>
<form method=\"post\" action=\"/transactions/open\">
  <label for=\"transactions_firstname\"> Firstname </label>
  <input type=\"text\" id=\"transactions_firstname\" name=\"transactions_firstname\" />

  <label for=\"transactions_secondname\"> Secondname </label>
  <input type=\"text\" id=\"transactions_secondname\" name=\"transactions_secondname\" />

  <input type=\"submit\" value=\"Open transaction\" />
</form>" >>.



init(Req, add) ->

    lager:info("Creating new transactions"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    Firstname = maps:get(<<"transactions_firstname">>, KeyValues),
    Secondname = maps:get(<<"transactions_secondname">>, KeyValues),

    % dispatch
    AccountNumber = 1,
    Body = io_lib:format(success(), [AccountNumber]),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    lager:info("Created account with transactions number ~p", [AccountNumber]),

    {ok, Req2, []};

init(Req0, index) ->
    ResponseBody = form(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           ResponseBody,
                           Req0),
    {ok, Req, []}.
