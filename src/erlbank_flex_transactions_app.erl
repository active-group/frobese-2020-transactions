-module(erlbank_flex_transactions_app).

-behaviour(application).

-export([start/2, stop/1]).

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_',
				       [{"/transactions/", web_frontend, index},
					{"/transactions/create", web_frontend,
					 create}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
				 [{port, 8001}],
				 #{env => #{dispatch => Dispatch}}).

start(_StartType, _StartArgs) ->
    lager:info("Starting transactions-service: ~p~n",
	       [node()]),
    start_cowboy(),
    store:init_database(),
    net_adm:ping('accounts@accounts-host'),
    Res = erlbank_flex_transactions_sup:start_link(),
    lager:info("Started transactions feed: ~p~n", [node()]),
    Res.

stop(_State) -> store:destroy_tables().

%% internal functions

