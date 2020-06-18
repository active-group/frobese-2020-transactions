%%%-------------------------------------------------------------------
%% @doc erlbank_flex_transactions top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_flex_transactions_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 0,
		 period => 1},
    ChildSpecs = [#{id =>
			transaction_server,       % mandatory
		    start => {transaction_server, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

