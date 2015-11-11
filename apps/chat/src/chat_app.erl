-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor behaviour callbacks
-export([init/1]).

%% API Function Exports
-export([start_client/0]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(DEF_PORT, 12345).

%% -compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_client() ->
  supervisor:start_child(tcp_client_sup, []).

%% ------------------------------------------------------------------
%% Application callbacks
%% ------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  lager:start(),
  lager:info("hello"),
  ListenPort = get_app_env(listen_port, ?DEF_PORT),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, chat_client]).

stop(_State) ->
  ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

init([Port, Module]) ->
  {ok,
    {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
        % chat server
        {chat_server_sup,
          {chat_server, start_link, []},
          permanent,
          2000,
          worker,
          [chat_server]
        },
        % tcp listener
        {tcp_server_sup,
          {tcp_listener, start_link, [Port, Module]},
          permanent,
          2000,
          worker,
          [tcp_listener]
        },
        % chat client
        % init([Module]) 함수를 반드시 정의해야 한다
        % init([Module]) 함수는 tcp_client_sup 프로세스에 의해 콜백된다
        {tcp_client_sup,
          {supervisor, start_link, [{local, tcp_client_sup}, ?MODULE, [Module]]},
          permanent,
          infinity,
          supervisor,
          []
        }
      ]
    }
  };
init([Module]) ->
  {ok,
    {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
        % TCP Client
        {undefined,                               % Id       = internal id
          {Module,start_link,[]},                  % StartFun = {M, F, A}
          temporary,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          []                                       % Modules  = [Module] | dynamic
        }
      ]
    }
  }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

get_app_env(Opt, Default) ->
  case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
      case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error -> Default
      end
  end.
