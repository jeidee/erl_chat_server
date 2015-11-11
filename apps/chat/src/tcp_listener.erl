-module(tcp_listener).

-behaviour(gen_server).

%% API Function Exports
-export([start_link/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state, {
  lsocket,  % listening socket
  server    % server pid
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Port, _Module]) ->
  %% 어플리케이션이 종료될 때, terminate/2가 호출되도록 설정
  process_flag(trap_exit, true),
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSocket} ->
      do_accept(),
      {ok, #state{lsocket = LSocket}};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({accepted, _Pid}, State = #state{}) ->
  {noreply, accept(State)};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_accept() ->
  gen_server:cast(self(), {accepted, self()}).

do_client_recv(ClientPid, Socket) ->
  gen_server:cast(ClientPid, {socket, Socket}).

accept(#state{lsocket = LSocket} = State) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  gen_tcp:send(Socket, <<"Please send your nickname...">>),
  do_accept(),
  {ok, Pid} = chat_app:start_client(),
  do_client_recv(Pid, Socket),
  State.


