-module(chat_client).

-behaviour(gen_server).

%% API Function Exports
-export([start_link/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([recv_loop/2]).

-record(state, {
  socket,     % client socket
  nickname    % nickname
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?MODULE, [], []).
%%   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  lager:info("chat_client:init()..."),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({socket, Socket}, _State) ->
  chat_server:new_user(self(), Socket),
  spawn(chat_client, recv_loop, [Socket, self()]),
  {noreply, #state{socket = Socket}};

handle_cast({msg, Msg}, State) ->
  if
    State#state.nickname == undefined ->
      Nickname = re:replace(binary_to_list(Msg), "\r\n", "", [global, {return, list}]),
      chat_server:set_nickname(self(), Nickname),
      {noreply, State#state{nickname = Msg}};
    true ->
      chat_server:msg(self(), Msg),
      {noreply, State}
  end;

handle_cast({stop, Reason}, State) ->
  {stop, Reason, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  chat_server:del_user(self()),
  gen_tcp:close(State#state.socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

process_msg(Pid, Msg) ->
  gen_server:cast(Pid, {msg, Msg}).

do_close(Pid) ->
  gen_server:cast(Pid, {stop, {shutdown, <<"Client is closed.">>}}).

recv_loop(Socket, Pid) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Msg} ->
      process_msg(Pid, Msg),
      recv_loop(Socket, Pid);
    {error, closed} ->
      do_close(Pid),
      ok
  end.
