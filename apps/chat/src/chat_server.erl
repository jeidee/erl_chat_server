-module(chat_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0,
  new_user/2,
  set_nickname/2,
  del_user/1,
  msg/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users}).
-record(user, {socket, nickname}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_user(ClientPid, Socket) ->
  gen_server:cast(chat_server, {new_user, ClientPid, Socket}).

set_nickname(ClientPid, Nickname) ->
  gen_server:cast(chat_server, {set_nickname, ClientPid, Nickname}).

del_user(ClientPid) ->
  gen_server:cast(chat_server, {del_user, ClientPid}).

msg(ClientPid, Msg) ->
  gen_server:cast(chat_server, {msg, ClientPid, Msg}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #state{users = dict:new()}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({new_user, ClientPid, Socket}, State) ->
  NewUsers = dict:store(ClientPid, #user{socket = Socket}, State#state.users),
  io:format("new_user ~p ~p ~p~n", [ClientPid, Socket, dict:to_list(NewUsers)]),
  {noreply, State#state{users = NewUsers}};

handle_cast({set_nickname, ClientPid, Nickname}, State) ->
  User = dict:fetch(ClientPid, State#state.users),
  NewUsers = dict:store(ClientPid, #user{socket = User#user.socket, nickname = Nickname}, State#state.users),
  io:format("set_nickname ~p ~p ~p~n", [ClientPid, Nickname, dict:to_list(NewUsers)]),
  {noreply, State#state{users = NewUsers}};

handle_cast({del_user, ClientPid}, State) ->
  NewUsers = dict:erase(ClientPid, State#state.users),
  io:format("del_user ~p ~p~n", [ClientPid, dict:to_list(NewUsers)]),
  {noreply, State#state{users = NewUsers}};

handle_cast({msg, ClientPid, Msg}, State) ->
  io:format("msg ~p ~p ~p~n", [ClientPid, Msg, dict:to_list(State#state.users)]),
  ClientPids = dict:fetch_keys(State#state.users),
  lists:foreach(fun(TargetPid) ->
    if TargetPid /= ClientPid ->
        Target = dict:fetch(TargetPid, State#state.users),
        ToMsg = io_lib:format("[~s] ~s", [Target#user.nickname, Msg]),
        gen_tcp:send(Target#user.socket, ToMsg);
      true ->
        ok
    end
  end, ClientPids),
  {noreply, State};

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

