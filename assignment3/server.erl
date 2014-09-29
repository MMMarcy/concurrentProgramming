-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
  case _Msg of
    {connect, {Pid, Nick}} -> connect(St, Pid, Nick);
    {disconnect, {Pid, Nick}} -> disconnect(St, Pid, Nick);
    {joinChat, {Pid, Chat}} -> joinChannel(St, Pid, Chat);
    {leaveChat, {Pid, Chat}} -> leaveChat(St, Pid, Chat);
    {msg, {Pid, Channel, Message}} -> forwardMessage(St, Pid, Channel, Message)
  end.


connect(St, Pid, Nick) ->
  UsedPids = St#server_st.connectedClients,
  UsedNicks = St#server_st.usedNicks,
  case UsedPids:find(Pid) of
    error -> case UsedNicks:find(Nick) of
               error ->
                 io:fwrite("Pid and Nick not found\n"),
                 {ok, St#server_st{connectedClients = UsedPids:append(Pid, ""), usedNicks = UsedNicks:append(Nick, "")}};
               _ ->
                 io:fwrite("Pid and not found, nick found\n"),
                 {user_already_connected, St}
             end;
    _ ->
      io:fwrite("Pid found\n"),
      {user_already_connected, St}
  end.


disconnect(St, Pid, Nick) ->
  UsedPids = St#server_st.connectedClients,
  UsedNicks = St#server_st.usedNicks,
  {ok, St#server_st{usedNicks = UsedNicks:erase(Nick), connectedClients = UsedPids:erase(Pid)}}.


joinChannel(St, Pid, Chat) ->
  case whereis(list_to_atom(Chat)) of
    undefined -> ChatPid = spawn(fun() -> spawnChatLoop() end),
      register(list_to_atom(Chat), ChatPid),
      ChatPid ! {join, Pid};
    ChatPid -> ChatPid ! {join, Pid}
  end,
  {ok, St}
.

spawnChatLoop() ->
  chatLoop([]).

chatLoop(St) ->
  receive
    {join, Pid} -> case lists:member(Pid, St) of
                     false -> chatLoop(lists:append(St, [Pid]));
                     _ -> chatLoop(St)
                   end;
    {leave, Pid} -> chatLoop(lists:delete(Pid, St));
    {msg, Pid, Msg} -> lists:delete(Pid, St)
  end.


leaveChat(St, Pid, Chat) ->
  case whereis(list_to_atom(Chat)) of
    ChatPid -> ChatPid ! {leave, Pid}
  end,
  {ok, St}.

forwardMessage(St, Pid, Channel, Msg) ->
  case whereis(list_to_atom(Channel)) of
    ChatPid -> ChatPid ! {msg, Pid, Msg}
  end,
  {ok, St}


initial_state(_Server) ->
  #server_st{connectedClients = dict:new(), usedNicks = dict:new()}.


debug_dictionary(Dict) ->
  io:format("~p~n", [Dict]).

debug_list(List) ->
  Printer = fun(E) -> io:format("E: ~p~n", [E]) end,
  lists:foreach(Printer, List).