-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
  case _Msg of
    {connect, {Pid, Nick}} -> connect(St, Pid, Nick);
    {disconnect, {Pid, Nick}} -> disconnect(St, Pid, Nick);
    {joinChat, {Pid, Chat}} -> joinChannel(St, Pid, Chat);
    {leaveChat, {Pid, Chat}} -> leaveChat(St, Pid, Chat);
    {msg, {Pid, Channel, Message, Nick}} -> forwardMessage(St, Pid, Channel, Message, Nick)
  end.


connect(St, Pid, Nick) ->
  UsedPids = St#server_st.connectedClients,
  UsedNicks = St#server_st.usedNicks,
  case UsedPids:find(Pid) of
    error -> case UsedNicks:find(Nick) of
               error ->
                 {ok, St#server_st{connectedClients = UsedPids:append(Pid, ""), usedNicks = UsedNicks:append(Nick, "")}};
               _ ->
                 {user_already_connected, St}
             end;
    _ ->
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
    {join, ClientPid} -> case lists:member(ClientPid, St) of
                           false -> chatLoop(lists:append(St, [ClientPid]));
                           _ -> chatLoop(St)
                         end;
    {leave, ClientPid, ServerPid} -> case lists:member(ClientPid, St) of
                                       true -> genserver:request(ServerPid, ok),
                                         chatLoop(lists:delete(ClientPid, St));
                                       _ -> genserver:request(ServerPid, user_not_joined)
                                     end;

    {msg, Channel, Pid, Msg, Nick} -> OtherClients = St,%lists:delete(Pid, St),
      SendMessage = fun(ClientPid) -> genserver:request(ClientPid, {msg, {Channel, Nick, Msg}}) end,
      lists:foreach(SendMessage, OtherClients),
      chatLoop(St)
  end.


leaveChat(St, Pid, Chat) ->
  case whereis(list_to_atom(Chat)) of
    ChatPid -> genserver:request(ChatPid, {leave, Pid, self()})
  end,
  receive
    Result -> {Result, St}
  end.

forwardMessage(St, Pid, Channel, Msg, Nick) ->
  case whereis(list_to_atom(Channel)) of
    ChatPid -> ChatPid ! {msg, Channel, Pid, Msg, Nick}
  end,
  {ok, St}.


initial_state(_Server) ->
  #server_st{connectedClients = dict:new(), usedNicks = dict:new()}.


