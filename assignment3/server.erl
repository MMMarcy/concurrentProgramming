-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

% Main loop for the server
loop(St, _Msg) ->
  case _Msg of
    {connect, {Pid, Nick}} -> connect(St, Pid, Nick);
    {disconnect, {Pid, Nick}} -> disconnect(St, Pid, Nick);
    {joinChat, {Pid, Chat}} -> joinChannel(St, Pid, Chat);
    {leaveChat, {Pid, Chat}} -> leaveChat(St, Pid, Chat);
    {msg, {Pid, Channel, Message, Nick}} -> forwardMessage(St, Pid, Channel, Message, Nick)
  end.

% If a client wants to connect to the server.
% First we check if the the client(PID) is already connected, 
% or if the username is already connected. If it is already conencted, we return user_already_connected and the 
% old state because nothing has changed. If the client/nick is not connected, we send back an ok and the new state which
% has appended the newly connected client(PID) and the nick of the client.
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

% If a client wants to disconnect, we return ok and the new state which as 
% deleted the client and nick from the state.
disconnect(St, Pid, Nick) ->
  UsedPids = St#server_st.connectedClients,
  UsedNicks = St#server_st.usedNicks,
  {ok, St#server_st{usedNicks = UsedNicks:erase(Nick), connectedClients = UsedPids:erase(Pid)}}.

% If a clients wants to join a channel.
% If the channel already exists, we send a message to that channel process that a user wants join.
% If the channel does not exists, we spawn a new process for it, and then sends a message that a user wants to join. 
joinChannel(St, Pid, Chat) ->
  case whereis(list_to_atom(Chat)) of
    undefined -> ChatPid = spawn(fun() -> spawnChatLoop() end),
      register(list_to_atom(Chat), ChatPid),
      ChatPid ! {join, Pid};
    ChatPid -> ChatPid ! {join, Pid}
  end,
  {ok, St}
.

% To initialise an "empty" channel
spawnChatLoop() ->
  chatLoop([]).

% The chat room process, where it recieves messages about users that wants to join/leave and messages.
chatLoop(St) ->
  receive
    % The client wants to join the channel, if it is already joined, we do nothing and return to the receive.
    % If it is not already join, we append the client to the state and return the new state to chatLoop
    {join, ClientPid} -> case lists:member(ClientPid, St) of
                           false -> chatLoop(lists:append(St, [ClientPid]));
                           _ -> chatLoop(St)
                         end;
    % The client wants to leave the channel, if it is not in the channel we return true, otherwise we update the state
    % and goes back to chatLoop.
    {leave, ClientPid} -> case lists:member(ClientPid, St) of
                                       true -> chatLoop(lists:delete(ClientPid, St));
                                       _ -> true
                                     end;
    % The channel received a msg. 
    % We filter away the author(client) of the message and sends it to all others clients in the chatroom.
    {msg, Channel, ClientPid, Msg, Nick} -> OtherClients = lists:delete(ClientPid, St),
      SendMessage = fun(OtherClientPid) -> genserver:request(OtherClientPid, {msg, {Channel, Nick, Msg}}) end,
      lists:foreach(SendMessage, OtherClients),
      chatLoop(St)
  end.

% If a client(Pid) wants to leave a channel(Chat), we send that channel process a message 
% that the client wants to leave
leaveChat(St, Pid, Chat) ->
  case whereis(list_to_atom(Chat)) of
    ChatPid -> ChatPid ! {leave, Pid}
  end,
  {ok, St}.

% An chatmessage shall be sent to the channel process.
forwardMessage(St, Pid, Channel, Msg, Nick) ->
  case whereis(list_to_atom(Channel)) of
    ChatPid -> ChatPid ! {msg, Channel, Pid, Msg, Nick}
  end,
  {ok, St}.

% Initiated with and empty dict of connectClients which will contain Pid of all clients connected.
% and an empty dict of usedNicks which will contain nick of all connected users.
initial_state(_Server) ->
  #server_st{connectedClients = dict:new(), usedNicks = dict:new()}.


