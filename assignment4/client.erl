-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").
-import(net_kernel, [connect_node/1]).


%%%%%%%%%%%%%%%
%%%% Connect Locally
%%%%%%%%%%%%%%%
% Function that handles the connection to a local server instance.
connectLocally(St, _Server) ->
  case whereis(list_to_atom(_Server)) of
    undefined -> {{error, server_not_reached, "PID not found"}, St};
    ServerPid -> case genserver:request(ServerPid, {connect, {self(), St#cl_st.nick}}) of
                   ok -> {ok, St#cl_st{serverPid = ServerPid}};
                   user_already_connected -> {{error, user_already_connected, "The user is already connected"}, St}
                 end
  end.
%%%%%%%%%%%%%%%
%%%% Connect Remotely
%%%%%%%%%%%%%%%
% This function connects the client to a remote server. The only difference from the one above is that instead the pid
% we also save the machine as a tuple.
connectRemotely(St, {_Server, Machine}) ->
  case connect_node(list_to_atom(Machine)) of
    true ->
      case genserver:request({list_to_atom(_Server), list_to_atom(Machine)}, {connect, {self(), St#cl_st.nick}}) of
        ok -> {ok, St#cl_st{serverPid = {list_to_atom(_Server), list_to_atom(Machine)}}};
        user_already_connected -> {{error, user_already_connected, "The user is already connected"}, St}
      end;
    false -> {{error, server_not_reached, "PID or Remote machine not found"}, St}
  end.


%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
% Function that according to the parameters connect the client locally or remotely
loop(St, {connect, Parameters}) ->
  case Parameters of
    {Server, Machine} -> connectRemotely(St, {Server, Machine});
    Server -> connectLocally(St, Server)
  end;


%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
% Sends a request containing clientPid and nick to the genserver that a client wants to disconnect from the server.
% If the user is not connected; error, user_not_connected will be returned.
% If the user is connected to a channel; error, leave_channel_first, the old state will be returned.
% If the disconnect gets ok from the server; ok, updated client state will be returned. 
loop(St, disconnect) ->
  case St#cl_st.chatrooms == [] of
    true -> case St#cl_st.serverPid of
              undefined -> {{error, user_not_connected, "Cannot perform action"}, St};
              ServerPid -> case genserver:request(ServerPid, {disconnect, {self(), St#cl_st.nick}}) of
                             ok -> {ok, St#cl_st{serverPid = undefined}};
                             _ -> io:fwrite("Something weird")
                           end
            end;
    _ -> {{error, leave_channels_first, ""}, St}
  end;


%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
% Sends a request containing clientPid and channel name to the genserver that a client wants to join a channel.
% If the user has already joined the channel; error, user_already_joined, the old state will be returned.
% If the user is not connected to a server; error, user_not_connected, the old state will be returned.
% If the join gets ok from the server; ok, updated client state will be returned. 
loop(St, {join, _Channel}) ->
  case lists:member(_Channel, St#cl_st.chatrooms) of
    true -> {{error, user_already_joined, ""}, St};
    _ -> case St#cl_st.serverPid of
           undefined -> {{error, not_connected, "Cannot perform action"}, St};
           ServerPid -> case genserver:request(ServerPid, {joinChat, {self(), _Channel}}) of
                          ok -> {ok, St#cl_st{chatrooms = lists:append(St#cl_st.chatrooms, [_Channel])}};
                          _ -> io:fwrite("Something weird")
                        end
         end
  end;


%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
% Sends a request containing clientPid and channel name to the genserver that a client wants to leave a channel.
% If the user is not in the channel; error, user_not_joined, the old state will be returned.
% If the user is not connected to a server; error, user_not_connected, the old state will be returned.
% If the join gets ok from the server; ok, updated client state will be returned.
loop(St, {leave, _Channel}) ->
  case lists:member(_Channel, St#cl_st.chatrooms) of
    true -> case St#cl_st.serverPid of
              undefined -> {{error, user_not_connected, "Cannot perform action"}, St};
              ServerPid -> case genserver:request(ServerPid, {leaveChat, {self(), _Channel}}) of
                             ok -> {ok, St#cl_st{chatrooms = lists:delete(_Channel, St#cl_st.chatrooms)}};
                             _ -> {error, user_not_joined, ""}
                           end
            end;
    _ -> {{error, user_not_joined, "Not connected to chat"}, St}
  end;


%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
% Sends a request containing clientPid, channel, message and nick to the genserver that a msg should be sent.
% If the user is not in the channel; error, user_not_joined, the old state will be returned.
% If the user is not connected to a server; error, not_connected, the old state will be returned.
% If the join gets ok from the server; ok, the old state.
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
  case lists:member(_Channel, St#cl_st.chatrooms) of
    true -> case St#cl_st.serverPid of
              undefined -> {{error, not_connected, "Cannot perform action"}, St};
              ServerPid -> case genserver:request(ServerPid, {msg, {self(), _Channel, _Msg, St#cl_st.nick}}) of
                             ok -> {ok, St};
                             _ -> io:fwrite("Something weird")
                           end
            end;
    _ -> {{error, user_not_joined, ""}, St}
  end;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
  {St#cl_st.nick, St};

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St, {nick, _Nick}) ->
  {ok, St#cl_st{nick = _Nick}};

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
  {St, St};

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st{gui = GUIName}, _MsgFromClient) ->
  {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name ++ "> " ++ Msg}),
  {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
  case _MsgFromClient of
    {_, {Channel, Nick, Msg}} -> {Channel, Nick, Msg}
  end.


initial_state(Nick, GUIName) ->
  #cl_st{nick = Nick, gui = GUIName, serverPid = undefined}.
