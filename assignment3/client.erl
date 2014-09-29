-module(client).
-export([loop/2, initial_state/2]).

-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, _Server}) ->
  case whereis(list_to_atom(_Server)) of
    undefined -> {{error, server_not_reached, "PID not found"}, St};
    ServerPid -> case genserver:request(ServerPid, {connect, {self(), St#cl_st.nick}}) of
                   ok -> {ok, St#cl_st{serverPid = ServerPid}};
                   user_already_connected -> {{error, user_already_connected, "The user is already connected"}, St}
                 end
  end;


%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
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
loop(St, {join, _Channel}) ->
  case lists:member(_Channel, St#cl_st.chatrooms) of
    true -> {{error, user_already_joined, ""}, St};
    _ -> case St#cl_st.serverPid of
           undefined -> {{error, not_connected, "Cannot perform action"}, St};
           ServerPid -> case genserver:request(ServerPid, {joinChat, {self(), _Channel}}) of
                          ok -> {ok, St#cl_st{chatrooms = lists:append(St#cl_st.chatrooms,[_Channel])}};
                          _ -> io:fwrite("Something weird")
                        end
         end
  end;


%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
  case St#cl_st.serverPid of
    undefined -> {{error, user_not_connected, "Cannot perform action"}, St};
    ServerPid -> case genserver:request(ServerPid, {leaveChat, {self(), _Channel}}) of
                   ok -> {ok, St#cl_st{chatrooms = lists:delete(_Channel, St#cl_st.chatrooms)}};
                   _ -> {error, user_not_joined, ""}
                 end
  end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
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
