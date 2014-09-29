-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, _Msg) ->
 case _Msg of
    {connect, Pid} -> connect(St, Pid);
    {disconnect, Pid} -> disconnect(St, Pid)
 end.


connect(St, Pid) ->
  OldDict = St#server_st.connectedClients,
  NewState = St#server_st{ connectedClients = OldDict:append(Pid, [])},
  debug_dictionary(NewState),
  Pid ! ok,
  {ok, NewState}.

disconnect(St, Pid) ->
  St.




initial_state(_Server) ->
    #server_st{connectedClients = dict:new()}.


debug_dictionary(Dict) ->
  io:format("~p~n",[Dict]).