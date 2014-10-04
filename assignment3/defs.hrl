% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
% nick: nickname of the client
% serverPid: process Id of the client process
% chatrooms: list of Pids of Chatroom processes that the client is connected to
-record(cl_st, {gui, nick, serverPid=undefined, chatrooms=[]}).
    
% This record defines the structure of the 
% server process. 
% connectedClients: Pids of connected clients
% usedNicks: Nickname of the conncected clients
-record(server_st, {connectedClients, usedNicks}).
