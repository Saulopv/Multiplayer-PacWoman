-module(networking).
-export([registerAsServer/2, registerAsClient/2, serverAtom/0, externalIP/0]).

% @doc Registers the given process as the server and prints out the IP adress.
registerAsServer(Pid, IP) ->
    register(serverAtom(), Pid),
    io:format("~nServer registered~nIP: ~p~n~n", [IP]).

% @doc Registers the given process as a client and returns the server Pid.
registerAsClient(Pid, ServerIP) ->
    ServerNodeAtom = getFullNodeAtom(serverAtom(), ServerIP),
    {serverAtom(), ServerNodeAtom} ! {register, Pid},
    receive {registered, ServerPid} -> ServerPid end.

% @doc Takes a node name as an atom and a host name as a string and formats it into a full name atom.
getFullNodeAtom(NodeAtom, HostName) ->
    FullName = atom_to_list(NodeAtom) ++ "@" ++ HostName,
    list_to_atom(FullName).

% @doc Returns the atom that the server is registered as.
serverAtom() -> 'pacServer'.

% @doc Returns the IP adress of the current node as a string.
% Based on answer from https://stackoverflow.com/questions/32984215/erlang-finding-my-ip-address
externalIP() ->
    {ok, Addrs} = inet:getifaddrs(),
    IP = hd([ Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
                size(Addr) == 4, Addr =/= {127,0,0,1} ]),
    inet:ntoa(IP).
