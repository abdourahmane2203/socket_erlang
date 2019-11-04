-module(test).
-export([run/1,worker/2,client_listener/2]).

client_listener(Client,Sock) ->
  case gen_tcp:recv(Sock,0) of
    {ok,Pack} ->
      Client ! {sendmsg,Pack},
      client_listener(Client,Sock);
    Other ->
      Other
  end.

client_loop(Server,Nick,Sock) ->
  receive
    {sendmsg,Msg} ->
      Server ! {broadcast,self(),lists:duplicate(13,$\s) ++ "\r" ++ Nick ++ ":" ++ Msg};
    {recvmsg,Msg} ->
      ok = gen_tcp:send(Sock,"\r" ++ Msg)
  end,
  ok = gen_tcp:send(Sock,Nick ++ ":"),
  client_loop(Server,Nick,Sock).

get_nick(Server,Sock) ->
  {ok,Pack} = gen_tcp:recv(Sock,7),
  
  Server ! {check_nick,self(),Pack},
  receive
    collision ->
      ok = gen_tcp:send(Sock,"Deja utilise:"),

      get_nick(Server,Sock);
    usable ->
      io:format(Pack),
      Pack
  end.

client_init(Server,Sock) ->
  ok = gen_tcp:send(Sock,"Bienvenu dans minimal chat avec erlang\r\n"),
  ok = gen_tcp:send(Sock,"Entrer un identifiant:\r\n"),
  Nick = get_nick(Server,Sock),

  ok = gen_tcp:send(Sock,Nick ++":\r\n"),

  Server ! {ready,self(),Nick},

  spawn(?MODULE,client_listener,[self(),Sock]),
  client_loop(Server,Nick,Sock).

worker(Server,LSock) ->
  case gen_tcp:accept(LSock) of
    {ok,CSock} ->
      Server ! new_worker,
      client_init(Server,CSock);
    Other ->
      Other
  end.

loop(LSock,Clients,Nicks) ->
  receive
    {ready,From,Nick} ->
      loop(LSock,[From|Clients],sets:add_element(Nick,Nicks));
    new_worker ->
      spawn(?MODULE,worker,[self(),LSock]),
      loop(LSock,Clients,Nicks);
    {check_nick,From,Nick} ->
      case sets:is_element(Nick,Nicks) of
        true ->
          From ! collision;
        false ->
          From ! usable
      end,
      loop(LSock,Clients,Nicks);
    {broadcast,From,Msg} ->
      BroadList = lists:filter(fun(Client) ->
        Client =/= From
                               end,
        Clients),
      lists:map(fun(Client) ->
        Client ! {recvmsg,Msg}
                end,
        BroadList),
      loop(LSock,Clients,Nicks)
  end.

run(Port) ->
  {ok,LSock} = gen_tcp:listen(Port,[{reuseaddr,true},
    {packet,0},{active,false}]),
  spawn(?MODULE,worker,[self(),LSock]),
  loop(LSock,[],sets:new()).