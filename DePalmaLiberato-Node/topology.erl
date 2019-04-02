-module(topology).
-export([start/0]).

sleep(N) -> receive after N*1000 -> ok end.

% lo fanno i watcher degli amici
pinger(ToPing, Handler) ->
  sleep(10),
  Ref = make_ref(),
  ToPing ! {ping, self(), Ref},
  receive
    {pong, Ref} -> pinger(ToPing, Handler)
  after 2000 -> Handler ! {dead, ToPing}
  end.

handler(ListaAmici, PidMain) ->
  NumeroAmici = length(ListaAmici),
  Ref = make_ref(),
  % controlla i messaggi da mandare, compresa la richiesta di amici
  case NumeroAmici of
    N when N = 1 or N = 2 -> hd(ListaAmici) ! {get_friends, self(), Ref};
    0 -> PidMain ! {sad},
  end,

  receive
    % gestisce la morte di un amico
    {dead, DeadFriend} -> handler(ListaAmici -- [DeadFriend], PidMain),

    % riceve la lista dopo che l'abbiamo richiesta perchè si è svuotata
    {list_from_main, ListaNuovaMain} -> %3 a caso
              Amici_only = lists:delete(PidMain, ListaNuovaMain),
              case length(Amici_only) of
                N when N >= 3 -> {A, _} = lists:split(3, Amici_only),
                                 handler(ListaAmici ++ A, PidMain)
                _ -> PidMain ! {sad},
                     handler(ListaAmici, PidMain)
              end,

    % riceve la lista degli amici di un amico per aggiungerli/o ai nostri (risposta dei nostri get_friends)
    {friends, Nonce, ListaNuova} -> %quanti ne mancano
              Amici_only = lists:delete(PidMain, ListaNuovaMain),
              case length(Amici_only) of
                N when N >= 2 ->
                  case NumeroAmici of
                    1 -> {A, _} = lists:split(2, Amici_only),
                                   handler(ListaAmici ++ A, PidMain);
                    2 -> {A, _} = lists:split(1, Amici_only),
                                   handler(ListaAmici ++ A, PidMain)
                  end,
                1 ->
                  case NumeroAmici of
                   N when N < 3 -> handler(ListaAmici ++ hd(Amici_only), PidMain)
                end,
              end,

    % manda la lista degli amici al main per rispondere agli amici che chiedono chi conosciamo
    {get_friends_from_main, PidMain} -> PidMain ! {ListaAmici, self()}

    end.

  start() -> 
