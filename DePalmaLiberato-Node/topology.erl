-module(topology).
-export([main/0]).

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
  PidHandler = self(),
  NumeroAmici = length(ListaAmici),
  Ref = make_ref(),
  % controlla i messaggi da mandare, compresa la richiesta di amici
  case NumeroAmici of
    N when N = 1 or N = 2 -> hd(ListaAmici) ! {get_friends, self(), Ref};
    0 -> PidMain ! {sad}
  end,

  receive
    % gestisce la morte di un amico
    {dead, DeadFriend} -> handler(ListaAmici -- [DeadFriend], PidMain);

    % riceve la lista dopo che l'abbiamo richiesta perchè si è svuotata
    {list_from_main, ListaNuovaMain} -> %3 a caso
              Amici_only = lists:delete(PidMain, ListaNuovaMain),
              case length(Amici_only) of
                N when N >= 3 -> {A, _} = lists:split(3, Amici_only),
                                 lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, A),
                                 handler(ListaAmici ++ A, PidMain)
                _ -> PidMain ! {sad},
                     handler(ListaAmici, PidMain)
              end;

    % riceve la lista degli amici di un amico per aggiungerli/o ai nostri (risposta dei nostri get_friends)
    {friends, Nonce, ListaNuova} -> %quanti ne mancano
              NoDuplicate = lists:filter(fun(Elem) -> not lists:member(Elem, ListaAmici), ListaNuova),
              Amici_only = lists:delete(PidMain, NoDuplicate),
              case length(Amici_only) of
                N when N >= 2 ->
                  case NumeroAmici of
                    1 -> {A, _} = lists:split(2, Amici_only),
                                  lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, A),
                                  handler(ListaAmici ++ A, PidMain);
                    2 -> {A, _} = lists:split(1, Amici_only),
                                  lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, A),
                                  handler(ListaAmici ++ A, PidMain)
                  end,
                1 ->
                  case NumeroAmici of
                   N when N < 3 ->
                    Amico = hd(Amici_only),
                    spawn(?MODULE, pinger, [Amico, PidHandler]),
                    handler(ListaAmici ++ Amico, PidMain)
                end
              end;

    % manda la lista degli amici al main per rispondere agli amici che chiedono chi conosciamo
    {get_friends_from_main, PidMain, Mittente, Nonce} -> PidMain ! {list_from_handler, ListaAmici, Mittente, Nonce}

    end.

  main(H1) ->
    Ref = make_ref(),
    case H1 of
      false -> Handler = spawn(?MODULE, handler, [[], self()])
    end,
    receive

      % risponde ai ping di tutti
      {ping, Mittente, Nonce} -> Mittente ! {pong, Nonce},
        main(true);

      % gestiscono le richieste di lista di amici dagli amici
      {get_friends, Mittente, Nonce} -> Handler ! {get_friends_from_main, self(), Mittente, Nonce},
        main(true);
      {list_from_handler, ListaAmici, Mittente, Nonce} -> Mittente ! {friends, Nonce, ListaAmici},
        main(true);

      % gestiscono la lista che arriva dal prof
      {sad} -> teacher_node ! {get_friends, self(), Ref},
        main(true);
      {friends, Nonce, ListaAmici} -> Handler ! {list_from_main, ListaAmici},
        main(true)

      end.
