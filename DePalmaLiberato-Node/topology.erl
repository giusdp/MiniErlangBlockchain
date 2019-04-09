-module(topology).
-export([main/2, test/0, handler/3, pinger/2, counter_tries/2]).

sleep(N) -> receive after N*1000 -> ok end.

% lo fanno i watcher degli amici
pinger(ToPing, Handler) ->
  sleep(10),
  %io:format("DPL: Pinging ~p...~n", [ToPing]),
  Ref = make_ref(),
  ToPing ! {ping, self(), Ref},
  receive
    {pong, Ref} -> pinger(ToPing, Handler)
  after 2000 -> Handler ! {dead, ToPing}
  end.

counter_tries(Counter, PidHandler) ->
  receive
    {one_more_time} ->
          case Counter of
            10 -> PidHandler ! {bored};
            _ -> counter_tries(Counter + 1, PidHandler)
          end
  end.

% ultimo attore da spawnare
handler(ListaAmici, PidMain, PidCounter) ->
  sleep(2),
  PidHandler = self(),
  case PidMain of
    none ->
      depalma_liberato ! {give_me_pid_final},
      receive
        {here_pid, PidM} -> io:format("DPL: Pid del main ricevuto: ~p~n", [PidM]),
                            handler(ListaAmici, PidM, spawn(?MODULE, counter_tries, [0, PidHandler]))
      end;
    _ ->
        io:format("DPL: Friends: ~p~n", [ListaAmici]),
        NumeroAmici = length(ListaAmici),
        Ref = make_ref(),
        % controlla i messaggi da mandare, compresa la richiesta di amici
        case NumeroAmici of
          0 -> PidMain ! {sad};
          1 -> hd(ListaAmici) ! {get_friends, PidHandler, Ref};
          2 -> Node = take_one_random(ListaAmici), io:format("DPL: Ho preso a caso: ~p~n", [Node]), Node ! {get_friends, PidHandler, Ref};
          _ -> ok
        end,
        receive

          {bored} ->  handler([], PidMain, spawn(?MODULE, counter_tries, [0, PidHandler]));
          % gestisce la morte di un amico

          {dead, DeadFriend} -> io:format("DPL: friend died: ~p~n", [DeadFriend]), handler(ListaAmici -- [DeadFriend], PidMain, PidCounter);

          % riceve la lista dopo che l'abbiamo richiesta perchè si è svuotata
          {list_from_main, ListaNuovaMain} -> %3 a caso
                    Amici_only = lists:delete(PidMain, ListaNuovaMain),
                    case length(Amici_only) of
                      N when N >= 3 -> RandomAmici = take_random(3, Amici_only),
                                      lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, RandomAmici),
                                      PidMain ! {update_friends, RandomAmici},
                                      handler(RandomAmici, PidMain, PidCounter);
                      _ -> handler(ListaAmici, PidMain, PidCounter)
                    end;

          % riceve la lista degli amici di un amico per aggiungerli/o ai nostri (risposta dei nostri get_friends)
          {friends, Nonce, ListaNuova} -> %quanti ne mancano
                    NoDuplicates = lists:filter(fun(Elem) -> not lists:member(Elem, ListaAmici) end, ListaNuova),
                    Amici_only = lists:delete(PidMain, NoDuplicates),
                    case length(Amici_only) of
                      N when N >= 2 ->
                        case NumeroAmici of
                          1 ->  RandomAmici = take_random(2, Amici_only),
                                lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, RandomAmici),
                                PidMain ! {update_friends, ListaAmici ++ RandomAmici},
                                handler(ListaAmici ++ RandomAmici, PidMain, PidCounter);
                          2 ->  RandomAmici = take_random(1, Amici_only),
                                lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, RandomAmici),
                                PidMain ! {update_friends, ListaAmici ++ RandomAmici},
                                handler(ListaAmici ++ RandomAmici, PidMain, PidCounter);
                          _ -> handler(ListaAmici, PidMain, PidCounter)
                        end;
                      1 ->
                        case NumeroAmici of
                          N when N < 3 ->
                            Amico = take_one_random(Amici_only),
                            spawn(?MODULE, pinger, [Amico, PidHandler]),
                            PidMain ! {update_friends, [Amico | ListaAmici]},
                            handler([Amico | ListaAmici], PidMain, PidCounter);
                          _ -> handler(ListaAmici, PidMain, PidCounter)
                        end;
                      0 -> PidCounter ! {one_more_time},
                          handler(ListaAmici, PidMain, PidCounter)
                    end;

          % manda la lista degli amici al main per rispondere agli amici che chiedono chi conosciamo
          {get_friends_from_main, Mittente, Nonce} -> PidMain ! {list_from_handler, ListaAmici, Mittente, Nonce},
                                                      handler(ListaAmici, PidMain, PidCounter)
      end
  end.

take_random(N, NodesList) ->
  case N of
    1 -> [take_one_random(NodesList)];
    2 -> First = take_one_random(NodesList),
        NoFsList = lists:delete(First, NodesList),
        Second = take_one_random(NoFsList),
        [First, Second];
    3 -> First = take_one_random(NodesList),
        NoFsList = lists:delete(First, NodesList),
        Second = take_one_random(NoFsList),
        NoSndList = lists:delete(Second, NoFsList),
        Third = take_one_random(NoSndList),
        [First, Second, Third]
  end.

take_one_random(NodesList) ->
  lists:nth(rand:uniform(length(NodesList)), NodesList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% End topology - begin gossiping %%%%%%%%
trans_handler(PidMain, ListaAmici, TransList) ->
  sleep(2),
  case PidMain of
    none ->
      depalma_liberato ! {give_me_pid, self()},
      receive
        {here_pid, PidM} -> io:format("DPL: Pid del main ricevuto: ~p~n", [PidM]),
                            trans_handler(PidM, ListaAmici, TransList)
      end;
    _ ->
      receive
        {update_friends, ListaNuova} -> trans_handler(PidMain, ListaNuova, TransList);
        {push, {IDtransazione, Payload}} ->
            case lists:member(IDtransazione, TransList) of
              true -> trans_handler(PidMain, ListaAmici, TransList);
              false -> lists:foreach(fun(Amico) -> Amico ! {push, {IDtransazione, Payload}} end, ListaAmici),
                       trans_handler(PidMain, ListaAmici, TransList ++ IDtransazione)
            end
      end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%
main(Handler, TransHandler) ->
  Ref = make_ref(),
  % case H1 of
  %   false -> Handler = spawn(?MODULE, handler, [[], self()]),
  %           io:format("Handler spawned~n");
  %   _ -> io:format("Dont spawn~n")
  % end,
  %io:format("Waiting for a messagge...~n"),
  receive
    {push, Transazione} -> TransHandler ! {push, Transazione},
                           main(Handler, TransHandler);

    {update_friends, ListaNuova} -> TransHandler ! {update_friends, ListaNuova},
                                    main(Handler, TransHandler);

    % adesso posso fare la unregister
    {give_me_pid_final} -> Handler ! {here_pid, self()},
                           unregister(depalma_liberato),
                           main(Handler, TransHandler);

    {give_me_pid, Richiedente} -> Richiedente ! {here_pid, self()}, main(Handler, TransHandler);
    % risponde ai ping di tutti
    {ping, Mittente, Nonce} -> % io:format("Sending pong...~n"),
      Mittente ! {pong, Nonce},
      main(Handler, TransHandler);

    % gestiscono le richieste di lista di amici dagli amici
    {get_friends, Mittente, Nonce} -> Handler ! {get_friends_from_main, Mittente, Nonce},
      main(Handler, TransHandler);
    {list_from_handler, ListaAmici, Mittente, Nonce} -> Mittente ! {friends, Nonce, ListaAmici},
      main(Handler, TransHandler);

    % gestiscono la lista che arriva dal prof
    {sad} -> io:format("DPL: sad received :-( ~n"),
        teacher_node ! {get_friends, self(), Ref},
        main(Handler, TransHandler);
    {friends, Nonce, ListaAmici} -> Handler ! {list_from_main, ListaAmici},
      main(Handler, TransHandler)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
   Prof = spawn(teacher_node, main, []),
  % Act1 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  % Act2 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  % Act3 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  % Act4 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act1 = spawn(nodo1, test, []),
  Act2 = spawn(nodo2, test, []),
  Act3 = spawn(nodo3, test, []),
  Act4 = spawn(nodo4, test, []),
  Handler = spawn(?MODULE, handler, [[], none, none]),
  Main = spawn(?MODULE, main, [Handler]),
  register(depalma_liberato, Main),
  Act3 ! {give_main, self()},
  receive
    {here_main, Nodo3} -> io:format("TEST: Pid nodo 3 ricevuto~p~n", [Nodo3])
  end,
  sleep(15),
  io:format("Killing nodo3: ~p~n", [Nodo3]),
  Nodo3 ! {die},
  test_ok.
