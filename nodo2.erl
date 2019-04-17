-module(nodo2).
-export([main/4, test/0, handler/3, trans_handler/4, 
        pinger/2, counter_tries/2, failed_push_tracker/1,
        block_handler/4, chain_handler/3, reconstruction_handler/4,
        miner_handler/2, miner/3]).

sleep(N) -> receive after N*1000 -> ok end.

% lo fanno i watcher degli amici
pinger(ToPing, Handler) ->
  sleep(10),
  %io:format("Nodo2: Pinging ~p...~n", [ToPing]),
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
      depalma_liberato_2 ! {give_me_pid_final},
      receive
        {here_pid, PidM} -> link(PidM),
                            handler(ListaAmici, PidM, spawn(?MODULE, counter_tries, [0, PidHandler]))
      end;
    _ ->
        io:format("Nodo2: Friends: ~p~n", [ListaAmici]),
        NumeroAmici = length(ListaAmici),
        Ref = make_ref(),
        % controlla i messaggi da mandare, compresa la richiesta di amici
        case NumeroAmici of
          0 -> PidMain ! {sad};
          1 -> hd(ListaAmici) ! {get_friends, PidHandler, Ref};
          2 -> Node = take_one_random(ListaAmici), io:format("Nodo2: Ho preso a caso: ~p~n", [Node]), Node ! {get_friends, PidHandler, Ref};
          _ -> ok
        end,
        receive

          {bored} ->  handler([], PidMain, spawn(?MODULE, counter_tries, [0, PidHandler]));
          % gestisce la morte di un amico

          {dead, DeadFriend} -> io:format("Nodo2: friend died: ~p~n", [DeadFriend]), handler(ListaAmici -- [DeadFriend], PidMain, PidCounter);

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
trans_handler(PidMain, ListaAmici, TransList, PidTracker) ->
  sleep(2),
  case PidMain of
    none ->
      depalma_liberato_2 ! {give_me_pid, self()},
      receive
        {here_pid, PidM} -> link(PidM),
                            trans_handler(PidM, ListaAmici, TransList, spawn_link(?MODULE, failed_push_tracker, [self()]))
      end;
    _ ->
      %io:format("Nodo2: translist = ~p~n", [TransList]),
      receive
        {give_trans_list, Sender} ->  case length(TransList) of 
                                        0 -> Sender ! {trans_list_empty};
                                        _ -> Sender ! {trans_list_non_empty, TransList}
                                      end,
                                      trans_handler(PidMain, ListaAmici, TransList, PidTracker);
        {remove_trans, ToBeRemoved} -> trans_handler(PidMain, ListaAmici, lists:delete(ToBeRemoved, TransList), PidTracker);
        {update_friends, ListaNuova} -> %io:format("Nodo2: TransHandler amici aggiornati.~p~n", [ListaNuova]),
                                        trans_handler(PidMain, ListaNuova, TransList, PidTracker);
        {push, {IDtransazione, Payload}} ->
          %io:format("Nodo2: transazione ricevuta, IDtransazione: ~p~n", [IDtransazione]),
            case lists:member(IDtransazione, TransList) of
              true -> %io:format("Nodo2: Transazione con id: ~p già presente~n", [IDtransazione]),
                      trans_handler(PidMain, ListaAmici, TransList, PidTracker);
              false -> %io:format("Nodo2: Transazione con id: ~p è nuova, la invio agli amici: ~p ~n", [IDtransazione, ListaAmici]),
                       case length(ListaAmici) of
                         0 -> io:format("Nodo2:TransHandler lista amici vuota, la mando al tracker.~n"),
                              PidTracker ! {failed_push, {IDtransazione, Payload}},
                              trans_handler(PidMain, ListaAmici, TransList, PidTracker);
                         _ -> lists:foreach(fun(Amico) ->
                                case rand:uniform(10) of
                                  1 -> ok;
                                  2 -> Amico ! {push, {IDtransazione, Payload}},
                                      Amico ! {push, {IDtransazione, Payload}};
                                  _ -> Amico ! {push, {IDtransazione, Payload}}
                                end
                              end, ListaAmici),
                              trans_handler(PidMain, ListaAmici, TransList ++ [IDtransazione], PidTracker)
                        end
            end
      end
  end.

failed_push_tracker(PidTransHandler) ->
  receive
    {failed_push, {IDtransazione, Payload}} -> sleep(5),
                                  PidTransHandler ! {push, {IDtransazione, Payload}},
                                  io:format("Nodo2: tracker: rimando transazione~n"),
                                  failed_push_tracker(PidTransHandler)
  end.


% send_failed_push(Amico, FailedPush) ->
%   lists:foreach(fun(Transazione) -> Amico ! {push, Transazione} end, FailedPush).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Chain & Blocks %%%%%
chain_handler(PidMain, ListaAmici, CatenaNostra) ->
  sleep(2),
  case PidMain of
    none ->
      depalma_liberato_2 ! {give_me_pid, self()},
      receive
        {here_pid, PidM} -> link(PidM),
                            chain_handler(PidM, ListaAmici, CatenaNostra)
      end;
    _ ->
      %io:format("Nodo2: ChainHandler catena: ~p~n", [CatenaNostra]), 
      receive
        {get_previous, Mittente, Nonce, Idblocco} -> 
          spawn(fun() -> give_previous_block(Mittente, Nonce, Idblocco, CatenaNostra) end), 
          chain_handler(PidMain, ListaAmici, CatenaNostra);
{give_head, Mittente} -> case length(CatenaNostra) of 
                                  0 ->  Mittente ! {catena_empty},
                                        chain_handler(PidMain, ListaAmici, CatenaNostra);
                                  _ ->  Mittente ! {head, hd(CatenaNostra)}, 
                                        chain_handler(PidMain, ListaAmici, CatenaNostra)
                                  end;
        {get_head, Mittente, Nonce} -> case length(CatenaNostra) of 
                                        0 ->  self() ! {get_head, Mittente, Nonce}, 
                                              chain_handler(PidMain, ListaAmici, CatenaNostra);
                                        _ ->  Mittente ! {head, Nonce, hd(CatenaNostra)}, 
                                              chain_handler(PidMain, ListaAmici, CatenaNostra)
                                        end;

        {update_friends, ListaNuova} -> chain_handler(PidMain, ListaNuova, CatenaNostra);

        {block_mined, Blocco} ->
          {_, _, Lista_di_transazioni, _} = Blocco,
          PidMain ! {remove_trans, Lista_di_transazioni},
          block_retransmission(ListaAmici, self(), Blocco),
          chain_handler(PidMain, ListaAmici, [Blocco|CatenaNostra]);

        {update, Mittente, Blocco} ->
          {IDnuovo_blocco, IDblocco_precedente, Lista_di_transazioni, Soluzione} = Blocco,
          case proof_of_work:check({IDnuovo_blocco, Lista_di_transazioni}, Soluzione) of 
            true -> 
              PidMain ! {remove_trans, Lista_di_transazioni},
              case length(CatenaNostra) of 

                0 -> % primo blocco che si aggiunge, aggiungi direttamente
                  block_retransmission(ListaAmici, self(), Blocco),
                  chain_handler(PidMain, ListaAmici, [Blocco|CatenaNostra]); 
                _ ->
                {Head_id, _, _, _} = hd(CatenaNostra),
                case IDblocco_precedente of
                  Head_id -> % Add normale
                    block_retransmission(ListaAmici, self(), Blocco),
                    chain_handler(PidMain, ListaAmici, [Blocco|CatenaNostra]); 
                  _ -> % Lancia handlers e crea nuova catena
                    spawn(?MODULE, block_handler, [[CatenaNostra, self(), Mittente, Blocco]]),
                    chain_handler(PidMain, ListaAmici, CatenaNostra)
                end
              end;
            
            false -> chain_handler(PidMain, ListaAmici, CatenaNostra) % blocco falso, non fare niente
          end;
        
        {catena_updated, Blocco, NuovaCatena} ->
          case length(NuovaCatena) of
            N when N > length(CatenaNostra) ->  block_retransmission(ListaAmici, self(), Blocco),
                                                chain_handler(PidMain, ListaAmici, NuovaCatena); % sostituisci la catena
            _ ->  block_retransmission(ListaAmici, self(), Blocco),
                  chain_handler(PidMain, ListaAmici, [Blocco|CatenaNostra]) % usa la tua catena + il nuovo blocco dall'update
          end

      end
  end.

give_previous_block(Mittente, Nonce, IDBlocco, Catena) ->
  case length(Catena) of 
    0 -> not_found;
    _ ->
        [Head | Tail] = Catena,
        {ID, _, _, _} = Head,
        case ID of
          IDBlocco -> Mittente ! {previous, Nonce, Head}; % Mittente ! {previous, Nonce, Blocco}
          _ -> give_previous_block(Mittente, Nonce, IDBlocco, Tail)
        end
  end.

block_retransmission(ListaAmici, PidSender, Blocco) ->
  lists:foreach(fun(Amico) ->
    case rand:uniform(10) of
      1 -> ok;
      2 -> Amico ! {update, PidSender, Blocco},
          Amico ! {update, PidSender, Blocco};
      _ -> Amico ! {update, PidSender, Blocco}
    end
  end, ListaAmici).

block_handler(CatenaNostra, PidChainHandler, Mittente, Blocco) ->
  Ref = make_ref(),
  % lancia algoritmo di ricostruzione
  Mittente ! {get_head, self(), Ref},
  receive 
    {head, Nonce, Head} -> spawn_link(?MODULE, reconstruction_handler, [self(), Mittente, Head, [Head]]) 
  end,
  receive
    {rec_handler_insert_normally} -> PidChainHandler ! {catena_updated, Blocco, [Blocco|CatenaNostra]};
    {rec_handler_catena, CatenaMittente} -> PidChainHandler ! {catena_updated, Blocco, CatenaMittente}
  end.

reconstruction_handler(PidChainHandler, Mittente, Blocco, CatenaMittente) ->
  Ref = make_ref(),
  {_, IDblocco_precedente, _, _} = Blocco,
  Mittente ! {get_previous, self(), Ref, IDblocco_precedente}, 
  receive 
    {previous, Nonce, {Id, Id_previous, Lista_trans, Sol}} -> 
      case proof_of_work:check({Id, Lista_trans}, Sol) of 
        true -> % va avanti da specifica 
          case Id of
            none -> % abbiamo ricostruito la catena
                PidChainHandler ! {rec_handler_catena, CatenaMittente};
            _ -> reconstruction_handler(PidChainHandler, Mittente, {Id, Id_previous, Lista_trans, Sol},
                                        CatenaMittente ++ [{Id, Id_previous, Lista_trans, Sol}])
          end;
        false -> PidChainHandler ! {rec_handler_insert_normally} % ha beccato uno con la catena falsa
      end
  after 10000 -> PidChainHandler ! {rec_handler_insert_normally} % il mittente e' morto mentre ricostruivamo la catena
  end.

miner_handler(TransHandler, ChainHandler) ->
  sleep(10),
  TransHandler ! {give_trans_list, self()},
  receive
    {trans_list_empty} -> io:format("Nodo2: lista transazioni vuota~n");

    {trans_list_non_empty, TransList} ->
      ChainHandler ! {give_head, self()},
      receive
        {catena_empty} -> % first block to mine
          io:format("Nodo2: mining started for the first time!~n"),
          PidMiner = spawn(?MODULE, miner, [TransList, none, self()]),
          receive
            {stop_mining} -> io:format("Nodo2: mining ABORTED!~n"), exit(PidMiner, kill);
            {mining_finished, Sol} -> io:format("Nodo2: Blocco minato!~n"), ChainHandler ! {block_mined, {make_ref(), none, TransList, Sol}}
          end;

        {head, Blocco} ->
          {IDBlocco, _, _, _} = Blocco,
          io:format("Nodo2: mining started!~n"),
          PidMiner = spawn(?MODULE, miner, [TransList, IDBlocco, self()]),
          receive
            {stop_mining} -> io:format("Nodo2: mining ABORTED!~n"), exit(PidMiner, kill);
            {mining_finished, Sol} -> io:format("Nodo2: Blocco minato!~n"), ChainHandler ! {block_mined, {make_ref(), IDBlocco, TransList, Sol}}
          end
      end
  end,
  miner_handler(TransHandler, ChainHandler).

miner(TransList, IDBlocco, Pid) -> 
    Sol = proof_of_work:solve({IDBlocco, TransList}),
    Pid ! {mining_finished, Sol}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%
main(Handler, TransHandler, ChainHandler, MinerHandler) ->
  Ref = make_ref(),
  receive
    {push, Transazione} -> TransHandler ! {push, Transazione};

    {update_friends, ListaNuova} -> TransHandler ! {update_friends, ListaNuova},
                                    ChainHandler ! {update_friends, ListaNuova};

    % adesso posso fare la unregister
    {give_me_pid_final} -> Handler ! {here_pid, self()},
                           unregister(depalma_liberato_2);

    {give_me_pid, Richiedente} -> Richiedente ! {here_pid, self()};
    % risponde ai ping di tutti
    {ping, Mittente, Nonce} -> % io:format("Sending pong...~n"),
      Mittente ! {pong, Nonce};

    % gestiscono le richieste di lista di amici dagli amici
    {get_friends, Mittente, Nonce} -> Handler ! {get_friends_from_main, Mittente, Nonce};

    {list_from_handler, ListaAmici, Mittente, Nonce} -> Mittente ! {friends, Nonce, ListaAmici};

    % gestiscono la lista che arriva dal prof
    {sad} -> io:format("Nodo2: sad received :-( ~n"),
      teacher_node ! {get_friends, self(), Ref};

    {friends, Nonce, ListaAmici} -> Handler ! {list_from_main, ListaAmici};

    {update, Mittente, Blocco} -> ChainHandler ! {update, Mittente, Blocco},
      MinerHandler ! {stop_mining};

    {get_previous, Mittente, Nonce, Idblocco_precedente} -> 
      ChainHandler ! {get_previous, Mittente, Nonce, Idblocco_precedente};

    {get_head, Mittente, Nonce} -> ChainHandler ! {get_head, Mittente, Nonce};

    {remove_trans, ToBeRemoved} -> TransHandler ! {remove_trans, ToBeRemoved}
  end,
  main(Handler, TransHandler, ChainHandler, MinerHandler).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  ChainHandler = spawn(?MODULE, chain_handler, [none, [], []]),
  TransHandler = spawn(?MODULE, trans_handler, [none, [], [], []]),
  Handler = spawn(?MODULE, handler, [[], none, none]),
  MinerHandler = spawn (?MODULE, miner_handler, [TransHandler, ChainHandler]),
  Main = spawn(?MODULE, main, [Handler, TransHandler, ChainHandler, MinerHandler]),
  register(depalma_liberato_2, Main),
  % TODO: Fare l'unregister nella test, dopo aver controllato che tutti hanno il pid del main.
  % TODO: gestire la morte
  sleep(23),
  io:format("Start transaction test...~n"),
  spawn(fun() -> test_transactions(Main, 0) end),

  test_ok.

test_transactions(Main, Counter) ->
  sleep(2),
  case Counter of 
    4 -> ok;
    _ ->
        case rand:uniform(2) of
          1 -> Main ! {push, {123, ciao}}, test_transactions(Main, Counter + 1);
          _ -> Main ! {push, {rand:uniform(100), ciao}}, test_transactions(Main, Counter + 1)
        end
  end.
