-module(topology).
-export([main/1, test/0, handler/2, pinger/2]).

sleep(N) -> receive after N*1000 -> ok end.

% lo fanno i watcher degli amici
pinger(ToPing, Handler) ->
  sleep(10),
  io:format("DPL: Pinging ~p...~n", [ToPing]),
  Ref = make_ref(),
  ToPing ! {ping, self(), Ref},
  receive
    {pong, Ref} -> pinger(ToPing, Handler)
  after 2000 -> Handler ! {dead, ToPing}
  end.

handler(ListaAmici, PidMain) ->
  sleep(2),

  case PidMain of 
    none -> 
      depalma_liberato ! {give_me_pid},
      receive 
        {here_pid, PidM} -> io:format("DPL: Pid del main ricevuto: ~p~n", [PidM]), handler(ListaAmici, PidM)
      end;
    _ -> io:format("Pid già avuto: ~p~n", [PidMain])
  end,

  PidHandler = self(),
  NumeroAmici = length(ListaAmici),
  Ref = make_ref(),
  % controlla i messaggi da mandare, compresa la richiesta di amici
  case NumeroAmici of
    0 -> PidMain ! {sad};
    1 -> hd(ListaAmici) ! {get_friends, PidHandler, Ref}; 
    2 -> hd(ListaAmici) ! {get_friends, PidHandler, Ref};
    _ -> io:format("Amici già al completo~n")
  end,
  io:format("DPL: Friends: ~p~n", [ListaAmici]),
  receive
    % gestisce la morte di un amico
  
    {dead, DeadFriend} -> io:format("DPL: friend died: ~p~n", [DeadFriend]), handler(ListaAmici -- [DeadFriend], PidMain);

    % riceve la lista dopo che l'abbiamo richiesta perchè si è svuotata
    {list_from_main, ListaNuovaMain} -> %3 a caso
              Amici_only = lists:delete(PidMain, ListaNuovaMain),
              case length(Amici_only) of
                N when N >= 3 -> {A, _} = lists:split(3, Amici_only),
                                lists:foreach(fun(P) -> spawn(?MODULE, pinger, [P, PidHandler]) end, A),
                                handler(ListaAmici ++ A, PidMain);
                _ -> handler(ListaAmici, PidMain)
              end;

    % riceve la lista degli amici di un amico per aggiungerli/o ai nostri (risposta dei nostri get_friends)
    {friends, Nonce, ListaNuova} -> %quanti ne mancano
              NoDuplicate = lists:filter(fun(Elem) -> not lists:member(Elem, ListaAmici) end, ListaNuova),
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
                  end;
                1 ->
                  case NumeroAmici of
                   N when N < 3 ->
                    Amico = hd(Amici_only),
                    spawn(?MODULE, pinger, [Amico, PidHandler]),
                    handler([Amico|ListaAmici], PidMain)
                end
              end;

    % manda la lista degli amici al main per rispondere agli amici che chiedono chi conosciamo
    {get_friends_from_main, Mittente, Nonce} -> PidMain ! {list_from_handler, ListaAmici, Mittente, Nonce}
  
  end.

main(Handler) ->
  Ref = make_ref(),
  % case H1 of
  %   false -> Handler = spawn(?MODULE, handler, [[], self()]),
  %           io:format("Handler spawned~n");
  %   _ -> io:format("Dont spawn~n")
  % end,
  %io:format("Waiting for a messagge...~n"),
  Handler ! {main_pid, self()},
  receive
  
    {give_me_pid} -> Handler ! {here_pid, self()}, unregister(depalma_liberato), main(Handler);
    {dieded} -> io:format("Act3 è morto.");
    % risponde ai ping di tutti
    {ping, Mittente, Nonce} -> % io:format("Sending pong...~n"),
      Mittente ! {pong, Nonce},
      main(Handler);

    % gestiscono le richieste di lista di amici dagli amici
    {get_friends, Mittente, Nonce} -> Handler ! {get_friends_from_main, Mittente, Nonce},
      main(Handler);
    {list_from_handler, ListaAmici, Mittente, Nonce} -> Mittente ! {friends, Nonce, ListaAmici},
      main(Handler);

    % gestiscono la lista che arriva dal prof
    {sad} -> io:format("DPL: sad received :-( ~n"),
        teacher_node ! {get_friends, self(), Ref},
        main(Handler);
    {friends, Nonce, ListaAmici} -> Handler ! {list_from_main, ListaAmici},
      main(Handler)
    end.

test() ->
   Prof = spawn(teacher_node, main, []),
  Act1 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act2 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act3 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act4 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),

  Handler = spawn(?MODULE, handler, [[], none]),
  Main = spawn(?MODULE, main, [Handler]),
  register(depalma_liberato, Main),
  sleep(5).  

  % sleep(15),
  % io:format("Killing act3...~n"),
  % depalma_liberato_3 ! {die, Main}.
%% TODO: register del main così per chiedere il Pid e poi usare il Pid invece del nome.


