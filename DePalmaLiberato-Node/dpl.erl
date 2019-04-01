-module(dpl).
-export([test/0, ask_prof/1, whenEmpty/3]).

sleep(N) -> receive after N*1000 -> ok end.

% nodo main (ping prof.)
ask_prof(Pid_main) ->
  teacher_node ! {ping, Pid_main, make_ref()},
  teacher_node ! {get_friends, Pid_main, make_ref()},
  io:format("Attesa~n"),
  receive
  %  {pong, Nonce} -> io:format("Pong ricevuto~n"),
  %  {ping, Mittente, Nonce} -> Mittente ! {pong, Nonce},
    {friends, Nonce, Lista_di_amici} -> io:format("Stampa Lista_di_amici ~p~n", [Lista_di_amici]),
                                        whenEmpty(Lista_di_amici, [], Pid_main)
  %  {get_friends, Mittente, Nonce} -> Mittente ! {friends, Nonce, Lista_di_amici}
  end.

whenEmpty(Add_amici, Lista_di_amici, Pid_main) ->
  Amici_only = lists:delete(Pid_main, Add_amici),
  case length(Amici_only) of
    N when N >= 3 -> {A, _} = lists:split(3, Amici_only),
                     Var = Lista_di_amici ++ A,
                     io:format("Stampa Lista_di_amici ~p~n", [Var]);
    _ -> ask_prof(Pid_main)
  end.






test() ->
  % far partire il nodo prof
  Prof = spawn(teacher_node, main, []),
  Act1 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act2 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act3 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  Act4 = spawn(fun() -> teacher_node ! {get_friends, self(), make_ref()} end),
  sleep(1),
  Act5 = spawn(?MODULE, ask_prof, [self()]).


  % ask_prof
