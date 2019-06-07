-module(user).
-export([user/1]).
-import(login, [login/2, logout/1, registerLogin/2, unregister/2]).

% Função que trata da autenticação de um utilizador
user(Sock) ->
    receive
        {tcp, _, Data} ->
            Message = string:split(string:trim(Data), ":", all),
            case lists:nth(1, Message) of
                <<"Login">> ->
                    case length(Message) of
                        3 ->
                            Username = lists:nth(2, Message),
                            Password = lists:nth(3, Message),
                            Res = login(Username, Password),
                            case Res of
                                ok ->
                                    gen_tcp:send(Sock, io_lib:format("LoginOk~n", [])),
                                    user(Sock, Username, newMatch(Sock, Username));
                                inexistentUser ->
                                    gen_tcp:send(Sock, io_lib:format("LoginInexistentUser~n", [])),
                                    user(Sock);
                                alreadyLoggedIn ->
                                    gen_tcp:send(Sock, io_lib:format("LoginAlreadyLoggedIn~n", [])),
                                    user(Sock);
                                wrongPassword ->
                                    gen_tcp:send(Sock, io_lib:format("LoginWrongPassword~n", [])),
                                    user(Sock)
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("LoginInvalid~n", [])),
                            user(Sock)
                    end;
                <<"Logout">> ->
                    case length(Message) of
                        2 ->
                            logout(lists:nth(2, Message)),
                            gen_tcp:send(Sock, io_lib:format("LogoutOk~n", []));
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("LogoutInvalid~n", []))
                    end,
                    user(Sock);
                <<"Register">> ->
                    case length(Message) of
                        3 ->
                            Username = lists:nth(2, Message),
                            Password = lists:nth(3, Message),
                            Res = registerLogin(Username, Password),
                            case Res of
                                ok ->
                                    gen_tcp:send(Sock, io_lib:format("RegisterOk~n", [])),
                                    user(Sock, Username, newMatch(Sock, Username));
                                userExists ->
                                    gen_tcp:send(Sock, io_lib:format("RegisterUserExists~n", [])),
                                    user(Sock)
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("RegisterInvalid~n", [])),
                            user(Sock)
                    end;
                <<"Unregister">> ->
                    case length(Message) of
                        3 ->
                            Res = unregister(lists:nth(2, Message), lists:nth(3, Message)),
                            case Res of
                                ok ->
                                    gen_tcp:send(Sock, io_lib:format("UnregisterOk~n", []));
                                notExists ->
                                    gen_tcp:send(Sock, io_lib:format("UnregisterUserNotExists~n", []));
                                wrongPassword ->
                                    gen_tcp:send(Sock, io_lib:format("UnregisterWrongPassword~n", []))
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("UnregisterInvalid~n", []))
                    end,
                    user(Sock);
                <<"Scores">> ->
                    case length(Message) of
                        1 ->
                            scoresManager ! {getScores, self()},
                            receive
                                {scores, Scores} ->
                                    gen_tcp:send(Sock, io_lib:format("BeginScores~n", [])),
                                    sendScores(Sock, #{scores => Scores}),
                                    gen_tcp:send(Sock, io_lib:format("EndScores~n", []))
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("ScoresInvalid~n", []))
                    end,
                    user(Sock);
                _ ->
                    gen_tcp:send(Sock, io_lib:format("UnknownOperation~n", [])),
                    user(Sock)
            end;
        {tcp_closed, _} ->
            io:format("User disconnected~n");
        {tcp_error, _, _} ->
            io:format("User disconnected with error~n")
    end.

% Função que devolve uma partida nova
newMatch(Sock, Username) ->
    matchManager ! {newPlayer, Username, self()},
    receive
        {initialMatch, MatchInfo, Match, matchManager} ->
            sendInitialMatchInfo(Sock, MatchInfo),
            Match;
        {tcp_closed, _} ->
            io:format("User ~s disconnected~n", [Username]),
            matchManager ! {leaveWaitMatch, Username, self()},
            logout(Username);
        {tcp_error, _, _} ->
            io:format("User ~s disconnected with error~n", [Username]),
            matchManager ! {leaveWaitMatch, Username, self()},
            logout(Username)
    end.

% Função que gere a conexão com um utilizador após o login
user(Sock, Username, Match) ->
    receive
        {matchOver, Leaderboard, Match} ->
            handleMatchOver(Sock, Username, Leaderboard)
    after
        0 ->
            receive
                {matchOver, Leaderboard, Match} ->
                    handleMatchOver(Sock, Username, Leaderboard);
                {updateMatch, MatchInfo, Match} ->
                    sendMatchInfo(Sock, MatchInfo),
                    user(Sock, Username, Match);
                {tcp, _, Data} ->
                    Message = string:split(string:trim(Data), ":", all),
                    case lists:nth(1, Message) of
                        <<"keyChanged">> ->
                            case length(Message) of
                                3 ->
                                    case lists:nth(3, Message) of 
                                        <<"true">> ->
                                            Match ! {keyChanged, lists:nth(2, Message), true, self()};
                                        <<"false">> ->
                                            Match ! {keyChanged, lists:nth(2, Message), false, self()}
                                    end;
                                _ ->
                                    gen_tcp:send(Sock, io_lib:format("keyChangedInvalid~n", []))
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("UnknownOperation~n", []))
                    end,
                    user(Sock, Username, Match);
                {tcp_closed, _} ->
                    io:format("User ~s disconnected~n", [Username]),
                    Match ! {leave, self()},
                    logout(Username);
                {tcp_error, _, _} ->
                    io:format("User ~s disconnected with error~n", [Username]),
                    Match ! {leave, self()},
                    logout(Username)
            end
    end.

% Função que indica ao jogador que a partida terminou
handleMatchOver(Sock, Username, Leaderboard) ->
    gen_tcp:send(Sock, io_lib:format("MatchOverBegin~n", [])),
    sendScores(Sock, #{scores => Leaderboard}),
    gen_tcp:send(Sock, io_lib:format("MatchOverEnd~n", [])),
    handleMatchOverAnswer(Sock, Username).

% Função que verifica se o utilizador deseja jogar outra partida
handleMatchOverAnswer(Sock, Username) ->
    receive
        {tcp, _, Data} ->
            Res = string:trim(Data),
            case Res of
                <<"Continue">> ->
                    user(Sock, Username, newMatch(Sock, Username));
                <<"Quit">> ->
                    io:format("User ~s disconnected~n", [Username]),
                    logout(Username),
                    user(Sock);
                _ ->
                    gen_tcp:send(Sock, io_lib:format("UnknownCommand~n", [])),
                    handleMatchOverAnswer(Sock, Username)
            end;
        {tcp_closed, _} ->
            io:format("User ~s disconnected~n", [Username]),
            logout(Username);
        {tcp_error, _, _} ->
            io:format("User ~s disconnected with error~n", [Username]),
            logout(Username)
    end.

% Função que envia os dados da partida (no seu estado inicial) para o cliente
sendInitialMatchInfo(Sock, MatchInfo) ->
    gen_tcp:send(Sock, io_lib:format("StartInitialMatchInfo~n", [])),
    sendPlayersInfo(Sock, MatchInfo),
    sendBlobsInfo(Sock, MatchInfo),
    sendCurrentTime(Sock, MatchInfo),
    sendScores(Sock, MatchInfo),
    gen_tcp:send(Sock, io_lib:format("EndInitialMatchInfo~n", [])).

% Função que envia os dados da partida (que foram atualizados) para o cliente
sendMatchInfo(Sock, UpdatedMatchInfo) ->
    gen_tcp:send(Sock, io_lib:format("StartMatchInfo~n", [])),
    sendPlayersInfo(Sock, UpdatedMatchInfo),
    sendBlobsInfo(Sock, UpdatedMatchInfo),
    sendCurrentTime(Sock, UpdatedMatchInfo),
    sendScores(Sock, UpdatedMatchInfo),
    gen_tcp:send(Sock, io_lib:format("EndMatchInfo~n", [])).

% Função que envia a informação de todos os jogadores para o cliente
sendPlayersInfo(Sock, MatchInfo) ->
    case maps:find(players, MatchInfo) of
        {ok, Players} ->
            PlayersList = [Player || {_, Player} <- maps:to_list(Players)],
            sendPlayerInfo(Sock, PlayersList);
        error ->
            nothingToSend
    end.

% Função que envia a informação de um jogador para o cliente
sendPlayerInfo(_, []) ->
    done;
sendPlayerInfo(Sock, [H|T]) ->
    {ok, Username} = maps:find(username, H),
    {ok, X} = maps:find(x, H),
    {ok, Y} = maps:find(y, H),
    {ok, Radius} = maps:find(radius, H),
    {ok, Score} = maps:find(score, H),
    {ok, Invisibility} = maps:find(invisibilityBonus, H),
    {ok, Speed} = maps:find(speedBonus, H),
    if
        Invisibility == -1 andalso Speed == -1 ->
            gen_tcp:send(Sock, io_lib:format("P:~s:~w:~w:~w:~w:~w:~w~n", [Username, X, Y, Radius, Score, false, false]));
        Invisibility == -1 andalso Speed /= -1 ->
            gen_tcp:send(Sock, io_lib:format("P:~s:~w:~w:~w:~w:~w:~w~n", [Username, X, Y, Radius, Score, false, true]));
        Invisibility /= -1 andalso Speed == -1 ->
            gen_tcp:send(Sock, io_lib:format("P:~s:~w:~w:~w:~w:~w:~w~n", [Username, X, Y, Radius, Score, true, false]));
        Invisibility /= -1 andalso Speed /= -1 ->
            gen_tcp:send(Sock, io_lib:format("P:~s:~w:~w:~w:~w:~w:~w~n", [Username, X, Y, Radius, Score, true, true]))
    end,
    sendPlayerInfo(Sock, T).

% Função que envia a informação das entidades para o cliente
sendBlobsInfo(Sock, MatchInfo) ->
    case maps:find(blobs, MatchInfo) of
        {ok, Blobs} ->
            sendBlobInfo(Sock, Blobs, 0);
        error ->
            nothingToSend
    end.

% Função que envia a informação das entidades, recursivamente, para o cliente
sendBlobInfo(_, [], _) ->
    blobsDone;
sendBlobInfo(Sock, [{Idx, Blob}|T], _) ->
    {ok, Type} = maps:find(type, Blob),
    {ok, X} = maps:find(x, Blob),
    {ok, Y} = maps:find(y, Blob),
    {ok, Radius} = maps:find(radius, Blob),
    gen_tcp:send(Sock, io_lib:format("B:~w:~w:~w:~w:~w~n", [Idx, Type, X, Y, Radius])),
    sendBlobInfo(Sock, T, ok);
sendBlobInfo(Sock, [H|T], Idx) ->
    {ok, Type} = maps:find(type, H),
    {ok, X} = maps:find(x, H),
    {ok, Y} = maps:find(y, H),
    {ok, Radius} = maps:find(radius, H),
    gen_tcp:send(Sock, io_lib:format("B:~w:~w:~w:~w:~w~n", [Idx, Type, X, Y, Radius])),
    sendBlobInfo(Sock, T, Idx+1).

% Função que envia a informação do tempo de início/tempo atual da partida para o cliente
sendCurrentTime(Sock, MatchInfo) ->
    case maps:find(currentTime, MatchInfo) of
        {ok, CT} ->
            gen_tcp:send(Sock, io_lib:format("CT:~w~n", [CT]));
        error ->
            nothingToSend
    end.

% Função que envia a informação das melhores pontuações para o cliente
sendScores(Sock, MatchInfo) ->
    case maps:find(scores, MatchInfo) of
        {ok, Scores} ->
            sendScore(Sock, Scores);
        error ->
            nothingToSend
    end.

% Função que envia a informação de uma pontuação para o cliente
sendScore(_, []) ->
    done;
sendScore(Sock, [{Username, Score}|T]) ->
    gen_tcp:send(Sock, io_lib:format("S:~s:~w~n", [Username, Score])),
    sendScore(Sock, T).
