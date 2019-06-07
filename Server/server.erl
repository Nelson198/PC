-module(server).
-export([server/0, server/1]).
-import(login, [loginHandler/1]).
-import(user, [user/1]).
-import(scores, [scoresHandler/2]).

% Função que inicia o servidor na porta 1234 do localhost com partidas de 2 jogadores
server() ->
    server(2).
% Função que inicia o servidor na porta 1234 do localhost com partidas de "NrPlayers" jogadores
server(NrPlayers) ->
    register(matchManager, spawn(fun() -> matchHandler([], [], NrPlayers) end)),
    register(loginManager, spawn(fun() -> loginHandler(#{}) end)),
    register(scoresManager, spawn(fun() -> scoresHandler([], []) end)),
    {ok, LSock} = gen_tcp:listen(1234, [binary, {packet, line}, {reuseaddr, true}]),
    io:format("Server ready. Waiting for connections...~n", []),
    acceptor(LSock).

% Função que aceita conexões dos utilizadores
acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("New connection~n", []),
    spawn(fun() -> acceptor(LSock) end),
    user(Sock).

% Função que gere todas as partidas, criando novas para os jogadores que querem jogar
matchHandler(Matches, PlayersWaiting, NrPlayers) ->
    receive
        {newPlayer, Username, Pid} ->
            PlayersWaiting1 = [{Pid, Username} | PlayersWaiting],
            if
                length(PlayersWaiting1) < NrPlayers ->
                    io:format("User ~s wants to play. Waiting for opponent...~n", [Username]),
                    matchHandler(Matches, PlayersWaiting1, NrPlayers);
                true ->
                    io:format("Opponent found (user ~s)~n", [Username]),
                    Match = spawn(fun()-> matchInitialize(PlayersWaiting1, #{}, [], #{}, os:system_time(second)) end),
                    matchHandler(Matches ++ [Match], [], NrPlayers)
            end;
        {leaveWaitMatch, Username, Pid} ->
            io:format("User ~s left~n", [Username]),
            matchHandler(Matches, PlayersWaiting -- [{Pid, Username}], NrPlayers);
        {matchOver, Match} ->
            io:format("Match ~p is over~n", [Match]),
            matchHandler(Matches -- [Match], PlayersWaiting, NrPlayers)
    end.

% Função que verifica se a posição de um jogador/objeto não coincide com a posição de todos os jogadores
checkPosition(_, _, _, []) ->
    true;
checkPosition(X, Y, Radius, [{_,Player} | Players]) ->
    {ok, X1} = maps:find(x, Player),
    {ok, Y1} = maps:find(y, Player),
    {ok, R} = maps:find(radius, Player),
    Dist = math:sqrt(math:pow(X1 - X, 2) + math:pow(Y1 - Y, 2)),
    if
        Dist < R + Radius ->
            false;
        true ->
            checkPosition(X, Y, Radius, Players)
    end.

% Função que cria os objetos iniciais
createBlobs(_, 0, _, Res) ->
    Res;
createBlobs(Type, N, Players, Res) ->
    X = rand:uniform(1367)-1,
    Y = rand:uniform(769)-1,
    Radius = rand:uniform(22)+3,
    Bool = checkPosition(X, Y, Radius, Players),
    if
        not Bool ->
            createBlobs(Type, N, Players, Res);
        true ->
            MaybeBonus = rand:uniform(100),
            case MaybeBonus of
                1 ->
                    Blob = #{type => speedBonus, x => X, y => Y, radius => 10, replacing => Type};
                2 ->
                    Blob = #{type => invisibilityBonus, x => X, y => Y, radius => 10, replacing => Type};
                _ ->
                    Blob = #{type => Type, x => X, y => Y, radius => Radius}
            end,
            NewRes = [Blob | Res],
            createBlobs(Type, N-1, Players, NewRes)
    end.

% Função que cria um jogador
createPlayer(Username, Players, PressedKeys, BestScore) ->
    X = rand:uniform(1367)-1,
    Y = rand:uniform(769)-1,
    Radius = 24,
    Bool = checkPosition(X, Y, Radius, Players),
    if
        BestScore == 0 ->
            BestScore1 = math:pi() * Radius * Radius;
        true ->
            BestScore1 = BestScore
    end,
    if 
        not Bool ->
            createPlayer(Username, Players, PressedKeys, BestScore1);
        true ->
            #{username => Username, x => X, y => Y, radius => Radius, score => math:pi() * Radius * Radius, pressedKeys => PressedKeys, bestScore => BestScore1, speedBonus => -1, invisibilityBonus => -1}
    end.

% Função que retorna a massa de um objeto/jogador
mass(Object) ->
    {ok, R} = maps:find(radius, Object),
    math:pi() * R * R.

% Função que verifica se um jogador come um objeto. Em caso afirmativo, devolve a diferença no raio ou o bónus atribuído
eatsBlob(Player, Blob) ->
    {ok, XP} = maps:find(x, Player),
    {ok, YP} = maps:find(y, Player),
    {ok, RP} = maps:find(radius, Player),
    {ok, XB} = maps:find(x, Blob),
    {ok, YB} = maps:find(y, Blob),
    {ok, RB} = maps:find(radius, Blob),
    {ok, Type} = maps:find(type, Blob),
    Dist = math:sqrt(math:pow(XB - XP, 2) + math:pow(YB - YP, 2)),
    case Type of
        food ->
            % Sobreposição total
            if
                Dist < RP - RB andalso RP > RB ->
                    Res = math:sqrt((mass(Player) + mass(Blob))/math:pi()) - RP,
                    {true, Res};
                true ->
                    false
            end;
        poison ->
            % Sobreposição parcial
            if
                Dist < RP + RB andalso RP > RB ->
                    Res = math:sqrt((mass(Player) - mass(Blob))/math:pi()) - RP,
                    {true, Res};
                true ->
                    false
            end;
        speedBonus ->
            % Sobreposição total
            if
                Dist < RP - RB andalso RP > RB ->
                    speedBonus;
                true ->
                    false
            end;
        invisibilityBonus ->
            % Sobreposição total
            if
                Dist < RP - RB andalso RP > RB ->
                    invisibilityBonus;
                true ->
                    false
            end
    end.

% Função que devolve uma lista dos índices dos objetos comidos e uma lista sem eles
getBlobsEaten(Player, [], _, {IdxFood, IdxPoison, RadiusChange, NewBlobs}) ->
    {IdxFood, IdxPoison, RadiusChange, NewBlobs, Player};
getBlobsEaten(Player, [empty | Blobs], Idx, {IdxFood, IdxPoison, RadiusChange, NewBlobs}) ->
    getBlobsEaten(Player, Blobs, Idx+1, {IdxFood, IdxPoison, RadiusChange, NewBlobs});
getBlobsEaten(Player, [Blob | Blobs], Idx, {IdxFood, IdxPoison, RadiusChange, NewBlobs}) ->
    case eatsBlob(Player, Blob) of
        {true, RadiusDiff} ->
            NewBlobs1 = lists:sublist(NewBlobs, Idx-1) ++ [empty] ++ lists:nthtail(Idx, NewBlobs),
            case maps:find(type, Blob) of
                {ok, poison} ->
                    getBlobsEaten(Player, Blobs, Idx+1, {IdxFood, [Idx|IdxPoison], RadiusChange + RadiusDiff, NewBlobs1});
                {ok, food} ->
                    getBlobsEaten(Player, Blobs, Idx+1, {[Idx|IdxFood], IdxPoison, RadiusChange + RadiusDiff, NewBlobs1})
            end;
        speedBonus ->
            NewPlayer = maps:update(speedBonus, os:system_time(millisecond), Player),
            NewBlobs1 = lists:sublist(NewBlobs, Idx-1) ++ [empty] ++ lists:nthtail(Idx, NewBlobs),
            case maps:find(replacing, Blob) of
                {ok, food} ->
                    getBlobsEaten(NewPlayer, Blobs, Idx+1, {[Idx|IdxFood], IdxPoison, RadiusChange, NewBlobs1});
                {ok, poison} ->
                    getBlobsEaten(NewPlayer, Blobs, Idx+1, {IdxFood, [Idx|IdxPoison], RadiusChange, NewBlobs1})
            end;
        invisibilityBonus ->
            NewPlayer = maps:update(invisibilityBonus, os:system_time(millisecond), Player),
            NewBlobs1 = lists:sublist(NewBlobs, Idx-1) ++ [empty] ++ lists:nthtail(Idx, NewBlobs),
            case maps:find(replacing, Blob) of
                {ok, food} ->
                    getBlobsEaten(NewPlayer, Blobs, Idx+1, {[Idx|IdxFood], IdxPoison, RadiusChange, NewBlobs1});
                {ok, poison} ->
                    getBlobsEaten(NewPlayer, Blobs, Idx+1, {IdxFood, [Idx|IdxPoison], RadiusChange, NewBlobs1})
            end;
        false ->
            getBlobsEaten(Player, Blobs, Idx+1, {IdxFood, IdxPoison, RadiusChange, NewBlobs})
    end.

% Função que verifica que objetos foram comidos e devolve o índice deles
calculateBlobsEaten([], _, Res) ->
    Res;
calculateBlobsEaten([{Pid, Player}|T], Blobs, {Food, Poison, PlayersRadiusChange, UpdatedPlayers}) ->
    MinRadius = 16,
    {IdxFood, IdxPoison, RadiusChange, NewBlobs, NewPlayer} = getBlobsEaten(Player, Blobs, 1, {[], [], 0, Blobs}),
    UpdatedPlayers1 = maps:put(Pid, NewPlayer, UpdatedPlayers),
    {ok, PlayerRadius} = maps:find(radius, NewPlayer),
    if
        PlayerRadius + RadiusChange =< MinRadius ->
            calculateBlobsEaten(T, NewBlobs, {IdxFood++Food, IdxPoison++Poison, [{Pid, MinRadius - PlayerRadius}|PlayersRadiusChange], UpdatedPlayers1});
        true ->
            calculateBlobsEaten(T, NewBlobs, {IdxFood++Food, IdxPoison++Poison, [{Pid, RadiusChange}|PlayersRadiusChange], UpdatedPlayers1})
    end.

% Função que verifica se um jogador come outro jogador. Em caso afirmativo, devolve o aumento no raio
eatsPlayer(Player1, Player2) ->
    {ok, XP1} = maps:find(x, Player1),
    {ok, YP1} = maps:find(y, Player1),
    {ok, RP1} = maps:find(radius, Player1),
    {ok, XP2} = maps:find(x, Player2),
    {ok, YP2} = maps:find(y, Player2),
    {ok, RP2} = maps:find(radius, Player2),
    Dist = math:sqrt(math:pow(XP2 - XP1, 2) + math:pow(YP2 - YP1, 2)),
    if
        RP1 > RP2 andalso Dist < RP1 - RP2 ->
            Res = math:sqrt((mass(Player1) + (mass(Player2)/4))/math:pi()) - RP1,
            {true, Res};
        true ->
            false
    end.

% Função que devolve uma lista dos Pid's dos jogadores comidos por um outro jogador
getPlayersEaten(_, [], _, Res) ->
    Res;
getPlayersEaten(Player, [dead | Players], Idx, {DeadPlayersPids, RadiusChange, NewPlayers}) ->
    getPlayersEaten(Player, Players, Idx+1, {DeadPlayersPids, RadiusChange, NewPlayers});
getPlayersEaten(Player1, [{Pid, Player2} | Players], Idx, {DeadPlayersPids, RadiusChange, NewPlayers}) ->
    case eatsPlayer(Player1, Player2) of
        {true, RadiusDiff} ->
            NewPlayers1 = lists:sublist(NewPlayers, Idx-1) ++ [dead] ++ lists:nthtail(Idx, NewPlayers),
            getPlayersEaten(Player1, Players, Idx+1, {[Pid|DeadPlayersPids], RadiusChange + RadiusDiff, NewPlayers1});
        false ->
            getPlayersEaten(Player1, Players, Idx+1, {DeadPlayersPids, RadiusChange, NewPlayers})
    end.

% Função que verifica que jogadores foram comidos e devolve o índice deles
calculatePlayersEaten([], _, Res) ->
    Res;
calculatePlayersEaten([{Pid, Player}|T], Players, {DeadPlayers, PlayersRadiusChange}) ->
    {DeadPlayersPids, RadiusChange, NewPlayers} = getPlayersEaten(Player, Players, 1, {[], 0, Players}),
    calculatePlayersEaten(T, NewPlayers, {DeadPlayers ++ DeadPlayersPids, [{Pid, RadiusChange}|PlayersRadiusChange]}).

% Função que atualiza o tamanho e a pontuação dos jogadores ou coloca-os de novo em jogo (caso tenham sido capturados)
updatePlayersStatus(_, [], [], Res) ->
    Res;
updatePlayersStatus(Players, [], [Dead|T], Res) ->
    {ok, Player} = maps:find(Dead, Players),
    {ok, Username} = maps:find(username, Player),
    {ok, BestScore} = maps:find(bestScore, Player),
    NewPlayer = createPlayer(Username, maps:to_list(Res), #{up => false, down => false, left => false, right => false}, BestScore),
    Res1 = maps:put(Dead, NewPlayer, Res),
    updatePlayersStatus(Players, [], T, Res1);
updatePlayersStatus(Players, [{Pid, Change}|T], DeadPlayers, Res) ->
    {ok, Player} = maps:find(Pid, Players),
    {ok, PlayerRadius} = maps:find(radius, Player),
    {ok, Username} = maps:find(username, Player),
    {ok, BestScore} = maps:find(bestScore, Player),
    NewPlayer1 = maps:put(radius, PlayerRadius + Change, Player),
    NewScore = mass(NewPlayer1),
    scoresManager ! {newScore, {Username, NewScore}},
    NewPlayer2 = maps:put(score, NewScore, NewPlayer1),
    if 
        NewScore > BestScore ->
            NewPlayer3 = maps:put(bestScore, NewScore, NewPlayer2);
        true ->
            NewPlayer3 = NewPlayer2
    end,
    Res1 = maps:put(Pid, NewPlayer3, Res),
    updatePlayersStatus(Players, T, DeadPlayers, Res1).

% Função que substitui os objetos capturados por objetos novos
replaceBlobs(Res, [], []) ->
    Res;
replaceBlobs(Res, [Blob|T], [Idx|Indices]) ->
    Res1 = lists:sublist(Res, Idx-1) ++ [Blob] ++ lists:nthtail(Idx, Res),
    replaceBlobs(Res1, T, Indices).

% Função que calcula as interações entre os jogadores e os objetos/jogadores, alterando o tamanho dos jogadores e devolvendo o número de objetos comidos de cada tipo
calculatePlayersInteractions(MatchInfo) ->
    {ok, Players} = maps:find(players, MatchInfo),
    {ok, Blobs} = maps:find(blobs, MatchInfo),

    % Verificar se os jogadores comem blobs
    {FoodIndices, PoisonIndices, PlayersRadiusChange, UpdatedPlayers} = calculateBlobsEaten(maps:to_list(Players), Blobs, {[], [], [], Players}),
    NewPlayers = updatePlayersStatus(UpdatedPlayers, PlayersRadiusChange, [], #{}),

    % Verificar se os jogadores comem outros jogadores
    {DeadPlayers, PlayersRadiusChange1} = calculatePlayersEaten(maps:to_list(NewPlayers), maps:to_list(NewPlayers), {[], []}),
    NewPlayers1 = updatePlayersStatus(NewPlayers, PlayersRadiusChange1, DeadPlayers, #{}),

    % Atualizar a informação resultante
    MatchInfo1 = maps:update(players, NewPlayers1, MatchInfo),
    NewBlobs = createBlobs(food, length(FoodIndices), maps:to_list(NewPlayers1), []) ++ createBlobs(poison, length(PoisonIndices), maps:to_list(NewPlayers1), []),
    Blobs1 = replaceBlobs(Blobs, NewBlobs, FoodIndices ++ PoisonIndices),
    MatchInfo2 = maps:update(blobs, Blobs1, MatchInfo1),
    UpdatedPlayersPids = lists:merge([lists:sort(PlayersRadiusChange), lists:sort(PlayersRadiusChange1), lists:map(fun(Pid) -> {Pid, -1} end, lists:sort(DeadPlayers))]),
    UpdatedPlayersPids1 = lists:map(fun({Pid, _}) -> Pid end, lists:filter(fun({_, RadiusChange}) -> RadiusChange /= 0 end, UpdatedPlayersPids)),
    {MatchInfo2, FoodIndices ++ PoisonIndices, UpdatedPlayersPids1}.

% Função que inicializa uma partida
matchInitialize([], Players, Pids, PressedKeys, Time) ->
    PidMatch = self(),
    MatchSender = spawn(fun() ->
        scoresManager ! {subscribe, self()},
        receive
            {scores, Scores} ->
                Scores
        end,
        Blobs = createBlobs(poison, 50, maps:to_list(Players), []) ++ createBlobs(food, 50, maps:to_list(Players), []),
        Match = #{players => Players, blobs => Blobs, currentTime => Time, scores => Scores},
        [Player ! {initialMatch, Match, PidMatch, matchManager} || Player <- Pids],
        matchSender(Match, Pids, PidMatch, os:system_time(millisecond), false)
    end),
    match(PressedKeys, Pids, MatchSender, Time);
matchInitialize([{Pid, Username}|T], Players, Pids, PressedKeys, Time) ->
    PlayerPressedKeys = #{up => false, down => false, left => false, right => false},
    PlayerData = createPlayer(Username, maps:to_list(Players), PlayerPressedKeys, 0),
    NewPlayers = maps:put(Pid, PlayerData, Players),
    NewPressedKeys = maps:put(Pid, PlayerPressedKeys, PressedKeys),
    matchInitialize(T, NewPlayers, [Pid | Pids], NewPressedKeys, Time).

% Função que gere uma partida entre vários utilizadores
match(PressedKeys, PlayersPids, MatchSender, BeginTime) ->
    CurrentTime = os:system_time(second),
    receive
        {keyChanged, Key, Change, Pid} ->
            {ok, PlayerKeys} = maps:find(Pid, PressedKeys),
            case Change of 
                false ->
                    case Key of 
                        <<"up">> ->
                            PlayerKeys1 = maps:update(up, false, PlayerKeys);
                        <<"down">> ->
                            PlayerKeys1 = maps:update(down, false, PlayerKeys);
                        <<"left">> ->
                            PlayerKeys1 = maps:update(left, false, PlayerKeys);
                        <<"right">> ->
                            PlayerKeys1 = maps:update(right, false, PlayerKeys)
                    end;
                true ->
                    T = os:system_time(millisecond),
                    case Key of 
                        <<"up">> ->
                            PlayerKeys1 = maps:update(up, {true, T}, PlayerKeys);
                        <<"down">> ->
                            PlayerKeys1 = maps:update(down, {true, T}, PlayerKeys);
                        <<"left">> ->
                            PlayerKeys1 = maps:update(left, {true, T}, PlayerKeys);
                        <<"right">> ->
                            PlayerKeys1 = maps:update(right, {true, T}, PlayerKeys)
                    end
            end,
            PressedKeys1 = maps:update(Pid, PlayerKeys1, PressedKeys),
            MatchSender ! {Pid, PlayerKeys1},
            match(PressedKeys1, PlayersPids, MatchSender, BeginTime);
        {leave, _} ->
            % Se um dos jogadores abandonar a partida, terminar a mesma
            MatchSender ! exit,
            matchManager ! {matchOver, self()}
    after
        (120 - (CurrentTime - BeginTime)) * 1000 ->
            % Se o tempo da partida superar os 2 minutos, avisar os jogadores de que a partida terminou
            MatchSender ! exit,
            matchManager ! {matchOver, self()}
    end.

% Função que atualiza a informação da partida e a envia aos clientes, 30 vezes por segundo
matchSender(Match, PlayersPids, PidMatch, PrevSimulation, ScoresUpdated) ->
    TimeBetweenUpdates = 33,
    CurrentTime = os:system_time(millisecond),
    if
        CurrentTime - PrevSimulation >= TimeBetweenUpdates ->
            Match1 = simulateSendMatch(Match, PlayersPids, PidMatch, ScoresUpdated),
            matchSender(Match1, PlayersPids, PidMatch, os:system_time(millisecond), false);
        true ->
            receive
                {scores, Scores} ->
                    Match1 = maps:update(scores, Scores, Match),
                    matchSender(Match1, PlayersPids, PidMatch, PrevSimulation, true);
                {Pid, PressedKeys} ->
                    {ok, Players} = maps:find(players, Match),
                    {ok, Player} = maps:find(Pid, Players), 
                    Player1 = maps:update(pressedKeys, PressedKeys, Player),
                    Players1 = maps:update(Pid, Player1, Players),
                    Match1 = maps:update(players, Players1, Match),
                    matchSender(Match1, PlayersPids, PidMatch, PrevSimulation, ScoresUpdated);
                exit ->
                    {ok, Players} = maps:find(players, Match),
                    Leaderboard = getLeaderboard(maps:to_list(Players), []),
                    [Player ! {matchOver, Leaderboard, PidMatch} || Player <- PlayersPids],
                    scoresManager ! {unsubscribe, self()},
                    done
            after
                TimeBetweenUpdates - (CurrentTime - PrevSimulation) ->
                    Match1 = simulateSendMatch(Match, PlayersPids, PidMatch, ScoresUpdated),
                    matchSender(Match1, PlayersPids, PidMatch, os:system_time(millisecond), false)
            end
    end.

% Função que devolve a tabela de pontuações de uma partida
getLeaderboard([], Res) ->
    lists:sort(fun({_, Score1}, {_, Score2}) -> Score1 >= Score2 end, Res);
getLeaderboard([{_, Player}|T], Res) ->
    {ok, Username} = maps:find(username, Player),
    {ok, Score} = maps:find(bestScore, Player),
    getLeaderboard(T, [{Username, Score}|Res]).

% Função que atualiza a posição dos jogadores, tendo em conta as teclas pressionadas por estes
updatePos(X, Y, _, _, _, _, []) ->
    {X,Y};
updatePos(X, Y, Acc, Radius, CT, SpeedBonus, [{K,{true,T}}|Keys]) ->
    MaxVel = 8 + SpeedBonus,
    case K of
        up ->
            Y1 = Y - min(MaxVel, Acc * (CT - T) / 500),
            Y2 = max(Y1, 0),
            X2 = X;
        down ->
            Y1 = Y + min(MaxVel, Acc * (CT - T) / 500),
            Y2 = min(Y1, 768),
            X2 = X;
        left ->
            X1 = X - min(MaxVel, Acc * (CT - T) / 500),
            X2 = max(X1, 0),
            Y2 = Y;
        right ->
            X1 = X + min(MaxVel, Acc * (CT - T) / 500),
            X2 = min(X1, 1366),
            Y2 = Y
    end,
    updatePos(X2, Y2, Acc, Radius, CT, SpeedBonus, Keys).

% Função que atualiza a informação dos jogadores, tendo em conta as teclas pressionadas por estes
updatePlayers(_, [], Res, UpdatedPlayers) ->
    {Res, UpdatedPlayers};
updatePlayers(CT, [{Pid,PlayerData}|Players], Res, UpdatedPlayers) ->
    BonusDuration = 10000, % Duração dos bónus, em milissegundos
    {ok, Speed} = maps:find(speedBonus, PlayerData),
    if
        Speed == -1 ->
            SpeedBonus = 0,
            PlayerData1 = PlayerData,
            PlayerWasUpdated = false;
        CT - Speed >= BonusDuration ->
            SpeedBonus = 0,
            PlayerData1 = maps:update(speedBonus, -1, PlayerData),
            PlayerWasUpdated = true;
        true ->
            SpeedBonus = 3,
            PlayerData1 = PlayerData,
            PlayerWasUpdated = false
    end,
    {ok, PressedKeys} = maps:find(pressedKeys, PlayerData1),
    {ok, Radius} = maps:find(radius, PlayerData1),
    {ok, X} = maps:find(x, PlayerData1),
    {ok, Y} = maps:find(y, PlayerData1),
    Acc = 1/(Radius * Radius * math:pi()) + 3 + SpeedBonus,
    TrueKeys = maps:filter(fun(_, V) ->
        case V of 
            {true, _} ->
                true; 
            _ ->
                false
        end
    end, PressedKeys),
    {X1, Y1} = updatePos(X, Y, Acc, Radius, CT, SpeedBonus, maps:to_list(TrueKeys)),
    PD1 = maps:update(x, X1, PlayerData1),
    PD2 = maps:update(y, Y1, PD1),
    {ok, Invisibility} = maps:find(invisibilityBonus, PD2),
    if
        Invisibility == -1 ->
            PD3 = PD2,
            PlayerWasUpdated1 = PlayerWasUpdated;
        CT - Invisibility >= BonusDuration ->
            PD3 = maps:update(invisibilityBonus, -1, PD2),
            PlayerWasUpdated1 = true;
        true ->
            PD3 = PD2,
            PlayerWasUpdated1 = PlayerWasUpdated
    end,
    Res1 = maps:put(Pid, PD3, Res),
    case {X1, Y1} of
        {X, Y} ->
            PlayerWasUpdated2 = PlayerWasUpdated1;
        _ ->
            PlayerWasUpdated2 = true
    end,
    if
        PlayerWasUpdated2 ->
            UpdatedPlayers1 = [Pid|UpdatedPlayers];
        true ->
            UpdatedPlayers1 = UpdatedPlayers
    end,
    updatePlayers(CT, Players, Res1, UpdatedPlayers1).

% Função que simula a partida num dado momento, enviando o resultado obtido aos jogadores
simulateSendMatch(Match, PlayersPids, PidMatch, ScoresUpdated) ->
    UpdatedInfo = #{},

    % Atualizar o tempo atual
    CT = os:system_time(second),
    case maps:find(currentTime, Match) of
        {ok, CT} ->
            Match1 = Match,
            UpdatedInfo1 = UpdatedInfo;
        _ ->
            Match1 = maps:update(currentTime, CT, Match),
            UpdatedInfo1 = maps:put(currentTime, CT, UpdatedInfo)
    end,

    % Atualizar a posição dos jogadores e o estado dos bónus
    {ok, Players} = maps:find(players, Match1),
    {Players1, UpdatedPlayersPids1} = updatePlayers(os:system_time(millisecond), maps:to_list(Players), #{}, []),
    Match2 = maps:update(players, Players1, Match1),

    % Verificar se os jogadores comem blobs ou outros jogadores, atualizando as suas pontuações e tamanhos
    {Match3, UpdatedBlobs, UpdatedPlayersPids2} = calculatePlayersInteractions(Match2),
    UpdatedInfo2 = updatePlayersBlobsInfo(Match3, UpdatedInfo1, UpdatedBlobs, lists:merge(lists:sort(UpdatedPlayersPids1), lists:sort(UpdatedPlayersPids2)), [], #{}),

    % Atualizar as melhores pontuações, se tiverem sido atualizadas pelo gestor de pontuações
    case ScoresUpdated of
        true ->
            {ok, Scores} = maps:find(scores, Match),
            UpdatedInfo3 = maps:put(scores, Scores, UpdatedInfo2);
        false ->
            UpdatedInfo3 = UpdatedInfo2
    end,

    % Enviar as informações atualizadas aos jogadores, se estas exitirem
    Size = maps:size(UpdatedInfo3),
    if
        Size == 0 ->
            nothingToSend;
        true ->
            [Player ! {updateMatch, UpdatedInfo3, PidMatch} || Player <- PlayersPids]
    end,
    Match3.

% Função que guarda a informação dos jogadores e dos objetos que foi alterada
updatePlayersBlobsInfo(_, Res, [], [], TmpBlobs, TmpPlayers) ->
    NumBlobs = length(TmpBlobs),
    if
        NumBlobs == 0 ->
            Res1 = Res;
        true ->
            Res1 = maps:put(blobs, TmpBlobs, Res)
    end,
    NumPlayers = maps:size(TmpPlayers),
    if
        NumPlayers == 0 ->
            Res2 = Res1;
        true ->
            Res2 = maps:put(players, TmpPlayers, Res1)
    end,
    Res2;
updatePlayersBlobsInfo(MatchInfo, Res, [], [P|RemainingPlayers], TmpBlobs, TmpPlayers) ->
    {ok, Players} = maps:find(players, MatchInfo),
    {ok, Player} = maps:find(P, Players),
    TmpPlayers1 = maps:put(P, Player, TmpPlayers),
    updatePlayersBlobsInfo(MatchInfo, Res, [], RemainingPlayers, TmpBlobs, TmpPlayers1);
updatePlayersBlobsInfo(MatchInfo, Res, [B|RemainingBlobs], UpdatedPlayers, TmpBlobs, TmpPlayers) ->
    {ok, Blobs} = maps:find(blobs, MatchInfo),
    updatePlayersBlobsInfo(MatchInfo, Res, RemainingBlobs, UpdatedPlayers, [{B-1, lists:nth(B, Blobs)}|TmpBlobs], TmpPlayers).
