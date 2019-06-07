-module(scores).
-export([scoresHandler/2]).

% Função que gere as melhores pontuações
scoresHandler(Scores, Subscribed) ->
    receive
        {getScores, Pid} ->
            Pid ! {scores, Scores},
            scoresHandler(Scores, Subscribed);
        {subscribe, Pid} ->
            Pid ! {scores, Scores},
            scoresHandler(Scores, [Pid|Subscribed]);
        {unsubscribe, Pid} ->
            scoresHandler(Scores, lists:delete(Pid, Subscribed));
        {newScore, {Username, Score}} ->
            case lists:filter(fun({Usr, _}) -> Usr == Username end, Scores) of
                [] ->
                    case length(Scores) of
                        5 ->
                            {_, Lowest} = lists:last(Scores),
                            if
                                Score > Lowest ->
                                    Scores1 = addScore({Username, Score}, lists:droplast(Scores)),
                                    sendScores(Scores1, Subscribed);
                                true ->
                                    Scores1 = Scores
                            end;
                        _ ->
                            Scores1 = addScore({Username, Score}, Scores),
                            sendScores(Scores1, Subscribed)
                    end;
                [{_, CurrentScore}] when Score =< CurrentScore ->
                    Scores1 = Scores;
                [{_, CurrentScore}] when Score >= CurrentScore ->
                    Scores1 = addScore({Username, Score}, lists:filter(fun({Usr, _}) -> Usr /= Username end, Scores)),
                    sendScores(Scores1, Subscribed)
            end,
            scoresHandler(Scores1, Subscribed)
    end.

% Função que envia as novas melhores pontuações a quem pretende recebê-las
sendScores(Scores, Receivers) ->
    [R ! {scores, Scores} || R <- Receivers].

% Função que adiciona uma pontuação à lista das melhores pontuações
addScore(Score, []) ->
    [Score];
addScore({Username1, S1}, [{_, S2}|_]=Scores) when S1 > S2 ->
    [{Username1, S1}|Scores];
addScore({Username1, S1}, [{Username2, S2}|T]) when S1 =< S2 ->
    [{Username2, S2}|addScore({Username1, S1}, T)].
