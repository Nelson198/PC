-module(login).
-export([loginHandler/1, registerLogin/2, unregister/2, login/2, logout/1]).

% Função que gere o login dos utilizadores
loginHandler(Map) ->
    receive
        {registerLogin, Username, Passwd, From} ->
            case maps:find(Username, Map) of
                {ok, _} ->
                    From ! {loginManager, userExists},
                    loginHandler(Map);
                error ->
                    From ! {loginManager, ok},
                    loginHandler(maps:put(Username, {Passwd, true}, Map))
            end;
        {unregister, Username, Passwd, From} ->
            case maps:find(Username, Map) of
                {ok, {P, _}} ->
                    case P of
                        Passwd ->
                            From ! {loginManager, ok},
                            loginHandler(maps:remove(Username, Map));
                        _ ->
                            From ! {loginManager, wrongPassword},
                            loginHandler(Map)
                    end;
                error ->
                    From ! {loginManager, notExists},
                    loginHandler(Map)
            end;
        {login, Username, Passwd, From} ->
            case maps:find(Username, Map) of
                {ok, {P, O}} ->
                    case {P, O} of
                        {Passwd, false} ->
                            From ! {loginManager, ok},
                            loginHandler(maps:update(Username, {Passwd, true}, Map));
                        {Passwd, true} ->
                            From ! {loginManager, alreadyLoggedIn},
                            loginHandler(Map);
                        _ ->
                            From ! {loginManager, wrongPassword},
                            loginHandler(Map)
                    end;
                error ->
                    From ! {loginManager, inexistentUser},
                    loginHandler(Map)
            end;
        {logout, Username, From} ->
            From ! {loginManager, ok},
            case maps:find(Username, Map) of
                {ok, {P, _}} ->
                    loginHandler(maps:update(Username, {P, false}, Map));
                error ->
                    loginHandler(Map)
            end
    end.

% Função que cria a conta de um utilizador
registerLogin(Username, Passwd) ->
    loginManager ! {registerLogin, Username, Passwd, self()},
    receive
        {loginManager, Res} -> Res
    end.

% Função que elimina a conta de um utilizador
unregister(Username, Passwd) ->
    loginManager ! {unregister, Username, Passwd, self()},
    receive
        {loginManager, Res} -> Res
    end.

% Função que inicia a sessão de um utilizador
login(Username, Passwd) ->
    loginManager ! {login, Username, Passwd, self()},
    receive
        {loginManager, Res} -> Res
    end.

% Função que termina a sessão de um utilizador
logout(Username) ->
    loginManager ! {logout, Username, self()},
    receive
        {loginManager, Res} -> Res
    end.
