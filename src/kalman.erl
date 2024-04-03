-module(kalman).

-export([kf_predict/3, kf_update/4]).
-export([kf/6, ekf/6]).

%% see https://en.wikipedia.org/wiki/Kalman_filter


%% A kalman filter without control input
kf({X0, P0}, F, H, Q, R, Z) ->  
    {Xp, Pp} = kf_predict({X0, P0}, F, Q),
    kf_update({Xp, Pp}, H, R, Z).


kf_predict({X0, P0}, F, Q) ->
    Xp = mat:'*'(F, X0),                            % From TFE Sébastien Kalbusch: eq. 2.1
    Pp = mat:eval([F, '*', P0, '*´', F, '+', Q]),   % Eq. 2.2
    {Xp, Pp}.


kf_update({Xp, Pp}, H, R, Z) ->
    S = mat:eval([H, '*', Pp, '*´', H, '+', R]),    % Part of eq. 2.3
    Sinv = mat:inv(S), 
    K = mat:eval([Pp, '*´', H, '*', Sinv]),         % Eq. 2.3
    Y = mat:'-'(Z, mat:'*'(H, Xp)),                 % Part of eq. 2.4
    X1 = mat:eval([K, '*', Y, '+', Xp]),            % Eq. 2.4
    P1 = mat:'-'(Pp, mat:eval([K, '*', H, '*', Pp])), % Eq. 2.5
    {X1, P1}.


%% An extended kalman filter without control input, [X0, P0, Q, R, Z] must be mat matrices, [F, Jf, H, Jh] must be functions
ekf({X0, P0}, {F, Jf}, {H, Jh}, Q, R, Z) ->
    % Prediction
    Xp = F(X0),
    Jfx = Jf(X0),
    Pp = mat:eval([Jfx, '*', P0, '*´', Jfx, '+', Q]),

    % Update
    Jhx = Jh(Xp),
    S = mat:eval([Jhx, '*', Pp, '*´', Jhx, '+', R]),
    Sinv = mat:inv(S),
    K = mat:eval([Pp, '*´', Jhx, '*', Sinv]),
    Y = mat:'-'(Z, H(Xp)),
    X1 = mat:eval([K, '*', Y, '+', Xp]),
    P1 = mat:'-'(Pp, mat:eval([K, '*', Jhx, '*', Pp])),
    {X1, P1}.