%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Auxiliary Prolog programs for the book             %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: varsin/2                               %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


varsin(Term,Vars):-
        varsin(Term,[],V),
        sort(V,Vars).

varsin(V,Vars,[V|Vars]):-
        var(V),!.
varsin(Term,V0,V):-
        functor(Term,_,N),
        varsin_args(N,Term,V0,V).

varsin_args(0,_,Vars,Vars).
varsin_args(N,Term,V0,V):-
        N>0, N1 is N-1,
        arg(N,Term,ArgN),
        varsin(ArgN,V0,V1),
        varsin_args(N1,Term,V1,V).

