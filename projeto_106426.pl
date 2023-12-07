% 106426   Pedro Barroso Rosa
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

%------------------------------------------
% vizinhanca(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhança
% Viz eh uma lista com as coordenadas da vizinhaça
%------------------------------------------
vizinhanca((CordLin,CordCol),Viz):-
    cimaBaixo(CordL,Cima,Baixo),
    esquerdaDireita(CordC,Esquerda,Direita),
    Viz = [(Cima,CordC), (CordL,Esquerda), (CordL,Direita), (Baixo,CordC)].

%------------------------------------------
% vizinhancaAlargada(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhança
% VizAl eh uma lista com as coordenadas da vizinhaça alargada
%------------------------------------------
vizinhancaAlargada((CordL,CordC),VizAl):-
    vizinhanca((CordL,CordC),Viz),

% As coordenadas das linhas de cima e de baixo
cimaBaixo(CordL,Cima,Baixo):-
    Baixo is CordL + 1,
    Cima is CordL - 1.

% As coordenadas das colunas ah direita e ah esquerda
esquerdaDireita(CordC,Esquerda,Direita):-
    Esquerda is CordC - 1,
    Direita is CordC + 1.

% Ultimo item da lista
ultimaVizinhanca([Ult],Ult).
ultimaVizinhanca([_|R],Ult):-
    ultimaVizinhanca(R,Ult).