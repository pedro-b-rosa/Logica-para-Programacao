% 106426   Pedro Barroso Rosa
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

%------------------------------------------
% vizinhanca(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhança
% Viz eh uma lista com as coordenadas da vizinhaça
%------------------------------------------
vizinhanca((CordLin, CordCol), Viz):-
    cimaBaixo(CordLin, Cima, Baixo),
    esquerdaDireita(CordCol, Esquerda, Direita),
    Viz = [(Cima, CordCol), (CordLin, Esquerda), (CordLin, Direita), (Baixo, CordCol)].

%------------------------------------------
% vizinhancaAlargada(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhança
% VizAl eh uma lista com as coordenadas da vizinhaça alargada
%------------------------------------------
vizinhancaAlargada((CordLin, CordCol), VizAl):-
    vizinhanca((CordLin, CordCol), Viz),
    cimaBaixo(CordLin, Cima, Baixo),
    esquerdaDireita(CordCol, Esquerda, Direita),
    Lista = [(Cima, Esquerda),(Cima, Direita),(Baixo, Esquerda),(Baixo, Direita)],
    juntar(Lista, Viz, VizAl).
    

% As coordenadas das linhas de cima e de baixo
cimaBaixo(CordLin, Cima, Baixo):-
    Baixo is CordLin + 1,
    Cima is CordLin - 1.

% As coordenadas das colunas ah direita e ah esquerda
esquerdaDireita(CordCol, Esquerda, Direita):-
    Esquerda is CordCol - 1,
    Direita is CordCol + 1.

% Junta e ordena duas listas
juntar(L,[],L).
juntar([(Lin1, Col1)|R1], [(Lin2, Col2)|R2], [(Lin1, Col1)|L]):-
    Lin1 < Lin2,
    juntar(R1, [(Lin2, Col2)|R2], L).
juntar([(Lin1, Col1)|R1], [(Lin2, Col2)|R2], [(Lin1, Col1)|L]):-
    Lin1 =< Lin2, Col1 < Col2,
    juntar(R1, [(Lin2, Col2)|R2], L).
juntar([(Lin1, Col1)|R1], [(Lin2, Col2)|R2], [(Lin2, Col2)|L]):-
    Lin1 > Lin2,
    juntar([(Lin1, Col1)|R1], R2, L).
juntar([(Lin1, Col1)|R1], [(Lin2, Col2)|R2], [(Lin2, Col2)|L]):-
    Lin1 >= Lin2, Col1 > Col2,
    juntar([(Lin1, Col1)|R1], R2, L).

%------------------------------------------
% todasCelulas(Tabuleiro, TodasCelulas)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TodasCelulas eh uma lista com as coordenadas de todas as celulas do tabuleiro
%------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas):- todasCelulas(Tabuleiro, 1, TodasCelulas).
todasCelulas([], _, []).
todasCelulas([P | R], Linha, Celulas):-
    linhas(P, Linha, Coor),
    Linha_N is Linha + 1,
    todasCelulas(R, Linha_N, RestoCelulas),
    append(Coor, RestoCelulas, Celulas).

% Devolve uma lista com as coordenadas de uma linha
linhas(Lista, Linha, Coor):- linhas(Lista, Linha, 0, Coor).
linhas([], _, _, []).
linhas([_|R], Linha, Coluna, [(Linha, Coluna_N) | Coor]):-
    Coluna_N is Coluna + 1,
    linhas(R, Linha, Coluna_N, Coor).

%------------------------------------------
% todasCelulas(Tabuleiro, TodasCelulas, Objecto)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TodasCelulas eh uma lista com as coordenadas de todas as celulas do tabuleiro
% Objecto eh uma tenda (t), relva (r), árvore (a) ou ainda uma variável
%------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas, Objecto).