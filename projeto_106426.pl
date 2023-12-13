% 106426   Pedro Barroso Rosa
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.

%------------------------------------------
% vizinhanca(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhanca
% Viz eh uma lista com as coordenadas da vizinhaca
%------------------------------------------
vizinhanca((CordLin, CordCol), Viz):-
    cimaBaixo(CordLin, Cima, Baixo),
    esquerdaDireita(CordCol, Esquerda, Direita),
    Viz = [(Cima, CordCol), (CordLin, Esquerda), (CordLin, Direita), (Baixo, CordCol)].

%------------------------------------------
% vizinhancaAlargada(Cord,Viz)
% Cord sao as coordenadas a que vamos calcular a vizinhanca
% VizAl eh uma lista com as coordenadas da vizinhaca alargada
%------------------------------------------
vizinhancaAlargada((CordLin, CordCol), VizAl):-
    vizinhanca((CordLin, CordCol), Viz),
    cimaBaixo(CordLin, Cima, Baixo),
    esquerdaDireita(CordCol, Esquerda, Direita),
    Lista = [(Cima, Esquerda),(Cima, Direita),(Baixo, Esquerda),(Baixo, Direita)],
    union(Lista,Viz,Lista1),
    sort(Lista1, VizAl).
    
% As coordenadas das linhas de cima e de baixo
cimaBaixo(CordLin, Cima, Baixo):-
    Baixo is CordLin + 1,
    Cima is CordLin - 1.

% As coordenadas das colunas ah direita e ah esquerda
esquerdaDireita(CordCol, Esquerda, Direita):-
    Esquerda is CordCol - 1,
    Direita is CordCol + 1.

%------------------------------------------
% todasCelulas(Tabuleiro, TodasCelulas)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TodasCelulas eh uma lista com as coordenadas de todas as celulas do tabuleiro
%------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas):-
    findall((Linhas, Colunas), (nth1(Linhas, Tabuleiro, ListaDeLinhas),nth1(Colunas, ListaDeLinhas,_)), TodasCelulas).

%------------------------------------------
% todasCelulas(Tabuleiro, TodasCelulas, Objecto)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TodasCelulas eh uma lista com as coordenadas de todas as celulas do tabuleiro
% Objecto pode ser uma tenda (t), relva (r), arvore (a) ou uma variavel
%------------------------------------------
todasCelulas(Tabuleiro, TodasCelulas, Objecto):-
    findall((Linhas, Colunas), (nth1(Linhas, Tabuleiro, ListaDeLinhas), nth1(Colunas, ListaDeLinhas, Celula), ((var(Objecto), var(Celula)); Celula == Objecto)), TodasCelulas).

%------------------------------------------
% calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% ContagemLinhas e ContagemColunas sao listas como o numero de vez que um objecto de repete nas linhas ou colunas
% Objecto pode ser uma tenda (t), relva (r), arvore (a) ou uma variavel
%------------------------------------------
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    contagem(Tabuleiro, ContagemLinhas, Objecto),
    transpose(Tabuleiro, TabuleiroTransposto),
    contagem(TabuleiroTransposto, ContagemColunas, Objecto).

% Conta o numero de vezes que um objecto aprarece e devolve uma lista com a soma por linhas
contagem([],[],_).
contagem([Linha|R], [Soma|Contagem], Objecto):-
    findall(Coluna, (nth1(Coluna, Linha, Celula), ((var(Objecto), var(Celula)); Celula == Objecto)), Lista),
    contagem(R, Contagem, Objecto),
    length(Lista, Soma).

%------------------------------------------
% celulaVazia(Tabuleiro, (L, C))
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% L e C Correspodem ah linha e ah coluna que queremos verificar
%------------------------------------------
celulaVazia(Tabuleiro, (L, C)):-
    todasCelulas(Tabuleiro, TodasCelulasTendas, t),
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    union(TodasCelulasTendas, TodasCelulasArvores, TodasCelulasCheias),
    not(member((L,C), TodasCelulasCheias)).

%------------------------------------------
% insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C))
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TendaOuRelva Objecto que queremos inserir pode (t) tenda ou (r) relva
% L e C Correspodem ah linha e ah coluna onde queremos inserir o Objecto
%------------------------------------------
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)):-
    celulaVazia(Tabuleiro, (L, C)),
    nth1(L, Tabuleiro, ListaDeLinhas),
    nth1(C, ListaDeLinhas, TendaOuRelva).
insereObjectoCelula(_,_,_).

%------------------------------------------
% insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2))
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% TendaOuRelva Objecto que queremos inserir pode (t) tenda ou (r) relva
% L eh a linha onde queremos inserir os Objectos
% C1 e C2 sao os limites das colunas em que vamos inserir os Objectos
%------------------------------------------
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    criadorDeListaEntreValores(C1, C2, ListaDeColunas),
    insereVariosObjectos(Tabuleiro, TendaOuRelva, L, ListaDeColunas).

% Predicado que devolve uma lista com os elementos entre dois valores
criadorDeListaEntreValores(C2, C2, [C2]).
criadorDeListaEntreValores(C1, C2, [C1|Lista]):-
    C1_N is C1 + 1,
    criadorDeListaEntreValores(C1_N, C2, Lista).

% chama a funcao insereObjectoCelula/3 ao longo de uma lista de coordenadas
insereVariosObjectos(_, _, _, []).
insereVariosObjectos(Tabuleiro, TendaOuRelva, L, [C|R]):-
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)),
    insereVariosObjectos(Tabuleiro, TendaOuRelva, L, R).

%------------------------------------------
% relva(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
%------------------------------------------
relva((Tabuleiro, L, C)):-
    length(L, Comprimento),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    insereLinhas(Tabuleiro, L, ContagemLinhas, Comprimento, r),
    transpose(Tabuleiro, TabuleiroTransposto),
    insereLinhas(TabuleiroTransposto, C, ContagemColunas, Comprimento, r).
    
% Insere a relva no tabuleiro
insereLinhas(Tabuleiro, Inf, Contagem, Comprimento, Objecto):- insereLinhas(Tabuleiro, Inf, Contagem, Comprimento, Objecto, 1).
insereLinhas(_,[],[],_,_,_).
insereLinhas(Tabuleiro, [P1|R1], [P1|R2], Comprimento, Objecto, L):-
    insereObjectoEntrePosicoes(Tabuleiro, Objecto, (L, 1), (L, Comprimento)),
    L_N is L + 1,
    insereLinhas(Tabuleiro, R1, R2, Comprimento, Objecto, L_N).
insereLinhas(Tabuleiro, [P1|R1], [P2|R2], Comprimento, Objecto, L):-
    P1 \== P2,
    L_N is L + 1,
    insereLinhas(Tabuleiro, R1, R2, Comprimento, Objecto, L_N).

%------------------------------------------
% inacessiveis(Tabuleiro)
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro 
%------------------------------------------
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    maplist(vizinhanca, TodasCelulasArvores, TodasVizinhancas),
    append(TodasVizinhancas, Vizinhancas),
    todasCelulas(Tabuleiro, TodasCelulas),
    subtract(TodasCelulas, Vizinhancas, CelulasInacessiveis),
    insereRelva(Tabuleiro, CelulasInacessiveis).

% Insere a relva no tabuleiro
insereRelva(_,[]).
insereRelva(Tabuleiro, [P|R]):-
    insereObjectoCelula(Tabuleiro, r, P),
    insereRelva(Tabuleiro, R).

%------------------------------------------
% aproveita(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
%------------------------------------------
aproveita((Tabuleiro, L, C)):-
    length(L, Comprimento),
    calculaVazios(Tabuleiro, ContagemLinhas),
    insereLinhas(Tabuleiro, L, ContagemLinhas, Comprimento, t),
    transpose(Tabuleiro, TabuleiroTransposto),
    calculaVazios(TabuleiroTransposto, ContagemColunas),
    insereLinhas(TabuleiroTransposto, C, ContagemColunas, Comprimento, t).

% Devolve uma lista com o numero de espacos vazios por linha
calculaVazios([],[]).
calculaVazios([Linha|R], [Soma|Contagem]):-
    findall(Coluna, (nth1(Coluna, Linha, Celula), var(Celula)), Lista),
    length(Lista, Soma),
    calculaVazios(R, Contagem).

%------------------------------------------
% limpaVizinhancas(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
%------------------------------------------
limpaVizinhancas((Tabuleiro, _, _)):-
    todasCelulas(Tabuleiro, TodasCelulas, t),
    maplist(vizinhancaAlargada, TodasCelulas, TodasVizinhancas),
    append(TodasVizinhancas, Vizinhancas),
    insereRelva(Tabuleiro, Vizinhancas).

%------------------------------------------
% unicaHipotese(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
%------------------------------------------
unicaHipotese((Tabuleiro, _, _)):-
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    maplist(vizinhanca, TodasCelulasArvores, TodasVizinhancas),
    append(TodasVizinhancas, Vizinhancas),