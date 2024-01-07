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
contagem([],[],_):-!.
contagem([Linha|R], [Soma|Contagem], Objecto):-
    findall(Coluna, (nth1(Coluna, Linha, Celula), ((var(Objecto), var(Celula)); Celula == Objecto)), Lista),
    contagem(R, Contagem, Objecto),
    length(Lista, Soma).

%------------------------------------------
% celulaVazia(Tabuleiro, (L, C))
% Tabuleiro eh a variavel com a matriz que representa o tabuleiro
% L e C Correspodem ah linha e ah coluna que queremos verificar
% Devolve true se a celula estiver vazia
%------------------------------------------
celulaVazia(Tabuleiro, (L, C)):-
    todasCelulas(Tabuleiro, TodasCelulasTendas, t),
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    union(TodasCelulasTendas, TodasCelulasArvores, TodasCelulasCheias),
    \+ member((L,C), TodasCelulasCheias).

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
criadorDeListaEntreValores(C1, C2, Lista) :-
    C1 =< C2,
    criadorDeListaEntreValoresIter(C1, C2, Lista).
criadorDeListaEntreValoresIter(C, C, [C]).
criadorDeListaEntreValoresIter(C1, C2, [C1 | Lista]) :-
    C1 < C2,
    C1_Next is C1 + 1,
    criadorDeListaEntreValoresIter(C1_Next, C2, Lista).

% chama a funcao insereObjectoCelula/3 ao longo de uma lista de coordenadas
insereVariosObjectos(_, _, _, []):-!.
insereVariosObjectos(Tabuleiro, TendaOuRelva, L, [C|R]):-
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)),
    insereVariosObjectos(Tabuleiro, TendaOuRelva, L, R).

%------------------------------------------
% relva(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
% Apos a aplicacao do predicado, tem relva em todas as linhas/colunas cujo numero de tendas ja atingiu o numero de tendas possivel nessas linhas/colunas
%------------------------------------------
relva((Tabuleiro, L, C)):-
    length(L, Comprimento),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    insereLinhas(Tabuleiro, L, ContagemLinhas, Comprimento, r),
    transpose(Tabuleiro, TabuleiroTransposto),
    insereLinhas(TabuleiroTransposto, C, ContagemColunas, Comprimento, r).
    
% Insere a relva no tabuleiro
insereLinhas(Tabuleiro, Inf, Contagem, Comprimento, Objecto):- insereLinhas(Tabuleiro, Inf, Contagem, Comprimento, Objecto, 1).
insereLinhas(_,[],[],_,_,_):-!.
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
% Apos a aplicacao do predicado, tem relva em todas as celulas que nao sao acessiveis
%------------------------------------------
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    maplist(vizinhanca, TodasCelulasArvores, TodasVizinhancas),
    append(TodasVizinhancas, Vizinhancas),
    todasCelulas(Tabuleiro, TodasCelulas),
    subtract(TodasCelulas, Vizinhancas, CelulasInacessiveis),
    insereRelva(Tabuleiro, CelulasInacessiveis).

% Insere a relva no tabuleiro
insereRelva(_,[]):-!.
insereRelva(Tabuleiro, [P|R]):-
    insereObjectoCelula(Tabuleiro, r, P),
    insereRelva(Tabuleiro, R).

%------------------------------------------
% aproveita(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
% Apos a aplicacao do predicado, tem tendas em todas as linhas e colunas as quais faltavam colocar X tendas e que tinham exatamente X posicoes livres
%------------------------------------------
aproveita((Tabuleiro, L, C)):-
    length(L, Comprimento),
    calculaVazios(Tabuleiro, ContagemLinhas),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhasTendas, ContagemColunasTendas, t),
    maplist(sub, ContagemLinhas, ContagemLinhasTendas, ContagemL),
    insereLinhas(Tabuleiro, L, ContagemL, Comprimento, t),
    transpose(Tabuleiro, TabuleiroTransposto),
    calculaVazios(TabuleiroTransposto, ContagemColunas),
    maplist(sub, ContagemColunas, ContagemColunasTendas, ContagemC),
    insereLinhas(TabuleiroTransposto, C, ContagemC, Comprimento, t),
    transpose(TabuleiroTransposto, Tabuleiro).

% Subetrai dois numeros
sub(X, Y, Sub):-
    Sub is X - Y.

% Devolve uma lista com o numero de espacos vazios por linha
calculaVazios([],[]):-!.
calculaVazios([Linha|R], [Soma|Contagem]):-
    findall(Coluna, (nth1(Coluna, Linha, Celula), var(Celula)), Lista),
    length(Lista, Soma),
    calculaVazios(R, Contagem).

%------------------------------------------
% limpaVizinhancas(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
% Apos a aplicacao do predicado, tem relva na vizinhanca alargada de todas as tendas
%------------------------------------------
limpaVizinhancas((Tabuleiro, _, _)):-
    todasCelulas(Tabuleiro, TodasCelulas, t),
    maplist(vizinhancaAlargada, TodasCelulas, TodasVizinhancas),
    append(TodasVizinhancas, Vizinhancas),
    insereRelva(Tabuleiro, Vizinhancas).

%------------------------------------------
% unicaHipotese(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
% Apos a aplicacao do predicado, todas as arvores que tinham apenas uma posicao livre na sua vizinhanca que lhes permitia ficar ligadas a uma tenda, tem agora uma tenda nessa posicao
%------------------------------------------
unicaHipotese((Tabuleiro, _, _)):-
    vazias(Tabuleiro, VizLivres),
    insereUnicaHipotese(Tabuleiro, VizLivres).

% Devolve as Vizinhancas vazias
vazias(Tabuleiro, VizLivres):-
    todasCelulas(Tabuleiro, CelulasArvores, a),
    maplist(vizinhanca, CelulasArvores, Vizinhancas),
    todasCelulas(Tabuleiro, TodasCelulas),
    maplist(verificaVizinhancasDentro(TodasCelulas, Tabuleiro), Vizinhancas, VizinhancasDentro),
    maplist(listaVizLivres(Tabuleiro), VizinhancasDentro, VizLivres).

% Verifica se nao tem nenhuma tenda nessa vizinhaca
verificaTenda(ListaViz, TodasCelulasTendas, VizinhacaPosivel) :-
    intersection(ListaViz, TodasCelulasTendas, Intersecao),
    (Intersecao = [] -> VizinhacaPosivel = ListaViz ; VizinhacaPosivel = []).

% Verifica se as coordenadas estao todas dentro do tabuleiro para cada vizinhanca
verificaVizinhancasDentro(TodasCelulas, Tabuleiro, Vizinhos, VizinhacaPosivel):-
    estaDentro(Vizinhos, TodasCelulas, VizinhosDentro),
    todasCelulas(Tabuleiro, TodasCelulasTendas, t),
    verificaTenda(VizinhosDentro, TodasCelulasTendas, VizinhacaPosivel).

% Verifica se as coordenadas estao todas dentro do tabuleiro dentro de uma vizinhaca
estaDentro([],_,[]):-!.
estaDentro([P|R], TodasCelulas, [P|Dentro]):-
    member(P, TodasCelulas),
    estaDentro(R, TodasCelulas, Dentro), !.
estaDentro([_|R], TodasCelulas, Dentro):-
    estaDentro(R, TodasCelulas, Dentro).

% Ver se tem apenas uma vizinhanca livre
listaVizLivres(Tabuleiro, Vizinhos, CelulasLivres):-
    todasCelulas(Tabuleiro, CelulasRelva, r),
    findall(Celula, (member(Celula, Vizinhos), celulaVazia(Tabuleiro, Celula), \+ member(Celula, CelulasRelva)), CelulasLivres).

% Insere no tabuleiro uma tenda caso seja a unica hipotese
insereUnicaHipotese(_,[]):-!.
insereUnicaHipotese(Tabuleiro, [[P]|R]):-
    length([P], 1),
    insereObjectoCelula(Tabuleiro, t, P),
    insereUnicaHipotese(Tabuleiro, R).

insereUnicaHipotese(Tabuleiro, [_|R]):-
    insereUnicaHipotese(Tabuleiro, R).

%------------------------------------------
% valida(LArv, LTen)
% LArv e LTen sao listas com todas as coordenadas das tendas e das arvores
% Eh avaliado para verdade se for possivel estabelecer uma relacao em que existe uma e uma unica tenda para cada arvore nas suas vizinhancas
%------------------------------------------
valida(Larv, Lten):-
    length(Larv, Comprimento),
    length(Lten, Comprimento),
    validaAux(Larv, Lten).

validaAux([], []):-!.
validaAux([Arv | RestArv], Lten) :-
    vizinhanca(Arv, Vizinhaca),
    member(Tenda, Lten),
    member(Tenda, Vizinhaca),
    delete(Lten, Tenda, Lten1),
    validaAux(RestArv, Lten1).
%------------------------------------------
% resolve(Puzzle)
% Puzzle eh a variavel com a matriz que representa o tabuleiro mais as listas com o numero de tendas por linhas e colunas
% Apos a aplicacao do predicado, o puzzle esta resolvido
%------------------------------------------
resolve(Puzzle):-
    resolveAux(Puzzle), 
    verifica(Puzzle), !.

% resolve Auxiliar
resolveAux(Puzzle):-
    verifica(Puzzle), !.
resolveAux(Puzzle):-
    chamaPred(Puzzle, Puzzle1),
    coloca(Puzzle1),
    writeln(Puzzle1),
    resolveAux(Puzzle1).

% chama os predicados para resolver o puzzle
chamaPred(Puzzle, Puzzle1):-
    relva(Puzzle),
    aproveita(Puzzle),
    relva(Puzzle),
    limpaVizinhancas(Puzzle),
    unicaHipotese(Puzzle),
    limpaVizinhancas(Puzzle),
    relva(Puzzle),
    (Tabuleiro, L, C) = Puzzle,
    inacessiveis(Tabuleiro),
    Puzzle1 = (Tabuleiro, L, C).

% Coloca uma tenda num sitio vazio qualquer
coloca((Tabuleiro, _, _)):-
    writeln('Coloca pre findall'),
    findall((Linha,Coluna), (celulaVazia(Tabuleiro, (Linha, Coluna)), posValida(Tabuleiro, (Linha,Coluna))), Hipoteses),
    writeln('Coloca pos findall'),
    member((Linha, Coluna), Hipoteses),
    writeln('Coloca pre insere'),
    insereObjectoCelula(Tabuleiro, t, (Linha, Coluna)).

% Verifica se a posicao eh valida
posValida(Puzzle, (L, C)):-
    writeln('PosValida'),
    todasCelulas(Puzzle, TodasCelulasRelva, r),
    todasCelulas(Puzzle, TodasCelulasTendas, t),
    \+ member((L, C), TodasCelulasRelva),
    \+ vizinhancaTendas([(L,C)], TodasCelulasTendas).

% verifica se o puzzle esta resolvido
verifica((Tabuleiro, L, C)):-
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    ContagemLinhas == L,
    ContagemColunas == C,
    todasCelulas(Tabuleiro, TodasCelulasTendas, t),
    todasCelulas(Tabuleiro, TodasCelulasArvores, a),
    \+ vizinhancaTendas(TodasCelulasTendas, TodasCelulasTendas),
    valida(TodasCelulasArvores, TodasCelulasTendas), !.

% verifica se as tendas nao estao nas vizinhancas alargadas umas das outras
vizinhancaTendas([], _):- fail, !.
vizinhancaTendas([Tenda | _], TodasTendas):-
    maplist(vizinhancaAlargada, TodasTendas, Vizinhancas),
    vizinhancaAux(Tenda, Vizinhancas).
vizinhancaTendas([_|R], TodasTendas):-
    vizinhancaTendas(R, TodasTendas).

vizinhancaAux(_, []):- fail ,!.
vizinhancaAux(Tenda, [Vizinhanca| _]):-
    member(Tenda, Vizinhanca),!.
vizinhancaAux(Tenda, [_|R]):-
    vizinhancaAux(Tenda, R).