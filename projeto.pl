% Tiago Miguel Santos Cardoso - Numero de Aluno: AL 103561.

%Predicado Extra (Nao especifico a nenhum predicado).

incr(X, X1) :-
    X1 is X + 1.

%  1. Predicado: extrai_ilhas_linha/3

%  // extrai_ilhas_linha(N_L, Linha, Ilhas)
%  // Lista Ordenada das Ilhas de uma Linha.

e_i_aux(_, [], _, []).

e_i_aux(N_L, [P | R], Index, Ilhas) :-
    P > 0,
    incr(Index, Index_I),
    Ilha_aux = ilha(P,(N_L,Index)),
    e_i_aux(N_L, R, Index_I, Ilhas_aux),
    Ilhas = [Ilha_aux| Ilhas_aux];

    P = 0,
    incr(Index, Index_I),
    e_i_aux(N_L, R, Index_I, Ilhas_aux),
    Ilhas = Ilhas_aux.

extrai_ilhas_linha(_, [], []).

extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    e_i_aux(N_L, Linha, 1, Ilhas).

%  2. Predicado: ilhas/2

%  // ilhas(Puzzle, Ilhas)
%  // Lista Ordenada das Ilhas de um Puzzle.


ilhas_aux([], _, []).

ilhas_aux([P | R], Index, Ilhas):-
    extrai_ilhas_linha(Index, P, Ilhas_P),
    incr(Index, Index_I),
    ilhas_aux(R, Index_I, Ilhas_R),
    append(Ilhas_P, Ilhas_R, Ilhas).

ilhas([], []).

ilhas(Puzzle, Ilhas):-
    ilhas_aux(Puzzle, 1, Ilhas).

%  3. Predicado: vizinhas/3

%  // vizinhas(Ilhas, Ilha, Vizinhas)
%  // Lista Ordenada das Vizinhas de uma Ilha.

%  // ilhas_vizinhas_LC(ilha(_,(X1,Y1)), ilha(_,(X2,Y2)))
%  // Verifica se a ilha esta na mesma linha ou coluna (False se for a propria ilha).

%  // ilhas_vizinhas_ilhas_entre(Ilhas, ilha(_,C1), ilha(_,C2))
%  // Verifica se existe alguma Ilha entre a Ilha em C1 e a Ilha em C2.

coordenada_ilha(ilha(_,C), C).

ilhas_vizinhas_LC(ilha(_,(X1,Y1)), ilha(_,(X2,Y2))):-
    X1 =:= X2, Y1 =\= Y2;
    X1 =\= X2, Y1 =:= Y2.

ilhas_vizinhas_ilhas_entre(Ilhas, ilha(_,C1), ilha(_,C2)):-
    posicoes_entre(C1, C2, Posicoes),
    maplist(coordenada_ilha, Ilhas, Ilhas_C),
    append(Posicoes, Ilhas_C, LC1),
    sort(LC1, LC2),
    length(LC1, L1), length(LC2, L2),
    L1 =\= L2.

vizinhas(Ilhas, Ilha, Vizinhas):-
    findall(X, (member(X, Ilhas), ilhas_vizinhas_LC(Ilha, X)), Potenciais_Vizinhas),
    findall(Potencial_Vizinha, (member(Potencial_Vizinha, Potenciais_Vizinhas), not(ilhas_vizinhas_ilhas_entre(Ilhas, Ilha, Potencial_Vizinha))), Vizinhas).

%  4. Predicado: estado/2

%  // estado(Ilhas, Estado)
%  // Lista Ordenada das Entradas referentes a cada Ilha.

%  // entrada_aux(Ilhas, Ilha, Entrada)
%  // Devolve a Referente a uma Ilha.

entrada_aux(Ilhas, Ilha, Entrada):-
    vizinhas(Ilhas, Ilha, Vizinhas),
    Entrada1 = [Ilha | [Vizinhas]],
    append(Entrada1, [[]], Entrada).

estado(Ilhas, Estado):-
    maplist(entrada_aux(Ilhas), Ilhas, Estado).

%  5. Predicado: posicoes_entre/3

%  // posicoes_entre(Pos1, Pos2, Posicoes)
%  // Lista Ordenada das Posicoes entre a Pos1 e a Pos2.

%  // bet(X, Y, R)
%  // Usada para encontrar os valores entre X e Y.

bet(X, Y, R) :- 
    X < Y, R = X.
bet(X, Y, R) :- 
    X < Y, X1 is X+1, 
    bet(X1, Y, R).

posicoes_entre((X1, Y1), (X2, Y2), Posicoes):-
    X1 =:= X2, Y1 < Y2,
    incr(Y1, Y_Aux),
    findall((X1, N), bet(Y_Aux,Y2, N), Posicoes);
    X1 =:= X2, Y1 > Y2,
    incr(Y2, Y_Aux),
    findall((X1, N), bet(Y_Aux,Y1, N), Posicoes);
    X1 < X2, Y1 =:= Y2,
    incr(X1, X_Aux),
    findall((N, Y1), bet(X_Aux,X2, N), Posicoes);
    X1 > X2, Y1 =:= Y2,
    incr(X2, X_Aux),
    findall((N, Y1), bet(X_Aux,X1, N), Posicoes).

%  6. Predicado: cria_ponte/3

%  cria_ponte(Pos1, Pos2, Ponte)
%  Cria uma ponte entre a Coordenada1 e a Coordenada2.

 cria_ponte((X1, Y1), (X2, Y2), Ponte):-
    X1 =:= X2, Y1 < Y2,
    Ponte = ponte((X1, Y1), (X1, Y2));
    X1 =:= X2, Y1 > Y2,
    Ponte = ponte((X2, Y2), (X1, Y1));
    X1 < X2, Y1 =:= Y2,
    Ponte = ponte((X1, Y1), (X2, Y2));
    X1 > X2, Y1 =:= Y2,
    Ponte = ponte((X2, Y2), (X1, Y1)).

%  7. Predicado: caminho_livre/5

%  // caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
%  // Verifica se a I e Vz continuam a ser Vizinhas depois da introducao de uma ponte entre Pos1 e Pos2.

caminho_livre(Pos1, Pos2, Posicoes, ilha(_,(X1,Y1)), ilha(_,(X2,Y2))):-
    Pos1 = (X1, Y1) , Pos2 = (X2, Y2);
    Pos1 = (X2, Y2) , Pos2 = (X1, Y1);
    posicoes_entre((X1, Y1), (X2, Y2), PE),
    append(Posicoes, PE, LC1),
    sort(LC1, LC2),
    length(LC1, L1), length(LC2, L2),
    L1 =:= L2.

%  8. Predicado: actualiza_vizinhas_entrada/5

%  // actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
%  // Entrada com as Vizinhas Atualizadas apos adicicionar uma Ponte entre Pos1 e Pos2.

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada):-
    Entrada = [Ilha | R],
    R = [Vizinhas| Pontes],
    posicoes_entre(Pos1, Pos2, Posicoes),
    findall(X, (member(X, Vizinhas), caminho_livre(Pos1, Pos2, Posicoes, Ilha, X)), Vizinhas_Aux),
    Entrada1 = [Ilha | [Vizinhas_Aux]],
    append(Entrada1, Pontes, Nova_Entrada).

%  9. Predicado: actualiza_vizinhas_apos_pontes/4

%  // actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
%  // Estado com as Entradas Atualizadas apos ponte entre Pos1 e Pos2.

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado):-
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, _), Estado, Novo_estado).

%  10. Predicado: ilhas_terminadas/2

%  //  ilhas_terminadas(Estado, Ilhas_term)
%  //  Lista com todas as Ilhas Terminadas.

ilha_terminada_aux(Entrada):-
    Entrada = [ilha(P,_) | R],
    not(P = 'X'),
    R = [_ |[Pontes]],
    length(Pontes,PL),
    P =:= PL.

ilhas_terminadas(Estado, Ilhas_Term):-
    findall(Ilha, (member([Ilha | R], Estado), ilha_terminada_aux([Ilha | R])), Ilhas_Term).

%  11. Predicado: tira_ilhas_terminadas_entrada/3

%  //  tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%  //  Entrada Resultante de tirar as ilhas terminadas da ilhas vizinhas.


tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):-
    Entrada = [Ilha | R_Aux],
    R_Aux = [Vizinhas_Aux | Pontes],
    findall(X, (member(X, Vizinhas_Aux), not(member(X, Ilhas_Term))), Vizinhas),
    Nova_Entrada = [Ilha | R],
    R = [Vizinhas | Pontes].
    
%  12. Predicado: tira_ilhas_terminadas/3

%  //  tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
%  //  Estado Resultante de Aplicar o predicado tira_ilhas_terminadas_entrada a todas as Entradas.

tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

%  13. Predicado Marca_ilhas_terminadas_entrada/3

%  //  marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
%  //  Entrada onde o numero de pontes e substituido por 'X' caso esta esteja Terminada.

marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada):-
    Entrada = [Ilha| R],
    Ilha = ilha(_,(X,Y)),
    member(Ilha, Ilhas_Term),
    Nova_Entrada = [ilha('X',(X,Y))| R];
    Entrada = [Ilha| _],
    not(member(Ilha, Ilhas_Term)),
    Nova_Entrada = Entrada.

%  14. Predicado marca_ilhas_terminadas/3

%  //  marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
%  //  Estado Resultante de Aplicar o predicado marca_ilhas_terminadas_entrada.

marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_Term), Estado, Novo_Estado).

%  15. Predicado trata_ilhas_terminadas/2

%  //  trata_ilhas_terminadas(Estado, Novo_estado)
%  //  Estado depois de aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas.

trata_ilhas_terminadas(Estado, Novo_Estado):-
    ilhas_terminadas(Estado, Ilhas_Term),
    tira_ilhas_terminadas(Estado, Ilhas_Term, Estado_Aux),
    marca_ilhas_terminadas(Estado_Aux, Ilhas_Term, Novo_Estado).

%  16. Predicado junta_pontes/5

%  //  junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
%  //  Estado obtido apos adicionar Num_Pontes entre Ilha1 e Ilha2.

multiplica_pontes(Ponte, Num_Pontes, Pontes) :-
    length(Pontes, Num_Pontes),        
    maplist(=(Ponte), Pontes).      

junta_pontes_aux(C1, C2, Num_Pontes, Entrada, Nova_Entrada):-
    member(C, [C1 , C2]),
    Entrada = [Ilha | R_Aux],
    coordenada_ilha(Ilha,C),
    R_Aux = [Vizinhas | [Pontes_Aux]],
    cria_ponte(C1, C2, Ponte),
    multiplica_pontes(Ponte, Num_Pontes, PontesM),
    Nova_Entrada = [Ilha| R],
    append(PontesM, Pontes_Aux, Pontes),
    R = [Vizinhas | [Pontes]];
    Nova_Entrada = Entrada.

junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Novo_Estado):-
    coordenada_ilha(Ilha1,C1), coordenada_ilha(Ilha2,C2),
    maplist(junta_pontes_aux(C1, C2, Num_Pontes), Estado, Estado_Aux1),
    actualiza_vizinhas_apos_pontes(Estado_Aux1, C1, C2, Estado_Aux2),
    trata_ilhas_terminadas(Estado_Aux2, Novo_Estado).