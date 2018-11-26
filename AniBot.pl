:- dynamic anime/1, genero/1, rating/2, popularidad/2, subirPop/1, preguntado/2, estadoDelPrograma/1, estadoDelProgramaGeneros/1.

baseOriginal(["Dragon Ball", "Naruto", "Bleach", "HunterXHunter", "Hamtaro", "Full Metal Alchemist"]).
baseOriginalGeneros([aventura, shoujo, shounen, kodomo, seinen, josei, ficcion,
    fantasia, mecha, sobrenatural, magia, gore]).

estadoDelPrograma(Animes) :- baseOriginal(Lista), Animes = Lista.
estadoDelProgramaGeneros(Generos) :- baseOriginalGeneros(Lista), Generos = Lista.

anime(X) :- estadoDelPrograma(Animes), member(X, Animes).

genero(X) :- estadoDelProgramaGeneros(Generos), member(X,Generos).

generoAnime("Naruto",[shounen, aventura]).
generoAnime("Dragon Ball",[shounen]).
generoAnime("Bleach",[shounen, sobrenatural]).
generoAnime("HunterXHunter",[seinen, aventura]).
generoAnime("Hamtaro",[kodomo]).
generoAnime("Full Metal Alchemist",[shounen, magia]).

rating("Dragon Ball",3).
rating("Naruto",1).
rating("Bleach",4).
rating("HunterXHunter",5).
rating("Hamtaro",2).
rating("Full Metal Alchemist",4).

popularidad("Dragon Ball",7).
popularidad("Naruto",5).
popularidad("Bleach",8).
popularidad("HunterXHunter",3).
popularidad("Hamtaro",10).
popularidad("Full Metal Alchemist",1).

% Poder mostrar los animés de un género ordenados por rating y/o popularidad, según
% pregunte el usuario, por defecto de mayor a menor. En caso de que se pregunte por ambos
% se suma el rating y popularidad y se ordena según el resultado.

% Predicado auxiliar
esGenero(Genero, Anime) :- 
    generoAnime(Anime, ListaGenero), 
    member(Genero, ListaGenero).

% Predicado para revertir una lista.
anadirALaCola(Elem, [], [Elem]).
anadirALaCola(Elem, [L|Ls], [R|Rs]) :- anadirALaCola(Elem, Ls, Respuesta), R = L, Rs = Respuesta, !.

reverse([X], [X]).
reverse([L|Ls], ListaRevertida) :- reverse(Ls, SubListaRevertida), anadirALaCola(L,SubListaRevertida, Respuesta), ListaRevertida = Respuesta, !. 

% Predicado para conseguir el anime con maximo rating y/o popularidad.
maximumRating([X], X).
maximumRating([L|Ls], X) :- maximumRating(Ls, Y), rating(L, Lr), rating(Y, Yr), Lr > Yr -> X = L, !; maximumRating(Ls, Y), X = Y, !.

maximumPopularidad([X], X).
maximumPopularidad([L|Ls], X) :- maximumPopularidad(Ls, Y), popularidad(L, Lr), popularidad(Y, Yr), Lr > Yr -> X = L, !; maximumPopularidad(Ls, Y), X = Y, !.

maximumRatingPlusPopularidad([X], X).
maximumRatingPlusPopularidad([L|Ls], X) :- 
    maximumRatingPlusPopularidad(Ls, Y),
    popularidad(L, P),
    rating(L, R),
    popularidad(Y, P1),
    rating(Y, R1),
    PopPlusRa is P + R,
    PopPlusRa1 is P1 + R1,
    PopPlusRa > PopPlusRa1 -> X = L, !; maximumRatingPlusPopularidad(Ls, Y), X = Y, !.


% Predicado para eliminar un elemento de una lista
deleteList(_, [], []).
deleteList(Elem, [L|Ls], Lista) :- Elem = L, Lista = Ls, !.
deleteList(Elem, [L|Ls], [O|Os]) :- Elem \= L, O = L, deleteList(Elem, Ls, ListaResultado), Os = ListaResultado, !.

% Predicado para ordenar por rating y/o popularidad.
ordenadoPorRating([A], [A]).
ordenadoPorRating(Animes, [L|Ls]) :- maximumRating(Animes, Anime), L = Anime,  deleteList(Anime, Animes, AnimesMenosUno), ordenadoPorRating(AnimesMenosUno, SubListaOrdenada), Ls = SubListaOrdenada, !.

ordenadoPorPopularidad([A], [A]).
ordenadoPorPopularidad(Animes, [L|Ls]) :- maximumPopularidad(Animes, Anime), L = Anime,  deleteList(Anime, Animes, AnimesMenosUno), ordenadoPorPopularidad(AnimesMenosUno, SubListaOrdenada), Ls = SubListaOrdenada, !.

% Este predicado ordena con respecto al resultado de la suma entre la popularidad y el rating
ordenadoPorPopPlusRa([A], [A]).
ordenadoPorPopPlusRa(Animes, [L|Ls]) :- 
    maximumRatingPlusPopularidad(Animes, Anime),
    L = Anime, 
    deleteList(Anime, Animes, AnimesMenosUno), 
    ordenadoPorPopPlusRa(AnimesMenosUno, SubListaOrdenada), 
    Ls = SubListaOrdenada, !.


% Predicado principal
animesPorGenero(Genero, Opcion, Ordenamiento, Animes) :- 
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "r", Ordenamiento = "ma" -> ordenadoPorRating(Lista, Lista1), Animes = Lista1;
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "p", Ordenamiento = "ma" -> ordenadoPorPopularidad(Lista, Lista1), Animes = Lista1;
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "r", Ordenamiento = "me" -> ordenadoPorRating(Lista, Lista1), reverse(Lista1, Lista2), Animes = Lista2;
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "p", Ordenamiento = "me" -> ordenadoPorPopularidad(Lista, Lista1), reverse(Lista1, Lista2), Animes = Lista2;
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "rp", Ordenamiento = "ma" -> ordenadoPorPopPlusRa(Lista, Lista1), Animes = Lista1;
    findall(Anime, esGenero(Genero, Anime), Lista), Opcion = "rp", Ordenamiento = "me" -> ordenadoPorPopPlusRa(Lista, Lista1), reverse(Lista1, Lista2), Animes = Lista2.

animesPorGeneros([], []).
animesPorGeneros([G|Gs], Animes) :-
    animesPorGenero(G, "r", "ma", Lista1),
    animesPorGeneros(Gs, Lista2),
    union(Lista1, Lista2, ListaResultado),
    Animes = ListaResultado.

% Poder mostar los animés con X número de estrellas dentro de cierto género (el género es
% un estado del chatbot que se debe conocer).

% tieneXEstrellas/3 Signfica que el Anime a tiene X estrellas y es estan en la Lista l
% Este predicado sirve para conseguir el conjunto de anime que tienen un numero de estrellas especificadas por el usuario
tieneXEstrellas(Anime, X, Lista) :- member(Anime, Lista), rating(Anime, Estrellas), Estrellas = X.

% animesConNumEstrellas\3 Este predicado agrupa todos los animes con X estrellas en una lista.
animesConNumEstrellas(Genero, Estrellas, Lista) :- animesPorGenero(Genero, ListaAnimes), findall(Anime, tieneXEstrellas(Anime, Estrellas, ListaAnimes), Animes), Lista = Animes.

% Poder mostrar los animes con X numero de estrellas.
animesConNumEstrellas(Estrellas, Lista) :- findall(Ani, anime(Ani), BaseAnimes), findall(Anime, tieneXEstrellas(Anime, Estrellas, BaseAnimes), Animes), Lista = Animes.

% Poder mostrar los animés buenos poco conocidos. Aquí se hace referencia a rating alto
% con popularidad baja.

% Predicado principal
aniBuenosPocaPop(Anime) :- popularidad(Anime,Nivel), Nivel<6, rating(Anime,Estrellas), Estrellas>3.

% este predicado agrupa todos los animes buenos poco conocidos en una lista
animesDeInteres(Lista) :- findall(Anime, aniBuenosPocaPop(Anime), Animes), Lista = Animes.

% Poder agregar a la base de datos un anime con su género y rating, si no está en la misma.
% La popularidad es opcional especificarla al agregarlo y por defecto es 1.

agregarAnime(Anime, Rating, 1) :- assertz(anime(Anime)), assertz(rating(Anime, Rating)), assertz(popularidad(Anime, 1)), !.
agregarAnime(Anime, Rating, Popularidad) :- Popularidad > 0, Rating > 0, Popularidad < 11, Rating < 6,
                                            assertz(anime(Anime)), assertz(rating(Anime, Rating)), assertz(popularidad(Anime, Popularidad)), !.

% Subir la popularidad del anime si los usuarios preguntan por él 5 o más veces.

% Predicado que actualiza la cantidad de veces que se ha preguntado por un anime.
preguntado(Anime) :- retract(preguntado(Anime, Veces)), VecesNuevo is Veces+1, assertz(preguntado(Anime,VecesNuevo)), !.

% Predicado que reinicia el valor de Veces a 0 cuando ya se ha preguntado 5 veces por un anime.
pregCero(Anime) :- retract(preguntado(Anime, _)), VecesNuevo is 0, assertz(preguntado(Anime,VecesNuevo)), !.

% Predicado que aumenta en uno (1) la popularidad de un anime dado
subirPop(Anime) :- preguntado(Anime, Veces), Veces > 4, retract(popularidad(Anime, Nivel)), NivelNuevo is Nivel+1, assertz(popularidad(Anime,NivelNuevo)), pregCero(Anime), !.

% -----------------------------------------------------IMPLEMENTACION DEL CHATBOT------------------------------------------------------------------------ %
% Definiendo el operador :
:- op(250, xfx, :).

% -------------------------------------------------------------AUTOMATAS--------------------------------------------------------------------------------- %
% Predicado que va de un nodo 1 a un nodo 2 cualesquiera 
traverse('#':'#',Tape1,Tape1,Tape2,Tape2).
traverse('#':L2,Tape1,Tape1,[L2|RestTape2],RestTape2).
traverse(L1:'#',[L1|RestTape1],RestTape1,Tape2,Tape2).
traverse(L1:L2,[L1|RestTape1],RestTape1,[L2|RestTape2],RestTape2).

% Automata para reconocer saludos 
inicialSaludos(1).
finalSaludos(2).
arcSaludos(1, 2, Palabra:Respuesta) :- member(Palabra, [hola, bonjour, hi, hello, salute]), respuestaSaludos(FraseRespuesta), Respuesta = FraseRespuesta.
arcSaludos(1, 4, hola:'#').
arcSaludos(4, 2, anibot:Respuesta) :- respuestaSaludos(FraseRespuesta), Respuesta = FraseRespuesta.
arcSaludos(1, 3, buenos:'#').
arcSaludos(3, 2, dias:Respuesta) :- respuestaSaludos(FraseRespuesta), Respuesta = FraseRespuesta.

% Automata para reconocer despedidas
inicialDespedida(1).
finalDespedida(2).
arcDespedida(1, 2, Palabra:Respuesta) :- member(Palabra, [adios, bye]), respuestaDespedida(FraseRespuesta), Respuesta = FraseRespuesta.
arcDespedida(1, 3, adios:'#').
arcDespedida(3, 2, anibot:Respuesta) :- respuestaDespedida(FraseRespuesta), Respuesta = FraseRespuesta.
arcDespedida(3, 4, ,:'#').
arcDespedida(1, 4, gracias:'#').
arcDespedida(4, 8, por:'#').
arcDespedida(8, 2, todo:Respuesta) :- respuestaDespedida(FraseRespuesta), Respuesta = FraseRespuesta.
arcDespedida(2, 4, '#':'#').
arcDespedida(4, 6, vere:'#').
arcDespedida(6, 7, tus:'#').
arcDespedida(7, 2, reconmedaciones:Respuesta) :- respuestaDespedida(FraseRespuesta), Respuesta = FraseRespuesta.

% Automata para reconocer pregunta sobre animes buenos pero poco conocidos.
inicialInteres(1).
finalInteres(2).
arcInteres(1, 4, quiero:'#').
arcInteres(4, 5, ver:'#').
arcInteres(5, 6, un:'#').
arcInteres(6, 7, anime:'#').
arcInteres(7, 8, interesante:'#').
arcInteres(8, 9, pero:'#').
arcInteres(9, 10, poco:'#').
arcInteres(10, 2, conocido:Respuesta) :- respuestaInteres(FraseRespuesta), Respuesta = FraseRespuesta.

% Automata para reconocer pregunta sobre los mejores ratings.

inicialMejores(1).
finalMejores(2).
arcMejores(1, 4, cuales:'#').
arcMejores(4, 5, son:'#').
arcMejores(5, 6, los:'#').
arcMejores(6, 7, mejores:'#').
arcMejores(7, 2, ratings:Respuesta) :- respuestaMejores(FraseRespuesta), Respuesta = FraseRespuesta.

% Automata para reconocer gustos 
inicialGustos(1).
finalGustos(2).
arcGustos(1, 3, me:'#').
arcGustos(3, 4, gusta:'#').
arcGustos(4, 5, '#':'#').
arcGustos(5, 6, Genero:'#') :- genero(Genero).
arcGustos(6, 5, ',':'#').
arcGustos(6, 7, '#':'#').
arcGustos(7, 2,'#':Respuesta) :- respuestaGustos(FraseRespuesta1), !, Respuesta = FraseRespuesta1.
arcGustos(7, 8, me:'#').
arcGustos(8, 9, recomiendas:'#').
arcGustos(9, 2, '#':Respuesta) :- respuestaGustos(FraseRespuesta1), Respuesta = FraseRespuesta1.
arcGustos(1, 10, Palabra:'#') :- member(Palabra, [recomiendame, cuales]).
arcGustos(10, 11, animes:'#').
arcGustos(11, 4, de:'#').

% Predicado para actualizar base de datos 
actualizarBase :- 
    findall(Genero, retract(subconGeneros(Genero)), ListaGenero),
    retract((estadoDelProgramaGeneros(Generos) :- baseOriginalGeneros(Lista), Generos = Lista)) -> assertz(estadoDelProgramaGeneros(ListaGenero));
    findall(Genero, retract(subconGeneros(Genero)), ListaGenero),
    retract(estadoDelProgramaGeneros(_)) -> assertz(estadoDelProgramaGeneros(ListaGenero)).


% Reconocedor de saludos.
reconoceSaludo(Node, [], []) :- 
    finalSaludos(Node).

reconoceSaludo(Node1, String1, Responder) :-
    arcSaludos(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceSaludo(Node2, NewString1, NewResponder).

% Reconocedor de despedidas.
reconoceDespedida(Node, [], []) :-
    finalDespedida(Node).

reconoceDespedida(Node1, String1, Responder) :-
    arcDespedida(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceDespedida(Node2, NewString1, NewResponder).

% Reconodedor de pregunta sobre anime bueno poco conocido,
reconoceInteres(Node, [], []) :-
    finalInteres(Node).

reconoceInteres(Node1, String1, Responder) :-
    arcInteres(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceInteres(Node2, NewString1, NewResponder).

% Reconocedor de pregunta sobre animes con mejor ratings.
reconoceMejores(Node, [], []) :-
    finalMejores(Node).

reconoceMejores(Node1, String1, Responder) :-
    arcMejores(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceMejores(Node2, NewString1, NewResponder).

% Reconocedor de gustos 
head([], []).
head([S|_], Cabeza) :- Cabeza = S.

reconoceGustos(Node, [], []) :-
    finalGustos(Node).

reconoceGustos(Node1, String1, Responder) :-
    arcGustos(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    head(String1, Cabeza), genero(Cabeza), \+(retract(subconGeneros(Cabeza))) ->  assertz(subconGeneros(Cabeza)), reconoceGustos(Node2, NewString1, NewResponder), !;
    arcGustos(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceGustos(Node2, NewString1, NewResponder), !.

% Reconocedor de animes sin automatas 

listaFrases([['"', 'Dragon', 'Ball', '"', es, muy, bueno, ',', lo, conoces], ['"', 'Naruto', '"', es, muy, bueno, ',', lo, conoces],
             ['"', 'Bleach', '"', es, muy, bueno, ',', lo, conoces], ['"', 'HunterXHunter', '"', es, muy, bueno,',', lo, conoces, '"'],
            ['"', 'Hamtaro', '"', es, muy, bueno, ',', lo, conoces], ['"', 'Full', 'Metal', 'Alchemist', '"', es, muy, bueno, ',', lo, conoces]]).

responderAnime(Anime, Estrellas, Generos, Pop) :-
    Pop > 0, Pop < 3 -> write("Si, "), write(Anime), write(" tiene rating "), write(Estrellas), write(" y genero es "), write(Generos), writeln(" pero es muy poco conocido");
    Pop > 2, Pop < 6 -> write("Si, "), write(Anime), write(" tiene rating "), write(Estrellas), write(" y genero es "), write(Generos), writeln(" pero es poco conocido");
    Pop > 5, Pop < 8 -> write("Si, "), write(Anime), write(" tiene rating "), write(Estrellas), write(" y genero es "), write(Generos), writeln(" es conocido");
    Pop > 7, Pop < 10 -> write("Si, "), write(Anime), write(" tiene rating "), write(Estrellas), write(" y genero es "), write(Generos), writeln(" es muy conocido");
    write("Si, "), write(Anime), write(" tiene rating "), write(Estrellas), write(" y genero es "), write(Generos), writeln(" es bastante conocido").

respuestaConoceAnime(Frase) :- 
    Frase == ['"', 'Dragon', 'Ball', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("Dragon Ball", Pop), rating("Dragon Ball", Estrellas), generoAnime("Dragon Ball", Generos), responderAnime("Dragon Ball", Estrellas, Generos, Pop), !; 
    Frase == ['"', 'Naruto', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("Naruto", Pop), rating("Naruto", Estrellas), generoAnime("Naruto", Generos), responderAnime("Naruto", Estrellas, Generos, Pop), !; 
    Frase == ['"', 'Bleach', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("Bleach", Pop), rating("Bleach", Estrellas), generoAnime("Bleach", Generos), responderAnime("Bleach", Estrellas, Generos, Pop), !; 
    Frase == ['"', 'HunterXHunter', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("HunterXHunter", Pop), rating("HunterXHunter", Estrellas), generoAnime("HunterXHunter", Generos), responderAnime("HunterXHunter", Estrellas, Generos, Pop), !; 
    Frase == ['"', 'Hamtaro', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("Hamtaro", Pop), rating("Hamtaro", Estrellas), generoAnime("Hamtaro", Generos), responderAnime("Hamtaro", Estrellas, Generos, Pop), !; 
    Frase == ['"', 'Full', 'Metal', 'Alchemist', '"', es, muy, bueno, ',', lo, conoces] -> popularidad("Full Metal Alchemist", Pop), rating("Full Metal Alchemist", Estrellas), generoAnime("Full Metal Alchemist", Generos), responderAnime("Full Metal Alchemist", Estrellas, Generos, Pop), !. 

%--------------------------------------------------------- Manejador de Respuestas 2. Para Automatas.------------------------------------------------------%

% Predicado para restaurar a su estado original la base de datos 
llevarBaseOriginal :- 
    retract(estadoDelProgramaGeneros(_)),
    assertz((estadoDelProgramaGeneros(Generos) :- baseOriginalGeneros(Lista), Generos = Lista)).

producirRespuestaAutomata(Frase, FraseRespuesta) :-
    reconoceSaludo(1, Frase, Respuesta) -> FraseRespuesta = Respuesta, !;
    reconoceDespedida(1, Frase, Respuesta) -> FraseRespuesta = Respuesta, !;
    reconoceInteres(1, Frase, Respuesta) -> FraseRespuesta = Respuesta, !;
    reconoceMejores(1, Frase, Respuesta) -> FraseRespuesta = Respuesta, !;
    reconoceGustos(1, Frase, Respuesta) -> llevarBaseOriginal, FraseRespuesta = Respuesta,!;
    respuestaConoceAnime(Frase), FraseRespuesta = "", !;
    respuestaError(Respuesta), FraseRespuesta = Respuesta, !.


% Emitir Respuesta Con Automatas
emitirRespuestaAutomata(Entrada, Respuesta) :- producirRespuestaAutomata(Entrada, FraseRespuesta), Respuesta = FraseRespuesta.

% Respuestas para cada uno de los topicos
respuestaSaludos(FraseRespuesta) :- 
    random_member(Respuesta, ["Hola soy el AniBot, en quieres que te ayude ? ", "Hello, mi nombre es AniBot que haremos hoy ? ", "AniBot presentandose, dime que necesitas "]),
    FraseRespuesta = Respuesta.

respuestaDespedida(FraseRespuesta) :-
    random_member(Respuesta, ["Adios, estamos en contacto!", "Chao, no dudes en usarme de nuevo", "Disfruta viendo nuestras recomendaciones", "Si necesitas otro anime escribeme"]),
    FraseRespuesta = Respuesta.

respuestaInteres(FraseRespuesta) :- animesDeInteres(Respuesta), write("Estos son algunos de los animes buenos poco conocidos "), FraseRespuesta = Respuesta.

respuestaMejores(FraseRespuesta) :- animesConNumEstrellas(5, Respuesta), write("Estos son los animes con los mejores ratings: "), FraseRespuesta = Respuesta.

respuestaGustos(FraseRespuesta) :- actualizarBase, estadoDelProgramaGeneros(Generos), animesPorGeneros(Generos, ListaAnimes), FraseRespuesta = ListaAnimes, write(Generos).

% Este predicado actuara como un sumidero para aquellas frases que no pueda reconocer ninguno de 
% los automatas
respuestaError(FraseRespuesta) :-
    random_member(Respuesta, [["Lo siento, no entiendo lo que me trata de decir "], ["Puede repetir lo que dijo, por favor "], ["No tengo disponible esa informacion "]]),
    FraseRespuesta = Respuesta.

transform_to_string([Frase|_], Frase2) :- Frase2 = Frase.
se_salio(Frase) :- member(Frase, ["Adios, estamos en contacto!", "Chao, no dudes en usarme de nuevo", "Disfruta viendo nuestras recomendaciones", "Si necesitas otro anime escribeme"]).
% IMPORTANTE readln lee del standar input pero convierte la entrada en una lista de las palabras 
% podria tener mucha utilidad

main_loop :- write("Bienvenido al AniBot: -> "),
             repeat,
                readln(N),
                nl, emitirRespuestaAutomata(N, Respuesta),
                transform_to_string(Respuesta, Salida),
                writeln(Salida),
            se_salio(Salida),
            !.