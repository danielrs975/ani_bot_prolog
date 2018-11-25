:- dynamic anime/1, genero/1, rating/2, popularidad/2, subirPop/1, preguntado/2.

anime(X) :- member(X,["Dragon Ball", "Naruto", "Bleach", "HunterXHunter", "Hamtaro", "Full Metal Alchemist"]).

genero(X) :- member(X,["Aventura", "Shoujo", "Shounen", "Kodomo", "Seinen", "Josei", "Ficción",
                    "Fantasía", "Mecha", "Sobrenatural", "Magia", "Gore"]).

generoAnime("Naruto",["Shounen","Aventura"]).
generoAnime("Dragon Ball",["Shounen"]).
generoAnime("Bleach",["Shounen", "Sobrenatural"]).
generoAnime("HunterXHunter",["Seinen", "Aventura"]).
generoAnime("Hamtaro",["Kodomo"]).
generoAnime("Full Metal Alchemist",["Shounen", "Magia"]).

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



% Poder mostar los animés con X número de estrellas dentro de cierto género (el género es
% un estado del chatbot que se debe conocer).

% tieneXEstrellas/3 Signfica que el Anime a tiene X estrellas y es estan en la Lista l
% Este predicado sirve para conseguir el conjunto de anime que tienen un numero de estrellas especificadas por el usuario
tieneXEstrellas(Anime, X, Lista) :- member(Anime, Lista), rating(Anime, Estrellas), Estrellas = X.

% animesConNumEstrellas\3 Este predicado agrupa todos los animes con X estrellas en una lista.
animesConNumEstrellas(Genero, Estrellas, Lista) :- animesPorGenero(Genero, ListaAnimes), findall(Anime, tieneXEstrellas(Anime, Estrellas, ListaAnimes), Animes), Lista = Animes.

% Poder mostrar los animés buenos poco conocidos. Aquí se hace referencia a rating alto
% con popularidad baja.

% Predicado principal
aniBuenosPocaPop(Anime) :- popularidad(Anime,Nivel), Nivel<6, rating(Anime,Estrellas), Estrellas>3.

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

% Reconocedor de saludos.
reconoceSaludo(Node, [], []) :- 
    finalSaludos(Node).

reconoceSaludo(Node1, String1, Responder) :-
    arcSaludos(Node1, Node2, Label),
    traverse(Label, String1, NewString1, Responder, NewResponder),
    reconoceSaludo(Node2, NewString1, NewResponder).


%--------------------------------------------------------- Manejador de Respuestas 2. Para Automatas.------------------------------------------------------%
producirRespuestaAutomata(Frase, FraseRespuesta) :-
    reconoceSaludo(1, Frase, Respuesta) -> FraseRespuesta = Respuesta. 

% Emitir Respuesta Con Automatas
emitirRespuestaAutomata(Entrada, Respuesta) :- producirRespuestaAutomata(Entrada, FraseRespuesta), Respuesta = FraseRespuesta.



% RECONOCEDOR VIEJO 

% Predicado para reconocer saludos 
saludos(Frase, saludos) :- member(Frase, [[hola], [hi], [buenos, dias], [hello], [bonjour], [salute]]), !.

% Predicado para reconocer despedidas
despedida(Frase, despedida) :- member(Frase, [[chao], [ya, no, necesito, mas, nada], [adios], [gracias]]).

% Reconocer el topico del cual se va hablar 
reconocerTopico(Frase, Topico) :- 
    saludos(Frase, X), Topico = X, !;
    despedida(Frase, X), Topico = X, !.

% Respuestas para cada uno de los topicos
respuestaSaludos(FraseRespuesta) :- 
    random_member(Respuesta, ["Hola soy el AniBot, en quieres que te ayude ?", "Hello, mi nombre es AniBot que haremos hoy ?", "AniBot presentandose, dime que necesitas"]),
    FraseRespuesta = Respuesta.

respuestaDespedida(FraseRespuesta) :-
    random_member(Respuesta, ["Adios, estamos en contacto!", "Chao, no dudes en usarme de nuevo", "Disfruta viendo nuestras recomendaciones", "Si necesitas otro anime escribeme"]),
    FraseRespuesta = Respuesta.

% Manejador de Respuestas
producirRespuesta(Topico, Frase) :-
    Topico = saludos -> respuestaSaludos(FraseRespuesta), Frase = FraseRespuesta;
    Topico = despedida -> respuestaDespedida(FraseRespuesta), Frase = FraseRespuesta.



% Emitir una respuesta. 
emitirRespuesta(Entrada, Respuesta) :- reconocerTopico(Entrada, Topico), producirRespuesta(Topico, FraseRespuesta), Respuesta = FraseRespuesta.



% IMPORTANTE readln lee del standar input pero convierte la entrada en una lista de las palabras 
% podria tener mucha utilidad

main_loop :- write("Bienvenido al AniBot: -> "),
             repeat,
                readln(N),
                write(N),
                nl, emitirRespuestaAutomata(N, Respuesta),
                write(Respuesta),
            despedida(N, despedida),
            !.