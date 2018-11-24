:- dynamic anime/1, genero/1, rating/2, popularidad/2.

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
esGenero(Genero, Anime) :- generoAnime(Anime, ListaGenero), member(Genero, ListaGenero).

% Predicado para conseguir el anime con maximo rating y/o popularidad.
maximumRating([X], X).
maximumRating([L|Ls], X) :- maximumRating(Ls, Y), rating(L, Lr), rating(Y, Yr), Lr > Yr -> X = L, !; maximumRating(Ls, Y), X = Y, !.

% Predicado para eliminar un elemento de una lista
deleteList(_, [], []).
deleteList(Elem, [L|Ls], Lista) :- Elem = L, Lista = Ls, !.
deleteList(Elem, [L|Ls], [O|Os]) :- Elem \= L, O = L, deleteList(Elem, Ls, ListaResultado), Os = ListaResultado, !.

% Predicado para ordenar por rating y/o popularidad.
ordenadoPorRating([A], [A]).
ordenadoPorRating(Animes, [L|Ls]) :- maximumRating(Animes, Anime), L = Anime,  deleteList(Anime, Animes, AnimesMenosUno), ordenadoPorRating(AnimesMenosUno, SubListaOrdenada), Ls = SubListaOrdenada, !.

% Predicado principal
animesPorGenero(Genero, Opcion, Animes) :- findall(Anime, esGenero(Genero, Anime), Lista), ordenadoPorRating(Lista, Lista1), Animes = Lista1.

% Poder mostar los animés con X número de estrellas dentro de cierto género (el género es
% un estado del chatbot que se debe conocer).

% tieneXEstrellas/3 Signfica que el Anime a tiene X estrellas y es estan en la Lista l
% Este predicado sirve para conseguir el conjunto de anime que tienen un numero de estrellas especificadas por el usuario
tieneXEstrellas(Anime, X, Lista) :- member(Anime, Lista), rating(Anime, Estrellas), Estrellas = X.

% animesConNumEstrellas\3 Este predicado agrupa todos los animes con X estrellas en una lista.
animesConNumEstrellas(Genero, Estrellas, Lista) :- animesPorGenero(Genero, ListaAnimes), findall(Anime, tieneXEstrellas(Anime, Estrellas, ListaAnimes), Animes), Lista = Animes.

% Poder mostrar los animés buenos poco conocidos. Aquí se hace referencia a rating alto
% con popularidad baja.

aniBuenosPocaPop(Anime,Lista) :- popularidad(Anime,Nivel), Nivel<6, rating(Anime,Estrellas), Estrellas>3.

is_quit_option(quit).

main_loop :- repeat,
                write("hola"),
                read(N),
                write(N),
            is_quit_option(N),
            !.