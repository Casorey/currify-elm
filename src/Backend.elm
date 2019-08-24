port module Backend exposing (..)
import Types exposing(Song)

import Utils exposing (..)
import Models exposing (Model)
import List exposing (range, map, filter,concat )
import String exposing(contains)

-- Existe la funcion findSong que recibe
-- una condicion y una lista de canciones
-- findSong : (Song -> Bool) -> List Song -> Song

-- Existe la funcion tailSafe que recibe
-- una lista de canciones y se queda con la cola
-- si la lista no tiene cola (tiene un solo elemento)
-- se queda con una lista vacia
-- tailSafe : List Song -> List Song

-- Existe idFirst que recibe una lista
-- de canciones y devuelve el id de la primera
-- idFirst : List Song -> String

-- Debería darnos la url de la cancion en base al id
urlById : String -> List Song -> String
urlById idSong songs = (findWithId idSong songs ).url

findWithId : String -> List Song -> Song
findWithId idSong songs = findSong (checkId idSong) songs

checkId : String -> Song -> Bool
checkId idSong song = (idSong == song.id)

-- Debería darnos las canciones que tengan ese texto en nombre o artista
filterByName : String -> List Song -> List Song
filterByName text songs = filter (findWithText text) songs

findWithText : String -> Song -> Bool
findWithText text song = checkName text song.name || checkName text song.artist

checkName : String -> String -> Bool
checkName text name = contains text name

-- Recibe un id y tiene que likear/dislikear una cancion
-- switchear song.liked 
toggleLike : String -> List Song -> List Song
toggleLike id songs = map (changeLike ) (filter (checkId id) songs)

changeLike : Song -> Song
changeLike song = if song.liked then dislike song else like song

like : Song -> Song
like song = {song |liked = True}

dislike : Song -> Song
dislike song = {song |liked = False}


-- Esta funcion tiene que decir si una cancion tiene
-- nuestro like o no, por ahora funciona mal...
-- hay que arreglarla  
isLiked : Song  -> Bool
isLiked song = song.liked

-- Recibe una lista de canciones y nos quedamos solo con las que
-- tienen un like
filterLiked : List Song -> List Song
filterLiked songs = filter isLiked songs

-- Agrega una cancion a la cola de reproduccion
-- (NO es necesario preocuparse porque este una sola vez)
addSongToQueue : Song -> List Song -> List Song
addSongToQueue song queue = queue ++ [song]

-- Saca una cancion de la cola
-- (NO es necesario que se elimine una sola vez si esta repetida) 
removeSongFromQueue : String -> List Song -> List Song
removeSongFromQueue id queue = filter (not (checkId id)) queue

-- Hace que se reproduzca la canción que sigue y la saca de la cola 
playNextFromQueue : Model -> Model
playNextFromQueue model = { model | queue = tailSafe model.queue}

-------- Funciones Listas --------

-- Esta funcion recibe el modelo y empieza a reproducir la
-- cancion que tenga el id que se pasa...
-- Mirar la función urlById
playSong : Model -> String -> Model
playSong model id = { model | playerUrl = urlById id model.songs, playing = (if id /= "" then Just True else Nothing) }

applyFilters : Model -> List Song
applyFilters model =
  model.songs
    |> filterByName model.filterText
    |> if model.onlyLiked then filterLiked else identity

port togglePlay : Bool -> Cmd msg
port songEnded : (Bool -> msg) -> Sub msg
