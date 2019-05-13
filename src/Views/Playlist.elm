module Views.Playlist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Components.Player exposing (..)
import Components.Nav exposing (..)
import Components.Hearth exposing (hearth)

import Msg exposing (..)
import Types exposing (Song)
import Models exposing (Model)
import Backend exposing (applyFilters, isLiked)

import Html.Events exposing (..)

songItem : Song -> Html Msg
songItem song =
  div [ class "song-container" ] [
    div [ class "song-cover-container-playlist"
        , onClick (AddToPlaylist song) ] [
      img [ class "song-cover"
          , src song.cover
          , width 150
          , height 150 ] []
    ],
    div [ class "song-data" ] [
      div [ class "song-info" ] [
        span [ class "song-name"
          , onClick (Play song.id) ] [ text song.name ],
        span [ class "song-artist" ] [ text song.artist ]
      ],
      div [ class "add-to-playlist" ] [
        i [ class "icon ion-ios-add-circle button-add"
          , onClick (AddToPlaylist song) ] []
      ]
    ]
  ]

playlistRow : Song -> Html Msg
playlistRow song =
  div [ class "playlist-row" ] [
    img [ class "playlist-song-cover"
        , onClick (Play song.id)
        , src song.cover
        , width 40
        , height 40 ] [],
    div [ class "playlist-song-info" ] [
      span [ class "song-name" ] [ text song.name ],
      span [ class "song-artist" ] [ text song.artist ]
    ],
    div [ class "remove-from-playlist" ] [
      i [ class "icon ion-ios-remove-circle button-remove"
        , onClick (RemoveFromPlaylist song.id) ] []
    ]
  ]

playlistView : Model -> Html Msg
playlistView model =
  div [] [
    div [ class "container"
        , class "playlist-container" ]
        ((List.map songItem << applyFilters) model)
    , div [ class "sidebar" ] [
      h1 [ class "playlist-name" ] [ text "pdeplaylist"],
      div [] ((List.map playlistRow << List.take 100) model.playlist)
    ]
  ]
