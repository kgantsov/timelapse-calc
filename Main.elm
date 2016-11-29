import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main : Program Never
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { interval_h : String
  , interval_m : String
  , interval_s : String
  , length_h : String
  , length_m : String
  , length_s : String
  , fps : String
  , size : String
  , event_duration : String
  , number_of_photos : String
  , total_memory_usage : String
  }


model : Model
model =
  Model "0" "0" "5" "0" "0" "15" "24" "16" "0" "0" "0"



-- UPDATE


type Msg
    = ShootingIntervalH String
    | ShootingIntervalM String
    | ShootingIntervalS String
    | ClipLengthH String
    | ClipLengthM String
    | ClipLengthS String
    | FramesPerSecond String
    | ImageSize String


update : Msg -> Model -> Model
update msg model =
  case msg of
    ShootingIntervalH h ->
      { model | interval_h = h }

    ShootingIntervalM m ->
      { model | interval_m = m }

    ShootingIntervalS s ->
      { model | interval_s = s }

    ClipLengthH h ->
      { model | length_h = h }

    ClipLengthM m ->
      { model | length_m = m }

    ClipLengthS s ->
      { model | length_s = s }

    FramesPerSecond frames ->
      { model | fps = frames }

    ImageSize size ->
      { model | size = size }



-- VIEW



view : Model -> Html Msg
view model =
    div [ class "wrapper"
        ]
        [ nav [ class "navigation"
              ]
              [ section [ class "container"
                        ]
                        [ a [ href "#", class "navbar-brand" ] [ text "Timelapse calculator" ] ]
              ]
        , section [ class "container"
              ]
              [ div [] [ div [ class "row"
                             ]
                             [ label [ class "column"
                                     ]
                                     [ text "Shooting Interval" ]
                             , div [ class "column"
                                   ]
                                   [ input [ type' "number"
                                           , placeholder "Hours"
                                           , value (.interval_h model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ShootingIntervalH
                                           ] []
                                   , span [ class "label" ] [ text "h" ]
                                   , input [ type' "number"
                                           , placeholder "Munutes"
                                           , value (.interval_m model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ShootingIntervalM
                                           ] []
                                   , span [ class "label" ] [ text "m" ]
                                   , input [ type' "number"
                                           , placeholder "Seconds"
                                           , value (.interval_s model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ShootingIntervalS
                                           ] []
                                   , span [ class "label" ] [ text "s" ]
                                   ]
                             ]
                       , div [ class "row"
                             ]
                             [ label [ class "column"
                                     ]
                                     [ text "Clip Length" ]
                             , div [ class "column"
                                   ]
                                   [ input [ type' "number"
                                           , placeholder "Hours"
                                           , value (.length_h model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ClipLengthH
                                           ] []
                                   , span [ class "label" ] [ text "h" ]
                                   , input [ type' "number"
                                           , placeholder "Munutes"
                                           , value (.length_m model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ClipLengthM
                                           ] []
                                   , span [ class "label" ] [ text "m" ]
                                   , input [ type' "number"
                                           , placeholder "Seconds"
                                           , value (.length_s model)
                                           , Html.Attributes.min "0"
                                           , Html.Attributes.max "59"
                                           , onInput ClipLengthS
                                           ] []
                                   , span [ class "label" ] [ text "s" ]
                                   ]
                             ]
                       , div [ class "row"
                             ]
                             [ label [ class "column"
                                     ]
                                     [ text "Frames per second" ]
                             , div [ class "column"
                                   ]
                                   [ input [ type' "number"
                                           , placeholder "Frames"
                                           , value (.fps model)
                                           , Html.Attributes.min "1"
                                           , Html.Attributes.max "100"
                                           , onInput FramesPerSecond
                                           ] []
                                   , span [ class "label" ] [ text "fps" ]
                                   ]
                             ]
                       , div [ class "row"
                             ]
                             [ label [ class "column"
                                     ]
                                     [ text "Image size" ]
                             , div [ class "column"
                                   ]
                                   [ input [ type' "number"
                                           , placeholder "Size"
                                           , value (.size model)
                                           , Html.Attributes.min "1"
                                           , Html.Attributes.max "100"
                                           , onInput ImageSize
                                           ] []
                                   , span [ class "label" ] [ text "MB" ]
                                   ]
                             ]
                       ]
                 , calculate model
                 ]
       ]


toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

nth : Int -> List String -> String
nth n l = (Maybe.withDefault "" (List.head (List.take 1 (List.drop n l))))

displaySize : Float -> String
displaySize size =
  case size of
    0 -> "0 B"
    _ ->
      let
        size_name = ["MB", "GB", "TB", "PB", "EB", "ZB", "YB"]
        i = floor (logBase 1024 size)
        p = 1024 ^ i
        s = size / (toFloat p)
        unit = nth i size_name
      in
        (toString s) ++ " " ++ unit

displayTime : Int -> String
displayTime seconds =
  let
    d = seconds // 86400
    h = (seconds % 86400) // 3600
    m = (seconds % 3600) // 60
    s = seconds % 60
  in
    (toString d) ++ "d " ++ (toString h) ++ "h " ++ (toString m) ++ "m " ++ (toString s) ++ "s "




calculate : Model -> Html msg
calculate model =
  let
    interval_h = (toInt (.interval_h model))
    interval_m = (toInt (.interval_m model))
    interval_s = (toInt (.interval_s model))
    length_h = (toInt (.length_h model))
    length_m = (toInt (.length_m model))
    length_s = (toInt (.length_s model))
    fps = (toInt (.fps model))
    size = (toInt (.size model))
    interval = interval_h * 3600 + interval_m * 60 + interval_s
    length = length_h * 3600 + length_m * 60 + length_s
    duration = interval * fps * length
    photos = length * fps
    total_size = toFloat (size * photos)
    d = round 1.2
  in
    table [] [ tr [
                  ]
                  [ td [] [ text "Event duration" ]
                  , td [
                       ]
                       [ text (displayTime duration)
                       ]
                  ]
             , tr [
                  ]
                  [ td [] [ text "Number of photos" ]
                  , td [
                       ]
                       [ text (toString photos)
                       ]
                  ]
             , tr [
                  ]
                  [ td [] [ text "Total memory usage" ]
                  , td [
                       ]
                       [ text (displaySize total_size)
                       ]
                  ]
             ]
