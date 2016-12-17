module Connect4 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, placeholder, style)
import Html.Events exposing (..)
import Maybe exposing (..)
import Json.Decode exposing (..)
import WebSocket


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Phase
    = NewGamePh
    | InputNamePh
    | SelectOpponentPh
    | PlayOthersPh
    | PlayComputerPh
    | EndPh


type alias Model =
    { -- next player's index
      turn :
        Int
        -- current player index
    , index :
        Int
        -- game board
    , board :
        List (List Int)
        -- index of winner
    , winner :
        Int
        -- local player's name
    , name :
        String
        -- opponent's name
    , opponent :
        String
        -- possible opponent's name
    , players :
        List String
        -- current state of play
    , phase :
        Phase
        -- last player's move
    , move :
        String
        -- local info message
    , info : String
    }


type Msg
    = Reset
    | Move Int Int
    | Players Int
    | ToServer String
    | FromServer String
    | InputName String
    | SendName
    | QuitGame
    | Play String


init : ( Model, Cmd Msg )
init =
    ( { turn = 1
      , index = 0
      , board = (List.repeat 7 (List.repeat 6 0))
      , winner = 0
      , name = ""
      , opponent = ""
      , players = []
      , phase = NewGamePh
      , move = ""
      , info = ""
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model.phase of
        NewGamePh ->
            div []
                [ h3 [] [ text "Player mode" ]
                , p []
                    [ radio (Players 1) "1 Player"
                    , radio (Players 2) "2 Players"
                    ]
                ]

        InputNamePh ->
            div []
                [ h3 [] [ text "Connect 4 - Room" ]
                , ul []
                    (List.map
                        (\p -> li [] [ text p ])
                        model.players
                    )
                , h3 [] [ text "Enter the room" ]
                , input
                    [ type_ "text"
                    , placeholder "Your Name"
                    , onInput InputName
                    ]
                    []
                , button [ onClick SendName ] [ text "Enter" ]
                ]

        SelectOpponentPh ->
            div []
                [ h3 []
                    [ text ("Select your oppoonent") ]
                , select
                    [ onInput Play ]
                    (option
                        []
                        [ text "--- Select ---" ]
                        :: (List.map
                                (\p ->
                                    option []
                                        [ text p ]
                                )
                                model.players
                           )
                    )
                ]

        PlayComputerPh ->
            div []
                [ h3 []
                    [ text "Not implemented yet"
                    ]
                , p []
                    (if model.winner == 0 then
                        [ button [ onClick QuitGame ] [ text "Quit" ] ]
                     else
                        [ text ("Computer is the winner")
                        , button [ onClick Reset ] [ text "Restart" ]
                        ]
                    )
                ]

        PlayOthersPh ->
            div []
                [ h3 [] [ text "Connect 4" ]
                , p []
                    (if model.winner == 0 then
                        [ text
                            ((if model.turn == model.index then
                                "Your turn to play, "
                              else
                                "Wait for your turn, "
                             )
                                ++ model.name
                                ++ " "
                            )
                        , button [ onClick QuitGame ] [ text "Quit" ]
                        ]
                     else
                        [ text
                            ((if model.winner == model.index then
                                "You"
                              else
                                model.opponent
                             )
                                ++ " won !!!"
                            )
                        , button [ onClick Reset ] [ text "Restart" ]
                        ]
                    )
                , div [ boardStyle ]
                    (List.indexedMap column model.board)
                ]

        EndPh ->
            div []
                [ h3 [] [ text "Game Ended" ]
                , div [] [ text model.info ]
                , button [ onClick Reset ] [ text "Restart" ]
                ]


wsAddress : String
wsAddress =
    "ws://localhost:8080/websocket"


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsAddress FromServer


type alias Wmsg =
    { key : String, val : String }


wsRespDecoder : Decoder Wmsg
wsRespDecoder =
    Json.Decode.map2 Wmsg (field "key" string) (field "val" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromServer s ->
            let
                m =
                    case decodeString wsRespDecoder s of
                        Ok r ->
                            case r.key of
                                "joined" ->
                                    { model
                                        | players =
                                            r.val :: model.players
                                    }

                                "started" ->
                                    { model
                                        | phase = PlayOthersPh
                                        , index =
                                            (case String.toInt r.val of
                                                Err msg ->
                                                    Debug.crash ("Not a number: " ++ r.val)

                                                Ok val ->
                                                    val
                                            )
                                    }

                                "move" ->
                                    let
                                        ( x, y, t ) =
                                            parseMove r.val
                                    in
                                        let
                                            updated =
                                                updateBoard x y t model.board
                                        in
                                            if updated == model.board || model.winner /= 0 then
                                                model
                                            else
                                                { model
                                                    | board = updated
                                                    , turn = nextTurn t
                                                    , winner = winner updated
                                                }

                                "quit" ->
                                    { model
                                        | phase = EndPh
                                        , info = r.val ++ " has terminated the game!"
                                    }

                                _ ->
                                    model

                        Err e ->
                            model
            in
                ( m, Cmd.none )

        ToServer s ->
            ( model, WebSocket.send wsAddress s )

        InputName s ->
            ( { model | name = s }, Cmd.none )

        SendName ->
            let
                json =
                    "{'msg':'join','game':'connect4','player':'" ++ model.name ++ "'}"
            in
                ( { model | phase = SelectOpponentPh }
                , WebSocket.send wsAddress json
                )

        Players n ->
            let
                json =
                    "{'msg':'players','game':'connect4'}"
            in
                ( { model | phase = InputNamePh }
                , WebSocket.send wsAddress json
                )

        Play s ->
            let
                json =
                    "{'msg':'start','players': ['" ++ s ++ "']}"
            in
                ( { model | opponent = s }
                , WebSocket.send wsAddress json
                )

        Move x y ->
            if model.index /= model.turn then
                ( model, Cmd.none )
            else
                let
                    updated =
                        updateBoard x y model.index model.board
                in
                    let
                        json =
                            -- just 2 players for now
                            "{'msg':'move','move': '" ++ toString x ++ "," ++ toString y ++ "'}"
                    in
                        if updated == model.board || model.winner /= 0 then
                            ( model, Cmd.none )
                        else
                            ( { model
                                | board = updated
                                , turn = nextTurn model.turn
                                , winner = winner updated
                              }
                            , WebSocket.send wsAddress json
                            )

        QuitGame ->
            let
                json =
                    "{'msg':'quit'}"
            in
                ( { model | phase = EndPh }
                , WebSocket.send wsAddress json
                )

        Reset ->
            init



-- helpers


parse_xyp : String -> String -> ( Int, Int, Int )
parse_xyp xy p =
    case String.split "," xy of
        [ x, y ] ->
            let
                x2 =
                    case String.toInt x of
                        Err msg ->
                            Debug.crash ("Not a number: " ++ x)

                        Ok val ->
                            val

                y2 =
                    case String.toInt y of
                        Err msg ->
                            Debug.crash ("Not a number: " ++ y)

                        Ok val ->
                            val

                p2 =
                    case String.toInt p of
                        Err msg ->
                            Debug.crash ("Not a number: " ++ p)

                        Ok val ->
                            val
            in
                ( x2, y2, p2 )

        _ ->
            Debug.crash ("Invalid move " ++ xy)


parseMove : String -> ( Int, Int, Int )
parseMove s =
    case String.split "@" s of
        [ xy, p ] ->
            parse_xyp xy p

        _ ->
            Debug.crash ("Invalid move " ++ s)


nextTurn : Int -> Int
nextTurn n =
    if n == 1 then
        2
    else
        1


{-| Index of last non-zero value in a list.

    indexOfCounter [1, 1, 0, 0] == 1
    indexOfCounter [0, 0, 0] == -1

-}
indexOfCounter : List Int -> Int
indexOfCounter column =
    List.foldr max
        -1
        (List.indexedMap
            (\k v ->
                if v /= 0 then
                    k
                else
                    -1
            )
            column
        )


{-| Insert the piece at co-ordinates if it matches connect4 game rules

    updateBoard 1 1 2 [[0,0,0],[1,0,0],[0,0,0]] == [[0,0,0],[1,2,0],[0,0,0]]
    updateBoard 1 1 2 [[0,0,0],[0,0,0],[0,0,0]] == [[0,0,0],[0,0,0],[0,0,0]]

-}
updateBoard : Int -> Int -> Int -> List (List Int) -> List (List Int)
updateBoard x y p board =
    List.indexedMap
        (\k col ->
            if k == y then
                List.indexedMap
                    (\k v ->
                        if k == x && x == ((indexOfCounter col) + 1) then
                            p
                        else
                            v
                    )
                    col
            else
                col
        )
        board



-- checker


{-| Find all sub lists of specified length that occur in the list

    listSubs 3 [1, 2, 3, 4] == [[1,2,3],[2,3,4]]
    listSubs 4 [1, 2, 3, 4] == [[1, 2, 3, 4]]
    listSubs 5 [1, 2, 3, 4] == []

-}
listSubs : Int -> List a -> List (List a)
listSubs n l =
    let
        build l =
            case l of
                [] ->
                    []

                h :: t ->
                    let
                        s =
                            List.take n l
                    in
                        if List.length s < n then
                            []
                        else
                            s :: build t
    in
        build l


winner board =
    let
        rows =
            List.foldr max 0 (List.map fourInRow board)
    in
        let
            columns =
                List.foldr max 0 (List.map fourInRow (transpose board))
        in
            let
                diagsUp =
                    List.foldr max 0 (List.map fourInRow (diags board))
            in
                let
                    diagsDown =
                        List.foldr max 0 (List.map fourInRow (diags (List.map List.reverse board)))
                in
                    max (max rows columns) (max diagsUp diagsDown)


fourInRow r =
    let
        seriesOf v =
            List.any (\x -> x) (List.map (List.all (\x -> x == v)) (listSubs 4 r))
    in
        if seriesOf 1 then
            1
        else if seriesOf 2 then
            2
        else
            0


nth n l =
    case l of
        [] ->
            0

        h :: t ->
            if n == 0 then
                h
            else
                nth (n - 1) t


lth n l =
    case l of
        [] ->
            []

        h :: t ->
            if n == 0 then
                h
            else
                lth (n - 1) t



-- [ [1, 2, 3]
-- , [4, 5, 6]
-- , [7, 8, 9]
-- ]
-- # transform #
-- [ [1, 4, 7]
-- , [2, 5, 8]
-- , [3, 6, 9]
-- ]


transpose b =
    let
        len =
            List.length (withDefault [] (List.head b))
    in
        let
            build n l =
                case l of
                    [] ->
                        []

                    h :: t ->
                        nth n h :: build n t
        in
            let
                rec n =
                    if n >= len then
                        []
                    else
                        build n b :: rec (n + 1)
            in
                rec 0



-- XXX overruns borders with some input. But still works for our use
-- Connect4.diags [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
-- [[10,8,6,0],[7,5,3],[4,2],[1],[12],[11,9]] : List (List number)


diags b =
    colDiags b ++ rowDiags b


colDiags b =
    let
        columns =
            List.length b
    in
        let
            getCell x y =
                if x < 0 then
                    []
                else
                    nth y (lth x b) :: getCell (x - 1) (y + 1)
        in
            let
                getCols x =
                    if x == 0 then
                        []
                    else
                        getCell (x - 1) 0 :: getCols (x - 1)
            in
                getCols columns


rowDiags b =
    let
        columns =
            List.length b
    in
        let
            rows =
                List.length (withDefault [] (List.head b))
        in
            let
                getCell x y =
                    if y == rows then
                        []
                    else
                        nth y (lth x b) :: getCell (x - 1) (y + 1)
            in
                let
                    getRows y =
                        if y == 0 then
                            []
                        else
                            getCell (columns - 1) y :: getRows (y - 1)
                in
                    getRows (rows - 1)



-- rendering


column y p =
    div [ style [ ( "display", "inline-block" ) ] ]
        (List.reverse (List.indexedMap (square y) p))


boardStyle =
    style
        [ ( "background", "blue" )
        , ( "display", "inline-block" )
        ]


square y x p =
    div
        [ style
            [ ( "width", "50px" )
            , ( "height", "50px" )
            , ( "border-radius", "25px" )
            , ( "background"
              , if p == 1 then
                    "yellow"
                else if p == 2 then
                    "red"
                else
                    "black"
              )
            , ( "margin", "10px" )
            ]
        , onClick (Move x y)
        ]
        []


radio : msg -> String -> Html msg
radio msg name =
    label []
        [ input [ type_ "radio", onClick msg ] []
        , text name
        ]
