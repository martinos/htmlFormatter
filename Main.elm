module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)


css : String -> Html msg
css path =
    node "link" [ rel "stylesheet", href path ] []


toParse =
    """
"""


bulma =
    div []
        [ css "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.1/css/bulma.min.css"
        , node "meta" [ attribute "name" "viewport", attribute "content" "width=device-width, initial-scale=1" ] []
        ]


model =
    { htmlInput = "" }


main =
    Html.beginnerProgram { model = model, update = update, view = view }


update msg model =
    case msg of
        SetInput str ->
            { model | htmlInput = str }


type Msg
    = SetInput String


view model =
    div []
        [ bulma
        , section [ class "section" ]
            [ div [ class "containder" ]
                [ div [ class "columns" ]
                    [ h1 [ class "title" ] [ text "Html Pretty" ] ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ h2 [ class "subtitle" ] [ text "Input" ]
                        , textarea
                            [ class "textarea"
                            , attribute "rows" "30"
                            , placeholder "Enter your json here"
                            , value model.htmlInput
                            , onInput SetInput
                            ]
                            []
                        ]
                    , div [ class "column is-half" ]
                        [ h2 [ class "subtitle" ] [ text "Output" ]
                        , pre [ id "copy-me", style [ ( "position", "relative" ), ( "overflow", "scroll" ) ] ]
                            [ code []
                                [ model.htmlInput |> parse |> Debug.log "PARSED" |> printNodes 0 |> text ]
                            , copyButton
                            ]
                        ]
                    ]
                ]
            ]
        ]


printNodes indent =
    List.filter filterText >> List.map (printNode indent) >> String.join ""


filterText node =
    case node of
        Text str ->
            if str |> String.trim |> String.isEmpty then
                False
            else
                True

        _ ->
            True


printNode : Int -> Node -> String
printNode indent node =
    case node of
        Text str ->
            printIndent indent ++ (str |> String.trim)

        Element name attributes nodes ->
            printElement indent name attributes nodes

        Comment str ->
            printComment (indent) str


printElement indent name _ nodes =
    (printIndent indent ++ oTag name)
        ++ (nodes |> printNodes (1 + indent))
        ++ (printIndent indent ++ cTag name)


oTag name =
    "<" ++ name ++ ">"


cTag name =
    "</" ++ name ++ ">"


printIndent indent =
    "\n" ++ String.repeat (2 * indent) " "


printComment indent str =
    printIndent indent ++ "<!-- " ++ str ++ " -->"


copyButton =
    button
        [ class "copy-button button is-small"
        , attribute "data-clipboard-target" "#copy-me"
        , style [ ( "position", "absolute" ), ( "top", "0.25rem" ), ( "right", "0.25rem" ) ]
        ]
        [ "Copy" |> text ]
