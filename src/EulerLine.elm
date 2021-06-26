module EulerLine exposing (..)

import Browser
import Geom exposing (..)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (checked, for, id, type_)
import Html.Events exposing (onCheck)
import Json.Decode as D
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes as Attr
import Svg.Events as Evt


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type HandleId
    = A
    | B
    | C


type alias Model =
    { a : Vec2
    , b : Vec2
    , c : Vec2
    , dragging : Maybe HandleId
    , showOrthocenter : Bool
    , showCentroid : Bool
    , showCircumcenter : Bool
    }


type Msg
    = Mouse Float Float
    | DragStart HandleId
    | DragStop
    | CheckOrthocenter Bool
    | CheckCentroid Bool
    | CheckCircumcenter Bool


handleRadius : Float
handleRadius =
    7


init : Model
init =
    { a = ( 10, 10 )
    , b = ( 10, 300 )
    , c = ( 200, 100 )
    , dragging = Nothing
    , showOrthocenter = False
    , showCentroid = False
    , showCircumcenter = False
    }


viewGuide : (Model -> List (Svg Msg)) -> Bool -> Model -> List (Svg Msg)
viewGuide viewf show tri =
    if show then
        viewf tri

    else
        []


svgEvents : Model -> List (Svg.Attribute Msg)
svgEvents model =
    case model.dragging of
        Nothing ->
            []

        Just _ ->
            [ Evt.on "mousemove"
                (D.map2 Mouse
                    (D.field "offsetX" D.float)
                    (D.field "offsetY" D.float)
                )
            , Evt.onMouseUp DragStop
            ]


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Euler line" ]
        , viewControls model
        , svg
            (svgEvents model
                ++ [ Attr.width "100%"
                   , Attr.height "400px"
                   , Attr.style "border: 1px solid lightgray; box-sizing: border-box;"
                   ]
            )
            (viewGuide viewOrthocenter model.showOrthocenter model
                ++ viewGuide viewCentroid model.showCentroid model
                ++ viewGuide viewCircumcenter model.showCircumcenter model
                ++ viewEulerLine model
                ++ viewTriangle model
            )
        ]


viewControls : Model -> Html Msg
viewControls config =
    div
        []
        [ checkbox CheckOrthocenter config.showOrthocenter "Show orthocenter"
        , checkbox CheckCentroid config.showCentroid "Show centroid"
        , checkbox CheckCircumcenter config.showCircumcenter "Show circumcenter"
        ]


viewEulerLine : Model -> List (Svg Msg)
viewEulerLine { a, b, c } =
    let
        ( orthoX, orthoY ) =
            orthocenter a b c

        ( centrX, centrY ) =
            centroid a b c

        ( circuX, circuY ) =
            circumcenter a b c
    in
    [ circle
        [ Attr.r "2"
        , Attr.fill "blue"
        , Attr.cx (String.fromFloat orthoX)
        , Attr.cy (String.fromFloat orthoY)
        ]
        []
    , circle
        [ Attr.r "2"
        , Attr.fill "green"
        , Attr.cx (String.fromFloat centrX)
        , Attr.cy (String.fromFloat centrY)
        ]
        []
    , circle
        [ Attr.r "2"
        , Attr.fill "red"
        , Attr.cx (String.fromFloat circuX)
        , Attr.cy (String.fromFloat circuY)
        ]
        []
    , svgLine [ Attr.stroke "black" ] ( orthoX, orthoY ) ( circuX, circuY )
    ]


checkbox : (Bool -> Msg) -> Bool -> String -> Html Msg
checkbox ontoggle isChecked label =
    div
        []
        [ Html.input [ type_ "checkbox", id label, onCheck ontoggle, checked isChecked ] []
        , Html.label [ for label ] [ text label ]
        ]


svgLine : List (Svg.Attribute msg) -> Vec2 -> Vec2 -> Svg msg
svgLine attrs ( ax, ay ) ( bx, by ) =
    line
        (attrs
            ++ [ Attr.x1 (String.fromFloat ax)
               , Attr.y1 (String.fromFloat ay)
               , Attr.x2 (String.fromFloat bx)
               , Attr.y2 (String.fromFloat by)
               ]
        )
        []


svgPath : List (Svg.Attribute msg) -> Bool -> List Vec2 -> Svg msg
svgPath attrs closed points =
    let
        point char ( x, y ) =
            char ++ String.fromFloat x ++ " " ++ String.fromFloat y

        path =
            case points of
                [] ->
                    ""

                x :: xs ->
                    String.join " " (point "M" x :: List.map (point "L") xs)
                        ++ (if closed then
                                " Z"

                            else
                                ""
                           )
    in
    Svg.path (Attr.d path :: attrs) []


svgArrow : List (Svg.Attribute msg) -> Vec2 -> Vec2 -> Svg msg
svgArrow attrs a b =
    Svg.g
        attrs
        [ svgLine [] a b
        , circle
            [ Attr.r "2"
            , Attr.fill "black"
            , Attr.cx (String.fromFloat <| Tuple.first b)
            , Attr.cy (String.fromFloat <| Tuple.second b)
            ]
            []
        ]


viewOrthocenter : Model -> List (Svg Msg)
viewOrthocenter { a, b, c } =
    let
        ab =
            sub a b

        bc =
            sub b c

        ca =
            sub c a

        ha =
            add b (project ab bc)

        hb =
            add c (project bc ca)

        hc =
            add a (project ca ab)
    in
    [ svgArrow [ Attr.stroke "#aaaaff" ] a ha
    , svgArrow [ Attr.stroke "#aaaaff" ] b hb
    , svgArrow [ Attr.stroke "#aaaaff" ] c hc
    ]


viewCentroid : Model -> List (Svg Msg)
viewCentroid { a, b, c } =
    let
        ma =
            mul 0.5 (add b c)

        mb =
            mul 0.5 (add c a)

        mc =
            mul 0.5 (add a b)
    in
    [ svgArrow [ Attr.stroke "#aaffaa" ] a ma
    , svgArrow [ Attr.stroke "#aaffaa" ] b mb
    , svgArrow [ Attr.stroke "#aaffaa" ] c mc
    ]


viewCircumcenter : Model -> List (Svg Msg)
viewCircumcenter { a, b, c } =
    let
        ma =
            mul 0.5 (add b c)

        mb =
            mul 0.5 (add c a)

        mc =
            mul 0.5 (add a b)

        center =
            rayInterceptRay
                ma
                (rot90 <| sub b c)
                mb
                (rot90 <| sub a c)
    in
    [ svgLine [ Attr.stroke "#ffaaaa" ] ma center
    , svgLine [ Attr.stroke "#ffaaaa" ] mb center
    , svgLine [ Attr.stroke "#ffaaaa" ] mc center
    ]


viewTriangle : Model -> List (Svg Msg)
viewTriangle { a, b, c } =
    [ svgPath
        [ Attr.stroke "black"
        , Attr.fill "none"
        ]
        True
        [ a, b, c ]
    , viewHandle A a
    , viewHandle B b
    , viewHandle C c
    ]


viewHandle : HandleId -> Vec2 -> Svg Msg
viewHandle name ( x, y ) =
    circle
        [ Attr.cx (String.fromFloat x)
        , Attr.cy (String.fromFloat y)
        , Attr.fill "red"
        , Attr.r (String.fromFloat handleRadius)
        , Evt.onMouseDown (DragStart name)
        ]
        []


updateTriangle : Model -> HandleId -> Vec2 -> Model
updateTriangle tri hid mouse =
    case hid of
        A ->
            { tri | a = mouse }

        B ->
            { tri | b = mouse }

        C ->
            { tri | c = mouse }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mouse x y ->
            case model.dragging of
                Nothing ->
                    model

                Just hid ->
                    updateTriangle model hid ( x, y )

        DragStart index ->
            { model | dragging = Just index }

        DragStop ->
            { model | dragging = Nothing }

        CheckOrthocenter flag ->
            { model | showOrthocenter = flag }

        CheckCentroid flag ->
            { model | showCentroid = flag }

        CheckCircumcenter flag ->
            { model | showCircumcenter = flag }
