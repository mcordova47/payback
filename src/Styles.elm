module Styles
    exposing
        ( table
        , th
        , td
        , gray
        , grayHighlight
        , primaryColor
        , fontColor
        )

import Css exposing (Color)
import Css.Colors as Colors
import Html.Styled as Html exposing (Html, Attribute, styled)
import Html.Styled.Attributes as Attributes exposing (css)


table : List (Attribute msg) -> List (Html msg) -> Html msg
table =
    styled Html.table
        [ Css.borderCollapse Css.collapse
        , Css.marginBottom (Css.px 10)
        ]


th : List (Attribute msg) -> List (Html msg) -> Html msg
th =
    styled Html.th
        [ Css.textAlign Css.left
        , Css.fontWeight Css.normal
        , Css.color gray
        , Css.fontSize (Css.px 14)
        , Css.padding2 (Css.px 0) (Css.px 10)
        ]


td : List (Attribute msg) -> List (Html msg) -> Html msg
td =
    styled Html.td
        [ Css.borderBottom3 (Css.px 1) Css.solid gray
        , Css.padding (Css.px 10)
        , Css.position Css.relative
        ]


primaryColor : Color
primaryColor =
    Css.hex "3f51b5"


gray : Color
gray =
    Css.hex "adadad"


grayHighlight : Color
grayHighlight =
    Css.hex "f7f7f7"


fontColor : Color
fontColor =
    Css.hex "323232"
