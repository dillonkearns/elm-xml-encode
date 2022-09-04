module Xml.Encode exposing
    ( encode
    , string, int, float, bool, object, null, list
    )

{-|

    Use this module for turning your Elm data into an `Xml` representation that can be either
    queried or decoded, or turned into a string.

@docs encode

@docs string, int, float, bool, object, null, list

-}

import Dict exposing (Dict)
import Xml exposing (Value(..), isValidXmlName)


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


propToString : Value -> String
propToString value =
    case value of
        StrNode str ->
            str

        IntNode n ->
            String.fromInt n

        BoolNode b ->
            boolToString b

        FloatNode f ->
            String.fromFloat f

        _ ->
            ""


propsToString : Dict String Value -> String
propsToString props =
    Dict.toList props
        |> List.map (\( key, value ) -> key ++ "=\"" ++ propToString value ++ "\"")
        |> String.join " "
        |> (\x ->
                if String.length x > 0 then
                    " " ++ x

                else
                    ""
           )


needsIndent : Value -> Bool
needsIndent nextValue =
    case nextValue of
        Object [] ->
            False

        Object _ ->
            True

        Tag _ _ _ ->
            True

        _ ->
            False


valueToString : Int -> Int -> Value -> String
valueToString level indent value =
    case value of
        Tag name props nextValue ->
            let
                indentString =
                    if needsIndent nextValue then
                        "\n"

                    else
                        ""

                indentClosing =
                    if needsIndent nextValue then
                        String.repeat (level * indent) " "

                    else
                        ""
            in
            String.repeat (level * indent) " "
                ++ "<"
                ++ name
                ++ propsToString props
                ++ ">"
                ++ indentString
                ++ valueToString level indent nextValue
                ++ indentString
                ++ indentClosing
                ++ "</"
                ++ name
                ++ ">"

        StrNode str ->
            str

        IntNode n ->
            String.fromInt n

        FloatNode n ->
            String.fromFloat n

        BoolNode b ->
            boolToString b

        Object xs ->
            List.map (valueToString (level + 1) indent) xs
                |> String.join "\n"

        DocType name props ->
            "<?"
                ++ name
                ++ propsToString props
                ++ "?>"


{-| Take a value, then generate a string from it
-}
encode : Int -> Value -> String
encode indent value =
    valueToString -1 indent value


{-| Encode a string

    string "hello" |> encode 0
    --> "hello"

-}
string : String -> Value
string str =
    StrNode str


{-| Encode an int

    int 15 |> encode 0
    --> "15"

-}
int : Int -> Value
int n =
    IntNode n


{-| Encode a float

    float 1.576 |> encode 0
    --> "1.576"

-}
float : Float -> Value
float n =
    FloatNode n


{-| Encode a bool

    bool True |> encode 0
    --> "true"

    bool True |> encode 0
    --> "true"

-}
bool : Bool -> Value
bool b =
    BoolNode b


{-| Encode an "object" (a tag)
-}
object : List ( String, Dict String Value, Value ) -> Value
object values =
    List.map (\( name, props, value ) -> Tag name props value) values
        |> Object



-- TODO: Attribute names AND tag names must be validated.
-- Tags are also created in jsonToXml!


{-| Encode an "object" (a tag) only allowing valid tag names

    import Dict

    objectSafe [ (" no valid tag!", Dict.empty, string "") ]
    --> Err "Invalid tag names: no valid tag!"

    objectSafe [ ("tagname", Dict.empty, string "") ]
    --> Ok "<tagname/>"

-}
objectSafe : List ( String, Dict String Value, Value ) -> Result String Value
objectSafe values =
    let
        invalidTagNames =
            List.filterMap
                (\( name, _, _ ) ->
                    if isValidXmlName name then
                        Nothing

                    else
                        Just name
                )
                values
    in
    if List.isEmpty invalidTagNames then
        List.map (\( name, props, value ) -> Tag name props value) values
            |> Object
            |> Ok

    else
        Err ("Invalid tag names: " ++ String.concat (List.intersperse ", " invalidTagNames))


{-| Encode a list of nodes, e.g

    import Dict

    list [ object [ ("Root", Dict.empty, null) ], int 5 ] |> encode 0
    --> "<Root></Root>\n5"

-}
list : List Value -> Value
list values =
    Object values


{-| Empty contents

    null |> encode 0
    --> ""

-}
null : Value
null =
    object []
