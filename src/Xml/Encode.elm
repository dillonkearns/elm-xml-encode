module Xml.Encode exposing
    ( encode, encodeWith, EncodeSettings, defaultEncodeSettings
    , string, int, float, bool, object, objectSafe, null, list, cdata
    , Value(..)
    -- Exposing Value type and constructors
    )

{-| Use this module for turning your Elm data into an XML string.

@docs encode, encodeWith, EncodeSettings, defaultEncodeSettings

@docs string, int, float, bool, object, objectSafe, null, list, cdata

@docs Value

-}

-- TODO: Attribute names AND tag names must be validated.
-- Tags are also created in jsonToXml!

import Dict exposing (Dict)
import Regex
import String


{-| Representation of the XML tree
-}
type Value
    = Tag String (Dict String Value) Value
    | StrNode String
    | IntNode Int
    | FloatNode Float
    | BoolNode Bool
    | NullNode
    | Object (List Value)
    | DocType String (Dict String Value)
    | CdataNode String


{-| Encode string with XML entities
-}
encodeXmlEntities : String -> String
encodeXmlEntities s =
    List.foldr (\( x, y ) z -> String.replace (String.fromChar x) ("&" ++ y ++ ";") z) s predefinedEntities


predefinedEntities : List ( Char, String )
predefinedEntities =
    -- https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
    [ ( '"', "quot" )
    , ( '\'', "apos" )
    , ( '<', "lt" )
    , ( '>', "gt" )

    -- & / &amp; must come last!
    , ( '&', "amp" )
    ]


{-| Escape CDATA content by splitting ]]> sequences
-}
escapeCdataContent : String -> String
escapeCdataContent str =
    -- Replace ]]> with ]]]]><![CDATA[> to properly escape it in CDATA
    String.replace "]]>" "]]]]><![CDATA[>" str


{-| Test that a string is a valid XML name.

This enforces the rules for XML tag and attribute names, as well as for the names of several less common constructs.

See _O'Reilly: XML in a Nutshell_: [2.4 XML Names](https://docstore.mik.ua/orelly/xml/xmlnut/ch02_04.htm).

-}
isValidXmlName : String -> Bool
isValidXmlName =
    let
        nameRegex =
            -- O'Reilly: XML in a Nutshell: https://docstore.mik.ua/orelly/xml/xmlnut/ch02_04.htm
            Maybe.withDefault Regex.never
                (Regex.fromString "^[_a-zA-Z0-9\\p{Letter}][-_.:a-zA-Z0-9\\p{Letter}]*$")
    in
    Regex.contains nameRegex


{-| Settings used by `encodeWith`.

  - `nullValue`: if not `omitNullTag`, encode a `NullNode` like this.
  - `omitNullTag`: if `True`, omit a tag if it has no attributes and
    its only content is a `NullNode`.

-}
type alias EncodeSettings =
    { nullValue : String
    , trueValue : String -- encode True like this
    , falseValue : String -- encode False like this
    , omitNullTag : Bool
    , attributeSingleQuoteInsteadOfDouble : Bool
    }


{-| Good default settings for `EncodeSettings`.
-}
defaultEncodeSettings : EncodeSettings
defaultEncodeSettings =
    { nullValue = ""
    , trueValue = "true"
    , falseValue = "false"
    , omitNullTag = True
    , attributeSingleQuoteInsteadOfDouble = False
    }


boolToString : EncodeSettings -> Bool -> String
boolToString setts b =
    if b then
        setts.trueValue

    else
        setts.falseValue


propToString : EncodeSettings -> Value -> String
propToString setts value =
    case value of
        StrNode str ->
            encodeXmlEntities str

        IntNode n ->
            String.fromInt n

        BoolNode b ->
            boolToString setts b

        FloatNode f ->
            String.fromFloat f

        CdataNode str ->
            -- CDATA should not be used in attributes, so escape it
            encodeXmlEntities str

        _ ->
            ""


propsToString : EncodeSettings -> Dict String Value -> String
propsToString setts props =
    let
        quote =
            if setts.attributeSingleQuoteInsteadOfDouble then
                "'"

            else
                "\""
    in
    props
        |> Dict.foldr (\key value acc -> (key ++ "=" ++ quote ++ propToString setts value ++ quote) :: acc) []
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


valueToString : EncodeSettings -> Int -> Int -> Value -> String
valueToString setts level indent value =
    case value of
        Tag name props nextValue ->
            if setts.omitNullTag && Dict.isEmpty props && nextValue == NullNode then
                ""

            else
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
                    ++ propsToString setts props
                    ++ ">"
                    ++ indentString
                    ++ valueToString setts level indent nextValue
                    ++ indentString
                    ++ indentClosing
                    ++ "</"
                    ++ name
                    ++ ">"

        StrNode str ->
            encodeXmlEntities str

        IntNode n ->
            String.fromInt n

        FloatNode n ->
            String.fromFloat n

        BoolNode b ->
            boolToString setts b

        NullNode ->
            setts.nullValue

        Object xs ->
            List.map (valueToString setts (level + 1) indent) xs
                |> String.join "\n"

        DocType name props ->
            "<?"
                ++ name
                ++ propsToString setts props
                ++ "?>"

        CdataNode str ->
            "<![CDATA[" ++ escapeCdataContent str ++ "]]>"


{-| Take a value, then generate a string from it
-}
encode : Int -> Value -> String
encode =
    encodeWith defaultEncodeSettings


{-| Take a value, then generate a string from it
-}
encodeWith : EncodeSettings -> Int -> Value -> String
encodeWith setts indent value =
    valueToString setts -1 indent value


{-| Encode a string

    string "hello" |> encode 0
    --> "hello"

    string "<hello>" |> encode 0
    --> "&lt;hello&gt;"

-}
string : String -> Value
string =
    StrNode


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


{-| Encode content as CDATA (Character Data) for embedding HTML or other XML content

    cdata "<p>This is <strong>bold</strong> text</p>" |> encode 0
    --> "<![CDATA[<p>This is <strong>bold</strong> text</p>]]>"

-}
cdata : String -> Value
cdata str =
    CdataNode str


{-| Encode an "object" (a tag)
-}
object : List ( String, Dict String Value, Value ) -> Value
object values =
    List.map (\( name, props, value ) -> Tag name props value) values
        |> Object


{-| Encode an "object" (a tag) only allowing valid tag names

    import Dict

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
