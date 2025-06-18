module Tests exposing (..)

import Dict
import Expect
import String
import Test exposing (..)
import Xml.Encode exposing (..)


example : Value
example =
    object
        [ ( "name", Dict.empty, string "noah" )
        , ( "age", Dict.empty, int 5 )
        ]


exampleAsString : String
exampleAsString =
    """
<name>noah</name>
<age>5</age>
"""
        |> String.trim


exampleWithProps : Value
exampleWithProps =
    object
        [ ( "person"
          , Dict.fromList
                [ ( "name", string "noah" )
                , ( "age", int 5 )
                ]
          , string "noah"
          )
        ]


exampleWithPropsAsString : String
exampleWithPropsAsString =
    """
    <person age="5" name="noah">noah</person>
"""
        |> String.trim


nestedExample : Value
nestedExample =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.empty, string "noah" )
                , ( "age", Dict.fromList [ ( "max", int 100 ) ], int 50 )
                ]
          )
        ]


nestedExampleAsString : String
nestedExampleAsString =
    """
<person>
    <name>noah</name>
    <age max="100">50</age>
</person>
"""
        |> String.trim


all : Test
all =
    describe "Encode tests"
        [ test "a basic tag is encoded properly" <|
            \_ ->
                Expect.equal exampleAsString (encode 0 example)
        , test "a tag with props is encoded properly" <|
            \_ ->
                Expect.equal exampleWithPropsAsString (encode 4 exampleWithProps)
        , test "a nested tag is encoded properly" <|
            \_ ->
                Expect.equal nestedExampleAsString (encode 4 nestedExample)
        , test "Encode tag with NullNode given omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" Dict.empty NullNode
                in
                Expect.equal (encode 0 val) ""
        , test "Encode tag with NullNode but also with attributes given omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" (Dict.fromList [ ( "attr", NullNode ) ]) NullNode
                in
                Expect.equal (encode 0 val) "<tagname attr=\"\"></tagname>"
        , test "Encode tag with NullNode given not omitNullTag" <|
            \_ ->
                let
                    val =
                        Tag "tagname" Dict.empty NullNode

                    setts =
                        { defaultEncodeSettings | omitNullTag = False }
                in
                Expect.equal (encodeWith setts 0 val) "<tagname></tagname>"
        , describe "Test attributes"
            [ test "Encode attribute value" <|
                \_ ->
                    let
                        val =
                            Tag "tagname" (Dict.fromList [ ( "attr", string "'" ) ]) (StrNode "")
                    in
                    Expect.equal (encode 0 val) "<tagname attr=\"&apos;\"></tagname>"
            , test "Encode attribute value with single quotes" <|
                \_ ->
                    let
                        val =
                            Tag "tagname" (Dict.fromList [ ( "attr", string "\"" ) ]) (StrNode "")

                        setts =
                            { defaultEncodeSettings | attributeSingleQuoteInsteadOfDouble = True }
                    in
                    Expect.equal (encodeWith setts 0 val) "<tagname attr='&quot;'></tagname>"
            ]
        ]