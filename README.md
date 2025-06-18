# elm-xml-encode

XML encoder for Elm with CDATA support.

This is a fork of [billstclair/elm-xml-eeue56](https://github.com/billstclair/elm-xml-eeue56/) that is focused on only XML encoding functionality and supporting CDATA tags.

## Installation

```bash
elm install dillonkearns/elm-xml-encode
```

## Basic Usage

```elm
import Xml.Encode exposing (..)
import Dict

xmlDocument : String
xmlDocument =
    object 
        [ ( "root", Dict.empty, string "Hello, World!" ) ]
        |> encode 0
-- Result: "<root>Hello, World!</root>"
```

## CDATA Support

Use `cdata` to embed HTML or other XML content without entity escaping:

```elm
import Xml.Encode exposing (..)
import Dict

-- Regular string encoding (entities are escaped)
regularContent : String
regularContent =
    object 
        [ ( "description", Dict.empty, string "<p>HTML & symbols</p>" ) ]
        |> encode 0
-- Result: "<description>&lt;p&gt;HTML &amp; symbols&lt;/p&gt;</description>"

-- CDATA encoding (content preserved as-is)
cdataContent : String  
cdataContent =
    object 
        [ ( "description", Dict.empty, cdata "<p>HTML & symbols</p>" ) ]
        |> encode 0
-- Result: "<description><![CDATA[<p>HTML & symbols</p>]]></description>"

-- CDATA automatically handles problematic sequences
problematicContent : String
problematicContent =
    object 
        [ ( "content", Dict.empty, cdata "Text with ]]> sequence" ) ]
        |> encode 0
-- Result: "<content><![CDATA[Text with ]]]]><![CDATA[> sequence]]></content>"
```

## Features

- XML encoding with entity escaping
- Support for attributes
- Validation of XML tag and attribute names
- CDATA support for embedding HTML or other XML-style content


## API Reference

See the [package documentation](https://package.elm-lang.org/packages/dillonkearns/elm-xml-encode/1.0.0/) for detailed API information.
