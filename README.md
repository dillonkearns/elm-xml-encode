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

## Features

- XML encoding with entity escaping
- Support for attributes
- Validation of XML tag and attribute names
- CDATA support for embedding HTML or other XML-style content


## API Reference

See the [package documentation](https://package.elm-lang.org/packages/dillonkearns/elm-xml-encode/latest/) for detailed API information.
