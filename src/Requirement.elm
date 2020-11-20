module Requirement exposing
    ( Requirement(..)
    , toString
    )

import Attribute exposing (Attribute)

type Requirement
    = AttributeRequirement Attribute Int

toString : Requirement -> String
toString r =
    case r of
        AttributeRequirement attr i ->
            String.fromInt i ++ " " ++ Attribute.toString attr