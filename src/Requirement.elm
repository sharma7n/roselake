module Requirement exposing
    ( Requirement(..)
    )

import Attribute exposing (Attribute)

type Requirement
    = AttributeRequirement Attribute Int