data StringTree = StringTree String [StringTree]

hierarchy :: StringTree
hierarchy = StringTree "Users"
            [ StringTree "Guest" []
            , StringTree "rasjonell"
                [StringTree "Desktop" []]
            , StringTree "someone_else" []
            ]

data DialogResponse = Yes
    | No
    | Help
    | Quit

data MaybeInt = NoInt
    | JustInt Int

defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x)      = x

data StringList = EmptyStringList
    | ConsStringList String StringList

lengthStringList :: StringList -> Int
lengthStringList EmptyStringList = 0
lengthStringList (ConsStringList _ tl) =
  1 + lengthStringList tl
