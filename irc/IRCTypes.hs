module IRCTypes where

data ServerMessage = ServerMessage
    { prefix :: Maybe String
    , command :: ServerCommand
    , params :: [String]
    } deriving Show

data ServerCommand
    = NumericCommand String
    | StringCommand String
    deriving Show
