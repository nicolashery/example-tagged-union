module Main (main) where

import Data.Aeson
  ( FromJSON (parseJSON),
    GFromJSON,
    GToJSON,
    Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
    SumEncoding (TaggedObject),
    ToJSON (toJSON),
    Value,
    Zero,
    camelTo2,
    defaultOptions,
    eitherDecode,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic, Rep)

data ObjectType
  = ObjectTypeUser
  | ObjectTypeGroup
  deriving (Show, Generic)

instance FromJSON ObjectType where
  parseJSON = defaultParseJSON "ObjectType"

instance ToJSON ObjectType where
  toJSON = defaultToJSON "ObjectType"

objectTypeToText :: ObjectType -> Text
objectTypeToText v = case v of
  ObjectTypeUser -> "user"
  ObjectTypeGroup -> "group"

data Object = Object
  { objectType :: ObjectType,
    objectId :: Text,
    objectName :: Text
  }
  deriving (Show, Generic)

instance FromJSON Object where
  parseJSON = defaultParseJSON "object"

instance ToJSON Object where
  toJSON = defaultToJSON "object"

data CreateObject = CreateObject
  { createObjectObject :: Object
  }
  deriving (Show, Generic)

instance FromJSON CreateObject where
  parseJSON = defaultParseJSON "createObject"

instance ToJSON CreateObject where
  toJSON = defaultToJSON "createObject"

data UpdateObject = UpdateObject
  { updateObjectObject :: Object
  }
  deriving (Show, Generic)

instance FromJSON UpdateObject where
  parseJSON = defaultParseJSON "updateObject"

instance ToJSON UpdateObject where
  toJSON = defaultToJSON "updateObject"

data DeleteObject = DeleteObject
  { deleteObjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DeleteObject where
  parseJSON = defaultParseJSON "deleteObject"

instance ToJSON DeleteObject where
  toJSON = defaultToJSON "deleteObject"

data Action
  = ActionCreateObject CreateObject
  | ActionUpdateObject UpdateObject
  | ActionDeleteObject DeleteObject
  | ActionDeleteAllObjects
  deriving (Show, Generic)

instance FromJSON Action where
  parseJSON = defaultParseJSON "Action"

instance ToJSON Action where
  toJSON = defaultToJSON "Action"

transformAction :: Action -> Text
transformAction action = case action of
  ActionCreateObject op ->
    let obj = createObjectObject op
     in T.intercalate
          " "
          [ "create_object",
            objectTypeToText (objectType obj),
            objectId obj,
            objectName obj
          ]
  ActionUpdateObject op ->
    let obj = updateObjectObject op
     in T.intercalate
          " "
          [ "update_object",
            objectTypeToText (objectType obj),
            objectId obj,
            objectName obj
          ]
  ActionDeleteObject op ->
    "delete_object " <> deleteObjectId op
  ActionDeleteAllObjects ->
    "delete_all_objects"

exampleActions :: [Action]
exampleActions =
  [ ActionCreateObject $
      CreateObject $
        Object
          { objectType = ObjectTypeUser,
            objectId = "1",
            objectName = "user1"
          },
    ActionUpdateObject $
      UpdateObject $
        Object
          { objectType = ObjectTypeUser,
            objectId = "1",
            objectName = "user1 updated"
          },
    ActionDeleteAllObjects
  ]

defaultToJSON :: (Generic a, GToJSON Zero (Rep a)) => String -> (a -> Value)
defaultToJSON prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop (length prefix),
        constructorTagModifier = camelTo2 '_' . drop (length prefix),
        sumEncoding = TaggedObject "type" "value"
      }

defaultParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> (Value -> Parser a)
defaultParseJSON prefix =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop (length prefix),
        constructorTagModifier = camelTo2 '_' . drop (length prefix),
        sumEncoding = TaggedObject "type" "value"
      }

main :: IO ()
main = do
  -- JSON encode
  let json = encodePretty exampleActions
  putStrLn "## JSON\n"
  putStrLn "```json"
  putStrLn $ LBS.unpack json
  putStrLn "```\n"

  -- JSON decode
  let decoded = eitherDecode json :: Either String [Action]
  case decoded of
    Left err -> putStrLn $ "Error decoding JSON: " ++ err
    Right actions -> do
      putStrLn "## Debug\n"
      putStrLn "```haskell"
      mapM_ print actions
      putStrLn "```\n"

  -- Transformed
  putStrLn "## Transformed\n"
  putStrLn "```"
  mapM_ (putStrLn . T.unpack . transformAction) exampleActions
  putStrLn "```"
