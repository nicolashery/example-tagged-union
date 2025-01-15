module Main (main, test, run) where

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
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO
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

data DirectoryObject = DirectoryObject
  { directoryObjectType :: ObjectType,
    directoryObjectId :: Text,
    directoryObjectProperties :: Map Text Text
  }
  deriving (Show, Generic)

instance FromJSON DirectoryObject where
  parseJSON = defaultParseJSON "directoryObject"

instance ToJSON DirectoryObject where
  toJSON = defaultToJSON "directoryObject"

data DirectoryRelation = DirectoryRelation
  { directoryRelationObjectType :: ObjectType,
    directoryRelationObjectId :: Text,
    directoryRelationRelation :: Text,
    directoryRelationSubjectType :: ObjectType,
    directoryRelationSubjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DirectoryRelation where
  parseJSON = defaultParseJSON "directoryRelation"

instance ToJSON DirectoryRelation where
  toJSON = defaultToJSON "directoryRelation"

data CreateObjectOperation = CreateObjectOperation
  { createObjectOperationObject :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON CreateObjectOperation where
  parseJSON = defaultParseJSON "createObjectOperation"

instance ToJSON CreateObjectOperation where
  toJSON = defaultToJSON "createObjectOperation"

data UpdateObjectOperation = UpdateObjectOperation
  { updateObjectOperationObject :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON UpdateObjectOperation where
  parseJSON = defaultParseJSON "updateObjectOperation"

instance ToJSON UpdateObjectOperation where
  toJSON = defaultToJSON "updateObjectOperation"

data DeleteObjectOperation = DeleteObjectOperation
  { deleteObjectOperationObjectType :: ObjectType,
    deleteObjectOperationObjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DeleteObjectOperation where
  parseJSON = defaultParseJSON "deleteObjectOperation"

instance ToJSON DeleteObjectOperation where
  toJSON = defaultToJSON "deleteObjectOperation"

data CreateRelationOperation = CreateRelationOperation
  { createRelationOperationRelation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON CreateRelationOperation where
  parseJSON = defaultParseJSON "createRelationOperation"

instance ToJSON CreateRelationOperation where
  toJSON = defaultToJSON "createRelationOperation"

data DeleteRelationOperation = DeleteRelationOperation
  { deleteRelationOperationRelation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON DeleteRelationOperation where
  parseJSON = defaultParseJSON "deleteRelationOperation"

instance ToJSON DeleteRelationOperation where
  toJSON = defaultToJSON "deleteRelationOperation"

data Operation
  = OperationCreateObject CreateObjectOperation
  | OperationUpdateObject UpdateObjectOperation
  | OperationDeleteObject DeleteObjectOperation
  | OperationCreateRelation CreateRelationOperation
  | OperationDeleteRelation DeleteRelationOperation
  deriving (Show, Generic)

instance FromJSON Operation where
  parseJSON = defaultParseJSON "Operation"

instance ToJSON Operation where
  toJSON = defaultToJSON "Operation"

data BatchRequest = BatchRequest
  { batchRequestOperations :: [Operation]
  }
  deriving (Show, Generic)

instance FromJSON BatchRequest where
  parseJSON = defaultParseJSON "batchRequest"

instance ToJSON BatchRequest where
  toJSON = defaultToJSON "batchRequest"

toImportRequestObject :: DirectoryObject -> Text
toImportRequestObject obj =
  let props = map (\(k, v) -> k <> ": " <> v) (Map.toList $ directoryObjectProperties obj)
   in T.intercalate "\n" $
        [ "kind: object",
          "type: " <> (objectTypeToText $ directoryObjectType obj),
          "id: " <> directoryObjectId obj
        ]
          ++ props

toImportRequestRelation :: DirectoryRelation -> Text
toImportRequestRelation rel =
  T.intercalate
    "\n"
    [ "kind: relation",
      "object_type: " <> (objectTypeToText $ directoryRelationObjectType rel),
      "object_id: " <> directoryRelationObjectId rel,
      "relation: " <> directoryRelationRelation rel,
      "subject_type: " <> (objectTypeToText $ directoryRelationSubjectType rel),
      "subject_id: " <> directoryRelationSubjectId rel
    ]

toImportRequest :: Operation -> Text
toImportRequest operation = case operation of
  OperationCreateObject op ->
    T.intercalate
      "\n"
      [ "op_code: set",
        toImportRequestObject (createObjectOperationObject op)
      ]
  OperationUpdateObject op ->
    T.intercalate
      "\n"
      [ "op_code: set",
        toImportRequestObject (updateObjectOperationObject op)
      ]
  OperationDeleteObject op ->
    -- T.intercalate
    --   "\n"
    --   [ "op_code: delete",
    --     toImportRequestObject (deleteObjectOperationObject op)
    --   ]
    -- -- Compiler error: Variable not in scope: deleteObjectOperationObject
    let obj =
          DirectoryObject
            { directoryObjectType = deleteObjectOperationObjectType op,
              directoryObjectId = deleteObjectOperationObjectId op,
              directoryObjectProperties = Map.empty
            }
     in T.intercalate
          "\n"
          [ "op_code: delete",
            toImportRequestObject obj
          ]
  OperationCreateRelation op ->
    T.intercalate
      "\n"
      [ "op_code: set",
        toImportRequestRelation (createRelationOperationRelation op)
      ]
  OperationDeleteRelation op ->
    T.intercalate
      "\n"
      [ "op_code: delete",
        toImportRequestRelation (deleteRelationOperationRelation op)
      ]

toImportRequests :: [Operation] -> Text
toImportRequests ops = T.intercalate "\n\n" (map toImportRequest ops)

exampleOperations :: [Operation]
exampleOperations =
  [ OperationCreateObject
      CreateObjectOperation
        { createObjectOperationObject =
            DirectoryObject
              { directoryObjectType = ObjectTypeUser,
                directoryObjectId = "b478779c-5e5e-4cd7-9bf3-1405326be526",
                directoryObjectProperties = Map.fromList [("email", "alice@example.com")]
              }
        },
    OperationUpdateObject
      UpdateObjectOperation
        { updateObjectOperationObject =
            DirectoryObject
              { directoryObjectType = ObjectTypeGroup,
                directoryObjectId = "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca",
                directoryObjectProperties = Map.fromList [("name", "admins")]
              }
        },
    OperationDeleteObject
      DeleteObjectOperation
        { deleteObjectOperationObjectType = ObjectTypeGroup,
          deleteObjectOperationObjectId = "c9b58dd9-b4f6-4325-ba52-3d8d70857363"
        },
    OperationCreateRelation
      CreateRelationOperation
        { createRelationOperationRelation =
            DirectoryRelation
              { directoryRelationObjectType = ObjectTypeGroup,
                directoryRelationObjectId = "7910720c-9789-4dd3-83a4-4c65eebd82b3",
                directoryRelationRelation = "member",
                directoryRelationSubjectType = ObjectTypeUser,
                directoryRelationSubjectId = "f32756fd-6a92-4034-8b86-c92cc9d9719f"
              }
        },
    OperationDeleteRelation
      DeleteRelationOperation
        { deleteRelationOperationRelation =
            DirectoryRelation
              { directoryRelationObjectType = ObjectTypeGroup,
                directoryRelationObjectId = "c3e65031-7455-45c8-acbd-59ec59d3e769",
                directoryRelationRelation = "member",
                directoryRelationSubjectType = ObjectTypeUser,
                directoryRelationSubjectId = "f50fd4aa3-d632-46d6-92da-21bcc1391287"
              }
        }
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

lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> T.Text
jsonToText = lsbToText . encodePretty

prettyPrintJson :: Value -> IO ()
prettyPrintJson = TIO.putStrLn . jsonToText

test :: IO ()
test = do
  prettyPrintJson $ toJSON $ BatchRequest {batchRequestOperations = exampleOperations}

run :: IO ()
run = do
  contents <- LBS.readFile "in.json"
  let decodeResult = eitherDecode contents :: Either String BatchRequest
  case decodeResult of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right request -> putStrLn $ T.unpack $ toImportRequests $ batchRequestOperations request

main :: IO ()
main = run
