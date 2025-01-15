module Main (main, test, run) where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)

data DirectoryObject = DirectoryObject
  { directoryObjectType :: Text,
    directoryObjectId :: Text,
    directoryObjectProperties :: [(Text, Text)]
  }
  deriving (Show, Generic)

instance FromJSON DirectoryObject

instance ToJSON DirectoryObject

data DirectoryRelation = DirectoryRelation
  { directoryRelationObjectType :: Text,
    directoryRelationObjectId :: Text,
    directoryRelationRelation :: Text,
    directoryRelationSubjectType :: Text,
    directoryRelationSubjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DirectoryRelation

instance ToJSON DirectoryRelation

data OperationType
  = OperationTypeCreateObject
  | OperationTypeUpdateObject
  | OperationTypeDeleteObject
  | OperationTypeCreateRelation
  | OperationTypeDeleteRelation
  deriving (Show, Generic)

instance FromJSON OperationType

instance ToJSON OperationType

data CreateObjectOperation = CreateObjectOperation
  { createObjectOperationObject :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON CreateObjectOperation

instance ToJSON CreateObjectOperation

data UpdateObjectOperation = UpdateObjectOperation
  { updateObjectOperationObject :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON UpdateObjectOperation

instance ToJSON UpdateObjectOperation

data DeleteObjectOperation = DeleteObjectOperation
  { deleteObjectOperationObjectType :: Text,
    deleteObjectOperationObjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DeleteObjectOperation

instance ToJSON DeleteObjectOperation

data CreateRelationOperation = CreateRelationOperation
  { createRelationOperationRelation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON CreateRelationOperation

instance ToJSON CreateRelationOperation

data DeleteRelationOperation = DeleteRelationOperation
  { deleteRelationOperationRelation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON DeleteRelationOperation

instance ToJSON DeleteRelationOperation

data Operation
  = OperationCreateObject CreateObjectOperation
  | OperationUpdateObject UpdateObjectOperation
  | OperationDeleteObject DeleteObjectOperation
  | OperationCreateRelation CreateRelationOperation
  | OperationDeleteRelation DeleteRelationOperation
  deriving (Show, Generic)

instance FromJSON Operation

instance ToJSON Operation

toImportRequestObject :: DirectoryObject -> Text
toImportRequestObject obj =
  let props = map (\(k, v) -> k <> ": " <> v) (directoryObjectProperties obj)
   in T.intercalate "\n" $
        [ "type: " <> directoryObjectType obj,
          "id: " <> directoryObjectId obj
        ]
          ++ props

toImportRequestRelation :: DirectoryRelation -> Text
toImportRequestRelation rel =
  T.intercalate
    "\n"
    [ "object_type: " <> directoryRelationObjectType rel,
      "object_id: " <> directoryRelationObjectId rel,
      "relation: " <> directoryRelationRelation rel,
      "subject_type: " <> directoryRelationSubjectType rel,
      "subject_id: " <> directoryRelationSubjectId rel
    ]

toImportRequest :: Operation -> Text
toImportRequest operation = case operation of
  OperationCreateObject op ->
    T.intercalate
      "\n"
      [ "op_code: " <> jsonToText (toJSON OperationTypeCreateObject),
        toImportRequestObject (createObjectOperationObject op)
      ]
  OperationUpdateObject op ->
    T.intercalate
      "\n"
      [ "op_code: " <> jsonToText (toJSON OperationTypeUpdateObject),
        toImportRequestObject (updateObjectOperationObject op)
      ]
  OperationDeleteObject op ->
    T.intercalate
      "\n"
      [ "op_code: " <> jsonToText (toJSON OperationTypeDeleteObject),
        "type: " <> (deleteObjectOperationObjectType op),
        "id: " <> (deleteObjectOperationObjectId op)
      ]
  OperationCreateRelation op ->
    T.intercalate
      "\n"
      [ "op_code: " <> jsonToText (toJSON OperationTypeCreateRelation),
        toImportRequestRelation (createRelationOperationRelation op)
      ]
  OperationDeleteRelation op ->
    T.intercalate
      "\n"
      [ "op_code: " <> jsonToText (toJSON OperationTypeDeleteRelation),
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
              { directoryObjectType = "user",
                directoryObjectId = "b478779c-5e5e-4cd7-9bf3-1405326be526",
                directoryObjectProperties = [("email", "alice@example.com")]
              }
        },
    OperationUpdateObject
      UpdateObjectOperation
        { updateObjectOperationObject =
            DirectoryObject
              { directoryObjectType = "group",
                directoryObjectId = "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca",
                directoryObjectProperties = [("name", "admins")]
              }
        },
    OperationDeleteObject
      DeleteObjectOperation
        { deleteObjectOperationObjectType = "group",
          deleteObjectOperationObjectId = "c9b58dd9-b4f6-4325-ba52-3d8d70857363"
        },
    OperationCreateRelation
      CreateRelationOperation
        { createRelationOperationRelation =
            DirectoryRelation
              { directoryRelationObjectType = "group",
                directoryRelationObjectId = "7910720c-9789-4dd3-83a4-4c65eebd82b3",
                directoryRelationRelation = "member",
                directoryRelationSubjectType = "user",
                directoryRelationSubjectId = "f32756fd-6a92-4034-8b86-c92cc9d9719f"
              }
        },
    OperationDeleteRelation
      DeleteRelationOperation
        { deleteRelationOperationRelation =
            DirectoryRelation
              { directoryRelationObjectType = "group",
                directoryRelationObjectId = "c3e65031-7455-45c8-acbd-59ec59d3e769",
                directoryRelationRelation = "member",
                directoryRelationSubjectType = "user",
                directoryRelationSubjectId = "f50fd4aa3-d632-46d6-92da-21bcc1391287"
              }
        }
  ]

lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> T.Text
jsonToText = lsbToText . encodePretty

prettyPrintJson :: Value -> IO ()
prettyPrintJson = TIO.putStrLn . jsonToText

test :: IO ()
test = do
  prettyPrintJson $ toJSON exampleOperations

run :: IO ()
run = do
  contents <- LBS.readFile "data.json"
  let decodeResult = eitherDecode contents :: Either String [Operation]
  case decodeResult of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right operations -> putStrLn $ T.unpack $ toImportRequests operations

main :: IO ()
main = test
