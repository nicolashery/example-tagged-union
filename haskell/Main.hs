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
  { objectType :: Text,
    objectId :: Text,
    properties :: [(Text, Text)]
  }
  deriving (Show, Generic)

instance FromJSON DirectoryObject

instance ToJSON DirectoryObject

directoryObjectObjectType :: DirectoryObject -> Text
directoryObjectObjectType (DirectoryObject t _ _) = t

directoryObjectObjectId :: DirectoryObject -> Text
directoryObjectObjectId (DirectoryObject _ i _) = i

data DirectoryRelation = DirectoryRelation
  { objectType :: Text,
    objectId :: Text,
    relation :: Text,
    subjectType :: Text,
    subjectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DirectoryRelation

instance ToJSON DirectoryRelation

directoryRelationObjectType :: DirectoryRelation -> Text
directoryRelationObjectType (DirectoryRelation t _ _ _ _) = t

directoryRelationObjectId :: DirectoryRelation -> Text
directoryRelationObjectId (DirectoryRelation _ i _ _ _) = i

directoryRelationRelation :: DirectoryRelation -> Text
directoryRelationRelation (DirectoryRelation _ _ r _ _) = r

data OperationType
  = CreateObject
  | UpdateObject
  | DeleteObject
  | CreateRelation
  | DeleteRelation
  deriving (Show, Generic)

instance FromJSON OperationType

instance ToJSON OperationType

data CreateObjectOperation = CreateObjectOperation
  { opType :: OperationType,
    object :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON CreateObjectOperation

instance ToJSON CreateObjectOperation

data UpdateObjectOperation = UpdateObjectOperation
  { opType :: OperationType,
    object :: DirectoryObject
  }
  deriving (Show, Generic)

instance FromJSON UpdateObjectOperation

instance ToJSON UpdateObjectOperation

data DeleteObjectOperation = DeleteObjectOperation
  { opType :: OperationType,
    objectType :: Text,
    objectId :: Text
  }
  deriving (Show, Generic)

instance FromJSON DeleteObjectOperation

instance ToJSON DeleteObjectOperation

data CreateRelationOperation = CreateRelationOperation
  { opType :: OperationType,
    relation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON CreateRelationOperation

instance ToJSON CreateRelationOperation

data DeleteRelationOperation = DeleteRelationOperation
  { opType :: OperationType,
    relation :: DirectoryRelation
  }
  deriving (Show, Generic)

instance FromJSON DeleteRelationOperation

instance ToJSON DeleteRelationOperation

data Operation
  = CreateObjectOp CreateObjectOperation
  | UpdateObjectOp UpdateObjectOperation
  | DeleteObjectOp DeleteObjectOperation
  | CreateRelationOp CreateRelationOperation
  | DeleteRelationOp DeleteRelationOperation
  deriving (Show, Generic)

instance FromJSON Operation

instance ToJSON Operation

toImportRequestObject :: DirectoryObject -> Text
toImportRequestObject obj =
  let props = map (\(k, v) -> k <> ": " <> v) (properties obj)
   in T.intercalate "\n" $
        [ "kind: object",
          "type: " <> directoryObjectObjectType obj,
          "id: " <> directoryObjectObjectId obj
        ]
          ++ props

toImportRequestRelation :: DirectoryRelation -> Text
toImportRequestRelation rel =
  T.intercalate
    "\n"
    [ "kind: relation",
      "object_type: " <> directoryRelationObjectType rel,
      "object_id: " <> directoryRelationObjectId rel,
      "relation: " <> directoryRelationRelation rel,
      "subject_type: " <> subjectType rel,
      "subject_id: " <> subjectId rel
    ]

toImportRequest :: Operation -> Text
toImportRequest op = case op of
  CreateObjectOp (CreateObjectOperation _ obj) ->
    T.intercalate "\n" ["op_code: set", toImportRequestObject obj]
  UpdateObjectOp (UpdateObjectOperation _ obj) ->
    T.intercalate "\n" ["op_code: set", toImportRequestObject obj]
  DeleteObjectOp (DeleteObjectOperation _ objType objId) ->
    T.intercalate "\n" ["op_code: delete", "kind: object", "type: " <> objType, "id: " <> objId]
  CreateRelationOp (CreateRelationOperation _ rel) ->
    T.intercalate "\n" ["op_code: set", toImportRequestRelation rel]
  DeleteRelationOp (DeleteRelationOperation _ rel) ->
    T.intercalate "\n" ["op_code: delete", toImportRequestRelation rel]

toImportRequests :: [Operation] -> Text
toImportRequests ops = T.intercalate "\n\n" (map toImportRequest ops)

exampleOperations :: [Operation]
exampleOperations =
  [ CreateObjectOp
      CreateObjectOperation
        { opType = CreateObject,
          object =
            DirectoryObject
              { objectType = "user",
                objectId = "b478779c-5e5e-4cd7-9bf3-1405326be526",
                properties = [("email", "alice@example.com")]
              }
        },
    UpdateObjectOp
      UpdateObjectOperation
        { opType = UpdateObject,
          object =
            DirectoryObject
              { objectType = "group",
                objectId = "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca",
                properties = [("name", "admins")]
              }
        },
    DeleteObjectOp
      DeleteObjectOperation
        { opType = DeleteObject,
          objectType = "group",
          objectId = "c9b58dd9-b4f6-4325-ba52-3d8d70857363"
        },
    CreateRelationOp
      CreateRelationOperation
        { opType = CreateRelation,
          relation =
            DirectoryRelation
              { objectType = "group",
                objectId = "7910720c-9789-4dd3-83a4-4c65eebd82b3",
                relation = "member",
                subjectType = "user",
                subjectId = "f32756fd-6a92-4034-8b86-c92cc9d9719f"
              }
        },
    DeleteRelationOp
      DeleteRelationOperation
        { opType = DeleteRelation,
          relation =
            DirectoryRelation
              { objectType = "group",
                objectId = "c3e65031-7455-45c8-acbd-59ec59d3e769",
                relation = "member",
                subjectType = "user",
                subjectId = "f50fd4aa3-d632-46d6-92da-21bcc1391287"
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
