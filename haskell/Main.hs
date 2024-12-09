module Main (main) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
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

main :: IO ()
main = do
  contents <- BL.readFile "data.json"
  let decodeResult = eitherDecode contents :: Either String [Operation]
  case decodeResult of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right operations -> putStrLn $ T.unpack $ toImportRequests operations
