## JSON

```json
[
    {
        "type": "create_object",
        "value": {
            "object": {
                "id": "1",
                "name": "user1",
                "type": "user"
            }
        }
    },
    {
        "type": "update_object",
        "value": {
            "object": {
                "id": "1",
                "name": "user1 updated",
                "type": "user"
            }
        }
    },
    {
        "type": "delete_all_objects"
    }
]
```

## Debug

```haskell
ActionCreateObject (CreateObject {createObjectObject = Object {objectType = ObjectTypeUser, objectId = "1", objectName = "user1"}})
ActionUpdateObject (UpdateObject {updateObjectObject = Object {objectType = ObjectTypeUser, objectId = "1", objectName = "user1 updated"}})
ActionDeleteAllObjects
```

## Transformed

```
create_object user 1 user1
update_object user 1 user1 updated
delete_all_objects
```
