## JSON

```json
[
  {
    "object": {
      "id": "1",
      "name": "user1",
      "type": "user"
    },
    "type": "create_object"
  },
  {
    "object": {
      "id": "1",
      "name": "user1 updated",
      "type": "user"
    },
    "type": "update_object"
  },
  {
    "id": "1",
    "type": "delete_object"
  },
  {
    "type": "delete_all_objects"
  }
]
```

## Debug

```go
&main.CreateObject{Object:main.Object{Type:ObjectType_User, ID:"1", Name:"user1"}}
&main.UpdateObject{Object:main.Object{Type:ObjectType_User, ID:"1", Name:"user1 updated"}}
&main.DeleteObject{ID:"1"}
&main.DeleteAllObjects{}
```

## Transformed

```
create_object user 1 user1
update_object user 1 user1 updated
delete_object 1
delete_all_objects
```

