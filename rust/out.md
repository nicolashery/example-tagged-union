## JSON

```json
[
  {
    "type": "create_object",
    "object": {
      "type": "user",
      "id": "1",
      "name": "user1"
    }
  },
  {
    "type": "update_object",
    "object": {
      "type": "user",
      "id": "1",
      "name": "user1 updated"
    }
  },
  {
    "type": "delete_all_objects"
  }
]
```

## Debug

```rust
CreateObject(CreateObject { object: Object { object_type: User, id: "1", name: "user1" } })
UpdateObject(UpdateObject { object: Object { object_type: User, id: "1", name: "user1 updated" } })
DeleteAllObjects
```

## Transformed

```
create_object user 1 user1
update_object user 1 user1 updated
delete_all_objects
```

