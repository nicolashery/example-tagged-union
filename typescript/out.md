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

```typescript
{
  type: "create_object",
  object: { type: "user", id: "1", name: "user1" }
}
{
  type: "update_object",
  object: { type: "user", id: "1", name: "user1 updated" }
}
{ type: "delete_all_objects" }
```

## Transformed

```
create_object user 1 user1
update_object user 1 user1 updated
delete_all_objects
```

