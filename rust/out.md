## JSON

```json
[
  {
    "type": "create_item",
    "item": {
      "type": "user",
      "id": "1",
      "name": "user1"
    }
  },
  {
    "type": "update_item",
    "item": {
      "type": "user",
      "id": "1",
      "name": "user1 updated"
    }
  },
  {
    "type": "delete_all_items"
  }
]
```

## Debug

```rust
CreateItem(CreateItem { item: Item { item_type: User, id: "1", name: "user1" } })
UpdateItem(UpdateItem { item: Item { item_type: User, id: "1", name: "user1 updated" } })
DeleteAllItems
```

## Transformed

```
create_item user 1 user1
update_item user 1 user1 updated
delete_all_items
```

