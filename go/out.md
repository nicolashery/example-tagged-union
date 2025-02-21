## JSON

```json
[
  {
    "item": {
      "id": "1",
      "name": "item1"
    },
    "type": "create_item"
  },
  {
    "item": {
      "id": "1",
      "name": "item1 updated"
    },
    "type": "update_item"
  },
  {
    "id": "1",
    "type": "delete_item"
  },
  {
    "type": "delete_all_items"
  }
]
```

## Debug

```go
&main.CreateItem{Item:main.Item{ID:"1", Name:"item1"}}
&main.UpdateItem{Item:main.Item{ID:"1", Name:"item1 updated"}}
&main.DeleteItem{ID:"1"}
&main.DeleteAllItems{}
```

## Transformed

```
create_item 1 item1
update_item 1 item1 updated
delete_item 1
delete_all_items
```

