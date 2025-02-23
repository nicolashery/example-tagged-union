## JSON

```json
[{
		"object":	{
			"type":	"user",
			"id":	"1",
			"name":	"user"
		},
		"_type":	"CreateObject"
	}, {
		"object":	{
			"type":	"user",
			"id":	"1",
			"name":	"user1 updated"
		},
		"_type":	"UpdateObject"
	}, {
		"id":	"1",
		"_type":	"DeleteObject"
	}, {
		"_type":	"DeleteAllObjects"
	}]
```

## Debug

```v
Action(CreateObject{
    object: Object{
        type: user
        id: '1'
        name: 'user'
    }
})
Action(UpdateObject{
    object: Object{
        type: user
        id: '1'
        name: 'user1 updated'
    }
})
Action(DeleteObject{
    id: '1'
})
Action(DeleteAllObjects{})
```

## Transformed

```
create_object user 1 user
update_object user 1 user1 updated
delete_object 1
delete_all_objects
```

