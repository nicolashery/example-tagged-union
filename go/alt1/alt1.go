package alt1

import (
	"encoding/json"
	"fmt"
)

type ObjectType string

const (
	ObjectType_User  ObjectType = "user"
	ObjectType_Group ObjectType = "group"
)

type Object struct {
	Type ObjectType `json:"type"`
	ID   string     `json:"id"`
	Name string     `json:"name"`
}

type ActionType string

const (
	ActionType_CreateObject     ActionType = "create_object"
	ActionType_UpdateObject     ActionType = "update_object"
	ActionType_DeleteObject     ActionType = "delete_object"
	ActionType_DeleteAllObjects ActionType = "delete_all_objects"
)

// "Bag of all the fields" approach
type Action struct {
	Type   ActionType `json:"type"`
	Object *Object    `json:"object,omitempty"`
	ID     *string    `json:"id,omitempty"`
}

func TransformAction(a *Action) string {
	var result string

	switch a.Type {
	case ActionType_CreateObject:
		result = fmt.Sprintf(
			"create_object %s %s %s", a.Object.Type, a.Object.ID, a.Object.Name,
		)
	case ActionType_UpdateObject:
		result = fmt.Sprintf(
			"update_object %s %s %s", a.Object.Type, a.Object.ID, a.Object.Name,
		)
	case ActionType_DeleteObject:
		result = fmt.Sprintf("delete_object %s", *a.ID)
	case ActionType_DeleteAllObjects:
		result = "delete_all_objects"
	}

	return result
}

func exampleActions() []Action {
	id := "1"
	return []Action{
		{
			Type: ActionType_CreateObject,
			Object: &Object{
				Type: ObjectType_User,
				ID:   "1",
				Name: "user1",
			},
		},
		{
			Type: ActionType_UpdateObject,
			Object: &Object{
				Type: ObjectType_User,
				ID:   "1",
				Name: "user1 updated",
			},
		},
		{
			Type: ActionType_DeleteObject,
			ID:   &id,
		},
		{
			Type: ActionType_DeleteAllObjects,
		},
	}
}

func Run() {
	actions := exampleActions()

	// JSON encode
	data, err := json.MarshalIndent(actions, "", "  ")
	if err != nil {
		panic(err)
	}
	fmt.Println("## JSON")
	fmt.Println()
	fmt.Println("```json")
	fmt.Println(string(data))
	fmt.Println("```")
	fmt.Println()

	// JSON decode
	actions2 := []Action{}
	if err := json.Unmarshal(data, &actions2); err != nil {
		panic(err)
	}
	fmt.Println("## Debug")
	fmt.Println()
	fmt.Println("```go")
	for _, action := range actions2 {
		fmt.Printf("%#v\n", action)
	}
	fmt.Println("```")
	fmt.Println()

	// Transform
	fmt.Println("## Transformed")
	fmt.Println()
	fmt.Println("```")
	for _, action := range actions {
		fmt.Println(TransformAction(&action))
	}
	fmt.Println("```")
	fmt.Println()
}
