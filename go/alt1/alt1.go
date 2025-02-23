package alt1

import "fmt"

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

// "Bag of all the things" approach
type Action struct {
	Type   ActionType `json:"type"`
	Object *Object    `json:"item"`
	ID     *string    `json:"id"`
}

func transformAction(a *Action) string {
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
