package alt2

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

// "Bag of all the branches" approach
type Action struct {
	createObject     *CreateObject
	updateObject     *UpdateObject
	deleteObject     *DeleteObject
	deleteAllObjects *DeleteAllObjects
}

type CreateObject struct {
	Object Object `json:"object"`
}

type UpdateObject struct {
	Object Object `json:"object"`
}

type DeleteObject struct {
	ID string `json:"id"`
}

type DeleteAllObjects struct{}

func NewActionFromCreateObject(v *CreateObject) Action {
	//exhaustruct:ignore
	return Action{
		createObject: v,
	}
}

func NewActionFromUpdateObject(v *UpdateObject) Action {
	//exhaustruct:ignore
	return Action{
		updateObject: v,
	}
}

func NewActionFromDeleteObject(v *DeleteObject) Action {
	//exhaustruct:ignore
	return Action{
		deleteObject: v,
	}
}

func NewActionFromDeleteAllObjects(v *DeleteAllObjects) Action {
	//exhaustruct:ignore
	return Action{
		deleteAllObjects: v,
	}
}

func (a *Action) Value() any {
	if a.createObject != nil {
		return a.createObject
	}

	if a.updateObject != nil {
		return a.updateObject
	}

	if a.deleteObject != nil {
		return a.deleteObject
	}

	if a.deleteAllObjects != nil {
		return a.deleteAllObjects
	}

	return nil
}

func (a *Action) MarshalJSON2() ([]byte, error) {
	if a.createObject != nil {
		return json.Marshal(&a.createObject)
	}

	if a.updateObject != nil {
		return json.Marshal(&a.updateObject)
	}

	if a.deleteObject != nil {
		return json.Marshal(&a.deleteObject)
	}

	if a.deleteAllObjects != nil {
		return json.Marshal(&a.deleteAllObjects)
	}

	return nil, nil
}

func (a *Action) MarshalJSON() ([]byte, error) {
	v := a.Value()

	data, err := json.Marshal(&v)
	if err != nil {
		return nil, err
	}

	var tagged map[string]any
	if err := json.Unmarshal(data, &tagged); err != nil {
		return nil, err
	}

	switch v.(type) {
	case *CreateObject:
		tagged["type"] = ActionType_CreateObject
	case *UpdateObject:
		tagged["type"] = ActionType_UpdateObject
	case *DeleteObject:
		tagged["type"] = ActionType_DeleteObject
	case *DeleteAllObjects:
		tagged["type"] = ActionType_DeleteAllObjects
	}

	return json.Marshal(&tagged)
}

func (a *Action) UnmarshalJSON(data []byte) error {
	var tagged struct {
		Type ActionType `json:"type"`
	}

	if err := json.Unmarshal(data, &tagged); err != nil {
		return err
	}

	var err error
	switch tagged.Type {
	case ActionType_CreateObject:
		err = json.Unmarshal(data, &a.createObject)
	case ActionType_UpdateObject:
		err = json.Unmarshal(data, &a.updateObject)
	case ActionType_DeleteObject:
		err = json.Unmarshal(data, &a.deleteObject)
	case ActionType_DeleteAllObjects:
		err = json.Unmarshal(data, &a.deleteAllObjects)
	}

	if err != nil {
		a.createObject = nil
		a.updateObject = nil
		a.deleteObject = nil
		a.deleteAllObjects = nil
		return err
	}

	return nil
}

func transformAction(action *Action) string {
	var result string

	switch v := action.Value().(type) {
	case *CreateObject:
		result = fmt.Sprintf(
			"create_object %s %s %s", v.Object.Type, v.Object.ID, v.Object.Name,
		)
	case *UpdateObject:
		result = fmt.Sprintf(
			"update_object %s %s %s", v.Object.Type, v.Object.ID, v.Object.Name,
		)
	case *DeleteObject:
		result = fmt.Sprintf("delete_object %s", v.ID)
	case *DeleteAllObjects:
		result = "delete_all_objects"
	}

	return result
}
