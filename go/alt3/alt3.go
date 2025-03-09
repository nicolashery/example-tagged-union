package alt3

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

// "Delayed decoding" approach
type Action struct {
	payload json.RawMessage
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

func NewActionFromCreateObject(v *CreateObject) (*Action, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}

	data, err = addActionType(data, ActionType_CreateObject)
	if err != nil {
		return nil, err
	}

	return &Action{
		payload: data,
	}, nil
}

func NewActionFromUpdateObject(v *UpdateObject) (*Action, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}

	data, err = addActionType(data, ActionType_UpdateObject)
	if err != nil {
		return nil, err
	}

	return &Action{
		payload: data,
	}, nil
}

func NewActionFromDeleteObject(v *DeleteObject) (*Action, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}

	data, err = addActionType(data, ActionType_DeleteObject)
	if err != nil {
		return nil, err
	}

	return &Action{
		payload: data,
	}, nil
}

func NewActionFromDeleteAllObjects(v *DeleteAllObjects) (*Action, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}

	data, err = addActionType(data, ActionType_DeleteAllObjects)
	if err != nil {
		return nil, err
	}

	return &Action{
		payload: data,
	}, nil
}

func addActionType(data json.RawMessage, tag ActionType) (json.RawMessage, error) {
	var tagged map[string]any
	if err := json.Unmarshal(data, &tagged); err != nil {
		return nil, err
	}

	tagged["type"] = tag
	data, err := json.Marshal(&tagged)
	if err != nil {
		return nil, err
	}

	return data, nil
}

func (a *Action) Tag() (ActionType, error) {
	var tag struct {
		Tag ActionType `json:"type"`
	}

	err := json.Unmarshal(a.payload, &tag)

	return tag.Tag, err
}

func (a *Action) Value() (any, error) {
	tag, err := a.Tag()
	if err != nil {
		return nil, err
	}

	var v any
	switch tag {
	case ActionType_CreateObject:
		v = new(CreateObject)
	case ActionType_UpdateObject:
		v = new(UpdateObject)
	case ActionType_DeleteObject:
		v = new(DeleteObject)
	case ActionType_DeleteAllObjects:
		v = new(DeleteAllObjects)
	default:
		return nil, fmt.Errorf("invalid action type: %s", tag)
	}

	err = json.Unmarshal(a.payload, v)
	if err != nil {
		return nil, err
	}

	return v, nil
}

func (a *Action) MarshalJSON() ([]byte, error) {
	data, err := json.Marshal(a.payload)

	return data, err
}

func (a *Action) UnmarshalJSON(data []byte) error {
	err := a.payload.UnmarshalJSON(data)
	return err
}

func TransformAction(action *Action) (string, error) {
	var result string

	value, err := action.Value()
	if err != nil {
		return "", err
	}

	switch v := value.(type) {
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

	return result, nil
}

func exampleActions() ([]Action, error) {
	actions := make([]Action, 0, 4)

	createAction, err := NewActionFromCreateObject(&CreateObject{
		Object: Object{
			Type: ObjectType_User,
			ID:   "1",
			Name: "user1",
		},
	})
	if err != nil {
		return nil, err
	}
	actions = append(actions, *createAction)

	updateAction, err := NewActionFromUpdateObject(&UpdateObject{
		Object: Object{
			Type: ObjectType_User,
			ID:   "1",
			Name: "user1 updated",
		},
	})
	if err != nil {
		return nil, err
	}
	actions = append(actions, *updateAction)

	deleteAction, err := NewActionFromDeleteObject(&DeleteObject{
		ID: "1",
	})
	if err != nil {
		return nil, err
	}
	actions = append(actions, *deleteAction)

	deleteAllAction, err := NewActionFromDeleteAllObjects(&DeleteAllObjects{})
	if err != nil {
		return nil, err
	}
	actions = append(actions, *deleteAllAction)

	return actions, nil
}

func Run() {
	actions, err := exampleActions()
	if err != nil {
		panic(err)
	}

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
		value, err := action.Value()
		if err != nil {
			panic(err)
		}
		fmt.Printf("%#v\n", value)
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
