package main

import (
	"encoding/json"
	"fmt"
)

type ObjectType int

const (
	ObjectType_User ObjectType = iota
	ObjectType_Group
)

var ObjectTypeStringMap = map[ObjectType]string{
	ObjectType_User:  "user",
	ObjectType_Group: "group",
}

var ObjectTypeValueMap = map[string]ObjectType{
	"user":  ObjectType_User,
	"group": ObjectType_Group,
}

func (t ObjectType) MarshalJSON() ([]byte, error) {
	return json.Marshal(ObjectTypeStringMap[t])
}

func (t *ObjectType) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	if v, ok := ObjectTypeValueMap[s]; ok {
		*t = v
		return nil
	}
	return fmt.Errorf("invalid ObjectType: %s", s)
}

func (t ObjectType) String() string {
	return ObjectTypeStringMap[t]
}

func (t ObjectType) GoString() string {
	var result string
	switch t {
	case ObjectType_User:
		result = "ObjectType_User"
	case ObjectType_Group:
		return "ObjectType_Group"
	}
	return result
}

type Object struct {
	Type ObjectType `json:"type"`
	ID   string     `json:"id"`
	Name string     `json:"name"`
}

type ActionType int

const (
	ActionType_CreateObject ActionType = iota
	ActionType_UpdateObject
	ActionType_DeleteObject
	ActionType_DeleteAllObjects
)

// note: `exhaustive` linter will catch if we miss an entry here
var ActionType_StringMap = map[ActionType]string{
	ActionType_CreateObject:     "create_object",
	ActionType_UpdateObject:     "update_object",
	ActionType_DeleteObject:     "delete_object",
	ActionType_DeleteAllObjects: "delete_all_objects",
}

var ActionType_ValueMap = make(map[string]ActionType, len(ActionType_StringMap))

func init() {
	for k, v := range ActionType_StringMap {
		ActionType_ValueMap[v] = k
	}
}

func (t ActionType) MarshalJSON() ([]byte, error) {
	return json.Marshal(ActionType_StringMap[t])
}

func (t *ActionType) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	if v, ok := ActionType_ValueMap[s]; ok {
		*t = v
		return nil
	}
	return fmt.Errorf("invalid ActionType: %s", s)
}

func (t ActionType) String() string {
	return ActionType_StringMap[t]
}

//sumtype:decl
type IsAction interface {
	// sealed interface to emulate sum type
	isAction()
}

type CreateObject struct {
	Object Object `json:"object"`
}

func (*CreateObject) isAction() {}

type UpdateObject struct {
	Object Object `json:"object"`
}

func (*UpdateObject) isAction() {}

type DeleteObject struct {
	ID string `json:"id"`
}

func (*DeleteObject) isAction() {}

type DeleteAllObjects struct{}

func (*DeleteAllObjects) isAction() {}

type Action struct {
	value IsAction
}

func NewAction(value IsAction) Action {
	return Action{value: value}
}

func (a *Action) Value() IsAction {
	return a.value
}

func (a *Action) MarshalJSON() ([]byte, error) {
	v := a.value

	data, err := json.Marshal(&v)
	if err != nil {
		return nil, err
	}

	var tagged map[string]any
	if err := json.Unmarshal(data, &tagged); err != nil {
		return nil, err
	}

	// note: `go-check-sumtype` linter will catch if we miss a case here
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
	var tag struct {
		Type ActionType `json:"type"`
	}

	if err := json.Unmarshal(data, &tag); err != nil {
		return err
	}

	var v IsAction
	// note: `exhaustive` linter will catch if we miss a case here
	switch tag.Type {
	case ActionType_CreateObject:
		v = new(CreateObject)
	case ActionType_UpdateObject:
		v = new(UpdateObject)
	case ActionType_DeleteObject:
		v = new(DeleteObject)
	case ActionType_DeleteAllObjects:
		v = new(DeleteAllObjects)
	}

	if err := json.Unmarshal(data, v); err != nil {
		return err
	}

	a.value = v
	return nil
}

func TransformAction(action *Action) string {
	var result string

	// note: `go-check-sumtype` linter will catch if we miss a case here
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

func exampleActions() []Action {
	return []Action{
		NewAction(&CreateObject{Object: Object{
			Type: ObjectType_User,
			ID:   "1",
			Name: "user1",
		}}),
		NewAction(&UpdateObject{Object: Object{
			Type: ObjectType_User,
			ID:   "1",
			Name: "user1 updated",
		}}),
		NewAction(&DeleteObject{ID: "1"}),
		NewAction(&DeleteAllObjects{}),
	}
}

func main() {
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
		fmt.Printf("%#v\n", action.Value())
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
