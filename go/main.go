package main

import (
	"encoding/json"
	"fmt"
)

type Item struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

type ActionType int

const (
	ActionType_CreateItem ActionType = iota
	ActionType_UpdateItem
	ActionType_DeleteItem
	ActionType_DeleteAllItems
)

// note: `exhaustive` linter will catch if we miss an entry here
var ActionTypeStringMap = map[ActionType]string{
	ActionType_CreateItem:     "create_item",
	ActionType_UpdateItem:     "update_item",
	ActionType_DeleteItem:     "delete_item",
	ActionType_DeleteAllItems: "delete_all_items",
}

// note: `exhaustive` linter can't catch missing entry here
var ActionTypeValueMap = map[string]ActionType{
	"create_item":      ActionType_CreateItem,
	"update_item":      ActionType_UpdateItem,
	"delete_item":      ActionType_DeleteItem,
	"delete_all_items": ActionType_DeleteAllItems,
}

func (t ActionType) MarshalJSON() ([]byte, error) {
	return json.Marshal(ActionTypeStringMap[t])
}

func (t *ActionType) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	if v, ok := ActionTypeValueMap[s]; ok {
		*t = v
		return nil
	}
	return fmt.Errorf("invalid ActionType: %s", s)
}

func (t ActionType) String() string {
	return ActionTypeStringMap[t]
}

//sumtype:decl
type IsAction interface {
	// sealed interface to emulate sum type
	isAction()
}

type CreateItem struct {
	Item Item `json:"item"`
}

func (*CreateItem) isAction() {}

type UpdateItem struct {
	Item Item `json:"item"`
}

func (*UpdateItem) isAction() {}

type DeleteItem struct {
	ID string `json:"id"`
}

func (*DeleteItem) isAction() {}

type DeleteAllItems struct{}

func (*DeleteAllItems) isAction() {}

type Action struct {
	value IsAction
}

func NewAction(value IsAction) Action {
	return Action{value: value}
}

func (a *Action) Value() IsAction {
	return a.value
}

func (a *Action) MarshalJSONAdjacentlyTagged() ([]byte, error) {
	var tagged struct {
		Type  ActionType      `json:"type"`
		Value json.RawMessage `json:"value,omitempty"`
	}

	// note: `go-check-sumtype` linter will catch if we miss a case here
	switch a.value.(type) {
	case *CreateItem:
		tagged.Type = ActionType_CreateItem
	case *UpdateItem:
		tagged.Type = ActionType_UpdateItem
	case *DeleteItem:
		tagged.Type = ActionType_DeleteItem
	case *DeleteAllItems:
		tagged.Type = ActionType_DeleteAllItems
	}

	value, err := json.Marshal(a.value)
	if err != nil {
		return nil, err
	}

	// don't output empty structs
	if string(value) != "{}" {
		tagged.Value = value
	}

	return json.Marshal(&tagged)
}

func (a *Action) UnmarshalJSONAdjacentlyTagged(data []byte) error {
	var tagged struct {
		Type  ActionType      `json:"type"`
		Value json.RawMessage `json:"value,omitempty"`
	}

	if err := json.Unmarshal(data, &tagged); err != nil {
		return err
	}

	var v IsAction
	// note: `exhaustive` linter will catch if we miss a case here
	switch tagged.Type {
	case ActionType_CreateItem:
		v = &CreateItem{}
	case ActionType_UpdateItem:
		v = &UpdateItem{}
	case ActionType_DeleteItem:
		v = &DeleteItem{}
	case ActionType_DeleteAllItems:
		v = &DeleteAllItems{}
	}

	if v == nil {
		return fmt.Errorf("unknown action type: %s", tagged.Type)
	}

	if tagged.Value == nil {
		a.value = v
		return nil
	}

	if err := json.Unmarshal(tagged.Value, v); err != nil {
		return err
	}

	a.value = v
	return nil
}

func (a *Action) MarshalJSONInternallyTagged1() ([]byte, error) {
	var data []byte
	var err error

	// note: `go-check-sumtype` linter will catch if we miss a case here
	switch v := a.value.(type) {
	case *CreateItem:
		tagged := struct {
			Type ActionType `json:"type"`
			CreateItem
		}{
			Type:       ActionType_CreateItem,
			CreateItem: *v,
		}
		data, err = json.Marshal(&tagged)
	case *UpdateItem:
		tagged := struct {
			Type ActionType `json:"type"`
			UpdateItem
		}{
			Type:       ActionType_UpdateItem,
			UpdateItem: *v,
		}
		data, err = json.Marshal(&tagged)
	case *DeleteItem:
		tagged := struct {
			Type ActionType `json:"type"`
			DeleteItem
		}{
			Type:       ActionType_DeleteItem,
			DeleteItem: *v,
		}
		data, err = json.Marshal(&tagged)
	case *DeleteAllItems:
		tagged := struct {
			Type ActionType `json:"type"`
			DeleteAllItems
		}{
			Type:           ActionType_DeleteAllItems,
			DeleteAllItems: *v,
		}
		data, err = json.Marshal(&tagged)
	}

	return data, err
}

func (a *Action) MarshalJSONInternallyTagged2() ([]byte, error) {
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
	switch a.value.(type) {
	case *CreateItem:
		tagged["type"] = ActionType_CreateItem
	case *UpdateItem:
		tagged["type"] = ActionType_UpdateItem
	case *DeleteItem:
		tagged["type"] = ActionType_DeleteItem
	case *DeleteAllItems:
		tagged["type"] = ActionType_DeleteAllItems
	}

	return json.Marshal(&tagged)
}

func (a *Action) UnmarshalJSONInternallyTagged(data []byte) error {
	var tag struct {
		Type ActionType `json:"type"`
	}

	if err := json.Unmarshal(data, &tag); err != nil {
		return err
	}

	var v IsAction
	// note: `exhaustive` linter will catch if we miss a case here
	switch tag.Type {
	case ActionType_CreateItem:
		v = &CreateItem{}
	case ActionType_UpdateItem:
		v = &UpdateItem{}
	case ActionType_DeleteItem:
		v = &DeleteItem{}
	case ActionType_DeleteAllItems:
		v = &DeleteAllItems{}
	}

	if err := json.Unmarshal(data, v); err != nil {
		return err
	}

	a.value = v
	return nil
}

func (a *Action) MarshalJSON() ([]byte, error) {
	return a.MarshalJSONInternallyTagged2()
}

func (a *Action) UnmarshalJSON(data []byte) error {
	return a.UnmarshalJSONInternallyTagged(data)
}

func transformAction(action *Action) string {
	var result string

	// note: `go-check-sumtype` linter will catch if we miss a case here
	switch v := action.Value().(type) {
	case *CreateItem:
		result = fmt.Sprintf("create_item %s %s", v.Item.ID, v.Item.Name)
	case *UpdateItem:
		result = fmt.Sprintf("update_item %s %s", v.Item.ID, v.Item.Name)
	case *DeleteItem:
		result = fmt.Sprintf("delete_item %s", v.ID)
	case *DeleteAllItems:
		result = "delete_all_items"
	}

	return result
}

func exampleActions() []Action {
	return []Action{
		NewAction(&CreateItem{Item: Item{ID: "1", Name: "item1"}}),
		NewAction(&UpdateItem{Item: Item{ID: "1", Name: "item1 updated"}}),
		NewAction(&DeleteItem{ID: "1"}),
		NewAction(&DeleteAllItems{}),
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
		fmt.Println(transformAction(&action))
	}
	fmt.Println("```")
	fmt.Println()
}
