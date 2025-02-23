package main

import (
	"encoding/json"
	"fmt"
)

// Alternative implementations of `MarshalJSON` and `UnmarshalJSON` for `Action`

func (a *Action) MarshalJSONInternallyTagged2() ([]byte, error) {
	var data []byte
	var err error

	// note: `go-check-sumtype` linter will catch if we miss a case here
	switch v := a.value.(type) {
	case *CreateObject:
		tagged := struct {
			Type ActionType `json:"type"`
			CreateObject
		}{
			Type:         ActionType_CreateObject,
			CreateObject: *v,
		}
		data, err = json.Marshal(&tagged)
	case *UpdateObject:
		tagged := struct {
			Type ActionType `json:"type"`
			UpdateObject
		}{
			Type:         ActionType_UpdateObject,
			UpdateObject: *v,
		}
		data, err = json.Marshal(&tagged)
	case *DeleteObject:
		tagged := struct {
			Type ActionType `json:"type"`
			DeleteObject
		}{
			Type:         ActionType_DeleteObject,
			DeleteObject: *v,
		}
		data, err = json.Marshal(&tagged)
	case *DeleteAllObjects:
		tagged := struct {
			Type ActionType `json:"type"`
			DeleteAllObjects
		}{
			Type:             ActionType_DeleteAllObjects,
			DeleteAllObjects: *v,
		}
		data, err = json.Marshal(&tagged)
	}

	return data, err
}

func (a *Action) MarshalJSONAdjacentlyTagged() ([]byte, error) {
	var tagged struct {
		Type  ActionType      `json:"type"`
		Value json.RawMessage `json:"value,omitempty"`
	}

	// note: `go-check-sumtype` linter will catch if we miss a case here
	switch a.value.(type) {
	case *CreateObject:
		tagged.Type = ActionType_CreateObject
	case *UpdateObject:
		tagged.Type = ActionType_UpdateObject
	case *DeleteObject:
		tagged.Type = ActionType_DeleteObject
	case *DeleteAllObjects:
		tagged.Type = ActionType_DeleteAllObjects
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
	case ActionType_CreateObject:
		v = new(CreateObject)
	case ActionType_UpdateObject:
		v = new(UpdateObject)
	case ActionType_DeleteObject:
		v = new(DeleteObject)
	case ActionType_DeleteAllObjects:
		v = new(DeleteAllObjects)
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
