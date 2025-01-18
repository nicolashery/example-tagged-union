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

func (t ObjectType) MarshallText() ([]byte, error) {
	return []byte(ObjectTypeStringMap[t]), nil
}

func (t *ObjectType) UnmarshallText(text []byte) error {
	if v, ok := ObjectTypeValueMap[string(text)]; ok {
		*t = v
		return nil
	}
	return fmt.Errorf("invalid ObjectType: %s", text)
}

func (t ObjectType) String() string {
	return ObjectTypeStringMap[t]
}

type DirectoryObject struct {
	Type       ObjectType        `json:"type"`
	ID         string            `json:"id"`
	Properties map[string]string `json:"properties"`
}

type DirectoryRelation struct {
	ObjectType  ObjectType `json:"object_type"`
	ObjectID    string     `json:"object_id"`
	Relation    string     `json:"relation"`
	SubjectType ObjectType `json:"subject_type"`
	SubjectID   string     `json:"subject_id"`
}

type Operation interface {
	isOperation()
	OperationType() string
}

type CreateObjectOperation struct {
	Object DirectoryObject `json:"object"`
}

func (*CreateObjectOperation) isOperation() {}
func (*CreateObjectOperation) OperationType() string {
	return "create_object"
}

type UpdateObjectOperation struct {
	Object DirectoryObject `json:"object"`
}

func (*UpdateObjectOperation) isOperation() {}
func (*UpdateObjectOperation) OperationType() string {
	return "update_object"
}

type DeleteObjectOperation struct {
	ObjectType ObjectType `json:"object_type"`
	ObjectID   string     `json:"object_id"`
}

func (*DeleteObjectOperation) isOperation() {}
func (*DeleteObjectOperation) OperationType() string {
	return "delete_object"
}

type CreateRelationOperation struct {
	Relation DirectoryRelation `json:"relation"`
}

func (*CreateRelationOperation) isOperation() {}
func (*CreateRelationOperation) OperationType() string {
	return "create_relation"
}

type DeleteRelationOperation struct {
	Relation DirectoryRelation `json:"relation"`
}

func (*DeleteRelationOperation) isOperation() {}
func (*DeleteRelationOperation) OperationType() string {
	return "delete_relation"
}

type OperationWrapper struct {
	Type  string    `json:"type"`
	Value Operation `json:"value"`
}

func (o OperationWrapper) MarshalJSON() ([]byte, error) {
	var value any
	switch v := o.Value.(type) {
	case *CreateObjectOperation:
		value = v
	case *UpdateObjectOperation:
		value = v
	case *DeleteObjectOperation:
		value = v
	case *CreateRelationOperation:
		value = v
	case *DeleteRelationOperation:
		value = v
	}

	if value == nil {
		return nil, fmt.Errorf("unknown operation type: %T", o.Value)
	}

	return json.Marshal(map[string]any{
		"type":  o.Type,
		"value": value,
	})
}

func (o *OperationWrapper) UnmarshalJSON(data []byte) error {
	var temp struct {
		Type  string          `json:"type"`
		Value json.RawMessage `json:"value"`
	}

	if err := json.Unmarshal(data, &temp); err != nil {
		return err
	}

	o.Type = temp.Type
	var op Operation
	switch temp.Type {
	case "create_object":
		op = &CreateObjectOperation{}
	case "update_object":
		op = &UpdateObjectOperation{}
	case "delete_object":
		op = &DeleteObjectOperation{}
	case "create_relation":
		op = &CreateRelationOperation{}
	case "delete_relation":
		op = &DeleteRelationOperation{}
	}

	if op == nil {
		return fmt.Errorf("unknown operation type: %s", temp.Type)
	}

	if err := json.Unmarshal(temp.Value, op); err != nil {
		return err
	}

	o.Value = op
	return nil
}

type IncomingRequest struct {
	Operations []OperationWrapper `json:"operations"`
}

func NewIncomingRequest(operations []Operation) IncomingRequest {
	var operationWrappers []OperationWrapper
	for _, op := range operations {
		operationWrappers = append(operationWrappers, OperationWrapper{
			Type:  op.OperationType(),
			Value: op,
		})
	}
	return IncomingRequest{
		Operations: operationWrappers,
	}
}

func (r IncomingRequest) GetOperations() []Operation {
	var operations []Operation
	for _, op := range r.Operations {
		operations = append(operations, op.Value)
	}
	return operations
}

var exampleOperations = []Operation{
	&CreateObjectOperation{
		Object: DirectoryObject{
			Type: ObjectType_User,
			ID:   "b478779c-5e5e-4cd7-9bf3-1405326be526",
			Properties: map[string]string{
				"email": "alice@example.com",
			},
		},
	},
	&UpdateObjectOperation{
		Object: DirectoryObject{
			Type: ObjectType_Group,
			ID:   "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca",
			Properties: map[string]string{
				"name": "admins",
			},
		},
	},
	&DeleteObjectOperation{
		ObjectType: ObjectType_Group,
		ObjectID:   "c9b58dd9-b4f6-4325-ba52-3d8d70857363",
	},
	&CreateRelationOperation{
		Relation: DirectoryRelation{
			ObjectType:  ObjectType_Group,
			ObjectID:    "7910720c-9789-4dd3-83a4-4c65eebd82b3",
			Relation:    "member",
			SubjectType: ObjectType_User,
			SubjectID:   "f32756fd-6a92-4034-8b86-c92cc9d9719f",
		},
	},
	&DeleteRelationOperation{
		Relation: DirectoryRelation{
			ObjectType:  ObjectType_Group,
			ObjectID:    "c3e65031-7455-45c8-acbd-59ec59d3e769",
			Relation:    "member",
			SubjectType: ObjectType_User,
			SubjectID:   "f50fd4aa3-d632-46d6-92da-21bcc1391287",
		},
	},
}

func test() {
	request := NewIncomingRequest(exampleOperations)

	data, err := json.MarshalIndent(request, "", "  ")
	if err != nil {
		fmt.Println("Error marshalling request:", err)
		return
	}
	fmt.Println(string(data))
}

func test2() {
	op := OperationWrapper{
		Type: "create_object",
		Value: &CreateObjectOperation{
			Object: DirectoryObject{
				Type: ObjectType_User,
				ID:   "b478779c-5e5e-4cd7-9bf3-1405326be526",
				Properties: map[string]string{
					"email": "alice@example.com",
				},
			},
		},
	}

	data, err := json.MarshalIndent(op, "", "  ")
	if err != nil {
		fmt.Println("Error marshalling operation:", err)
		return
	}
	fmt.Println(string(data))
}

func main() {
	test()
}
