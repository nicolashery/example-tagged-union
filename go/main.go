package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
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

type OperationType int

const (
	OperationType_Unknown OperationType = iota
	OperationType_CreateObject
	OperationType_UpdateObject
	OperationType_DeleteObject
	OperationType_DeleteAllObjects
	OperationType_CreateRelation
	OperationType_DeleteRelation
	OperationType_DeleteAllRelations
)

var OperationTypeStringMap = map[OperationType]string{
	OperationType_CreateObject:       "create_object",
	OperationType_UpdateObject:       "update_object",
	OperationType_DeleteObject:       "delete_object",
	OperationType_DeleteAllObjects:   "delete_all_objects",
	OperationType_CreateRelation:     "create_relation",
	OperationType_DeleteRelation:     "delete_relation",
	OperationType_DeleteAllRelations: "delete_all_relations",
}

var OperationTypeValueMap = map[string]OperationType{
	"create_object":        OperationType_CreateObject,
	"update_object":        OperationType_UpdateObject,
	"delete_object":        OperationType_DeleteObject,
	"delete_all_objects":   OperationType_DeleteAllObjects,
	"create_relation":      OperationType_CreateRelation,
	"delete_relation":      OperationType_DeleteRelation,
	"delete_all_relations": OperationType_DeleteAllRelations,
}

func (t OperationType) MarshalJSON() ([]byte, error) {
	return json.Marshal(OperationTypeStringMap[t])
}

func (t *OperationType) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	if v, ok := OperationTypeValueMap[s]; ok {
		*t = v
		return nil
	}
	return fmt.Errorf("invalid OperationType: %s", s)
}

func (t OperationType) String() string {
	return OperationTypeStringMap[t]
}

//sumtype:decl
type Operation interface {
	isOperation()
	OperationType() OperationType
}

type CreateObjectOperation struct {
	Object DirectoryObject `json:"object"`
}

func (*CreateObjectOperation) isOperation() {}
func (*CreateObjectOperation) OperationType() OperationType {
	return OperationType_CreateObject
}

type UpdateObjectOperation struct {
	Object DirectoryObject `json:"object"`
}

func (*UpdateObjectOperation) isOperation() {}
func (*UpdateObjectOperation) OperationType() OperationType {
	return OperationType_UpdateObject
}

type DeleteObjectOperation struct {
	ObjectType ObjectType `json:"object_type"`
	ObjectID   string     `json:"object_id"`
}

func (*DeleteObjectOperation) isOperation() {}
func (*DeleteObjectOperation) OperationType() OperationType {
	return OperationType_DeleteObject
}

type DeleteAllObjectsOperation struct{}

func (*DeleteAllObjectsOperation) isOperation() {}
func (*DeleteAllObjectsOperation) OperationType() OperationType {
	return OperationType_DeleteAllObjects
}

type CreateRelationOperation struct {
	Relation DirectoryRelation `json:"relation"`
}

func (*CreateRelationOperation) isOperation() {}
func (*CreateRelationOperation) OperationType() OperationType {
	return OperationType_CreateRelation
}

type DeleteRelationOperation struct {
	Relation DirectoryRelation `json:"relation"`
}

func (*DeleteRelationOperation) isOperation() {}
func (*DeleteRelationOperation) OperationType() OperationType {
	return OperationType_DeleteRelation
}

type DeleteAllRelationsOperation struct{}

func (*DeleteAllRelationsOperation) isOperation() {}
func (*DeleteAllRelationsOperation) OperationType() OperationType {
	return OperationType_DeleteAllRelations
}

type OperationWrapper struct {
	Value Operation
}

func (o OperationWrapper) MarshalJSON() ([]byte, error) {
	var tagged struct {
		Type  OperationType   `json:"type"`
		Value json.RawMessage `json:"value,omitempty"`
	}

	var value json.RawMessage
	var err error
	switch v := o.Value.(type) {
	case *CreateObjectOperation:
		tagged.Type = OperationType_CreateObject
		value, err = json.Marshal(v)
	case *UpdateObjectOperation:
		tagged.Type = OperationType_UpdateObject
		value, err = json.Marshal(v)
	case *DeleteObjectOperation:
		tagged.Type = OperationType_DeleteObject
		value, err = json.Marshal(v)
	case *DeleteAllObjectsOperation:
		tagged.Type = OperationType_DeleteAllObjects
	case *CreateRelationOperation:
		tagged.Type = OperationType_CreateRelation
		value, err = json.Marshal(v)
	case *DeleteRelationOperation:
		tagged.Type = OperationType_DeleteRelation
		value, err = json.Marshal(v)
	case *DeleteAllRelationsOperation:
		tagged.Type = OperationType_DeleteAllRelations
	}

	if err != nil {
		return nil, err
	}

	if tagged.Type == OperationType_Unknown {
		return nil, fmt.Errorf("unknown operation type: %T", o.Value)
	}

	tagged.Value = value

	return json.Marshal(tagged)
}

func (o *OperationWrapper) UnmarshalJSON(data []byte) error {
	var tagged struct {
		Type  OperationType   `json:"type"`
		Value json.RawMessage `json:"value,omitempty"`
	}

	if err := json.Unmarshal(data, &tagged); err != nil {
		return err
	}

	var op Operation
	switch tagged.Type {
	case OperationType_CreateObject:
		op = &CreateObjectOperation{}
	case OperationType_UpdateObject:
		op = &UpdateObjectOperation{}
	case OperationType_DeleteObject:
		op = &DeleteObjectOperation{}
	case OperationType_DeleteAllObjects:
		op = &DeleteAllObjectsOperation{}
	case OperationType_CreateRelation:
		op = &CreateRelationOperation{}
	case OperationType_DeleteRelation:
		op = &DeleteRelationOperation{}
	case OperationType_DeleteAllRelations:
		op = &DeleteAllRelationsOperation{}
	}

	if op == nil {
		return fmt.Errorf("unknown operation type: %s", tagged.Type)
	}

	if tagged.Value == nil {
		o.Value = op
		return nil
	}

	if err := json.Unmarshal(tagged.Value, op); err != nil {
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

type OpCode int

const (
	OpCode_Set OpCode = iota
	OpCode_Delete
)

var OpCodeStringMap = map[OpCode]string{
	OpCode_Set:    "set",
	OpCode_Delete: "delete",
}

func (o OpCode) String() string {
	return OpCodeStringMap[o]
}

type DirectoryKind int

const (
	DirectoryKind_Object DirectoryKind = iota
	DirectoryKind_Relation
)

var DirectoryKindStringMap = map[DirectoryKind]string{
	DirectoryKind_Object:   "object",
	DirectoryKind_Relation: "relation",
}

func (k DirectoryKind) String() string {
	return DirectoryKindStringMap[k]
}

func (o *DirectoryObject) ToOutgoingMessage(opCode OpCode) string {
	var result []string
	result = append(result, "op_code: "+opCode.String())
	result = append(result, "kind: object")
	result = append(result, "type: "+o.Type.String())
	result = append(result, "id: "+o.ID)

	for key, value := range o.Properties {
		result = append(result, key+": "+value)
	}

	return strings.Join(result, "\n")
}

func (r *DirectoryRelation) ToOutgoingMessage(opCode OpCode) string {
	var result []string
	result = append(result, "op_code: "+opCode.String())
	result = append(result, "kind: relation")
	result = append(result, "object_type: "+r.ObjectType.String())
	result = append(result, "object_id: "+r.ObjectID)
	result = append(result, "relation: "+r.Relation)
	result = append(result, "subject_type: "+r.SubjectType.String())
	result = append(result, "subject_id: "+r.SubjectID)

	return strings.Join(result, "\n")
}

func DeleteAllOutgoingMessage(kind DirectoryKind) string {
	var result []string
	result = append(result, "op_code: "+OpCode_Delete.String())
	result = append(result, "kind: "+kind.String())
	result = append(result, "all: true")

	return strings.Join(result, "\n")
}

func transform(operation Operation) string {
	var result string
	switch op := operation.(type) {
	case *CreateObjectOperation:
		result = op.Object.ToOutgoingMessage(OpCode_Set)
	case *UpdateObjectOperation:
		result = op.Object.ToOutgoingMessage(OpCode_Set)
	case *DeleteObjectOperation:
		obj := DirectoryObject{
			Type:       op.ObjectType,
			ID:         op.ObjectID,
			Properties: map[string]string{},
		}
		result = obj.ToOutgoingMessage(OpCode_Delete)
	case *DeleteAllObjectsOperation:
		result = DeleteAllOutgoingMessage(DirectoryKind_Object)
	case *CreateRelationOperation:
		result = op.Relation.ToOutgoingMessage(OpCode_Set)
	case *DeleteRelationOperation:
		result = op.Relation.ToOutgoingMessage(OpCode_Delete)
	case *DeleteAllRelationsOperation:
		result = DeleteAllOutgoingMessage(DirectoryKind_Relation)
	}

	return result
}

func transformMany(operations []Operation) string {
	var result []string
	for _, op := range operations {
		result = append(result, transform(op))
	}
	return strings.Join(result, "\n\n")
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
	&DeleteAllObjectsOperation{},
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
	&DeleteAllRelationsOperation{},
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

func run() {
	bytes, err := os.ReadFile("in.json")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var request IncomingRequest
	if err := json.Unmarshal(bytes, &request); err != nil {
		fmt.Println("Error unmarshalling request:", err)
		return
	}

	result := transformMany(request.GetOperations())
	fmt.Println(result)
}

func main() {
	run()
}
