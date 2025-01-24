module main

import json
import os

enum ObjectType {
	user
	group
}

fn (o ObjectType) to_string() string {
	return match o {
		.user { 'user' }
		.group { 'group' }
	}
}

struct DirectoryObject {
	type       ObjectType
	id         string
	properties map[string]string
}

struct DirectoryRelation {
	object_type  ObjectType
	object_id    string
	relation     string
	subject_type ObjectType
	subject_id   string
}

struct CreateObjectOperation {
	object DirectoryObject
}

struct UpdateObjectOperation {
	object DirectoryObject
}

struct DeleteObjectOperation {
	object_type ObjectType
	object_id   string
}

struct DeleteAllObjectsOperation {}

struct CreateRelationOperation {
	relation DirectoryRelation
}

struct DeleteRelationOperation {
	relation DirectoryRelation
}

struct DeleteAllRelationsOperation {}

type Operation = CreateObjectOperation
	| UpdateObjectOperation
	| DeleteObjectOperation
	| DeleteAllObjectsOperation
	| CreateRelationOperation
	| DeleteRelationOperation
	| DeleteAllRelationsOperation

struct IncomingRequest {
	operations []Operation
}

enum OpCode {
	set
	delete
}

fn (o OpCode) to_string() string {
	return match o {
		.set { 'set' }
		.delete { 'delete' }
	}
}

enum DirectoryKind {
	object
	relation
}

fn (o DirectoryKind) to_string() string {
	return match o {
		.object { 'object' }
		.relation { 'relation' }
	}
}

fn (o DirectoryObject) to_outgoing_message(op_code OpCode) string {
	mut result := [
		'op_code: ${op_code.to_string()}',
		'kind: ${DirectoryKind.object.to_string()}',
		'type: ${o.type.to_string()}',
		'id: ${o.id}',
	]

	for key, value in o.properties {
		result << '${key}: ${value}'
	}

	return result.join_lines()
}

fn (o DirectoryRelation) to_outgoing_message(op_code OpCode) string {
	mut result := [
		'op_code: ${op_code.to_string()}',
		'kind: ${DirectoryKind.relation.to_string()}',
		'object_type: ${o.object_type.to_string()}',
		'object_id: ${o.object_id}',
		'relation: ${o.relation}',
		'subject_type: ${o.subject_type.to_string()}',
		'subject_id: ${o.subject_id}',
	]

	return result.join_lines()
}

fn delete_all_outgoing_message(kind DirectoryKind) string {
	return [
		'op_code: ${OpCode.delete.to_string()}',
		'kind: ${kind.to_string()}',
		'all: true',
	].join_lines()
}

fn transform(op Operation) string {
	return match op {
		CreateObjectOperation {
			op.object.to_outgoing_message(OpCode.set)
		}
		UpdateObjectOperation {
			op.object.to_outgoing_message(OpCode.set)
		}
		DeleteObjectOperation {
			DirectoryObject{
				type: op.object_type
				id:   op.object_id
			}.to_outgoing_message(OpCode.delete)
		}
		DeleteAllObjectsOperation {
			delete_all_outgoing_message(DirectoryKind.object)
		}
		CreateRelationOperation {
			op.relation.to_outgoing_message(OpCode.set)
		}
		DeleteRelationOperation {
			op.relation.to_outgoing_message(OpCode.delete)
		}
		DeleteAllRelationsOperation {
			delete_all_outgoing_message(DirectoryKind.relation)
		}
	}
}

fn transform_many(ops []Operation) string {
	return ops.map(transform).join('\n\n')
}

fn example_operations() []Operation {
	return [
		CreateObjectOperation{
			object: DirectoryObject{
				type:       ObjectType.user
				id:         'b478779c-5e5e-4cd7-9bf3-1405326be526'
				properties: {
					'email': 'alice@example.com'
				}
			}
		},
		UpdateObjectOperation{
			object: DirectoryObject{
				type:       ObjectType.group
				id:         '2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca'
				properties: {
					'name': 'admins'
				}
			}
		},
		DeleteObjectOperation{
			object_type: ObjectType.group
			object_id:   'c9b58dd9-b4f6-4325-ba52-3d8d70857363'
		},
		DeleteAllObjectsOperation{},
		CreateRelationOperation{
			relation: DirectoryRelation{
				object_type:  ObjectType.group
				object_id:    '7910720c-9789-4dd3-83a4-4c65eebd82b3'
				relation:     'member'
				subject_type: ObjectType.user
				subject_id:   'f32756fd-6a92-4034-8b86-c92cc9d9719f'
			}
		},
		DeleteRelationOperation{
			relation: DirectoryRelation{
				object_type:  ObjectType.group
				object_id:    'c3e65031-7455-45c8-acbd-59ec59d3e769'
				relation:     'member'
				subject_type: ObjectType.user
				subject_id:   'f50fd4aa3-d632-46d6-92da-21bcc1391287'
			}
		},
		DeleteAllRelationsOperation{},
	]
}

fn test() {
	request := IncomingRequest{
		operations: example_operations()
	}
	println(json.encode_pretty(request))
}

fn run() {
	data := os.read_file('in.json') or {
		eprintln('Error reading file: ${err}')
		return
	}

	request := json.decode(IncomingRequest, data) or {
		eprintln('Error decoding JSON: ${err}')
		return
	}

	result := transform_many(request.operations)

	println(result)
}

fn main() {
	run()
}
