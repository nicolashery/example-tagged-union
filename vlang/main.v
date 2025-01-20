module main

import json

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

struct CreateRelationOperation {
	relation DirectoryRelation
}

struct DeleteRelationOperation {
	relation DirectoryRelation
}

type Operation = CreateObjectOperation
	| UpdateObjectOperation
	| DeleteObjectOperation
	| CreateRelationOperation
	| DeleteRelationOperation

struct IncomingRequest {
	operations []Operation
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
	]
}

fn test() {
	request := IncomingRequest{
		operations: example_operations()
	}
	println(json.encode_pretty(request))
}

fn main() {
	test()
}
