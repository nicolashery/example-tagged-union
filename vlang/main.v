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

struct Object {
	type ObjectType
	id   string
	name string
}

struct CreateObject {
	object Object
}

struct UpdateObject {
	object Object
}

struct DeleteObject {
	id string
}

struct DeleteAllObjects {}

type Action = CreateObject | UpdateObject | DeleteObject | DeleteAllObjects

fn transform_action(action Action) string {
	return match action {
		CreateObject {
			'create_object ${action.object.type} ${action.object.id} ${action.object.name}'
		}
		UpdateObject {
			'update_object ${action.object.type} ${action.object.id} ${action.object.name}'
		}
		DeleteObject {
			'delete_object ${action.id}'
		}
		DeleteAllObjects {
			'delete_all_objects'
		}
	}
}

fn example_actions() []Action {
	return [
		CreateObject{
			object: Object{
				type: ObjectType.user
				id:   '1'
				name: 'user'
			}
		},
		UpdateObject{
			object: Object{
				type: ObjectType.user
				id:   '1'
				name: 'user1 updated'
			}
		},
		DeleteObject{
			id: '1'
		},
		DeleteAllObjects{},
	]
}

fn main() {
	mut actions := example_actions()

	// JSON encode
	json_str := json.encode_pretty(actions)
	println('## JSON')
	println('')
	println('```json')
	println(json_str)
	println('```')
	println('')

	// JSON decode
	actions = json.decode([]Action, json_str) or {
		eprintln('Error decoding JSON: ${err}')
		return
	}
	println('## Debug')
	println('')
	println('```v')
	for action in actions {
		println(action)
	}
	println('```')
	println('')

	// Transformed
	println('## Transformed')
	println('')
	println('```')
	for action in example_actions() {
		println(transform_action(action))
	}
	println('```')
	println('')
}
