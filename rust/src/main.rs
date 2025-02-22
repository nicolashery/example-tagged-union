use std::fmt;

use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ObjectType {
    User,
    Group,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            ObjectType::User => "user",
            ObjectType::Group => "group",
        };
        write!(f, "{}", output)
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Object {
    #[serde(rename = "type")]
    object_type: ObjectType,
    id: String,
    name: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct CreateObject {
    object: Object,
}

#[derive(Serialize, Deserialize, Debug)]
struct UpdateObject {
    object: Object,
}

#[derive(Serialize, Deserialize, Debug)]
struct DeleteObject {
    id: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
enum Action {
    CreateObject(CreateObject),
    UpdateObject(UpdateObject),
    DeleteObject(DeleteObject),
    DeleteAllObjects,
}

fn transform_action(action: Action) -> String {
    return match action {
        Action::CreateObject(CreateObject { object }) => {
            format!(
                "create_object {} {} {}",
                object.object_type, object.id, object.name
            )
        }
        Action::UpdateObject(UpdateObject { object }) => {
            format!(
                "update_object {} {} {}",
                object.object_type, object.id, object.name
            )
        }
        Action::DeleteObject(DeleteObject { id }) => {
            format!("delete_object {}", id)
        }
        Action::DeleteAllObjects => "delete_all_objects".to_string(),
    };
}

fn example_actions() -> Vec<Action> {
    vec![
        Action::CreateObject(CreateObject {
            object: Object {
                object_type: ObjectType::User,
                id: "1".to_string(),
                name: "user1".to_string(),
            },
        }),
        Action::UpdateObject(UpdateObject {
            object: Object {
                object_type: ObjectType::User,
                id: "1".to_string(),
                name: "user1 updated".to_string(),
            },
        }),
        Action::DeleteAllObjects,
    ]
}

pub fn main() {
    let actions = example_actions();

    // JSON encode
    let json = serde_json::to_string_pretty(&actions).unwrap();
    println!("## JSON");
    println!();
    println!("```json");
    println!("{}", json);
    println!("```");
    println!();

    // JSON decode
    let actions2: Vec<Action> = serde_json::from_str(&json).unwrap();
    println!("## Debug");
    println!();
    println!("```rust");
    for action in &actions2 {
        println!("{:?}", action);
    }
    println!("```");
    println!();

    // Transform
    println!("## Transformed");
    println!();
    println!("```");
    for action in actions {
        println!("{}", transform_action(action));
    }
    println!("```");
    println!();
}
