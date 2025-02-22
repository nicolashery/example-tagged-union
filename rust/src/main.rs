use std::fmt;

use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
enum ItemType {
    User,
    Group,
}

impl fmt::Display for ItemType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            ItemType::User => "user",
            ItemType::Group => "group",
        };
        write!(f, "{}", output)
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Item {
    #[serde(rename = "type")]
    item_type: ItemType,
    id: String,
    name: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct CreateItem {
    item: Item,
}

#[derive(Serialize, Deserialize, Debug)]
struct UpdateItem {
    item: Item,
}

#[derive(Serialize, Deserialize, Debug)]
struct DeleteItem {
    id: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
enum Action {
    CreateItem(CreateItem),
    UpdateItem(UpdateItem),
    DeleteItem(DeleteItem),
    DeleteAllItems,
}

fn transform_action(action: Action) -> String {
    return match action {
        Action::CreateItem(CreateItem { item }) => {
            format!("create_item {} {} {}", item.item_type, item.id, item.name)
        }
        Action::UpdateItem(UpdateItem { item }) => {
            format!("update_item {} {} {}", item.item_type, item.id, item.name)
        }
        Action::DeleteItem(DeleteItem { id }) => {
            format!("delete_item {}", id)
        }
        Action::DeleteAllItems => "delete_all_items".to_string(),
    };
}

fn example_actions() -> Vec<Action> {
    vec![
        Action::CreateItem(CreateItem {
            item: Item {
                item_type: ItemType::User,
                id: "1".to_string(),
                name: "user1".to_string(),
            },
        }),
        Action::UpdateItem(UpdateItem {
            item: Item {
                item_type: ItemType::User,
                id: "1".to_string(),
                name: "user1 updated".to_string(),
            },
        }),
        Action::DeleteAllItems,
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
