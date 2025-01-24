use serde::{Deserialize, Serialize};
use serde_json;
use std::{fmt, fs};

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
#[serde(rename_all = "snake_case")]
struct DirectoryObject {
    #[serde(rename = "type")]
    object_type: ObjectType,
    id: String,
    properties: std::collections::HashMap<String, String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct DirectoryRelation {
    object_type: ObjectType,
    object_id: String,
    relation: String,
    subject_type: ObjectType,
    subject_id: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct CreateObjectOperation {
    object: DirectoryObject,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct UpdateObjectOperation {
    object: DirectoryObject,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct DeleteObjectOperation {
    object_type: ObjectType,
    object_id: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct CreateRelationOperation {
    relation: DirectoryRelation,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct DeleteRelationOperation {
    relation: DirectoryRelation,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "snake_case")]
enum Operation {
    CreateObject(CreateObjectOperation),
    UpdateObject(UpdateObjectOperation),
    DeleteObject(DeleteObjectOperation),
    DeleteAllObjects,
    CreateRelation(CreateRelationOperation),
    DeleteRelation(DeleteRelationOperation),
    DeleteAllRelations,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
struct IncomingRequest {
    operations: Vec<Operation>,
}

#[derive(Debug)]
enum OpCode {
    Set,
    Delete,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            OpCode::Set => "set",
            OpCode::Delete => "delete",
        };
        write!(f, "{}", output)
    }
}

enum DirectoryKind {
    Object,
    Relation,
}

impl fmt::Display for DirectoryKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            DirectoryKind::Object => "object",
            DirectoryKind::Relation => "relation",
        };
        write!(f, "{}", output)
    }
}

fn object_to_outgoing_message(obj: &DirectoryObject, op_code: OpCode) -> String {
    let mut result = vec![
        format!("op_code: {}", op_code),
        format!("kind: {}", DirectoryKind::Object),
        format!("type: {}", obj.object_type),
        format!("id: {}", obj.id),
    ];

    for (key, value) in &obj.properties {
        result.push(format!("{}: {}", key, value));
    }

    result.join("\n")
}

fn relation_to_outgoing_message(rel: &DirectoryRelation, op_code: OpCode) -> String {
    let result = vec![
        format!("op_code: {}", op_code),
        format!("kind: {}", DirectoryKind::Relation),
        format!("object_type: {}", rel.object_type),
        format!("object_id: {}", rel.object_id),
        format!("relation: {}", rel.relation),
        format!("subject_type: {}", rel.subject_type),
        format!("subject_id: {}", rel.subject_id),
    ];

    result.join("\n")
}

fn delete_all_outgoing_message(kind: DirectoryKind) -> String {
    let result = vec![
        format!("op_code: {}", OpCode::Delete),
        format!("kind: {}", kind),
        "all: true".to_string(),
    ];

    result.join("\n")
}

fn transform(operation: &Operation) -> String {
    match operation {
        Operation::CreateObject(op) => object_to_outgoing_message(&op.object, OpCode::Set),
        Operation::UpdateObject(op) => object_to_outgoing_message(&op.object, OpCode::Set),
        Operation::DeleteObject(op) => {
            let obj = DirectoryObject {
                object_type: op.object_type.clone(),
                id: op.object_id.clone(),
                properties: std::collections::HashMap::new(),
            };
            object_to_outgoing_message(&obj, OpCode::Delete)
        }
        Operation::DeleteAllObjects => delete_all_outgoing_message(DirectoryKind::Object),
        Operation::CreateRelation(op) => relation_to_outgoing_message(&op.relation, OpCode::Set),
        Operation::DeleteRelation(op) => relation_to_outgoing_message(&op.relation, OpCode::Delete),
        Operation::DeleteAllRelations => delete_all_outgoing_message(DirectoryKind::Relation),
    }
}

fn transform_many(ops: &[Operation]) -> String {
    ops.iter().map(transform).collect::<Vec<_>>().join("\n\n")
}

fn example_operations() -> Vec<Operation> {
    vec![
        Operation::CreateObject(CreateObjectOperation {
            object: DirectoryObject {
                object_type: ObjectType::User,
                id: "b478779c-5e5e-4cd7-9bf3-1405326be526".to_string(),
                properties: {
                    let mut props = std::collections::HashMap::new();
                    props.insert("email".to_string(), "alice@example.com".to_string());
                    props
                },
            },
        }),
        Operation::UpdateObject(UpdateObjectOperation {
            object: DirectoryObject {
                object_type: ObjectType::Group,
                id: "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca".to_string(),
                properties: {
                    let mut props = std::collections::HashMap::new();
                    props.insert("name".to_string(), "admins".to_string());
                    props
                },
            },
        }),
        Operation::DeleteObject(DeleteObjectOperation {
            object_type: ObjectType::Group,
            object_id: "c9b58dd9-b4f6-4325-ba52-3d8d70857363".to_string(),
        }),
        Operation::DeleteAllObjects,
        Operation::CreateRelation(CreateRelationOperation {
            relation: DirectoryRelation {
                object_type: ObjectType::Group,
                object_id: "7910720c-9789-4dd3-83a4-4c65eebd82b3".to_string(),
                relation: "member".to_string(),
                subject_type: ObjectType::User,
                subject_id: "f32756fd-6a92-4034-8b86-c92cc9d9719f".to_string(),
            },
        }),
        Operation::DeleteRelation(DeleteRelationOperation {
            relation: DirectoryRelation {
                object_type: ObjectType::Group,
                object_id: "c3e65031-7455-45c8-acbd-59ec59d3e769".to_string(),
                relation: "member".to_string(),
                subject_type: ObjectType::User,
                subject_id: "f50fd4aa3-d632-46d6-92da-21bcc1391287".to_string(),
            },
        }),
        Operation::DeleteAllRelations,
    ]
}

#[allow(dead_code)]
fn test() {
    let request = IncomingRequest {
        operations: example_operations(),
    };

    println!("{}", serde_json::to_string_pretty(&request).unwrap());
}

fn run() {
    let data = fs::read_to_string("in.json").expect("Unable to read file");
    let request: IncomingRequest =
        serde_json::from_str(&data).expect("JSON was not well-formatted");

    let result = transform_many(&request.operations);

    println!("{}", result);
}

fn main() {
    run();
}
