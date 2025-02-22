import { z } from "zod";

const ObjectType = z.enum([
  "user",
  "group",
]);

type ObjectType = z.infer<typeof ObjectType>;

const Object = z.object({
  type: ObjectType,
  id: z.string(),
  name: z.string(),
});

type Object = z.infer<typeof Object>;

const CreateObject = z.object({
  type: z.literal("create_object"),
  object: Object,
});

type CreateObject = z.infer<typeof CreateObject>;

const UpdateObject = z.object({
  type: z.literal("update_object"),
  object: Object,
});

type UpdateObject = z.infer<typeof UpdateObject>;

const DeleteObject = z.object({
  type: z.literal("delete_object"),
  id: z.string(),
});

type DeleteObject = z.infer<typeof DeleteObject>;

const DeleteAllObjects = z.object({
  type: z.literal("delete_all_objects"),
});

type DeleteAllObjects = z.infer<typeof DeleteAllObjects>;

const Action = z.discriminatedUnion("type", [
  CreateObject,
  UpdateObject,
  DeleteObject,
  DeleteAllObjects,
]);

type Action =
  | CreateObject
  | UpdateObject
  | DeleteObject
  | DeleteAllObjects;

function transform(action: Action): string {
  switch (action.type) {
    case "create_object":
      return `create_object ${action.object.type} ${action.object.id} ${action.object.name}`;

    case "update_object":
      return `update_object ${action.object.type} ${action.object.id} ${action.object.name}`;

    case "delete_object":
      return `delete_object ${action.id}`;

    case "delete_all_objects":
      return "delete_all_objects";

    default: {
      const _exhaustiveCheck: never = action;
      return _exhaustiveCheck;
    }
  }
}

const exampleActions: Action[] = [
  {
    type: "create_object",
    object: {
      type: "user",
      id: "1",
      name: "user1",
    },
  },
  {
    type: "update_object",
    object: {
      type: "user",
      id: "1",
      name: "user1 updated",
    },
  },
  {
    type: "delete_all_objects",
  },
];

function run() {
  // JSON encode
  console.log("## JSON");
  console.log();
  console.log("```json");
  console.log(JSON.stringify(exampleActions, null, 2));
  console.log("```");
  console.log();

  // JSON decode (validation)
  const actions = Action.array().parse(exampleActions);
  console.log("## Debug");
  console.log();
  console.log("```typescript");
  actions.forEach((action) => console.log(action));
  console.log("```");
  console.log();

  // Transform
  console.log("## Transformed");
  console.log();
  console.log("```");
  exampleActions.forEach((action) => console.log(transform(action)));
  console.log("```");
  console.log();
}

if (import.meta.main) {
  run();
}
