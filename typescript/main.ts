import { z } from "zod";

const ObjectType = z.enum([
  "user",
  "group",
]);

// type ObjectType = z.infer<typeof ObjectType>;
type ObjectType =
  | "user"
  | "group";

const DirectoryObject = z.object({
  type: ObjectType,
  id: z.string(),
  properties: z.record(z.string(), z.string()),
});

// type DirectoryObject = z.infer<typeof DirectoryObject>;
type DirectoryObject = {
  type: ObjectType;
  id: string;
  properties: Record<string, string>;
};

const DirectoryRelation = z
  .object({
    objectType: ObjectType,
    objectId: z.string(),
    relation: z.string(),
    subjectType: ObjectType,
    subjectId: z.string(),
  });

// type DirectoryRelation = z.infer<typeof DirectoryRelation>;
type DirectoryRelation = {
  objectType: ObjectType;
  objectId: string;
  relation: string;
  subjectType: ObjectType;
  subjectId: string;
};

const CreateObjectOperation = z.object({
  object: DirectoryObject,
});

// type CreateObjectOperation = z.infer<typeof CreateObjectOperation>;
type CreateObjectOperation = {
  object: DirectoryObject;
};

const UpdateObjectOperation = z.object({
  object: DirectoryObject,
});

// type UpdateObjectOperation = z.infer<typeof UpdateObjectOperation>;
type UpdateObjectOperation = {
  object: DirectoryObject;
};

const DeleteObjectOperation = z.object({
  objectType: ObjectType,
  objectId: z.string(),
});

// type DeleteObjectOperation = z.infer<typeof DeleteObjectOperation>;
type DeleteObjectOperation = {
  objectType: ObjectType;
  objectId: string;
};

const CreateRelationOperation = z.object({
  relation: DirectoryRelation,
});

// type CreateRelationOperation = z.infer<typeof CreateRelationOperation>;
type CreateRelationOperation = {
  relation: DirectoryRelation;
};

const DeleteRelationOperation = z.object({
  relation: DirectoryRelation,
});

// type DeleteRelationOperation = z.infer<typeof DeleteRelationOperation>;
type DeleteRelationOperation = {
  relation: DirectoryRelation;
};

const Operation = z.discriminatedUnion("type", [
  z.object({
    type: z.literal("create_object"),
    value: CreateObjectOperation,
  }),
  z.object({
    type: z.literal("update_object"),
    value: UpdateObjectOperation,
  }),
  z.object({
    type: z.literal("delete_object"),
    value: DeleteObjectOperation,
  }),
  z.object({
    type: z.literal("create_relation"),
    value: CreateRelationOperation,
  }),
  z.object({
    type: z.literal("delete_relation"),
    value: DeleteRelationOperation,
  }),
]);

// type Operation = z.infer<typeof Operation>;
type Operation =
  | { type: "create_object"; value: CreateObjectOperation }
  | { type: "update_object"; value: UpdateObjectOperation }
  | { type: "delete_object"; value: DeleteObjectOperation }
  | { type: "create_relation"; value: CreateRelationOperation }
  | { type: "delete_relation"; value: DeleteRelationOperation };

const IncomingRequest = z.object({
  operations: z.array(Operation),
});

// type IncomingRequest = z.infer<typeof IncomingRequest>;
type IncomingRequest = {
  operations: Operation[];
};

const OpCode = z.enum([
  "set",
  "delete",
]);

// type OpCode = z.infer<typeof OpCode>;
type OpCode =
  | "set"
  | "delete";

function objectToOutgoingMessage(obj: DirectoryObject, opCode: OpCode): string {
  const result = [
    "op_code: " + opCode,
    "kind: object",
    "type: " + obj.type,
    "id: " + obj.id,
  ];

  for (const [key, value] of Object.entries(obj.properties)) {
    result.push(key + ": " + value);
  }

  return result.join("\n");
}

function relationToOutgoingMessage(
  rel: DirectoryRelation,
  opCode: OpCode,
): string {
  const result = [
    "op_code: " + opCode,
    "kind: relation",
    "object_type: " + rel.objectType,
    "object_id: " + rel.objectId,
    "relation: " + rel.relation,
    "subject_type: " + rel.subjectType,
    "subject_id: " + rel.subjectId,
  ];

  return result.join("\n");
}

function transform(operation: Operation): string {
  const { type, value: op } = operation;

  switch (type) {
    case "create_object":
      return objectToOutgoingMessage(op.object, OpCode.enum.set);

    case "update_object":
      return objectToOutgoingMessage(op.object, OpCode.enum.set);

    case "delete_object": {
      const obj: DirectoryObject = {
        type: op.objectType,
        id: op.objectId,
        properties: {},
      };
      return objectToOutgoingMessage(obj, OpCode.enum.delete);
    }

    case "create_relation":
      return relationToOutgoingMessage(op.relation, OpCode.enum.set);

    case "delete_relation":
      return relationToOutgoingMessage(op.relation, OpCode.enum.delete);

    default: {
      const _exhaustiveCheck: never = op;
      return _exhaustiveCheck;
    }
  }
}

function transformMany(ops: Operation[]): string {
  return ops.map(transform).join("\n\n");
}

const exampleOperations: Operation[] = [
  {
    type: "create_object",
    value: {
      object: {
        type: ObjectType.enum.user,
        id: "b478779c-5e5e-4cd7-9bf3-1405326be526",
        properties: {
          email: "alice@example.com",
        },
      },
    },
  },
  {
    type: "update_object",
    value: {
      object: {
        type: ObjectType.enum.group,
        id: "2ca6785b-a2ef-4a62-a5f6-5e2314ae59ca",
        properties: {
          name: "admins",
        },
      },
    },
  },
  {
    type: "delete_object",
    value: {
      objectType: ObjectType.enum.group,
      objectId: "c9b58dd9-b4f6-4325-ba52-3d8d70857363",
    },
  },
  {
    type: "create_relation",
    value: {
      relation: {
        objectType: ObjectType.enum.group,
        objectId: "7910720c-9789-4dd3-83a4-4c65eebd82b3",
        relation: "member",
        subjectType: ObjectType.enum.user,
        subjectId: "f32756fd-6a92-4034-8b86-c92cc9d9719f",
      },
    },
  },
  {
    type: "delete_relation",
    value: {
      relation: {
        objectType: ObjectType.enum.group,
        objectId: "c3e65031-7455-45c8-acbd-59ec59d3e769",
        relation: "member",
        subjectType: ObjectType.enum.user,
        subjectId: "f50fd4aa3-d632-46d6-92da-21bcc1391287",
      },
    },
  },
];

function _test() {
  const request = IncomingRequest.parse({
    operations: exampleOperations,
  });

  console.log(JSON.stringify(request, null, 2));
}

function run() {
  const decoder = new TextDecoder("utf-8");
  const bytes = Deno.readFileSync("in.json");

  const request = IncomingRequest.parse(JSON.parse(decoder.decode(bytes)));

  const result = transformMany(request.operations);

  console.log(result);
}

function main() {
  run();
}

if (import.meta.main) {
  main();
}
