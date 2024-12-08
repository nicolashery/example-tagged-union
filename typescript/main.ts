import { z } from "zod";

const DirectoryObject = z.object({
  type: z.string(),
  id: z.string(),
  properties: z.record(z.string(), z.string()),
});

type DirectoryObject = z.infer<typeof DirectoryObject>;

const DirectoryRelation = z
  .object({
    objectType: z.string(),
    objectId: z.string(),
    relation: z.string(),
    subjectType: z.string(),
    subjectId: z.string(),
  });

type DirectoryRelation = z.infer<typeof DirectoryRelation>;

const OperationType = z.enum([
  "create_object",
  "update_object",
  "delete_object",
  "create_relation",
  "delete_relation",
]);

type OperationType = z.infer<typeof OperationType>;

const CreateObject = OperationType.enum.create_object;
const UpdateObject = OperationType.enum.update_object;
const DeleteObject = OperationType.enum.delete_object;
const CreateRelation = OperationType.enum.create_relation;
const DeleteRelation = OperationType.enum.delete_relation;

const CreateObjectOperation = z.object({
  type: z.literal(CreateObject),
  object: DirectoryObject,
});

type CreateObjectOperation = z.infer<typeof CreateObjectOperation>;

const UpdateObjectOperation = z.object({
  type: z.literal(UpdateObject),
  object: DirectoryObject,
});

type UpdateObjectOperation = z.infer<typeof UpdateObjectOperation>;

const DeleteObjectOperation = z.object({
  type: z.literal(DeleteObject),
  objectType: z.string(),
  objectId: z.string(),
});

type DeleteObjectOperation = z.infer<typeof DeleteObjectOperation>;

const CreateRelationOperation = z.object({
  type: z.literal(CreateRelation),
  relation: DirectoryRelation,
});

type CreateRelationOperation = z.infer<typeof CreateRelationOperation>;

const DeleteRelationOperation = z.object({
  type: z.literal(DeleteRelation),
  relation: DirectoryRelation,
});

type DeleteRelationOperation = z.infer<typeof DeleteRelationOperation>;

const Operation = z.discriminatedUnion("type", [
  CreateObjectOperation,
  UpdateObjectOperation,
  DeleteObjectOperation,
  CreateRelationOperation,
  DeleteRelationOperation,
]);

type Operation = z.infer<typeof Operation>;

function toImportRequestObject(obj: DirectoryObject): string {
  const result = [
    "kind: object",
    "type: " + obj.type,
    "id: " + obj.id,
  ];

  for (const [key, value] of Object.entries(obj.properties)) {
    result.push(key + ": " + value);
  }

  return result.join("\n");
}

function toImportRequestRelation(rel: DirectoryRelation): string {
  return [
    "kind: relation",
    "object_type: " + rel.objectType,
    "object_id: " + rel.objectId,
    "relation: " + rel.relation,
    "subject_type: " + rel.subjectType,
    "subject_id: " + rel.subjectId,
  ].join("\n");
}

function toImportRequest(op: Operation): string {
  switch (op.type) {
    case CreateObject:
      return [
        "op_code: set",
        toImportRequestObject(op.object),
      ].join("\n");

    case UpdateObject:
      return [
        "op_code: set",
        toImportRequestObject(op.object),
      ].join("\n");

    case DeleteObject: {
      // return [
      //   "op_code: delete",
      //   toImportRequestObject(op.object)
      // ].join("\n");
      // // Type error: Property 'object' does not exist on type ...
      const obj: DirectoryObject = {
        type: op.objectType,
        id: op.objectId,
        properties: {},
      };
      return [
        "op_code: delete",
        toImportRequestObject(obj),
      ].join("\n");
    }

    case CreateRelation:
      return [
        "op_code: set",
        toImportRequestRelation(op.relation),
      ].join("\n");

    case DeleteRelation:
      return [
        "op_code: delete",
        toImportRequestRelation(op.relation),
      ].join("\n");

    default: {
      const _exhaustiveCheck: never = op;
      return _exhaustiveCheck;
    }
  }
}

function toImportRequests(ops: Operation[]): string {
  return ops.map(toImportRequest).join("\n\n");
}

function main() {
  const decoder = new TextDecoder("utf-8");
  const bytes = Deno.readFileSync("in.json");

  const fileSchema = z.object({
    operations: z.array(Operation),
  });
  const data = fileSchema.parse(JSON.parse(decoder.decode(bytes)));

  const result = toImportRequests(data.operations);

  console.log(result);
}

if (import.meta.main) {
  main();
}
