# OpenAPI

Install [Node](https://github.com/nvm-sh/nvm) and dependencies:

```
npm install
```

Preview API docs using [Redocly](https://redocly.com/docs/cli/quickstart):

```
npm run preview
```

For code generation using [OpenAPI Generator](https://openapi-generator.tech/), make sure [Java](https://sdkman.io/) is installed then run:

```
npm run openapi-generator-typescript
npm run openapi-generator-go
```

For TypeScript code generation using [swagger-typescript-api](https://github.com/acacode/swagger-typescript-api), run:

```
npm run swagger-typescript-api
```

For Go code generation using [oapi-codegen](https://github.com/oapi-codegen/oapi-codegen), make sure [Go](https://go.dev/dl/) is installed then run:

```
mkdir -p gen/oapi-codegen
go generate
```
