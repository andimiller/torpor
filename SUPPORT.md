# Supported

What works today, per milestone. Anything not listed fails with an error naming the feature; the
goal is that nothing half-works silently.

## M0 — in progress

| Feature | State |
|---|---|
| Parse SDL (`sangria-parser`, pinned 4.x) | yes |
| Merge fragments: one owner per type, `extend` from other sources | yes |
| Root types: `extend type Query` synthesized; explicit `schema { ... }` supported | yes |
| Field collision detection with both origins | yes |
| Position info (`source:line:column`) on every merge error | yes |
| All merge errors reported in one pass, not just the first | yes |
| Reject client queries or custom directive definitions passed as SDL | yes |
| Parse executable documents (operations, fragments, values) | yes |
| Select an operation by name; require a name when several exist | yes |
| Query validation: the subset below | yes |
| Field type reference checks (unknown field types) | partially: reported when a selection reaches them |
| Custom directives | no |
| Plan ADT (serialisable) and planner: fields → nodes, key-flow edges, levels | yes |
| Executor: one batch call per node per level, keys deduped | yes |
| Errors as values: `UPSTREAM_UNAVAILABLE`, `MISSING_KEY`, `INVALID_PARENT_VALUE`, `INVALID_INPUT` | yes |
| Response assembly keyed by alias, partial data preserved | yes |
| Resolver DSL: `root` (typed args + result) and keyed `field(...).key(...).batch(...)` | yes |
| Arguments on keyed fields: typed from the SDL, passed to the batch, substituted variables and SDL defaults | yes |
| Union/interface planning: `... on T`, named fragments, `__typename` anywhere | yes; a resolver supplies a member's `__typename` (`resolver.tagged` helps) |
| Null bubbling (non-null field resolving to null) | yes: the nearest nullable ancestor is nulled, the error keeps the original path |
| Input coercion and defaults for literals and variables (built-in scalars, enums, input objects) | yes; custom scalar input follows the `Scalar` binding's `Decoder` |
| Hooks: per-node span/timing, slow-query log, per-batch authorization, global error mapper | yes (`Hooks[F]`, `Torpor.withHooks`) |
| `torpor-testkit` (`assertBatched`) | no: call-count assertions live in `ExecutorSuite` for now |
| `maxDepth` / `maxFields` as plan phases | yes |
| SDL↔resolver coverage check at startup (missing and unexpected resolvers) | yes |
| HTTP: `POST /graphql`, JSON body, variables, `operationName`, GraphQL errors as 200s | yes |

### Validation subset

Implemented (spec section in brackets):

- [5.1.1] executable definitions only in a query document
- [5.2.1.1] operation name uniqueness; [5.2.2.1] lone anonymous operation
- [5.3.1] field selections (field exists on the parent type, `__typename`, unions)
- [5.3.2] field selection merging: same response key must be the same field with the same arguments
- [5.3.3] leaf field selections (no selection set on leaves, required on composites)
- [5.4.1] argument names; [5.4.2] argument uniqueness; [5.4.2.1] required arguments (honours defaults)
- [5.5.1.1] fragment name uniqueness; [5.5.1.2] fragment type existence, spread target defined
- [5.5.2.1] fragment spread cycles
- [5.7.1] directive names; [5.7.2] directive locations; [5.7.3] uniqueness per location
- [5.8.1] variable uniqueness; [5.8.2] variables are input types; [5.8.3] all variable uses defined (through fragments)
- operation type exists for the operation kind

Not implemented, and therefore rejected or deferred (see "Scoping discipline" in the notes):

- [5.6.1] the full spec coercion table: built-in scalars, enums and input objects are coerced in the planner; a custom scalar's values pass through to its decoder
- [5.8.5] variable usages allowed: a variable's declared type is not compared with the argument's type
- [5.3.2] cross-fragment merging and return-type compatibility (the planner rejects conflicting response keys where type conditions overlap)
- [5.5.1.1] possible fragment spreads are checked by the planner at execution time, not by validation
- [5.5.1.3] unused fragments, [5.8.4] unused variables (harmless; not rejected)
- introspection (`__schema`, `__type` in a normal query), `@defer`, `@stream`, subscriptions: rejected with `unsupported`
- custom directives: rejected at SDL merge time

### Executor

- One node = one batch call. Keys are extracted from the parent value, deduped, and sent once; a missing result becomes a
  `MISSING_KEY` error at that response path with `null` data.
- A resolver that raises becomes `UPSTREAM_UNAVAILABLE` errors at every position it served: partial data survives and
  hedgehogs' fail-fast visitor is never triggered (invariant 1). There is a test for exactly this.
- Error paths are absolute response paths: response keys (aliases) plus list indices.
- Node results are keyed by path, which is how assembly finds each child value; execution and assembly share the same
  deterministic walk, so they cannot disagree.
- Fields are answered inline only when they came from the type definition; anything from `extend type` is a node. Keys
  come from the enclosing parent value, not from the parent's data for that field.

### Planner

- Field ownership comes from the merged schema: a field declared by `extend type` gets its own node; a field declared in
  the type definition is read inline off the value the parent node produced. Root fields always get a node.
- Variables are substituted before planning, so the plan only holds literals (no coercion, see above).
- Fragments are expanded and merged by response key before a selection set is used anywhere, so batch grouping does not
  depend on query text.
- `@skip`/`@include` are evaluated at plan time; a non-boolean `if` is an error.
- Type conditions that narrow the parent type (interfaces/unions) are rejected as unsupported until M1.
- Mutation root fields are planned as independent nodes; serial execution is the executor's job.
- Two aliases of the same field plan two nodes: node sharing across response keys is not implemented (duplicate call,
  never a wrong answer).

### M0 exit criteria

All three from `pitch.md` are met and tested:

- pet shop `pets` + `owners` runs with 2 backend calls (`ExecutorSuite`; 3 pets, 2 owners, keys deduped)
- conformance subset green (`ValidationSuite`, 58 fixtures)
- an unserved SDL field fails startup with the field named (`LimitsAndCoverageSuite`)

## M1 — durable runs (in progress)

| Feature | State |
|---|---|
| `SuspendableDagVisitor` behind `Durable`, `Step.suspend`/`Step.complete` resolver DSL | yes |
| Snapshot codecs: plan + hedgehogs `DagSnapshot` + node results, versioned | yes |
| `SnapshotStore` compare-and-swap: `create`/`load`/`replace(expected)`/`delete(expected)` | yes, in memory |
| Resume claims the run with a CAS *before* executing (exactly one writer per run) | yes |
| Schema fingerprint (from the merged model, not source text) stored and checked on resume | yes |
| Surives a restart: fresh store + fresh `IORuntime` from the encoded snapshot | yes, tested |
| Postgres `SnapshotStore` with skunk (`UPDATE ... WHERE revision = ?`) | yes, compiled but not integration-tested here (no Postgres in this environment); DDL in `torpor-snapshot.sql` |
| `RunResult.Failed` backstop in hedgehogs | not needed: `NodeRuntime.run` and `Durable.attemptResume` catch synchronous throws and raised errors at the node boundary and convert them to field errors, so hedgehogs' fail-fast segment is never reached. Kept on the hedgehogs backlog as defence against defects in its own bookkeeping. |
| Keyed (batch) fields that suspend | no: only root fields may suspend |
| Principal/claims in the snapshot, TTL'd resume tokens, auth at resume | no (M2) |
| Incremental-delivery wire format for suspension | no (M2) |

## M2 — incremental delivery (in progress)

| Feature | State |
|---|---|
| `torpor-explain`: mermaid plan + per-level call report | yes |
| Incremental-delivery wire format (`hasNext`/`pending`/`incremental`) for suspended runs | no |
| SSE endpoint, resume-over-reconnect by token | no |
| `@defer` in queries | no |

## Known v1 limitations

- An input object or argument of a custom scalar decodes with the `Decoder` of the Scala type its `Scalar` binding names; an `ID` argument is a `String` by default, or whatever `Scalar["ID", …]` says, so an integer literal and a string literal need a matching decoder.
- A union or interface member's `__typename` comes from the value the resolver produced; harnesses that do not add it must use `resolver.tagged` or return JSON with `__typename` set.

- `.suspending` resolvers run under a service identity; snapshots carry the principal and claims,
  never caller credentials. A resolver that needs live caller credentials cannot suspend.
- A deploy invalidates in-flight suspended runs (schema hash mismatch). No snapshot migration.
- Suspension granularity is a plan node, not an entity: `Pet.owner` suspends for the whole group.
