# Same ESI example, twice: torpor vs Caliban

Two implementations of the same thing — unauthenticated EVE Online ESI behind a GraphQL endpoint. The ESI client
(config, wire types, HTTP calls) is shared in `examples/shared-esi`, so the two files differ only in the GraphQL layer:

- `examples/esi/.../Esi.scala` — torpor
- `examples/esi-caliban/.../EsiCaliban.scala` — Caliban

Sharing the client removes the duplication that used to make both files ~100 lines longer, and leaves each file with
the part the comparison is actually about. It also means the two schemas are built over identical wire types.

Both are one file: ESI client, schema and HTTP server in the order they are used.

The only thing not mirrored is torpor's `/explain`: Caliban has no plan to print.

```
sbt "esi/run"         # torpor,  localhost:8080/graphql
sbt "esiCaliban/run"  # Caliban, localhost:8080/graphql — run one at a time
```

Both answer the same request:

```bash
curl -s localhost:8080/graphql -H 'content-type: application/json' \
  -d '{"query":"{ character(id: 443630591) { name corporation { name ticker alliance { name } } } }"}'
# {"data":{"character":{"name":"The Mittani","corporation":{"name":"State War Academy","ticker":"SWA","alliance":null}}}}
```

## Where they differ

| | torpor | Caliban |
|---|---|---|
| schema | SDL strings per source, merged; `checkCoverage` proves every field has a resolver | derived from case classes; a missing type is a compile error |
| unknown field | caught at startup by the coverage check | caught at compile time |
| laziness | a field becomes a plan node only when the client selects it | field is a `UIO`, so it runs only when selected |
| batching | one ESI call per level, however many rows | one call per row: 3 characters means 3 corporation calls |
| where the schema lives | SDL strings; resolvers are looked up by field coordinate | derived from case classes that also carry the resolvers |
| carrier fields | `corporation_id` rides in the inter-node JSON, never in the SDL | `corporationId` is a closure argument, never in the schema |
| effect | cats-effect `IO` | ZIO `Task`; the shared ESI client is effect-polymorphic, so both use it unchanged |
| HTTP | `TorporHttp.routes` | `Http4sAdapter.makeHttpService` over `HttpInterpreter` |
| serving | `Schema`, `Torpor[IO]`, routes | `GraphQL[Any]` → `GraphQLInterpreter` → http4s route |

## What is not identical

The SDL. Caliban maps `Long` to its own `Long` scalar and has no `ID` for numeric ids, so the readable difference is:

```graphql
character(id: Long, name: String): Character   # Caliban
character(id: ID, name: String): Character     # torpor
route(origin: Long!, destination: Long!)       # Caliban
route(origin: ID!, destination: ID!)           # torpor
```

Clients send a bare number either way, so the queries above work against both. `id: Long!` also replaces torpor's
`id: ID!` in the output. Everything else — nullability, `corporation: Corporation!`, `alliance: Alliance`, a nullable
`route` list — matches.

Both expose the same fields (`characters(ids: [Long!]!): [Character!]!` and the route), so the gap is measurable
rather than asserted. Calls are what each GraphQL layer asks the client for; requests are what the client puts on the
wire, which is where a per-row resolver pays twice.

| query | torpor calls / requests | Caliban calls / requests |
|---|---|---|
| `characters(ids: [1001, 1002, 1003]) { name }` | 1 / 2 | 3 / 6 |
| `... { corporation { name } }` | 2 / 4 | 6 / 9 |
| `route(origin: 4000, destination: 5000) { constellation { region { name } } }` (2 systems) | 3 / 6 | 7 / 7 |

Both test suites assert their own numbers (`EsiSuite`, `EsiCalibanSuite`). `characters/affiliation/` and
`universe/names/` are real batches, so a plan collapses those levels and per-row resolution cannot. Corporations,
systems and constellations have no batch endpoint in ESI, so both pay one request per key there — which is why the
route query, the one that walks systems, is where the two counts nearly meet. Planning still helps it: the route is
one node per level, and every level costs what it costs however many systems ESI returns.

## Reading the two files for shape

Caliban: the data types used by the resolvers are the schema. `Corporation` exists only to carry
`alliance: UIO[Option[Alliance]]`, and the ESI client's `EsiCorporation` is a *second* type for the
same row, because the schema type cannot be pure. To answer "is `alliance` nullable?" and "when does
the alliance endpoint get called?" you read the same line.

Torpor: the schema is text, the resolvers are functions in a table, and the ESI wire types stay pure
data. The contract can be read, reviewed and handed to a client team without reading any Scala. The
price is two artifacts that can drift — which is what `checkCoverage` is for, at startup rather than
at compile time.
