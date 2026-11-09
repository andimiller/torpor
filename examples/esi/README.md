# Torpor example: EVE Online ESI

Unauthenticated ESI, with the descriptive `User-Agent` ESI requires. Nothing here needs a token.

```
sbt "esi/run"
curl -s localhost:8080/graphql -H 'content-type: application/json' -d '{"query":"{ character(name: \"Rex\") { name corporation { name ticker alliance { name } } } }"}'
```

## Schema

```graphql
type Character { id: ID! name: String! }
type Corporation { id: ID! name: String! ticker: String! }
type Alliance { id: ID! name: String! ticker: String! }
type SolarSystem { id: ID! name: String! }
type Constellation { id: ID! name: String! }
type Region { id: ID! name: String! }
extend type Query {
  character(id: ID, name: String): Character
  characters(ids: [ID!]!): [Character!]!
  route(origin: ID!, destination: ID!, flag: String): [SolarSystem!]
}
extend type Character { corporation: Corporation! }
extend type Corporation { alliance: Alliance }
extend type SolarSystem { constellation: Constellation! }
extend type Constellation { region: Region! }
```

Each source's SDL is a `schema"""…"""` literal, so it is parsed and validated when this file compiles; the resolvers
take their coordinates from the same value (`characters.coordinate("Query", "character")`), and the text goes straight
into `Source(...)`. Merged at startup, the whole schema is also what a golden test would compare.

Three sources: `characters` owns `Character` and the root lookup; `routes` owns the route lookup and the two types it
returns; `universe` owns everything that hydrates them — `corporation`, `alliance`, `SolarSystem.constellation`,
`Constellation.region`. A field declared in a type definition is read from the value its parent node produced; a field
declared by `extend type` in another source needs its own node, so `extend` is what makes hydration lazy. `{ character(id:
1) { name } }` never calls the corporations endpoint.

`corporationId`, `allianceId`, `constellationId` and `regionId` are carrier fields: they travel in the JSON between
nodes and never appear in the SDL. A corporation with no alliance returns `null`, not an error, and so does a route ESI
does not know.

## ESI calls

| Field | ESI |
|---|---|
| `character(id:)` | `POST /latest/characters/affiliation/` + `POST /latest/universe/names/` |
| `character(name:)` | `POST /latest/universe/ids/`, then the same two calls |
| `characters(ids:)` | the same two calls for the whole list, whatever its length |
| `Character.corporation` | `GET /latest/corporations/{id}/`, one call per distinct corporation: 3 characters, 2 corporations, 2 requests |
| `Corporation.alliance` | `GET /latest/alliances/{id}/`, batched, only when selected |
| `route(origin:, destination:)` | `GET /latest/route/{origin}/{destination}/?flag=`, one call, then one `GET /latest/universe/systems/{id}/` per system on the route |
| `SolarSystem.constellation` | `GET /latest/universe/constellations/{id}/`, one request per distinct constellation: 12 systems, 6 constellations, 6 requests |
| `Constellation.region` | `POST /latest/universe/names/`, a real batch: every region on the route in 1 request |

`corporation`, `alliance`, `constellation` and `region` are extension fields, so they are plan nodes only when selected.
The batch API makes the fan-out unavoidable: `characters` cannot cost one corporation call per row. ESI has no batch
endpoint for corporations, systems or constellations, so those levels are one call in the plan and one request per key
on the wire; `characters/affiliation/` and `universe/names/` are real batches, and those levels collapse however long
the list is. `EsiApi` is a
trait so tests use a fake: the suite asserts call counts, including that an unselected corporation costs zero requests,
that 3 characters with 2 distinct corporations cost one corporations call, and that a route's 2 regions cost one names
call.

## Explain

`POST /explain` takes the same body as `/graphql` and prints the plan instead of running it — the mermaid graph and the
per-level call report:

```
curl -s localhost:8080/explain -H 'content-type: application/json' \
  -d '{"query":"{ route(origin: 30000142, destination: 30002187) { constellation { region { name } } } }"}'
```

```
flowchart LR
  n0["routes: Query.route"]
  n1["universe: SolarSystem.constellation"]
  n2["universe: Constellation.region"]
  n0 --> n1
  n1 --> n2

depth 4, 4 fields, 3 calls over 3 level(s)
level 1: routes x1 (Query.route)
level 2: universe x1 (SolarSystem.constellation)
level 3: universe x1 (Constellation.region)
```

One call per level, before any of them happen. Variables are not substituted on this route: it takes literal queries.
