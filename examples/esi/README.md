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
extend type Query { character(id: ID, name: String): Character }
extend type Character { corporation: Corporation! }
extend type Corporation { alliance: Alliance }
```

Each source's SDL is a `schema"""…"""` literal, so it is parsed and validated when this file compiles; the resolvers
take their coordinates from the same value (`characters.coordinate("Query", "character")`), and the text goes straight
into `Source(...)`. Merged at startup, the pair is also what a golden test would compare.

Two sources: `characters` owns `Character` and the root lookup; `universe` extends it with `corporation`, and
`Corporation` with `alliance`. Hydration is optional because an extension field only becomes a plan node when
the client selects it — `{ character(id: 1) { name } }` never calls the corporations endpoint.

`corporationId` and `allianceId` are carrier fields: they travel in the JSON between nodes and never appear in
the SDL. A corporation with no alliance returns `null`, not an error.

## ESI calls

| Field | ESI |
|---|---|
| `character(id:)` | `GET /latest/characters/{id}/` |
| `character(name:)` | `POST /latest/universe/ids/`, then the same characters call |
| `characters(ids:)` | one `characters` call for the whole list |
| `Character.corporation` | `GET /latest/corporations/{id}/`, one call per level: 3 characters, 2 distinct corporations, 1 call |
| `Corporation.alliance` | `GET /latest/alliances/{id}/`, batched, only when selected; a corporation with no alliance is `null` |

`corporation: Corporation!` and `alliance: Alliance` are extension fields, so they are plan nodes only when
selected. The batch API makes the fan-out unavoidable: `characters` cannot cost one corporation call per row.
`EsiApi` is a trait so tests use a fake: the suite asserts call counts, including that an unselected
corporation costs zero requests and that 3 characters with 2 distinct corporations cost one corporations call.
