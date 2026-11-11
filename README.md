# Torpor

GraphQL for Scala that keeps the **contract as data** and the **code as code**.

You write the schema as SDL, check it when the code compiles, and register resolvers against field coordinates.
Fan-out takes a batch of keys, so one request costs one call per level instead of one per row. When a backend
can't answer in time, the run suspends to a table and resumes when the work finishes — days later if it has to.

Built on cats-effect and [hedgehogs](https://github.com/andimiller/hedgehogs).

```scala
import cats.data.NonEmptyList
import cats.effect.IO
import com.comcast.ip4s.*
import net.andimiller.torpor.examples.esi.client.*
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.source.*
import org.http4s.ember.server.EmberServerBuilder

// 1. Write your types and interfaces like you usually would. `EsiApi` is the client from examples/shared-esi. Your
//    domain types know nothing about GraphQL: no effect fields, no schema annotations.

val esi: EsiApi[IO] = new EsiApi[IO] {
  def characterIdByName(name: String): IO[Option[Long]]                    = IO.pure(None)
  def characters(ids: NonEmptyList[Long]): IO[Map[Long, EsiCharacter]]     = IO.pure(Map.empty)
  def corporations(ids: NonEmptyList[Long]): IO[Map[Long, EsiCorporation]] = IO.pure(Map.empty)
  def alliances(ids: NonEmptyList[Long]): IO[Map[Long, EsiAlliance]]       = IO.pure(Map.empty)
  // unused here; the route example in examples/esi serves these
  def route(origin: Long, destination: Long, flag: Option[String]): IO[Option[List[Long]]]                        = IO.pure(None)
  def systems(ids: NonEmptyList[Long]): IO[Map[Long, EsiSolarSystem]]                                             = IO.pure(Map.empty)
  def constellations(ids: NonEmptyList[Long]): IO[Map[Long, EsiConstellation]]                                    = IO.pure(Map.empty)
  def names(ids: NonEmptyList[Long]): IO[Map[Long, String]]                                                       = IO.pure(Map.empty)
}

// 2. Say which Scala type a GraphQL scalar's arguments decode to. This lives in the wiring, not on the domain types:
//    ESI ids are numbers, so `ID` arguments decode as `Long`.
given Scalar["ID", Long]()

// 3. Define your GraphQL schemas, as SDL, per source. Both are parsed and validated when this compiles, and each
//    gives you the text to merge plus its field coordinates as members: `universe.Character.corporation` compiles
//    only if that SDL declares the field, and `characters.Query.character` carries that field's arguments' types.
val characters = schema"""type Character { id: ID! name: String! }
  |extend type Query { character(id: ID, name: String): Character }"""

val universe = schema"""type Corporation { id: ID! name: String! }
  |extend type Character { corporation: Corporation! }"""

// 4. Wire up resolution: a root field's `args` are typed from its SDL arguments (`args.id: Option[Long]`), and a
//    hydrated field takes a batch of keys. Hydration only runs when the client selects it, and then once for the
//    whole set.
val charactersSource = Source[IO]("characters", characters)(
  resolver.root[IO](characters.Query.character) { args =>
    args.id match {
      case Some(id) => esi.characters(NonEmptyList.one(id)).map(_.get(id))
      case None     =>
        esi.characterIdByName(args.name.getOrElse("")).flatMap {
          case None     => IO.pure(Option.empty[EsiCharacter])
          case Some(id) => esi.characters(NonEmptyList.one(id)).map(_.get(id))
        }
    }
  }
)

val universeSource = Source[IO]("universe", universe)(
  resolver.field[IO](universe.Character.corporation)
    .key((character: EsiCharacter) => character.corporationId)
    .batch((ids: NonEmptyList[Long]) => esi.corporations(ids))
)

// 5. Assemble. Each source's type records which fields its SDL needs resolved and what its resolvers produce and
//    decode, so this checks, when it compiles, that every source resolves exactly the fields it must (here
//    `Query.character` and `Character.corporation`), and that the sources link: `Character.corporation` reads
//    `corporationId` from each character, and `characters` produces characters as `EsiCharacter`, which has it.
//    `startup` merges the fragments, repeats the coverage check against the merged schema, and raises one exception
//    listing every problem. Running it is what makes the schema servable.
val started: IO[(Torpor[IO], net.andimiller.torpor.schema.Schema)] = Torpor[IO](charactersSource, universeSource).startup

// 6. Mount into a server: one http4s route, the same one for every schema.
val server = started.flatMap { case (torpor, schema) =>
  EmberServerBuilder
    .default[IO]
    .withHost(host"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(TorporHttp.routes(torpor, schema).orNotFound)
    .build
    .useForever
}
```

Every line above is compiled by mdoc against the real modules. The runnable version — with the ESI client in
`examples/shared-esi`, real arguments and a live HTTP server — is [`examples/esi`](examples/esi).

## Why

**The contract is data; the code is code.** The SDL is a text artifact — reviewable, diffable, sendable to a
client team, checked by `schema"""…"""` at compile time. Resolvers are functions in a table, and your domain
types never learn that GraphQL exists. In code-first libraries the schema *is* the resolver graph, so a data
type grows an effect field every time a value becomes lazy, and the effect type quietly decides the
contract's nullability.

**Batching is impossible to get wrong.** A resolver that fans out takes `NonEmptyList[K] => F[Map[K, V]]`.
There is no single-key overload, so three characters cost one corporations call, not three — the plan groups
each level and dedupes keys. Against ESI, character rows are two batch requests for any number of ids
(`characters/affiliation/` plus `universe/names/`), and a route's regions are one; the test suite pins the
call counts.

**A run can outlive the request.** `Step.suspend(handle)` parks the whole run: the snapshot (plan, node
states, handles) goes to a `SnapshotStore` with compare-and-swap on a revision, so two webhooks racing to
resume cannot both execute. Resume from a fresh process with a fresh runtime; nothing but the stored JSON is
needed. No workflow engine involved.

**One contract, one plan.** `Plan` is plain serialisable data — the same value the executor runs, the snapshot
stores, and `Explain.mermaid` draws. What you inspect is what executes, before it executes: the example serves
`POST /explain`, which prints the plan for a query without running a resolver.

## Status

Works today:

| | |
|---|---|
| SDL | per-source fragments, `extend` across sources, merge with typed errors carrying `source:line:column` |
| Compile-time checks | `schema"""…"""` (text + coordinates, validated; an undeclared coordinate does not compile; root resolver arguments typed from the SDL) and `sdl"""…"""` (text only); assembling sources checks that each source resolves exactly the fields its SDL requires, and that every keyed field's parent type has the fields it reads |
| Validation | a documented subset of the spec's rules; unsupported constructs fail loudly |
| Planner | query → DAG of batch calls, normalised selection sets, variables substituted to literals |
| Executor | one call per node per level, errors as values with response paths, alias-keyed assembly |
| Limits | `maxDepth`, `maxFields`, coverage check (at compile time, and again at startup) |
| Durable runs | suspend/resume, `SnapshotStore` (in-memory, and Postgres via skunk) with `claim` |
| HTTP | `POST /graphql`, GraphQL errors as 200s |
| Tools | introspection (`__schema`, `__type`), `Explain.mermaid` / `Explain.report` |

Not built yet — do not assume these work:

- `@defer`/`@stream`, SSE, a resume endpoint (M2). A suspended run is currently usable programmatically only.
- Null bubbling, full value coercion, subtype/interface planning (`__typename` is rejected in normal queries).
- Subscriptions. Keyed (batch) fields cannot suspend. Node timeouts, cost metrics.
- Postgres store integration tests (it compiles; the CAS contract is proven against the in-memory store).

## What it is not

- **Not a federation gateway.** Sources are in-process modules, not remote GraphQL services. Bring your own
  clients.
- **Not a SQL engine.** If your data is relational and query shape is everything, use
  [Grackle](https://github.com/typelevel/grackle).
- **Not ZIO.** `Torpor[F]` for `F[_]: Concurrent`.
- **Not a client or a codegen.** [Caliban](https://github.com/ghostdogpr/caliban) covers that ground.
- **Not done.** See [docs/roadmap.md](docs/roadmap.md).

## Modules

| Module | Owns |
|---|---|
| `torpor-core` | plan data, `Resolver` SPI, executor, response assembly |
| `torpor-schema` | SDL merge, validation subset, executable documents, introspection |
| `torpor-macros` | `schema"""…"""`, `sdl"""…"""` |
| `torpor-source` | planner, resolver DSL, `Torpor` facade, `Durable` |
| `torpor-snapshot` | snapshot codecs, `SnapshotStore` (in-memory + skunk) |
| `torpor-explain` | mermaid plan, per-level call report |
| `torpor-http` | http4s routes |
| `torpor-testkit` | golden test for the merged contract: sources in, one `.graphqls` out, a diff if it drifts |

Only `torpor-schema` may import `sangria.*` (the parser); `scripts/check-imports.sh` enforces it.

## Example

[`examples/esi`](examples/esi) is a working service over EVE Online's ESI: unauthenticated, custom User-Agent,
lookup by id or name, optionally hydrating corporation and alliance, plus a route between two solar systems
that hydrates systems, constellations and regions — the multi-level fan-out — and a `/explain` route for the
plan behind any query.
[`examples/esi-caliban`](examples/esi-caliban) is the same character and corporation service in Caliban, for
comparison — including the call counts that show why the batch signature matters.

## Build

```
sbt test
./scripts/check-imports.sh
```

Scala 3.3 LTS, JVM.
