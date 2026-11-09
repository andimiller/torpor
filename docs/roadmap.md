# Roadmap and the suspension contract

The README says what works. This file says what is next and what the durable path commits to.

## Milestones

| Milestone | Scope | Exit criterion |
|---|---|---|
| M0 — done | multi-source merge, planner, executor, limits, coverage check, `POST /graphql` | pet shop `pets` + `owners` in 2 calls; validation subset green; an unserved SDL field fails startup |
| M1 — done, except below | durable runs: `Step.suspend`, snapshot codecs, CAS `SnapshotStore`, `claim`, schema fingerprint | suspend → restart → resume to completion; a raced resume loses cleanly |
| M2 — in progress | incremental delivery: `@defer` over SSE, resume-over-reconnect on the same wire format | a client reconnects by token and receives exactly the missing payloads |
| M3 | cost report, per-node metrics, per-node timeouts and retries | `explain` reports call counts per level and flags nodes that can suspend |

Done from M2: `Explain` (mermaid + per-level report) and introspection.
Outstanding from M1: principal and TTL on snapshots; Postgres store integration tests.

## Suspension contract

These are commitments, not implementation details.

- **Node outputs are total.** A resolver failure becomes a field error value at the boundary; nothing raises
  into the scheduler. A raised error would cancel in-flight fibers and orphan a `Waiting` handle — a submitted
  warehouse job with no record — so the boundary catches synchronous throws too.
- **Granularity is a plan node.** `Pet.owner` cannot suspend for one pet of ten; the group completes,
  suspends or errors together. Keyed fields cannot suspend yet; only root fields can.
- **Exactly one writer per run.** `SnapshotStore.claim` is an atomic take: the second webhook gets `Conflict`
  and does nothing. Compare-and-swap on a revision the caller read is not enough — a caller that loads after
  the claim would adopt it as its own baseline.
- **Schema drift is refused.** Every snapshot stores a fingerprint of the merged schema, computed from the
  model rather than source text. `resume` refuses a mismatch with a typed error; a deploy invalidates
  in-flight runs. Snapshot migration is explicitly not v1.
- **No cancellation from outside.** An outer timeout loses the snapshot. Node timeouts, when they land, are
  implemented inside the node and convert to an error value.

## Open questions

- Resume tokens: opaque, TTL'd, bound to the principal. Not implemented; today a run id is the token and
  anyone who has it can resume.
- Auth at resume: snapshots should store the principal's subject and claims, never credentials.
- Incremental delivery needs assembly to *omit* pending subtrees rather than null them, which is the first
  real decision in M2.
