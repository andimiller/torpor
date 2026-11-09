CREATE TABLE IF NOT EXISTS torpor_runs (
  id         text primary key,
  revision   bigint      not null,
  payload    text        not null,
  claimed    boolean     not null default false,
  updated_at timestamptz not null default now()
);
