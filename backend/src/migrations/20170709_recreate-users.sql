DROP TABLE IF EXISTS users;

CREATE TABLE users(
  id bigserial PRIMARY KEY NOT NULL,
  name text NOT NULL,
  email text UNIQUE NOT NULL,
  -- hydra accounts
  hydra_password VARCHAR(60),
  -- github accounts
  github_id text UNIQUE,
  github_token bytea UNIQUE
);
