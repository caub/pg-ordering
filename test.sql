CREATE TABLE categories (id INTEGER PRIMARY KEY, name text);
CREATE TABLE things (id INTEGER PRIMARY KEY, name text);
 
CREATE TABLE category_member (
  category_id INTEGER NOT NULL REFERENCES categories,
  thing_id INTEGER NOT NULL REFERENCES things ON DELETE cascade,
  PRIMARY KEY (category_id,thing_id),
  p INTEGER NOT NULL, q INTEGER NOT NULL,

  EXCLUDE USING btree ((p::float8/q) with =) DEFERRABLE;
);
 
-- CREATE UNIQUE INDEX ON category_member (category_id, (p::float8/q));


