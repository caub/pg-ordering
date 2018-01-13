
 
CREATE TABLE todo_list (
  id BIGSERIAL PRIMARY KEY,
  text TEXT NOT NULL,

  p INTEGER NOT NULL, q INTEGER NOT NULL,

  EXCLUDE USING btree ((p::float8/q) WITH =) DEFERRABLE
);
 
-- CREATE UNIQUE INDEX ON todo_list (id, (p::float8/q));

INSERT INTO todo_list(text) VALUES ('foo'), ('bar'), ('qux'), ('wat');


SELECT place_item(1, NULL, FALSE); -- insert foo at start
SELECT place_item(2, 1, FALSE); -- insert bar after foo

SELECT * FROM todo_list;

