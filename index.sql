-- find an intermediate fraction between p1/q1 and p2/q2.
--
-- The fraction chosen is the highest fraction in the Stern-Brocot
-- tree which falls strictly between the specified values. This is
-- intended to avoid going deeper in the tree unnecessarily when the
-- list is already sparse due to deletion or moving of items, but in
-- fact the case when the two items are already adjacent in the tree
-- is common so we shortcut it. As a bonus, this method always
-- generates fractions in lowest terms, so there is no need for GCD
-- calculations anywhere.
--
-- Inputs must not be null (caller's responsibility), but the value
-- p2=1 q2=0 is allowed for the upper bound without causing any
-- division by zero errors.
 
CREATE OR REPLACE FUNCTION find_intermediate(p1 INTEGER, q1 INTEGER,
                                             p2 INTEGER, q2 INTEGER,
                                             OUT p INTEGER, OUT q INTEGER)
  LANGUAGE plpgsql immutable strict
AS $f$
  DECLARE
    pl INTEGER := 0;
    ql INTEGER := 1;
    ph INTEGER := 1;
    qh INTEGER := 0;
  BEGIN
    IF (p1::BIGINT*q2 + 1) <> (p2::BIGINT*q1) THEN
      loop
        p := pl + ph;
        q := ql + qh;
        IF (p::BIGINT*q1 <= q::BIGINT*p1) THEN
          pl := p; ql := q;
        elsif (p2::BIGINT*q <= q2::BIGINT*p) THEN
          ph := p; qh := q;
        ELSE
          exit;
        END IF;
      END loop;
    ELSE
      p := p1 + p2;
      q := q1 + q2;
    END IF;
  END;
$f$;
 


-- assumes the table have the INT columns p and q, with an exclusion contraint (see test)

-- insert or move item ID next to REL_ID,
-- before it if IS_BEFORE is true, otherwise after. REL_ID may
-- be null to indicate a position off the end of the list.
 
CREATE OR REPLACE FUNCTION place_item(_tbl regclass,
                                          id INTEGER,
                                          rel_id INTEGER,
                                          is_before BOOLEAN)
  RETURNS void
  LANGUAGE plpgsql
  volatile called ON NULL INPUT
AS $f$
  DECLARE
    p1 INTEGER; q1 INTEGER;   -- fraction below insert position
    p2 INTEGER; q2 INTEGER;   -- fraction above insert position
    r_rel DOUBLE PRECISION;   -- p/q of the rel_id row
    np INTEGER; nq INTEGER;   -- new insert position fraction
  BEGIN
 
    -- moving a record to its own position is a no-op
    IF rel_id=id THEN RETURN; END IF;
 
    -- if we're positioning next to a specified row, it must exist
    IF rel_id IS NOT NULL THEN
      SELECT t.p, t.q INTO strict p1, q1
        FROM _tbl t
       WHERE t.id=rel_id;
      r_rel := p1::float8 / q1;
    END IF;
 
    -- find the next adjacent row in the desired direction
    -- (might not exist).
    IF is_before THEN
      p2 := p1; q2 := q1;
      SELECT t2.p, t2.q INTO p1, q1
        FROM _tbl t2
       WHERE t2.id <> id
         AND (p::float8/q) < COALESCE(r_rel,'infinity')
       ORDER BY (p::float8/q) DESC LIMIT 1;
    ELSE
      SELECT t2.p, t2.q INTO p2, q2
        FROM _tbl t2
       WHERE t2.id <> id
         AND (p::float8/q) > COALESCE(r_rel,0)
       ORDER BY (p::float8/q) ASC LIMIT 1;
    END IF;
 
    -- compute insert fraction
    SELECT * INTO np,nq FROM find_intermediate(COALESCE(p1,0),COALESCE(q1,1),
                                               COALESCE(p2,1),COALESCE(q2,0));
 
    -- move or insert the specified row
    UPDATE _tbl t
       SET (p,q) = (np,nq) WHERE t.id=id;
    IF NOT found THEN
      INSERT INTO t VALUES (id, np, nq);
    END IF;
 
    -- want to renormalize both to avoid possibility of integer overflow
    -- and to ensure that distinct fraction values map to distinct float8
    -- values. Bounding to 10 million gives us reasonable headroom while
    -- not requiring frequent normalization.
 
    IF (np > 10000000) OR (nq > 10000000) THEN
      perform renormalize(_tbl, id);
    END IF;
  END;
$f$;
 


-- the purpose of the update is as follows: we want to assign
-- a new series of values 1/2, 3/2, 5/2, ... to the existing rows,
-- maintaining the existing order
 
CREATE OR REPLACE FUNCTION renormalize(_tbl regclass, id INTEGER)
  RETURNS void
  LANGUAGE plpgsql
  volatile strict
AS $f$
  BEGIN
    UPDATE _tbl t SET p = s2.new_rnum, q = 2 
      FROM (SELECT row_number() OVER (ORDER BY p::float8/q) AS new_rnum 
              FROM _tbl t2 WHERE t2.id=id
          ) s2
      WHERE s2.is_new
       AND t.id=id;
  END;
$f$;
