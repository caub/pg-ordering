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
 
-- insert or move item ACT_ID in category CAT_ID next to REL_ID,
-- before it if IS_BEFORE is true, otherwise after. REL_ID may
-- be null to indicate a position off the end of the list.
 
CREATE OR REPLACE FUNCTION cat_place_item(cat_id INTEGER,
                                          act_id INTEGER,
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
    IF rel_id=act_id THEN RETURN; END IF;
 
    -- if we're positioning next to a specified row, it must exist
    IF rel_id IS NOT NULL THEN
      SELECT cm.p, cm.q INTO strict p1, q1
        FROM category_member cm
       WHERE cm.category_id=cat_id AND cm.thing_id=rel_id;
      r_rel := p1::float8 / q1;
    END IF;
 
    -- find the next adjacent row in the desired direction
    -- (might not exist).
    IF is_before THEN
      p2 := p1; q2 := q1;
      SELECT cm2.p, cm2.q INTO p1, q1
        FROM category_member cm2
       WHERE cm2.category_id=cat_id AND cm2.thing_id <> act_id
         AND (p::float8/q) < COALESCE(r_rel,'infinity')
       ORDER BY (p::float8/q) DESC LIMIT 1;
    ELSE
      SELECT cm2.p, cm2.q INTO p2, q2
        FROM category_member cm2
       WHERE cm2.category_id=cat_id AND cm2.thing_id <> act_id
         AND (p::float8/q) > COALESCE(r_rel,0)
       ORDER BY (p::float8/q) ASC LIMIT 1;
    END IF;
 
    -- compute insert fraction
    SELECT * INTO np,nq FROM find_intermediate(COALESCE(p1,0),COALESCE(q1,1),
                                               COALESCE(p2,1),COALESCE(q2,0));
 
    -- move or insert the specified row
    UPDATE category_member
       SET (p,q) = (np,nq) WHERE category_id=cat_id AND thing_id=act_id;
    IF NOT found THEN
      INSERT INTO category_member VALUES (cat_id, act_id, np, nq);
    END IF;
 
    -- want to renormalize both to avoid possibility of integer overflow
    -- and to ensure that distinct fraction values map to distinct float8
    -- values. Bounding to 10 million gives us reasonable headroom while
    -- not requiring frequent normalization.
 
    IF (np > 10000000) OR (nq > 10000000) THEN
      perform cat_renormalize(cat_id);
    END IF;
  END;
$f$;
 
-- Renormalize the fractions of items in CAT_ID, preserving the
-- existing order. The new fractions are not strictly optimal, but
-- doing better would require much more complex calculations.
--
-- the purpose of the complex update is as follows: we want to assign
-- a new series of values 1/2, 3/2, 5/2, ... to the existing rows,
-- maintaining the existing order, but because the unique expression
-- index is not deferrable, we want to avoid assigning any new value
-- that collides with an existing one.
--
-- We do this by calculating, for each existing row with an x/2 value,
-- which position in the new sequence it would appear at. This is done
-- by adjusting the value of p downwards according to the number of
-- earlier values in sequence. To see why, consider:
--
--   existing values:    3, 9,13,15,23
--   new simple values:  1, 3, 5, 7, 9,11,13,15,17,19,21
--                          *     *  *        *
--   adjusted values:    1, 5, 7,11,17,19,21,25,27,29,31
--
--   points of adjustment: 3, 7 (9-2), 9 (13-4, 15-6), 15 (23-8)
--
-- The * mark the places where an adjustment has to be applied.
--
-- Having calculated the adjustment points, the adjusted value is
-- simply the simple value adjusted upwards according to the number of
-- points passed (counting multiplicity).
 
CREATE OR REPLACE FUNCTION cat_renormalize(cat_id INTEGER)
  RETURNS void
  LANGUAGE plpgsql
  volatile strict
AS $f$
  BEGIN
    UPDATE category_member cm SET p = s2.new_rnum, q = 2 
      FROM (SELECT thing_id, row_number() OVER (ORDER BY p::float8/q) AS new_rnum 
              FROM category_member cm2 WHERE cm2.category_id=cat_id
          ) s2
      WHERE s2.thing_id=cm.thing_id
       AND s2.is_new
       AND cm.category_id=cat_id;
  END;
$f$;
