-- update sequences
BEGIN;

LOCK TABLE content IN EXCLUSIVE MODE;

SELECT
  setval (
    pg_get_serial_sequence ('content', 'id'),
    COALESCE(MAX(id), 0) + 1,
    false
  )
FROM
  content;

SELECT
  setval (
    pg_get_serial_sequence ('image', 'content_id'),
    COALESCE(MAX(content_id), 0) + 1,
    false
  )
FROM
  image;

SELECT
  setval (
    pg_get_serial_sequence ('flickr', 'content_id'),
    COALESCE(MAX(content_id), 0) + 1,
    false
  )
FROM
  flickr;

COMMIT;
