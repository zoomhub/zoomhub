-- Fill in missing `mime` values which are incorrectly stored in `error` column:
BEGIN TRANSACTION;
UPDATE
  content
SET
  mime = error,
  error = NULL
WHERE
  (
    error LIKE 'application/%'
    OR error LIKE 'audio/%'
    OR error LIKE 'binary/%'
    OR error LIKE 'image/%'
    OR error LIKE 'multipart/%'
    OR error LIKE 'octet/%'
    OR error LIKE 'text/%'
    OR error LIKE 'video/%'
  )
  AND mime IS NULL;
END TRANSACTION;
