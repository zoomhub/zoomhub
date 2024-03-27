SELECT
  ('http://zoomhub.net/' || content.hash_id) AS URL,
  content.num_views AS Views,
  ROUND(image.width * image.height / 1e6, 1) AS Megapixels,
  submitter_email as Author
FROM
  content,
  image
WHERE
  content.id = image.content_id
  AND content.state = 'completed:success'
  AND content.num_views >= 200
ORDER BY
  (image.width * image.height) DESC
LIMIT
  100;
