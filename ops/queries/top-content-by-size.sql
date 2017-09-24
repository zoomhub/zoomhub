SELECT
  ("http://zoomhub.net/" || content.hashId) AS URL,
  content.numViews AS Views,
  ROUND(image.width * image.height / 1e6, 1) AS Megapixels
FROM
  content, image
WHERE
  content.id = image.contentid AND
  content.state = 'completed:success' AND
  content.numViews >= 200
ORDER BY (image.width * image.height) DESC
LIMIT 100;
