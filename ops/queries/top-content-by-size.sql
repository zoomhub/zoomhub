SELECT
  content.hashId, content.numViews, image.width, image.height
FROM
  content, image
WHERE
  content.id = image.contentid AND
  content.state = 'completed:success'
ORDER BY (image.width * image.height) DESC
LIMIT 10;
