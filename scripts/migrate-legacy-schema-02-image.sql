-- Import `ImageInfo` table:
BEGIN TRANSACTION;
INSERT INTO image (
    contentId
  , initializedAt
  , width
  , height
  , tileSize
  , tileOverlap
  , tileFormat
  )
  SELECT
    content.id
  , ImageInfo.Timestamp
  , ImageInfo.Width
  , ImageInfo.Height
  , ImageInfo.TileSize
  , ImageInfo.TileOverlap
  , ImageInfo.TileFormat
  FROM content JOIN ImageInfo ON content.hashId=ImageInfo.Id;
END TRANSACTION;
