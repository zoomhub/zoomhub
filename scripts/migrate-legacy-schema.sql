-- Import Import `ContentInfoGroup*` tables:
BEGIN TRANSACTION;
INSERT INTO content (/* id
  , */hashId
  , typeId
  , url
  , state
  , initializedAt
  -- , activeAt
  -- , completedAt
  , title
  , attributionText
  , attributionLink
  , mime
  , size
  , error
  , progress
  , abuseLevelId
  , numAbuseReports
  , version
  )
    SELECT
    /* NULL -- id
    , */Id
    , Type
    , Url
    , 'initialized'
    , Timestamp
    -- , NULL -- activeAt
    -- , NULL -- completedAt
    , Title
    , AttributionText
    , AttributionLink
    , Mime
    , Size
    , Error
    , Progress
    , AbuseLevel
    , NumAbuseReports
    , Version
    FROM ContentInfoGroup1

    UNION

    SELECT
      /* NULL -- id
      , */Id
      , Type
      , Url
      , 'initialized'
      , Timestamp
      -- , NULL -- activeAt
      -- , NULL -- completedAt
      , Title
      , AttributionText
      , AttributionLink
      , Mime
      , Size
      , Error
      , Progress
      , AbuseLevel
      , NumAbuseReports
      , Version
      FROM ContentInfoGroup2

  ORDER BY Timestamp ASC;
END TRANSACTION;

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

-- Import `FlickrPhotoInfo` table:
BEGIN TRANSACTION;
INSERT INTO flickr (
    contentId
  , farmId
  , serverId
  , photoId
  , secret
  , sizeId
  , isPublic
  , licenseId
  , originalExtension
  , originalSecret
  , ownerNSID
  , ownerRealName
  , ownerUsername
  , photoPageURL
  )
  SELECT
    content.id
  , FlickrPhotoInfo.FarmId
  , FlickrPhotoInfo.ServerId
  , FlickrPhotoInfo.PhotoId
  , FlickrPhotoInfo.Secret
  , FlickrPhotoInfo.Size
  , FlickrPhotoInfo.IsPublic
  , FlickrPhotoInfo.License
  , FlickrPhotoInfo.OriginalExtension
  , FlickrPhotoInfo.OriginalSecret
  , FlickrPhotoInfo.OwnerNsid
  , FlickrPhotoInfo.OwnerRealName
  , FlickrPhotoInfo.OwnerUserName
  , FlickrPhotoInfo.PhotoPageUrl
  FROM content JOIN FlickrPhotoInfo ON content.hashId=FlickrPhotoInfo.Id;
END TRANSACTION;

-- Replace empty strings with NULL:
BEGIN TRANSACTION;
UPDATE content SET activeAt = NULL WHERE activeAt = '';
UPDATE content SET attributionLink = NULL WHERE attributionLink = '';
UPDATE content SET attributionText = NULL WHERE attributionText = '';
UPDATE content SET completedAt = NULL WHERE completedAt = '';
UPDATE content SET error = NULL WHERE error = '';
UPDATE content SET mime = NULL WHERE mime = '';
UPDATE content SET size = NULL WHERE size = '';
UPDATE content SET title = NULL WHERE title = '';

UPDATE flickr SET originalExtension = NULL WHERE originalExtension = '';
UPDATE flickr SET originalSecret = NULL WHERE originalSecret = '';
UPDATE flickr SET ownerRealName = NULL WHERE ownerRealName = '';
UPDATE flickr SET photoPageURL = NULL WHERE photoPageURL = '';
END TRANSACTION;

-- Normalize booleans:
BEGIN TRANSACTION;
UPDATE flickr SET isPublic = 1 WHERE isPublic = 'true';
UPDATE flickr SET isPublic = 0 WHERE isPublic = 'false';
END TRANSACTION;

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

-- Drop legacy tables:
BEGIN TRANSACTION;
DROP TABLE ContentInfoGroup1;
DROP TABLE ContentInfoGroup2;
DROP TABLE FlickrPhotoInfo;
DROP TABLE ImageInfo;
END TRANSACTION;

-- Compact database:
VACUUM;
