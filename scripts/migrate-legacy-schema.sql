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
INSERT INTO image (
    contentId
  , farmId
  , serverId
  , photoId
  , secret
  , sizeType
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
