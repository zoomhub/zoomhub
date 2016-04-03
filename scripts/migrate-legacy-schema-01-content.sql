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
