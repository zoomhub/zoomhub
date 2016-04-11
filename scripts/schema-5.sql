PRAGMA user_version = 5;

CREATE TABLE content (
  id integer PRIMARY KEY AUTOINCREMENT,
  hashId text UNIQUE NOT NULL,
  typeId integer NOT NULL,
  url text UNIQUE NOT NULL,
  state text NOT NULL,
  initializedAt datetime NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  activeAt datetime,
  completedAt datetime,
  title text,
  attributionText text,
  attributionLink text,
  mime text,
  size integer,
  error text,
  progress real NOT NULL DEFAULT(0.0),
  abuseLevelId integer NOT NULL DEFAULT(0),
  numAbuseReports integer NOT NULL DEFAULT(0),
  version integer NOT NULL DEFAULT(5) -- legacy `Version` column
);

CREATE TABLE image (
  contentId integer PRIMARY KEY,
  initializedAt datetime NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  width integer NOT NULL,
  height integer NOT NULL,
  tileSize integer NOT NULL,
  tileOverlap integer NOT NULL,
  tileFormat text NOT NULL,
  FOREIGN KEY(contentId) REFERENCES content(id)
);

CREATE TABLE flickr (
  contentId integer PRIMARY KEY,
  farmId integer NOT NULL,
  serverId integer NOT NULL,
  photoId text NOT NULL,
  secret text NOT NULL,
  sizeId integer NOT NULL,
  isPublic integer NOT NULL,
  licenseId integer NOT NULL,
  originalExtension text,
  originalSecret text,
  ownerNSID text NOT NULL,
  ownerRealName text,
  ownerUsername text NOT NULL,
  photoPageURL text,
  FOREIGN KEY(contentId) REFERENCES content(id)
);

CREATE INDEX content_activeAt_index ON content (activeAt);
CREATE INDEX content_completedAt_index ON content (completedAt);
CREATE INDEX content_hashId_index ON content (hashId);
CREATE INDEX content_initializedAt_index ON content (initializedAt);
CREATE INDEX content_state_index ON content (state);
CREATE INDEX content_url_index ON content (url);
CREATE UNIQUE INDEX content_hashId_index_unique ON content (hashId);
CREATE UNIQUE INDEX content_url_index_unique ON content (url);
