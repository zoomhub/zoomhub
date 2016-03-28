CREATE TABLE IF NOT EXISTS content (
  id integer PRIMARY KEY AUTOINCREMENT,
  hashId text UNIQUE NOT NULL,
  url text UNIQUE NOT NULL,
  state text NOT NULL,
  initializedAt datetime DEFAULT(CURRENT_TIMESTAMP),
  activeAt datetime,
  completedAt datetime,
  mime text,
  size integer,
  progress real NOT NULL DEFAULT(0.0),
  dzi_width integer,
  dzi_height integer,
  dzi_tileSize integer,
  dzi_tileOverlap integer,
  dzi_tileFormat text
);

CREATE UNIQUE INDEX IF NOT EXISTS hashId_index_unique ON content (hashId);

CREATE UNIQUE INDEX IF NOT EXISTS url_index_unique ON content (url);

CREATE INDEX IF NOT EXISTS state_index ON content (state);

CREATE INDEX IF NOT EXISTS url_index ON content (url);

CREATE INDEX IF NOT EXISTS hashId_index ON content (hashId);
