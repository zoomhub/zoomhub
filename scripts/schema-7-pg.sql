-- Migrates SQLite schema imported into PostgreSQL via `pgloader`:

BEGIN;

-- content
-- id
CREATE SEQUENCE content_id_seq;
ALTER SEQUENCE content_id_seq OWNED BY content.id;
SELECT SETVAL('content_id_seq', (SELECT MAX(id) + 1 FROM content), false);

ALTER TABLE content ALTER COLUMN id SET DATA TYPE bigint;
ALTER TABLE content ALTER COLUMN id SET DEFAULT nextval('content_id_seq'::regclass);
ALTER TABLE content ALTER COLUMN id SET NOT NULL;
ALTER TABLE content ADD CONSTRAINT content_id_key UNIQUE (id);

-- hash_id
ALTER TABLE content RENAME COLUMN hashid TO hash_id;
ALTER TABLE content ALTER COLUMN hash_id SET NOT NULL;
ALTER TABLE content ADD CONSTRAINT content_hash_id_key UNIQUE (hash_id);

-- type_id
ALTER TABLE content RENAME COLUMN typeid TO type_id;
ALTER TABLE content ALTER COLUMN type_id SET NOT NULL;

-- url
ALTER TABLE content ALTER COLUMN url SET NOT NULL;
ALTER TABLE content ADD CONSTRAINT content_url_key UNIQUE (url);

-- state
ALTER TABLE content ALTER COLUMN state SET NOT NULL;

-- created_at
ALTER TABLE content RENAME COLUMN initializedat TO initialized_at;
ALTER TABLE content ALTER COLUMN initialized_at SET NOT NULL;

-- active_at
ALTER TABLE content RENAME COLUMN activeat TO active_at;

-- completed_at
ALTER TABLE content RENAME COLUMN completedat TO completed_at;

-- attribution_text
ALTER TABLE content RENAME COLUMN attributiontext TO attribution_text;

-- attribution_link
ALTER TABLE content RENAME COLUMN attributionlink TO attribution_link;

-- progress
ALTER TABLE content ALTER COLUMN progress SET NOT NULL;

-- abuse_level_id
ALTER TABLE content RENAME COLUMN abuselevelid TO abuse_level_id;
ALTER TABLE content ALTER COLUMN abuse_level_id SET NOT NULL;

-- num_abuse_reports
ALTER TABLE content RENAME COLUMN numabusereports TO num_abuse_reports;
ALTER TABLE content ALTER COLUMN num_abuse_reports SET NOT NULL;

-- num_views
ALTER TABLE content RENAME COLUMN numviews TO num_views;
ALTER TABLE content ALTER COLUMN num_views SET NOT NULL;

-- version
ALTER TABLE content ALTER COLUMN version SET DATA TYPE int;
ALTER TABLE content ALTER COLUMN version SET NOT NULL;
ALTER TABLE content ALTER COLUMN version SET DEFAULT '7'::int;


-- image
-- content_id
ALTER TABLE image RENAME COLUMN contentid TO content_id;
ALTER TABLE image ALTER COLUMN content_id SET NOT NULL;
ALTER TABLE image ADD CONSTRAINT image_content_id_key UNIQUE (content_id);
ALTER TABLE image ADD FOREIGN KEY(content_id) REFERENCES content(id);

-- initialized_at
ALTER TABLE image RENAME COLUMN initializedat TO created_at;
ALTER TABLE image ALTER COLUMN created_at SET NOT NULL;

-- width
ALTER TABLE image ALTER COLUMN width SET NOT NULL;

-- height
ALTER TABLE image ALTER COLUMN height SET NOT NULL;

-- tile_size
ALTER TABLE image RENAME COLUMN tilesize TO tile_size;
ALTER TABLE image ALTER COLUMN tile_size SET NOT NULL;

-- tile_overlap
ALTER TABLE image RENAME COLUMN tileoverlap TO tile_overlap;
ALTER TABLE image ALTER COLUMN tile_overlap SET NOT NULL;

-- tile_format
ALTER TABLE image RENAME COLUMN tileformat TO tile_format;
ALTER TABLE image ALTER COLUMN tile_format SET NOT NULL;


-- flickr
-- content_id
ALTER TABLE flickr RENAME COLUMN contentid TO content_id;
ALTER TABLE flickr ALTER COLUMN content_id SET NOT NULL;
ALTER TABLE flickr ADD CONSTRAINT flickr_content_id_key UNIQUE (content_id);
ALTER TABLE flickr ADD FOREIGN KEY(content_id) REFERENCES content(id);

-- farm_id
ALTER TABLE flickr RENAME COLUMN farmid TO farm_id;
ALTER TABLE flickr ALTER COLUMN farm_id SET NOT NULL;

-- server_id
ALTER TABLE flickr RENAME COLUMN serverid TO server_id;
ALTER TABLE flickr ALTER COLUMN server_id SET NOT NULL;

-- photo_id
ALTER TABLE flickr RENAME COLUMN photoid TO photo_id;
ALTER TABLE flickr ALTER COLUMN photo_id SET NOT NULL;

-- secret
ALTER TABLE flickr ALTER COLUMN secret SET NOT NULL;

-- size_id
ALTER TABLE flickr RENAME COLUMN sizeid TO size_id;
ALTER TABLE flickr ALTER COLUMN size_id SET NOT NULL;

-- is_public
ALTER TABLE flickr RENAME COLUMN ispublic TO is_public;
ALTER TABLE flickr ALTER COLUMN is_public SET NOT NULL;

-- license_id
ALTER TABLE flickr RENAME COLUMN licenseid TO license_id;
ALTER TABLE flickr ALTER COLUMN license_id SET NOT NULL;

-- original_extension
ALTER TABLE flickr RENAME COLUMN originalextension TO original_extension;

-- original_secret
ALTER TABLE flickr RENAME COLUMN originalsecret TO original_secret;

-- owner_nsid
ALTER TABLE flickr RENAME COLUMN ownernsid TO owner_nsid;
ALTER TABLE flickr ALTER COLUMN owner_nsid SET NOT NULL;

-- owner_real_name
ALTER TABLE flickr RENAME COLUMN ownerrealname TO owner_real_name;

-- owner_username
ALTER TABLE flickr RENAME COLUMN ownerusername TO owner_username;
ALTER TABLE flickr ALTER COLUMN owner_username SET NOT NULL;

-- photo_page_url
ALTER TABLE flickr RENAME COLUMN photopageurl TO photo_page_url;

COMMIT;
