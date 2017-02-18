DROP TABLE IF EXISTS public.content;
DROP TABLE IF EXISTS public.image;
DROP TABLE IF EXISTS public.flickr;

BEGIN;

CREATE TABLE IF NOT EXISTS public.content (
  id bigserial NOT NULL,
  hashid text NOT NULL,
  typeid bigint NOT NULL,
  url text NOT NULL,
  state text NOT NULL,
  initializedat timestamp with time zone DEFAULT now(),
  activeat timestamp with time zone,
  completedat timestamp with time zone,
  title text,
  attributiontext text,
  attributionlink text,
  mime text,
  size bigint,
  error text,
  progress real DEFAULT '0'::real NOT NULL,
  abuselevelid bigint DEFAULT '0'::bigint NOT NULL,
  numabusereports bigint DEFAULT '0'::bigint NOT NULL,
  numviews bigint DEFAULT '0'::bigint NOT NULL,
  "version" bigint DEFAULT '4'::bigint NOT NULL,
  PRIMARY KEY(id)
);

CREATE TABLE IF NOT EXISTS public.image (
  contentid bigint NOT NULL,
  initializedat timestamp with time zone DEFAULT now() NOT NULL,
  width bigint NOT NULL,
  height bigint NOT NULL,
  tilesize integer NOT NULL,
  tileoverlap integer NOT NULL,
  tileformat text NOT NULL
);

CREATE TABLE IF NOT EXISTS public.flickr (
  contentid bigint NOT NULL,
  farmid bigint NOT NULL,
  serverid bigint NOT NULL,
  photoid text NOT NULL,
  secret text NOT NULL,
  sizeid bigint NOT NULL,
  ispublic bigint NOT NULL,
  licenseid bigint NOT NULL,
  originalextension text,
  originalsecret text,
  ownernsid text NOT NULL,
  ownerrealname text,
  ownerusername text NOT NULL,
  photopageurl text
);

COMMIT;
