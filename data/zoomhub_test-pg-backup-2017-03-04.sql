--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.2
-- Dumped by pg_dump version 9.6.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: content; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE content (
    id bigint NOT NULL,
    hash_id text NOT NULL,
    type_id bigint NOT NULL,
    url text NOT NULL,
    state text NOT NULL,
    initialized_at timestamp with time zone DEFAULT now() NOT NULL,
    active_at timestamp with time zone,
    completed_at timestamp with time zone,
    title text,
    attribution_text text,
    attribution_link text,
    mime text,
    size bigint,
    error text,
    progress real DEFAULT '0'::real NOT NULL,
    abuse_level_id bigint DEFAULT '0'::bigint NOT NULL,
    num_abuse_reports bigint DEFAULT '0'::bigint NOT NULL,
    num_views bigint DEFAULT '0'::bigint NOT NULL,
    version integer DEFAULT 7 NOT NULL
);


ALTER TABLE content OWNER TO dani;

--
-- Name: content_id_seq; Type: SEQUENCE; Schema: public; Owner: dani
--

CREATE SEQUENCE content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE content_id_seq OWNER TO dani;

--
-- Name: content_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dani
--

ALTER SEQUENCE content_id_seq OWNED BY content.id;


--
-- Name: flickr; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE flickr (
    content_id bigint NOT NULL,
    farm_id bigint NOT NULL,
    server_id bigint NOT NULL,
    photo_id text NOT NULL,
    secret text NOT NULL,
    size_id bigint NOT NULL,
    is_public bigint NOT NULL,
    license_id bigint NOT NULL,
    original_extension text,
    original_secret text,
    owner_nsid text NOT NULL,
    owner_real_name text,
    owner_username text NOT NULL,
    photo_page_url text
);


ALTER TABLE flickr OWNER TO dani;

--
-- Name: image; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE image (
    content_id bigint NOT NULL,
    initialized_at timestamp with time zone DEFAULT now() NOT NULL,
    width bigint NOT NULL,
    height bigint NOT NULL,
    tile_size bigint NOT NULL,
    tile_overlap bigint NOT NULL,
    tile_format text NOT NULL
);


ALTER TABLE image OWNER TO dani;

--
-- Name: content id; Type: DEFAULT; Schema: public; Owner: dani
--

ALTER TABLE ONLY content ALTER COLUMN id SET DEFAULT nextval('content_id_seq'::regclass);


--
-- Data for Name: content; Type: TABLE DATA; Schema: public; Owner: dani
--

INSERT INTO content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) VALUES (1, '100', 1, 'http://e.i.uol.com.br/outros/0907/090731cielao1.jpg', 'initialized', '2009-09-09 18:47:38.058-07', NULL, NULL, NULL, NULL, NULL, 'image/jpeg', 33998, NULL, 1, 0, 0, 0, 3);
INSERT INTO content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) VALUES (2, 'hdfm', 1, 'http://wdh.blob.core.windows.net/deepzoom-sources/hampi/HampiCB/02-Lakshmi,%20The%20Temple%20Elephant/09-Lakshmi%20and%20little%20girl.jpg', 'completed:success', '2011-01-17 20:56:09.782-08', NULL, NULL, NULL, NULL, NULL, 'image/jpeg', 15533083, NULL, 1, 0, 0, 0, 4);
INSERT INTO content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) VALUES (3, '100U', 1, 'http://www.archdrawing.ireland.anglican.org/archive/files/a0630962f3164841684eea66b6552445.JPG', 'initialized', '2013-08-17 16:30:39.464-07', NULL, NULL, NULL, NULL, NULL, 'image/jpeg', 202199, NULL, 1, 0, 0, 0, 4);
INSERT INTO content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) VALUES (4, '4rcn', 1, 'http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg', 'completed:success', '2014-04-11 13:41:21.717-07', NULL, NULL, NULL, NULL, NULL, 'image/jpeg', 9115770, NULL, 1, 0, 0, 0, 4);
INSERT INTO content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) VALUES (5, 'h', 1, 'http://upload.wikimedia.org/wikipedia/commons/3/36/SeattleI5Skyline.jpg#zoomhub=h', 'completed:success', '2016-04-03 22:54:55-07', NULL, NULL, NULL, NULL, NULL, 'image/jpeg', 3232686, NULL, 1, 2, 37, 0, 3);

--
-- Name: content_id_seq; Type: SEQUENCE SET; Schema: public; Owner: dani
--

SELECT pg_catalog.setval('content_id_seq', 6, false);


--
-- Data for Name: flickr; Type: TABLE DATA; Schema: public; Owner: dani
--



--
-- Data for Name: image; Type: TABLE DATA; Schema: public; Owner: dani
--

INSERT INTO image (content_id, initialized_at, width, height, tile_size, tile_overlap, tile_format) VALUES (1, '2009-07-31 12:29:22.312007-07', 208, 208, 254, 1, 'jpg');
INSERT INTO image (content_id, initialized_at, width, height, tile_size, tile_overlap, tile_format) VALUES (2, '2011-01-17 20:55:08.337606-08', 4032, 6048, 254, 1, 'jpg');
INSERT INTO image (content_id, initialized_at, width, height, tile_size, tile_overlap, tile_format) VALUES (3, '2013-08-17 16:30:31.321688-07', 1824, 1368, 254, 1, 'jpg');
INSERT INTO image (content_id, initialized_at, width, height, tile_size, tile_overlap, tile_format) VALUES (4, '2014-04-11 13:40:50.718786-07', 5058, 3750, 254, 1, 'jpg');
INSERT INTO image (content_id, initialized_at, width, height, tile_size, tile_overlap, tile_format) VALUES (5, '2009-07-30 21:20:13.596581-07', 4013, 2405, 254, 1, 'jpg');


--
-- Name: content content_hash_id_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY content
    ADD CONSTRAINT content_hash_id_key UNIQUE (hash_id);


--
-- Name: content content_id_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY content
    ADD CONSTRAINT content_id_key UNIQUE (id);


--
-- Name: content content_url_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY content
    ADD CONSTRAINT content_url_key UNIQUE (url);


--
-- Name: flickr flickr_content_id_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY flickr
    ADD CONSTRAINT flickr_content_id_key UNIQUE (content_id);


--
-- Name: content idx_29290_content_id_index_uniqe; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY content
    ADD CONSTRAINT idx_29290_content_id_index_uniqe PRIMARY KEY (id);


--
-- Name: image image_content_id_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_content_id_key UNIQUE (content_id);


-- --
-- -- Name: idx_29290_content_activeat_desc_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_activeat_desc_index ON content USING btree (active_at);


-- --
-- -- Name: idx_29290_content_completedat_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_completedat_index ON content USING btree (completed_at);


-- --
-- -- Name: idx_29290_content_hashid_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_hashid_index ON content USING btree (hash_id);


-- --
-- -- Name: idx_29290_content_hashid_index_unique; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE UNIQUE INDEX idx_29290_content_hashid_index_unique ON content USING btree (hash_id);


-- --
-- -- Name: idx_29290_content_initializedat_desc_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_initializedat_desc_index ON content USING btree (initialized_at);


-- --
-- -- Name: idx_29290_content_numviews_desc_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_numviews_desc_index ON content USING btree (num_views);


-- --
-- -- Name: idx_29290_content_state_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_state_index ON content USING btree (state);


-- --
-- -- Name: idx_29290_content_url_index; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE INDEX idx_29290_content_url_index ON content USING btree (url);


-- --
-- -- Name: idx_29290_content_url_index_unique; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE UNIQUE INDEX idx_29290_content_url_index_unique ON content USING btree (url);


-- --
-- -- Name: idx_29290_sqlite_autoindex_content_1; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE UNIQUE INDEX idx_29290_sqlite_autoindex_content_1 ON content USING btree (hash_id);


-- --
-- -- Name: idx_29290_sqlite_autoindex_content_2; Type: INDEX; Schema: public; Owner: dani
-- --

-- CREATE UNIQUE INDEX idx_29290_sqlite_autoindex_content_2 ON content USING btree (url);


--
-- Name: flickr flickr_content_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY flickr
    ADD CONSTRAINT flickr_content_id_fkey FOREIGN KEY (content_id) REFERENCES content(id);


--
-- Name: flickr flickr_contentid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY flickr
    ADD CONSTRAINT flickr_contentid_fkey FOREIGN KEY (content_id) REFERENCES content(id);


--
-- Name: image image_content_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_content_id_fkey FOREIGN KEY (content_id) REFERENCES content(id);


--
-- Name: image image_contentid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_contentid_fkey FOREIGN KEY (content_id) REFERENCES content(id);


--
-- PostgreSQL database dump complete
--

