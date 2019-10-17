
--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.10
-- Dumped by pg_dump version 11.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: hashids; Type: SCHEMA; Schema: -; Owner: dani
--

CREATE SCHEMA hashids;


ALTER SCHEMA hashids OWNER TO dani;

--
-- Name: _prepare(character varying, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids._prepare(INOUT alphabet character varying, salt character varying DEFAULT ''::character varying, OUT alphabet_arr character varying[], OUT alphabet_length integer, OUT original_alphabet character varying, OUT original_alphabet_arr character varying[], OUT separators character varying, OUT separators_arr character varying[], OUT separators_length integer, OUT guards character varying, OUT guards_length integer) RETURNS record
    LANGUAGE plpgsql
    AS $$
      declare
        min_alphabet_length integer := 16;
        sep_div integer := 3.5;
        guard_div integer := 12;
        guard_count integer;
        cur_sep varchar;
        diff varchar;
      begin
        if alphabet is null then
          alphabet := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
        end if;

        original_alphabet := alphabet;
        original_alphabet_arr := regexp_split_to_array( alphabet, '' );
        guards := '';
        separators := 'cfhistuCFHISTU';
        separators_arr := regexp_split_to_array( separators, '' );
        alphabet_arr := hashids.unique_alphabet( alphabet := alphabet, separators := separators );

        alphabet := array_to_string( alphabet_arr, '' );
        alphabet_length := array_length( alphabet_arr, 1 );

        if alphabet_length < min_alphabet_length then
          raise exception '[hash_id] Alphabet must contain at least % unique characters', min_alphabet_length;
        end if;

        if array_position( alphabet_arr, ' ' ) is not null then
          raise exception '[hash_id] error: Alphabet cannot contain spaces';
        end if;

        for i in 1..length( separators ) loop
          cur_sep := array_position( original_alphabet_arr, separators_arr [i] );

          if cur_sep is null then
            separators := substr( separators, 1, i ) || ' ' || substr( separators, i + 1 );
          end if;
        end loop;

        separators := regexp_replace( separators, '[ ]', '', 'g' );
        separators := hashids.shuffle( separators, salt );

        if ( length( separators ) < 1 or ( length( alphabet ) / length( separators ) ) > sep_div ) then
          separators_length = ceil( length( alphabet ) / sep_div );

          if ( separators_length > length( separators ) ) then
            diff := separators_length - length( separators );
            separators := separators || substr( alphabet, 1, diff );
            alphabet := substr( alphabet, diff );
          end if;
        end if;

        alphabet := hashids.shuffle( alphabet, salt );
        guard_count := ceil( length( alphabet ) / guard_div );

        if length( alphabet ) < 3 then
          guards := substr( separators, 1, guard_count );
          separators := substr( separators, guard_count );
        else
          guards := substr( alphabet, 1, guard_count );
          alphabet := substr( alphabet, guard_count + 1 );
        end if;

        alphabet_arr := regexp_split_to_array( alphabet, '' );
        alphabet_length := length( alphabet );
        separators_length := length( separators );
        guards_length := length( guards );
      end;
      $$;


ALTER FUNCTION hashids._prepare(INOUT alphabet character varying, salt character varying, OUT alphabet_arr character varying[], OUT alphabet_length integer, OUT original_alphabet character varying, OUT original_alphabet_arr character varying[], OUT separators character varying, OUT separators_arr character varying[], OUT separators_length integer, OUT guards character varying, OUT guards_length integer) OWNER TO dani;

--
-- Name: decode(character varying, character varying, character varying, integer); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.decode(id character varying, alphabet character varying DEFAULT NULL::character varying, salt character varying DEFAULT ''::character varying, min_length integer DEFAULT NULL::integer) RETURNS bigint[]
    LANGUAGE plpgsql
    AS $$
      declare
        optns record;
        numbers bigint [];
        empty_array bigint [];
        parts varchar [];
        parts_count integer;
        id_breakdown varchar;
        lottery varchar;
        sub_id varchar;
        buffer varchar;
        idx integer := 1;
      begin
        numbers := array [] :: bigint [];
        empty_array := numbers;

        if ( id is null or length( id ) = 0 ) then
          return empty_array;
        end if;

        optns := hashids._prepare( salt := salt, alphabet := alphabet );
        alphabet := optns.alphabet;
        parts := hashids.split( id, optns.guards );
        parts_count = array_length( parts, 1 );

        if parts_count = 3 or parts_count = 2 then
          idx := 2;
        end if;

        id_breakdown := parts [idx];

        lottery := substr( id_breakdown, 1, 1 );
        id_breakdown := substr( id_breakdown, 2 );

        parts := hashids.split( id_breakdown, optns.separators );
        parts_count = array_length( parts, 1 );

        for i in 1..parts_count loop
          sub_id := parts [i];
          buffer := lottery || salt || alphabet;

          alphabet := hashids.shuffle( alphabet, substr( buffer, 1, optns.alphabet_length ) );
          numbers := numbers || hashids.from_alphabet( sub_id, alphabet );
        end loop;

        if (
          array_length( numbers, 1 ) = 0 or
          hashids.encode(
              number := numbers,
              alphabet := optns.original_alphabet,
              salt := salt,
              min_length := min_length
          ) is distinct from id
        ) then
          return empty_array;
        end if;

        return numbers;
      end;
      $$;


ALTER FUNCTION hashids.decode(id character varying, alphabet character varying, salt character varying, min_length integer) OWNER TO dani;

--
-- Name: decode_hex(character varying, character varying, integer, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.decode_hex(id character varying, salt character varying DEFAULT ''::character varying, min_length integer DEFAULT NULL::integer, alphabet character varying DEFAULT NULL::character varying) RETURNS character varying
    LANGUAGE plpgsql
    AS $$
      declare
        hex varchar = '';
        numbers bigint [];
      begin
        numbers := hashids.decode( id := id, salt := salt, min_length := min_length, alphabet := alphabet );

        for i in 1..array_length( numbers, 1 ) loop
          hex := hex || substr( to_hex( numbers [i] ), 2 );
        end loop;

        return hex;
      end;
      $$;


ALTER FUNCTION hashids.decode_hex(id character varying, salt character varying, min_length integer, alphabet character varying) OWNER TO dani;

--
-- Name: encode(anyelement, character varying, integer, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.encode(number anyelement, salt character varying DEFAULT ''::character varying, min_length integer DEFAULT NULL::integer, alphabet character varying DEFAULT NULL::character varying) RETURNS text
    LANGUAGE plpgsql
    AS $_$
      declare
        optns record;
        alphabet_arr varchar [];
        alphabet_length int;
        separators_length int;
        guards_length int;
        guard_index integer;
        guards varchar;
        guard varchar = '';
        separators varchar;
        i integer := 0;
        hash_id text := '';
        numbers_id_int bigint := 0;
        numbers bigint [];
        numbers_length integer;
        current_num bigint;
        lottery varchar := '';
        half_length integer;
        excess integer;
        buffer text := '';
        last_id text;
      begin
        optns := hashids._prepare( salt := salt, alphabet := alphabet );
        alphabet := optns.alphabet;
        alphabet_arr := optns.alphabet_arr;
        alphabet_length := optns.alphabet_length;
        separators := optns.separators;
        separators_length := optns.separators_length;
        guards := optns.guards;
        guards_length := optns.guards_length;

        if min_length is null then
          min_length := 0;
        end if;

        if number :: text ~ '^\{.*\}$' then -- if number parameter is an array
          numbers := number;
        else
          numbers := array [number];
        end if;

        numbers_length := array_length( numbers, 1 );

        if numbers_length = 0 then
          return hash_id;
        end if;

        for i in 0..numbers_length - 1 loop
          numbers_id_int := numbers_id_int + ( numbers [i + 1] % ( i + 100 ) );
        end loop;

        hash_id := alphabet_arr [( numbers_id_int % alphabet_length ) + 1];
        lottery := hash_id;

        for i in 0..numbers_length - 1 loop
          current_num := numbers [i + 1];
          buffer := lottery || salt || alphabet;

          alphabet := hashids.shuffle( alphabet, substr( buffer, 1, alphabet_length ) );
          last_id := hashids.to_alphabet( current_num, alphabet );

          hash_id := hash_id || last_id;

          if ( i < numbers_length - 1 ) then
            current_num = current_num % ascii( substr( last_id, 1, 1 ) ) + i;
            hash_id := hash_id || substr( separators, ( current_num % separators_length ) :: integer + 1, 1 );
          end if;
        end loop;

        if length( hash_id ) < min_length then
          guard_index := ( numbers_id_int + ascii( substr( hash_id, 1, 1 ) ) ) % guards_length;
          guard := substr( guards, guard_index + 1, 1 );

          hash_id = guard || hash_id;

          if length( hash_id ) < min_length then
            guard_index := ( numbers_id_int + ascii( substr( hash_id, 3, 1 ) ) ) % guards_length;
            guard := substr( guards, guard_index + 1, 1 );

            hash_id := hash_id || guard;
          end if;
        end if;

        half_length = ( length( alphabet ) / 2 );

        while ( length( hash_id ) < min_length ) loop
          alphabet := hashids.shuffle( alphabet, alphabet );
          hash_id := substr( alphabet, half_length + 1 ) || hash_id || substr( alphabet, 1, half_length );

          excess := length( hash_id ) - min_length;

          if excess > 0 then
            hash_id := substr( hash_id, ( excess / 2 ) + 1, min_length );
          end if;
        end loop;

        return hash_id;
      end;
      $_$;


ALTER FUNCTION hashids.encode(number anyelement, salt character varying, min_length integer, alphabet character varying) OWNER TO dani;

--
-- Name: encode_hex(character varying, character varying, integer, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.encode_hex(hex character varying, salt character varying DEFAULT ''::character varying, min_length integer DEFAULT NULL::integer, alphabet character varying DEFAULT NULL::character varying) RETURNS character varying
    LANGUAGE plpgsql
    AS $_$
      declare
        parts varchar [];
        numbers bigint [];
        number bigint;
      begin
        if not hex ~ '^[0-9a-fA-F]+$' then
          return '';
        end if;

        execute 'select array(select t[1] from regexp_matches( $1, ''[\w\\W]{1,12}'', ''g'') t)'
        into parts
        using hex;

        for i in 1..array_length( parts, 1 ) loop
          number := ( 'x' || lpad( ( '1' || parts [i] ), 16, '0' ) ) :: bit( 64 ) :: bigint;
          numbers := array_append( numbers, number );
        end loop;

        return hashids.encode( number := numbers, salt := salt, min_length := min_length, alphabet := alphabet );
      end;
      $_$;


ALTER FUNCTION hashids.encode_hex(hex character varying, salt character varying, min_length integer, alphabet character varying) OWNER TO dani;

--
-- Name: from_alphabet(character varying, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.from_alphabet(id character varying, alphabet character varying, OUT number bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        parts varchar [] := regexp_split_to_array( id, '' );
        parts_length integer := array_length( parts, 1 );
        letter varchar;
        letter_position integer;
      begin
        number := 0;

        for i in 1..parts_length loop
          letter := parts [i];
          letter_position := array_position( alphabet_arr, letter ) - 1;

          number := number * length( alphabet ) + letter_position;
        end loop;
      end;
      $$;


ALTER FUNCTION hashids.from_alphabet(id character varying, alphabet character varying, OUT number bigint) OWNER TO dani;

--
-- Name: shuffle(character varying, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.shuffle(alphabet character varying DEFAULT ''::character varying, salt character varying DEFAULT ''::character varying) RETURNS character varying
    LANGUAGE plpgsql
    AS $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        alphabet_length integer := length( alphabet );
        char_position integer := 1;
        shuffle_v integer := 0;
        shuffle_p integer := 0;
        shuffle_j integer := 0;
        shuffle_integer integer := 0;
        shuffle_tmp varchar;
        salt_char varchar;
        old_position integer;
        new_position integer;
      begin
        if length( salt ) < 1 then
          return alphabet;
        end if;

        for i in reverse ( alphabet_length - 1 )..1 loop
          shuffle_v := shuffle_v % length( salt );
          char_position := shuffle_v + 1;

          salt_char := substr( salt, char_position, 1 );
          shuffle_integer := ascii( salt_char );
          shuffle_p = shuffle_p + shuffle_integer;
          shuffle_j = ( shuffle_integer + shuffle_v + shuffle_p ) % i;

          old_position = shuffle_j + 1;
          new_position = i + 1;

          shuffle_tmp = alphabet_arr [new_position];

          alphabet_arr [new_position] = alphabet_arr [old_position];
          alphabet_arr [old_position] = shuffle_tmp;
          shuffle_v := ( shuffle_v + 1 );
        end loop;

        return array_to_string( alphabet_arr, '' );
      end;
      $$;


ALTER FUNCTION hashids.shuffle(alphabet character varying, salt character varying) OWNER TO dani;

--
-- Name: split(character varying, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.split(id character varying DEFAULT ''::character varying, separators character varying DEFAULT ''::character varying, OUT parts character varying[]) RETURNS character varying[]
    LANGUAGE plpgsql
    AS $$
      begin
        if length( separators ) < 1 then
          parts := '{}' :: varchar [];
        else
          parts := regexp_split_to_array( regexp_replace( id, '[' || separators || ']', ' ', 'g' ), ' ' );
        end if;
      end;
      $$;


ALTER FUNCTION hashids.split(id character varying, separators character varying, OUT parts character varying[]) OWNER TO dani;

--
-- Name: to_alphabet(bigint, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.to_alphabet(number bigint, alphabet character varying) RETURNS text
    LANGUAGE plpgsql
    AS $$
      declare
        id text := '';
        current_number bigint := number;
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        alphabet_length integer := length( alphabet );
      begin
        while current_number > 0 loop
          id := alphabet_arr [( current_number % alphabet_length ) + 1] || id;
          current_number := current_number / alphabet_length;
        end loop;

        return id;
      end;
      $$;


ALTER FUNCTION hashids.to_alphabet(number bigint, alphabet character varying) OWNER TO dani;

--
-- Name: unique_alphabet(character varying, character varying); Type: FUNCTION; Schema: hashids; Owner: dani
--

CREATE FUNCTION hashids.unique_alphabet(alphabet character varying DEFAULT ''::character varying, separators character varying DEFAULT ''::character varying, OUT new_alphabet character varying[]) RETURNS character varying[]
    LANGUAGE plpgsql
    AS $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        separators_arr varchar [] := regexp_split_to_array( separators, '' );
        letter varchar;
      begin
        new_alphabet := '{}' :: varchar [];

        for i in 1..array_length( alphabet_arr, 1 ) loop
          letter := alphabet_arr [i];

          if (
            array_position( new_alphabet, letter ) is not null or
            array_position( separators_arr, letter ) is not null
          ) then
            continue;
          end if;

          new_alphabet := array_append( new_alphabet, letter );
        end loop;
      end;
      $$;


ALTER FUNCTION hashids.unique_alphabet(alphabet character varying, separators character varying, OUT new_alphabet character varying[]) OWNER TO dani;

--
-- Name: content_before_insert(); Type: FUNCTION; Schema: public; Owner: dani
--

CREATE FUNCTION public.content_before_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
          DECLARE
            iterations INT := 0;
            max_iterations INT := 100;
            current_id INT := NEW.id;
            select_by_hash_id_query TEXT = 'SELECT id FROM ' || quote_ident(TG_TABLE_NAME) || ' WHERE hash_id=';
            found TEXT;
            new_hash_id TEXT;
            hashids_min_length INT := 3;
            hashids_secret_salt TEXT;
          BEGIN
              hashids_secret_salt := (SELECT value FROM config WHERE key='hashids_salt') ;

              LOOP
                new_hash_id := hashids.encode(
                  number := current_id,
                  min_length := hashids_min_length,
                  salt := hashids_secret_salt
                );
                EXECUTE select_by_hash_id_query || quote_literal(new_hash_id) INTO found;

                IF found IS NULL THEN
                  EXIT;
                END IF;

                IF iterations > max_iterations THEN
                  RAISE EXCEPTION
                    'Too many iterations to find new hash ID. Max: %, current: %.',
                    max_iterations, iterations
                  USING HINT = 'Check content table for hash ID collisions';
                END IF;

                iterations := iterations + 1;
                current_id := current_id + 1;
              END LOOP;

              NEW.title = NEW.title || '-' || iterations;
              NEW.hash_id := new_hash_id;
              RETURN NEW;
          END;
      $$;


ALTER FUNCTION public.content_before_insert() OWNER TO dani;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: config; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE public.config (
    id bigint NOT NULL,
    key text NOT NULL,
    value text NOT NULL
);


ALTER TABLE public.config OWNER TO dani;

--
-- Name: config_id_seq; Type: SEQUENCE; Schema: public; Owner: dani
--

CREATE SEQUENCE public.config_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.config_id_seq OWNER TO dani;

--
-- Name: config_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dani
--

ALTER SEQUENCE public.config_id_seq OWNED BY public.config.id;


--
-- Name: content; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE public.content (
    id bigint NOT NULL,
    hash_id text NOT NULL,
    type_id integer DEFAULT 0 NOT NULL,
    url text NOT NULL,
    state text DEFAULT 'initialized'::text NOT NULL,
    initialized_at timestamp with time zone DEFAULT now() NOT NULL,
    active_at timestamp with time zone,
    completed_at timestamp with time zone,
    title text,
    attribution_text text,
    attribution_link text,
    mime text,
    size bigint,
    error text,
    progress double precision DEFAULT 0 NOT NULL,
    abuse_level_id integer DEFAULT 0 NOT NULL,
    num_abuse_reports bigint DEFAULT 0 NOT NULL,
    num_views bigint DEFAULT 0 NOT NULL,
    version integer DEFAULT 4 NOT NULL
);


ALTER TABLE public.content OWNER TO dani;

--
-- Name: content_id_seq; Type: SEQUENCE; Schema: public; Owner: dani
--

CREATE SEQUENCE public.content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.content_id_seq OWNER TO dani;

--
-- Name: content_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dani
--

ALTER SEQUENCE public.content_id_seq OWNED BY public.content.id;


--
-- Name: flickr; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE public.flickr (
    content_id bigint NOT NULL,
    farm_id integer NOT NULL,
    server_id integer NOT NULL,
    photo_id text NOT NULL,
    secret text NOT NULL,
    size_id integer NOT NULL,
    is_public boolean NOT NULL,
    license_id integer NOT NULL,
    original_extension text,
    original_secret text,
    owner_nsid text NOT NULL,
    owner_real_name text,
    owner_username text NOT NULL,
    photo_page_url text
);


ALTER TABLE public.flickr OWNER TO dani;

--
-- Name: flickr_content_id_seq; Type: SEQUENCE; Schema: public; Owner: dani
--

CREATE SEQUENCE public.flickr_content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.flickr_content_id_seq OWNER TO dani;

--
-- Name: flickr_content_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dani
--

ALTER SEQUENCE public.flickr_content_id_seq OWNED BY public.flickr.content_id;


--
-- Name: image; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE public.image (
    content_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    width bigint NOT NULL,
    height bigint NOT NULL,
    tile_size integer NOT NULL,
    tile_overlap integer NOT NULL,
    tile_format text NOT NULL
);


ALTER TABLE public.image OWNER TO dani;

--
-- Name: image_content_id_seq; Type: SEQUENCE; Schema: public; Owner: dani
--

CREATE SEQUENCE public.image_content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.image_content_id_seq OWNER TO dani;

--
-- Name: image_content_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: dani
--

ALTER SEQUENCE public.image_content_id_seq OWNED BY public.image.content_id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: dani
--

CREATE TABLE public.schema_migrations (
    name text NOT NULL,
    executed_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO dani;

--
-- Name: config id; Type: DEFAULT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.config ALTER COLUMN id SET DEFAULT nextval('public.config_id_seq'::regclass);


--
-- Name: content id; Type: DEFAULT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.content ALTER COLUMN id SET DEFAULT nextval('public.content_id_seq'::regclass);


--
-- Name: flickr content_id; Type: DEFAULT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.flickr ALTER COLUMN content_id SET DEFAULT nextval('public.flickr_content_id_seq'::regclass);


--
-- Name: image content_id; Type: DEFAULT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.image ALTER COLUMN content_id SET DEFAULT nextval('public.image_content_id_seq'::regclass);


--
-- Data for Name: config; Type: TABLE DATA; Schema: public; Owner: dani
--

COPY public.config (id, key, value) FROM stdin;
1	hashids_salt	secret-salt
\.


--
-- Data for Name: content; Type: TABLE DATA; Schema: public; Owner: dani
--

-- COPY public.content (id, hash_id, type_id, url, state, initialized_at, active_at, completed_at, title, attribution_text, attribution_link, mime, size, error, progress, abuse_level_id, num_abuse_reports, num_views, version) FROM stdin;
-- 1	X75	0	https://example.com/1	initialized	2019-10-16 00:45:24.267478-04	\N	\N	\N	\N	\N	\N	\N	\N	0	0	0	0	4
-- 8	9jZ	0	https://example.com/yJL	initialized	2019-10-16 00:40:24.383983-04	\N	\N	\N	\N	\N	\N	\N	\N	0	0	0	300	4
-- 9	BNQ	1	https://example.com/foo	active	2019-10-16 00:00:23.383983-04	2019-10-16 00:00:24.383983-04	\N	\N	\N	\N	\N	\N	\N	0	0	0	0	4
-- 7	Xar	0	https://example.com/yOJ	active	2019-10-16 00:30:24.383983-04	2019-10-16 00:45:24.442116-04	\N	\N	\N	\N	\N	\N	\N	0	0	0	200	4
-- 6	9xe	0	https://example.com/X75	active	2019-10-16 00:30:24.383983-04	2019-10-16 00:45:24.611363-04	\N	\N	\N	\N	\N	\N	\N	0	0	0	100	4
-- \.


--
-- Data for Name: flickr; Type: TABLE DATA; Schema: public; Owner: dani
--

COPY public.flickr (content_id, farm_id, server_id, photo_id, secret, size_id, is_public, license_id, original_extension, original_secret, owner_nsid, owner_real_name, owner_username, photo_page_url) FROM stdin;
\.


--
-- Data for Name: image; Type: TABLE DATA; Schema: public; Owner: dani
--

COPY public.image (content_id, created_at, width, height, tile_size, tile_overlap, tile_format) FROM stdin;
\.


--
-- Data for Name: schema_migrations; Type: TABLE DATA; Schema: public; Owner: dani
--

COPY public.schema_migrations (name, executed_at) FROM stdin;
Install V8 extension	2019-10-16 00:45:24.187161-04
Initialize Hashids encode function	2019-10-16 00:45:24.187161-04
Initial setup	2019-10-16 00:45:24.187161-04
Insert Hashids secret	2019-10-16 00:45:24.187161-04
Create content hash_id trigger	2019-10-16 00:45:24.187161-04
\.


--
-- Name: config_id_seq; Type: SEQUENCE SET; Schema: public; Owner: dani
--

SELECT pg_catalog.setval('public.config_id_seq', 1, true);


--
-- Name: content_id_seq; Type: SEQUENCE SET; Schema: public; Owner: dani
--

SELECT pg_catalog.setval('public.content_id_seq', 17, true);


--
-- Name: flickr_content_id_seq; Type: SEQUENCE SET; Schema: public; Owner: dani
--

SELECT pg_catalog.setval('public.flickr_content_id_seq', 1, false);


--
-- Name: image_content_id_seq; Type: SEQUENCE SET; Schema: public; Owner: dani
--

SELECT pg_catalog.setval('public.image_content_id_seq', 1, false);


--
-- Name: config config_unique_key; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.config
    ADD CONSTRAINT config_unique_key UNIQUE (key);


--
-- Name: content content_unique_hash_id; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.content
    ADD CONSTRAINT content_unique_hash_id UNIQUE (hash_id);


--
-- Name: content content_unique_url; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.content
    ADD CONSTRAINT content_unique_url UNIQUE (url);


--
-- Name: schema_migrations migrations_unique_name; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT migrations_unique_name UNIQUE (name);


--
-- Name: config pk_config; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.config
    ADD CONSTRAINT pk_config PRIMARY KEY (id);


--
-- Name: content pk_content; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.content
    ADD CONSTRAINT pk_content PRIMARY KEY (id);


--
-- Name: flickr pk_flickr; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.flickr
    ADD CONSTRAINT pk_flickr PRIMARY KEY (content_id);


--
-- Name: image pk_image; Type: CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.image
    ADD CONSTRAINT pk_image PRIMARY KEY (content_id);


--
-- Name: content content_before_insert; Type: TRIGGER; Schema: public; Owner: dani
--

CREATE TRIGGER content_before_insert BEFORE INSERT ON public.content FOR EACH ROW EXECUTE PROCEDURE public.content_before_insert();


--
-- Name: image fk_content_id; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.image
    ADD CONSTRAINT fk_content_id FOREIGN KEY (content_id) REFERENCES public.content(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: flickr fk_content_id; Type: FK CONSTRAINT; Schema: public; Owner: dani
--

ALTER TABLE ONLY public.flickr
    ADD CONSTRAINT fk_content_id FOREIGN KEY (content_id) REFERENCES public.content(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--
