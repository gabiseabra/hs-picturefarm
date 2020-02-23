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
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: picture_tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.picture_tags (
    picture_uuid uuid NOT NULL,
    tag character varying NOT NULL
);


--
-- Name: pictures; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pictures (
    id integer NOT NULL,
    uuid uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    file_name character varying NOT NULL,
    file_hash character(32) NOT NULL,
    url character varying NOT NULL,
    mime_type character varying NOT NULL,
    created_at timestamp(6) without time zone DEFAULT now() NOT NULL,
    updated_at timestamp(6) without time zone DEFAULT now() NOT NULL,
    resource_type character varying
);


--
-- Name: pictures_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pictures_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pictures_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pictures_id_seq OWNED BY public.pictures.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: tag_aliases; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tag_aliases (
    tag character varying NOT NULL,
    alias character varying NOT NULL,
    created_at timestamp(6) without time zone DEFAULT now() NOT NULL,
    updated_at timestamp(6) without time zone DEFAULT now() NOT NULL
);


--
-- Name: pictures id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pictures ALTER COLUMN id SET DEFAULT nextval('public.pictures_id_seq'::regclass);


--
-- Name: picture_tags picture_tags_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.picture_tags
    ADD CONSTRAINT picture_tags_pkey PRIMARY KEY (picture_uuid, tag);


--
-- Name: pictures pictures_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pictures
    ADD CONSTRAINT pictures_pkey PRIMARY KEY (id);


--
-- Name: pictures pictures_uuid_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pictures
    ADD CONSTRAINT pictures_uuid_key UNIQUE (uuid);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: tag_aliases tag_aliases_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tag_aliases
    ADD CONSTRAINT tag_aliases_pkey PRIMARY KEY (tag, alias);


--
-- Name: pictures_file_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX pictures_file_name_idx ON public.pictures USING btree (file_name);


--
-- Name: picture_tags picture_tags_picture_uuid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.picture_tags
    ADD CONSTRAINT picture_tags_picture_uuid_fkey FOREIGN KEY (picture_uuid) REFERENCES public.pictures(uuid);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20200125213105'),
    ('20200125213320'),
    ('20200125213531'),
    ('20200223083851');
