--
-- PostgreSQL database dump
--

-- SET client_encoding = 'UTF8';
-- SET standard_conforming_strings = off;
-- SET check_function_bodies = false;
-- SET client_min_messages = warning;
-- SET escape_string_warning = off;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: lisp
--

--CREATE PROCEDURAL LANGUAGE plpgsql;


-- ALTER PROCEDURAL LANGUAGE plpgsql OWNER TO lisp;

-- SET search_path = public, pg_catalog;

-- SET default_tablespace = '';

-- SET default_with_oids = false;

--
-- Name: forums; Type: TABLE; Schema: public; Owner: lisp; Tablespace: 
--

CREATE TABLE rlf_forums (
    forum_id integer NOT NULL,
    description text,
    all_topics integer DEFAULT 0
);


ALTER TABLE public.rlf_forums OWNER TO lisp;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: lisp; Tablespace: 
--

CREATE TABLE rlf_messages (
    message_id integer NOT NULL,
    topic_id integer,
    message text,
    author character varying(32),
    created timestamp without time zone
);


ALTER TABLE public.rlf_messages OWNER TO lisp;

--
-- Name: topics; Type: TABLE; Schema: public; Owner: lisp; Tablespace: 
--

CREATE TABLE rlf_topics (
    topic_id integer NOT NULL,
    forum_id integer,
    title character varying(64),
    all_message integer DEFAULT 0,
    last_message integer,
    first_message integer
);


ALTER TABLE public.rlf_topics OWNER TO lisp;

--
-- Name: created_fix(); Type: FUNCTION; Schema: public; Owner: lisp
--

CREATE FUNCTION rlf_created_fix() RETURNS trigger
    AS $$
BEGIN
   IF NEW.created IS NULL THEN
      NEW.created := now();
   END IF;
   RETURN NEW; 
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.rlf_created_fix() OWNER TO lisp;

--
-- Name: new_topic(integer, character varying, text, character varying); Type: FUNCTION; Schema: public; Owner: lisp
--

CREATE FUNCTION rlf_new_topic(f_id integer, ttl character varying, msg text, auth character varying) RETURNS integer
    AS $$
DECLARE
    t_id integer;
BEGIN
    SELECT nextval('rlf_topics_topic_id_seq') INTO t_id;
    INSERT INTO rlf_topics (topic_id, forum_id, title) VALUES (t_id, f_id, ttl);
    INSERT INTO rlf_messages(topic_id, message, author) VALUES (t_id, msg, auth);
    RETURN t_id;
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.rlf_new_topic(f_id integer, ttl character varying, msg text, auth character varying) OWNER TO lisp;

--
-- Name: update_last_message(); Type: FUNCTION; Schema: public; Owner: lisp
--

CREATE FUNCTION rlf_update_last_message() RETURNS trigger
    AS $$
BEGIN
   IF (SELECT first_message FROM rlf_topics WHERE topic_id = NEW.topic_id) IS NULL THEN
       UPDATE rlf_topics SET first_message = NEW.message_id WHERE topic_id = NEW.topic_id;
   ELSE
       UPDATE rlf_topics SET last_message = NEW.message_id, all_message = 1 + all_message WHERE topic_id = NEW.topic_id;
   END IF;
   RETURN NEW; 
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.rlf_update_last_message() OWNER TO lisp;

--
-- Name: update_topic_count(); Type: FUNCTION; Schema: public; Owner: lisp
--

CREATE FUNCTION rlf_update_topic_count() RETURNS trigger
    AS $$
BEGIN
   UPDATE rlf_forums SET all_topics = 1 + all_topics WHERE forum_id = NEW.forum_id;
   RETURN NEW; 
END;
$$
    LANGUAGE plpgsql;


ALTER FUNCTION public.rlf_update_topic_count() OWNER TO lisp;

--
-- Name: forums_forum_id_seq; Type: SEQUENCE; Schema: public; Owner: lisp
--

CREATE SEQUENCE rlf_forums_forum_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.rlf_forums_forum_id_seq OWNER TO lisp;

--
-- Name: forums_forum_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lisp
--

ALTER SEQUENCE rlf_forums_forum_id_seq OWNED BY rlf_forums.forum_id;


--
-- Name: messages_message_id_seq; Type: SEQUENCE; Schema: public; Owner: lisp
--

CREATE SEQUENCE rlf_messages_message_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.rlf_messages_message_id_seq OWNER TO lisp;

--
-- Name: messages_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lisp
--

ALTER SEQUENCE rlf_messages_message_id_seq OWNED BY rlf_messages.message_id;


--
-- Name: topics_topic_id_seq; Type: SEQUENCE; Schema: public; Owner: lisp
--

CREATE SEQUENCE rlf_topics_topic_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.rlf_topics_topic_id_seq OWNER TO lisp;

--
-- Name: topics_topic_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lisp
--

ALTER SEQUENCE rlf_topics_topic_id_seq OWNED BY rlf_topics.topic_id;


--
-- Name: forum_id; Type: DEFAULT; Schema: public; Owner: lisp
--

ALTER TABLE rlf_forums ALTER COLUMN forum_id SET DEFAULT nextval('rlf_forums_forum_id_seq'::regclass);


--
-- Name: message_id; Type: DEFAULT; Schema: public; Owner: lisp
--

ALTER TABLE rlf_messages ALTER COLUMN message_id SET DEFAULT nextval('rlf_messages_message_id_seq'::regclass);


--
-- Name: topic_id; Type: DEFAULT; Schema: public; Owner: lisp
--

ALTER TABLE rlf_topics ALTER COLUMN topic_id SET DEFAULT nextval('rlf_topics_topic_id_seq'::regclass);


--
-- Name: forums_pkey; Type: CONSTRAINT; Schema: public; Owner: lisp; Tablespace: 
--

ALTER TABLE ONLY rlf_forums
    ADD CONSTRAINT rlf_forums_pkey PRIMARY KEY (forum_id);


--
-- Name: messages_pkey; Type: CONSTRAINT; Schema: public; Owner: lisp; Tablespace: 
--

ALTER TABLE ONLY rlf_messages
    ADD CONSTRAINT rlf_messages_pkey PRIMARY KEY (message_id);


--
-- Name: topics_pkey; Type: CONSTRAINT; Schema: public; Owner: lisp; Tablespace: 
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_pkey PRIMARY KEY (topic_id);


--
-- Name: message_update; Type: TRIGGER; Schema: public; Owner: lisp
--

CREATE TRIGGER rlf_message_update
    BEFORE INSERT OR UPDATE ON rlf_messages
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: update_last_message; Type: TRIGGER; Schema: public; Owner: lisp
--

CREATE TRIGGER rlf_update_last_message
    AFTER INSERT ON rlf_messages
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_update_last_message();


--
-- Name: update_topic_count; Type: TRIGGER; Schema: public; Owner: lisp
--

CREATE TRIGGER rlf_update_topic_count
    AFTER INSERT ON rlf_topics
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_update_topic_count();


--
-- Name: messages_topic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lisp
--

ALTER TABLE ONLY rlf_messages
    ADD CONSTRAINT rlf_messages_topic_id_fkey FOREIGN KEY (topic_id) REFERENCES rlf_topics(topic_id);


--
-- Name: topics_first_message_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lisp
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_first_message_fkey FOREIGN KEY (first_message) REFERENCES rlf_messages(message_id);


--
-- Name: topics_forum_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lisp
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_forum_id_fkey FOREIGN KEY (forum_id) REFERENCES rlf_forums(forum_id);


--
-- Name: topics_last_message_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lisp
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_last_message_fkey FOREIGN KEY (last_message) REFERENCES rlf_messages(message_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

--REVOKE ALL ON SCHEMA public FROM PUBLIC;
--REVOKE ALL ON SCHEMA public FROM postgres;
--GRANT ALL ON SCHEMA public TO postgres;
--GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

----------------------------------------------------------------------------------------------------
-- Update
----------------------------------------------------------------------------------------------------

ALTER TABLE rlf_forums ADD COLUMN pretty_forum_id varchar(32) UNIQUE;
