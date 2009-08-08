--
-- PostgreSQL database dump
--

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- Name: plpgsql; Type: PROCEDURAL LANGUAGE; Schema: -; Owner: -
--

CREATE PROCEDURAL LANGUAGE plpgsql;


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: confirmations; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE confirmations (
    mark character(40) NOT NULL,
    user_id integer,
    created timestamp without time zone
);


--
-- Name: forgot; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE forgot (
    mark character(40) NOT NULL,
    user_id integer,
    created timestamp without time zone
);


--
-- Name: formats; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE formats (
    format_id integer NOT NULL,
    user_id integer,
    title character varying(64),
    code text,
    created timestamp with time zone
);


--
-- Name: rlf_forums; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE rlf_forums (
    forum_id integer NOT NULL,
    description text,
    all_topics integer DEFAULT 0,
    pretty_forum_id character varying(32)
);


--
-- Name: rlf_messages; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE rlf_messages (
    message_id integer NOT NULL,
    topic_id integer,
    message text,
    author character varying(32),
    created timestamp without time zone
);


--
-- Name: rlf_topics; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE rlf_topics (
    topic_id integer NOT NULL,
    forum_id integer,
    title character varying(64),
    all_message integer DEFAULT 0,
    last_message integer,
    first_message integer
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE users (
    user_id integer NOT NULL,
    login character varying(32) NOT NULL,
    email character varying(64) NOT NULL,
    password character(32) NOT NULL,
    status integer,
    theme character varying(32)
);


--
-- Name: add_format_code(character varying, character varying, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_format_code(vlogin character varying, vtitle character varying, vcode text) RETURNS integer
    AS $$
DECLARE
   id integer;
   uid integer;
BEGIN
   SELECT nextval('formats_format_id_seq') INTO id;
   SELECT user_id INTO uid FROM users WHERE login = vlogin;   
   INSERT INTO formats (format_id, user_id, title, code) VALUES (id, uid, vtitle, vcode);
   RETURN id;
END;
$$
    LANGUAGE plpgsql;


--
-- Name: add_new_user(character varying, character varying, character, character); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_new_user(name character varying, mail character varying, pswd character, confirmation character) RETURNS integer
    AS $$
DECLARE
   id integer;
BEGIN
   SELECT nextval('users_user_id_seq') INTO id;
   INSERT INTO users (user_id, login, email, password, status) VALUES (id, name, mail, pswd, 1);
   INSERT INTO confirmations (mark, user_id) VALUES (confirmation, id);
   RETURN id;
END;
$$
    LANGUAGE plpgsql;


--
-- Name: rlf_created_fix(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: rlf_delete_message(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION rlf_delete_message(msg integer) RETURNS integer
    AS $$
DECLARE
    topic integer;
    last_msg integer;
    first_msg integer;
BEGIN
    SELECT topic_id INTO topic FROM rlf_messages WHERE message_id = msg;
    IF topic IS NOT NULL THEN
       SELECT last_message, first_message INTO last_msg, first_msg FROM rlf_topics WHERE topic_id = topic;
       IF last_msg = msg THEN
          SELECT message_id INTO last_msg FROM rlf_messages WHERE topic_id = 2 ORDER BY created DESC LIMIT 1 OFFSET 1;
          IF last_msg = first_msg THEN
             last_msg = NULL;
           END IF;
          UPDATE rlf_topics SET last_message = last_msg WHERE topic_id = topic;
       END IF;
       UPDATE rlf_topics SET all_message = (all_message - 1) WHERE topic_id = topic;
       DELETE FROM rlf_messages WHERE message_id = msg;
    END IF;
    RETURN  topic;
END;
$$
    LANGUAGE plpgsql;


--
-- Name: rlf_delete_topic(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION rlf_delete_topic(topic integer) RETURNS character varying
    AS $$
DECLARE
    forum integer;
    last_msg integer;
BEGIN
    SELECT forum_id, last_message INTO forum, last_msg FROM rlf_topics WHERE topic_id = topic;
    IF forum IS NOT NULL THEN
        IF last_msg IS NOT NULL THEN
            UPDATE rlf_topics SET last_message = NULL WHERE topic_id = topic;
        END IF;
        DELETE FROM rlf_topics WHERE topic_id = topic;
        UPDATE rlf_forums SET all_topics = (all_topics - 1) WHERE forum_id = forum;
    END IF;    
    RETURN (SELECT pretty_forum_id FROM rlf_forums WHERE forum_id = forum);
END;
$$
    LANGUAGE plpgsql;


--
-- Name: rlf_new_topic(integer, character varying, text, character varying); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: rlf_new_topic(character varying, character varying, text, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION rlf_new_topic(p_f_id character varying, ttl character varying, msg text, auth character varying) RETURNS integer
    AS $$
DECLARE
    f_id integer;
    t_id integer;
BEGIN
    SELECT forum_id INTO f_id FROM rlf_forums WHERE pretty_forum_id = p_f_id;
    SELECT nextval('rlf_topics_topic_id_seq') INTO t_id;
    INSERT INTO rlf_topics (topic_id, forum_id, title) VALUES (t_id, f_id, ttl);
    INSERT INTO rlf_messages(topic_id, message, author) VALUES (t_id, msg, auth);
    RETURN t_id;
END;
$$
    LANGUAGE plpgsql;


--
-- Name: rlf_update_last_message(); Type: FUNCTION; Schema: public; Owner: -
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


--
-- Name: rlf_update_topic_count(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION rlf_update_topic_count() RETURNS trigger
    AS $$
BEGIN
   UPDATE rlf_forums SET all_topics = 1 + all_topics WHERE forum_id = NEW.forum_id;
   RETURN NEW; 
END;
$$
    LANGUAGE plpgsql;


--
-- Name: formats_format_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE formats_format_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: formats_format_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE formats_format_id_seq OWNED BY formats.format_id;


--
-- Name: rlf_forums_forum_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE rlf_forums_forum_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: rlf_forums_forum_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE rlf_forums_forum_id_seq OWNED BY rlf_forums.forum_id;


--
-- Name: rlf_messages_message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE rlf_messages_message_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: rlf_messages_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE rlf_messages_message_id_seq OWNED BY rlf_messages.message_id;


--
-- Name: rlf_topics_topic_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE rlf_topics_topic_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: rlf_topics_topic_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE rlf_topics_topic_id_seq OWNED BY rlf_topics.topic_id;


--
-- Name: users_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE users_user_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: users_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE users_user_id_seq OWNED BY users.user_id;


--
-- Name: format_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE formats ALTER COLUMN format_id SET DEFAULT nextval('formats_format_id_seq'::regclass);


--
-- Name: forum_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE rlf_forums ALTER COLUMN forum_id SET DEFAULT nextval('rlf_forums_forum_id_seq'::regclass);


--
-- Name: message_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE rlf_messages ALTER COLUMN message_id SET DEFAULT nextval('rlf_messages_message_id_seq'::regclass);


--
-- Name: topic_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE rlf_topics ALTER COLUMN topic_id SET DEFAULT nextval('rlf_topics_topic_id_seq'::regclass);


--
-- Name: user_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE users ALTER COLUMN user_id SET DEFAULT nextval('users_user_id_seq'::regclass);


--
-- Name: confirmations_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY confirmations
    ADD CONSTRAINT confirmations_pkey PRIMARY KEY (mark);


--
-- Name: forgot_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY forgot
    ADD CONSTRAINT forgot_pkey PRIMARY KEY (mark);


--
-- Name: formats_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY formats
    ADD CONSTRAINT formats_pkey PRIMARY KEY (format_id);


--
-- Name: rlf_forums_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY rlf_forums
    ADD CONSTRAINT rlf_forums_pkey PRIMARY KEY (forum_id);


--
-- Name: rlf_forums_pretty_forum_id_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY rlf_forums
    ADD CONSTRAINT rlf_forums_pretty_forum_id_key UNIQUE (pretty_forum_id);


--
-- Name: rlf_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY rlf_messages
    ADD CONSTRAINT rlf_messages_pkey PRIMARY KEY (message_id);


--
-- Name: rlf_topics_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_pkey PRIMARY KEY (topic_id);


--
-- Name: users_login_key; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_login_key UNIQUE (login);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_id);


--
-- Name: confiramation_insert_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER confiramation_insert_trigger
    BEFORE INSERT ON confirmations
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: forgot_insert_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER forgot_insert_trigger
    BEFORE INSERT ON confirmations
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: forgot_insert_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER forgot_insert_trigger
    BEFORE INSERT ON forgot
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: formats_insert_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER formats_insert_trigger
    BEFORE INSERT ON formats
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: rlf_message_update; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER rlf_message_update
    BEFORE INSERT OR UPDATE ON rlf_messages
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();


--
-- Name: rlf_update_last_message; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER rlf_update_last_message
    AFTER INSERT ON rlf_messages
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_update_last_message();


--
-- Name: rlf_update_topic_count; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER rlf_update_topic_count
    AFTER INSERT ON rlf_topics
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_update_topic_count();


--
-- Name: confirmations_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY confirmations
    ADD CONSTRAINT confirmations_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE;


--
-- Name: forgot_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY forgot
    ADD CONSTRAINT forgot_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE;


--
-- Name: formats_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY formats
    ADD CONSTRAINT formats_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE;


--
-- Name: rlf_messages_topic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY rlf_messages
    ADD CONSTRAINT rlf_messages_topic_id_fkey FOREIGN KEY (topic_id) REFERENCES rlf_topics(topic_id) ON DELETE CASCADE;


--
-- Name: rlf_topics_first_message_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_first_message_fkey FOREIGN KEY (first_message) REFERENCES rlf_messages(message_id);


--
-- Name: rlf_topics_forum_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_forum_id_fkey FOREIGN KEY (forum_id) REFERENCES rlf_forums(forum_id);


--
-- Name: rlf_topics_last_message_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY rlf_topics
    ADD CONSTRAINT rlf_topics_last_message_fkey FOREIGN KEY (last_message) REFERENCES rlf_messages(message_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

