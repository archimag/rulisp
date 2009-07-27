----------------------------------------------------------------------------------------------------
-- Update-1
----------------------------------------------------------------------------------------------------

ALTER TABLE rlf_forums ADD COLUMN pretty_forum_id varchar(32) UNIQUE;

ALTER TABLE rlf_messages DROP CONSTRAINT rlf_messages_topic_id_fkey;
ALTER TABLE rlf_messages 
      ADD CONSTRAINT rlf_messages_topic_id_fkey
      FOREIGN KEY (topic_id) REFERENCES rlf_topics(topic_id) ON DELETE CASCADE;

CREATE OR REPLACE FUNCTION rlf_new_topic(p_f_id varchar(32), ttl character varying, msg text, auth character varying) RETURNS integer
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

CREATE OR REPLACE FUNCTION rlf_delete_message(msg integer) RETURNS integer
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

ALTER FUNCTION public.rlf_delete_message(msg integer) OWNER TO lisp;


CREATE OR REPLACE FUNCTION rlf_delete_topic (topic integer) RETURNS varchar(32)
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

ALTER FUNCTION public.rlf_delete_topic(msg integer) OWNER TO lisp;