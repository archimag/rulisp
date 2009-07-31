--- paste.sql

CREATE TABLE formats (
    format_id serial PRIMARY KEY,
    user_id integer REFERENCES users(user_id) ON DELETE CASCADE,
    title varchar (64), 
    code text,
    created timestamp with time zone
);

ALTER TABLE formats OWNER TO lisp;

CREATE TRIGGER formats_insert_trigger
    BEFORE INSERT ON formats
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();

CREATE OR REPLACE FUNCTION add_format_code (vlogin varchar(32), vtitle varchar(64), vcode text) RETURNS integer
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
