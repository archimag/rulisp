--- users.update-01

ALTER TABLE users ALTER COLUMN password TYPE char(32);

ALTER TABLE users ADD COLUMN status integer;

CREATE TABLE confirmations (
   mark char(40) PRIMARY KEY,
   user_id integer REFERENCES users(user_id) ON DELETE CASCADE,
   created  timestamp without time zone
);

ALTER TABLE confirmations OWNER TO lisp;

CREATE TRIGGER confiramation_insert_trigger
    BEFORE INSERT ON confirmations
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();

CREATE OR REPLACE FUNCTION add_new_user (name varchar(32), mail varchar(64), pswd char(32), confirmation char(40)) RETURNS integer
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



CREATE TABLE forgot (
   mark char(40) PRIMARY KEY,
   user_id integer REFERENCES users(user_id) ON DELETE CASCADE,
   created  timestamp without time zone  
);

CREATE TRIGGER forgot_insert_trigger
    BEFORE INSERT ON confirmations
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();
