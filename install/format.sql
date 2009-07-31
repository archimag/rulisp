--- paste.sql

CREATE TABLE pastes (
    paste_id serial PRIMARY KEY,
    user_id integer REFERENCES users(user_id) ON DELETE CASCADE,
    title varchar (64), 
    code text,
    created timestamp without time zone
);

ALTER TABLE pastes OWNER TO lisp;

CREATE TRIGGER pastes_insert_trigger
    BEFORE INSERT ON pastes
    FOR EACH ROW
    EXECUTE PROCEDURE rlf_created_fix();

