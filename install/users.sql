--  users

CREATE TABLE users (
  user_id serial NOT NULL,
  login character varying(32) NOT NULL,
  email character varying(64) NOT NULL,
  password text NOT NULL,
  CONSTRAINT users_pkey PRIMARY KEY (user_id),
  CONSTRAINT users_login_key UNIQUE (login)
);

ALTER TABLE users OWNER TO lisp;
