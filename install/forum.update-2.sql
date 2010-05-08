--- Поддержка древовидного форума ---

ALTER TABLE rlf_messages ADD COLUMN reply_on integer;

ALTER TABLE rlf_messages ADD CONSTRAINT parent_message_id 
   FOREIGN KEY (reply_on) REFERENCES rlf_messages (message_id) 
   ON DELETE CASCADE;
