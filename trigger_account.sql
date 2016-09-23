CREATE OR REPLACE FUNCTION update_modified_column()	
RETURNS TRIGGER AS $$
BEGIN
    NEW.last_modified = now();
    RETURN NEW;	
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER account_lastupd
BEFORE UPDATE ON account
FOR EACH ROW EXECUTE PROCEDURE update_modified_column();