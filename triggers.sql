CREATE OR REPLACE FUNCTION upd_timestamp() RETURNS TRIGGER 
LANGUAGE plpgsql
AS
$$
BEGIN
    NEW.last_modification = now();
    RETURN NEW;
END;
$$;

CREATE TRIGGER account_lastupd
  BEFORE UPDATE
  ON account
  FOR EACH ROW
  EXECUTE PROCEDURE upd_timestamp();
  
CREATE TRIGGER ccy_lastupd
  BEFORE UPDATE
  ON currency
  FOR EACH ROW
  EXECUTE PROCEDURE upd_timestamp();
  
CREATE TRIGGER fund_lastupd
  BEFORE UPDATE
  ON fund
  FOR EACH ROW
  EXECUTE PROCEDURE upd_timestamp();
  
CREATE TRIGGER fundinvtr_lastupd
  BEFORE UPDATE
  ON fund_investment_transaction
  FOR EACH ROW
  EXECUTE PROCEDURE upd_timestamp();
  
CREATE TRIGGER fundprice_lastupd
  BEFORE UPDATE
  ON fund_price
  FOR EACH ROW
  EXECUTE PROCEDURE upd_timestamp();

--DROP TRIGGER account_lastupd ON account