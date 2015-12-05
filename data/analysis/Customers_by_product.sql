USE ecommerce;

DROP PROCEDURE IF EXISTS PivotingView;
DELIMITER //
CREATE PROCEDURE PivotingView ()
BEGIN

DROP VIEW IF EXISTS `ProductsVsCustomers`;
DROP VIEW IF EXISTS `ProductsVsCustomers_Pivot`;

SET @c = CONCAT("CREATE VIEW ProductsVsCustomers AS (SELECT ProductID, ProductName, Quantity*UnitPrice Revenue");
SET @d = CONCAT("CREATE VIEW ProductsVsCustomers_Pivot AS (SELECT ProductID, ProductName, sum(Revenue) Revenue");

    SELECT @a := count(CustomerID) from customers;
    SET @p1 = 0;
    WHILE (@p1 < @a) DO
      SET @p1 = @p1 + 1;
      SET @aux1 = CONCAT("SELECT @b := CustomerID from customers limit ",@p1-1,",1;");
      PREPARE stmt FROM @aux1;
      EXECUTE stmt;
      DEALLOCATE PREPARE stmt;
      
      SET @c = CONCAT(@c, ", CASE WHEN CustomerID = '",@b,"' THEN Quantity END AS ", @b," ");
      SET @d = CONCAT(@d, ", sum(",@b,") AS ", @b," ");
    END WHILE;  

    SET @c = CONCAT(@c, "FROM ordersCustomers);");
    select @c;
    PREPARE stmt FROM @c;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;

    SET @d = CONCAT(@d, "FROM ProductsVsCustomers GROUP BY ProductID);");
    select @d;
    PREPARE stmt FROM @d;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;

    
END
//
DELIMITER ;

DROP VIEW IF EXISTS `ordersCustomers`;
CREATE VIEW ordersCustomers AS
SELECT a.OrderID, sum(a.Quantity) Quantity, a.ProductID, c.ProductName, b.CustomerID, a.UnitPrice 
    FROM order_details a 
    LEFT JOIN 
    orders b ON a.OrderID=b.OrderID
    LEFT JOIN 
    products c ON a.ProductID=c.ProductID
    GROUP BY ProductID, CustomerID;

call PivotingView();  

