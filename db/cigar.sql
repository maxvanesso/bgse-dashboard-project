DROP DATABASE IF EXISTS cigar;

CREATE DATABASE cigar;

/*
Attach database and create master table
*/

USE cigar;

/*
Create final database - Client
*/

DROP TABLE IF EXISTS client;

CREATE TABLE client (
	ClientID INT(10) NOT NULL,
	State NVARCHAR(10),
	Age TINYINT,
	PRIMARY KEY(ClientID),
	INDEX (State)
);

/*
Create final database - invoice
*/

DROP TABLE IF EXISTS invoice;

CREATE TABLE invoice (
	InvoiceNumber INT(10) NOT NULL,
	InvoiceDate DATE,
	ClientID INT(10),
	PRIMARY KEY(InvoiceNumber),
	FOREIGN KEY(ClientID) REFERENCES client(ClientID)
);


/*
	3. Create table: manufacturer
*/

DROP TABLE IF EXISTS manufacturer;

CREATE TABLE manufacturer (
	ManufacturerID TINYINT NOT NULL,
	Manufacturer NVARCHAR(20),
	PRIMARY KEY(ManufacturerID)
);

/*
Create final database - product
*/

DROP TABLE IF EXISTS product;

CREATE TABLE product (
	BrandID SMALLINT NOT NULL,
	Brand NVARCHAR(60),
	BrandFamily NVARCHAR(60),
	Category NVARCHAR(20),
	ManufacturerID TINYINT,
	PRIMARY KEY(BrandID),
	FOREIGN KEY(ManufacturerID) REFERENCES manufacturer(ManufacturerID)
);

/*
Create final database - invoice_detail
*/

DROP TABLE IF EXISTS invoice_detail;

CREATE TABLE invoice_detail (
	InvoiceNumber INT(10),
	BrandID SMALLINT,
	Volume SMALLINT,
	Sales DOUBLE,
	FOREIGN KEY(InvoiceNumber) REFERENCES invoice(InvoiceNumber),
	FOREIGN KEY(BrandID) REFERENCES product(BrandID)
);

DROP PROCEDURE getbestsellers;
DELIMITER $$
CREATE PROCEDURE getbestsellers(
stateofinterest NVARCHAR(10),
number_rank INT(10))
BEGIN
	SELECT p.Brand, SUM(i.Volume) AS Total, 
100*SUM(i.Volume)/(SELECT SUM(i.Volume) AS Total
FROM product p INNER JOIN invoice_detail i ON p.BrandID=i.BrandID 
INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
INNER JOIN client c ON I.ClientID=c.ClientID 
WHERE c.State= stateofinterest) AS Percentage
FROM product p INNER JOIN invoice_detail i ON p.BrandID=i.BrandID 
INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
INNER JOIN client c ON I.ClientID=c.ClientID 
WHERE c.State= stateofinterest 
GROUP BY Brand ORDER BY Total DESC LIMIT number_rank;
END $$
DELIMITER ;


DROP PROCEDURE getbestprofiters;
DELIMITER $$
CREATE PROCEDURE getbestprofiters(
stateofinterest NVARCHAR(10),
number_rank INT(10))
BEGIN
	SELECT p.Brand, SUM(i.Sales) AS Total, 
100*SUM(i.Sales)/(SELECT SUM(i.Sales) AS Total
FROM product p INNER JOIN invoice_detail i ON p.BrandID=i.BrandID 
INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
INNER JOIN client c ON I.ClientID=c.ClientID 
WHERE c.State= stateofinterest) AS Percentage
FROM product p INNER JOIN invoice_detail i ON p.BrandID=i.BrandID 
INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
INNER JOIN client c ON I.ClientID=c.ClientID 
WHERE c.State= stateofinterest 
GROUP BY Brand ORDER BY Total DESC LIMIT number_rank;
END $$
DELIMITER ;

/*
END CODE // END CODE // END CODE
*/

