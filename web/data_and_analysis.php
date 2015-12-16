<?php

	include 'functions.php';
	$GLOBALS['graphid'] = 0;

	// Load libraries
	document_header();

	// Create connection
	$link = connect_to_db();
?>
	<div id="data" style="display: none">
	
	<h2>Data</h2>
	
	<p>We carried out an analysis of past transactions, with the objective of gathering information on the types of products that generate the highest revenues as well as extracting information on the customer base. The results displayed in this page provide valuable insights to teams in sales and marketing aiming to refine their strategy.</p>
	
<?php
   // Total Revenue by product
    
   $query = "SELECT dayname(I.InvoiceDate) as Day, TRUNCATE(SUM(i.Sales),2) as Total_sales FROM cigar.invoice_detail i INNER JOIN cigar.invoice I ON i.InvoiceNumber = I.InvoiceNumber GROUP BY dayname(I.InvoiceDate) ORDER BY FIELD(dayname(I.InvoiceDate), 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')";
   $title = "Sales by day";
   query_and_print_graph($query,$title,"Dolars");
?>	

<p>Here we can see the average amount of revenue generated per day of the week. The fact that Monday is the highest grossing day on average could be explained by the wholesaling nature of a large portion of JR Cigar's activities - clients will tend to replenish stocks sold over the past week and weekend.</p>

<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, sum(i.Volume) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total desc limit 5";
	$title = "Best sellers";
	query_and_print_graph($query,$title,"Number of cigars");
?>

	<p>This chart indicates the brands of cigars that were the most in demand over the period of time covered by the dataset. The metric examined here is the total number of cigars sold and could provide cues as to which brands could be put on sale.</p>
<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, sum(i.Volume) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total asc limit 5";
	$title = "Least sellers";
	query_and_print_graph($query,$title,"Number of cigars");
?>
	
	<p>At the other end of the spectrum, this chart indicates the worst performing brands of cigars in terms of volume sold. Managers could either decide to remove them altogether from the catalogue, or come up with innovative marketing strategies specifically tailored to increasing the visibility and reputation of these brands.</p>
	
<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, truncate(sum(i.Sales),2) as Total	from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total desc limit 5";
	$title = "Most profitables";
	query_and_print_graph($query,$title,"Dollars");
?>

	<p>Here we display the cigar brands which generated the most profit over the time period we evaluated. We notice that only two of the top 5 brands are also in the most sold brands by volume, highlighting the differences between products which result in the highest profit margins and those which consumers are most drawn towards.</p>

<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, truncate(sum(i.Sales),2) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total asc limit 5";
	$title = "Least profitables";
	query_and_print_graph($query,$title,"Dollars");
?>

	<p>Looking at the least profitable brands, there is a higher crossover with the least sold brands per volume. Once again, this gives us cues as to which brands could be cut entirely from the product portfolio, or re-branded entirely.</p>

	<p>Another important aspect of business intelligence is to map out where the customers are located. JR Cigars customers in North America appear to cluster on the East Coast, with a particularly large base in New York and Florida, as well as in California. Smaller clusters appear in Texas and in the Great Lakes Region.</p>
		
	<center><img src="Rplotfinal.svg" style="width: 80%"></center>

	<p>Such information could lead to targeted ad campaigns from the marketing department. Of additional interest could be a further analytical break down of the clusters of customers to better understand consumption drivers. Florida for example has a big customer base because of a large population of retirees, typically more fond of cigars than the rest of the population.</p>
	
<?php
	// Page body. Write here your queries
	
	$query = "Select * from sales";
	$title = "Time series";
	query_and_print_series($query,$title,"Dollars");
?>
	<p> In the next tab, we take this analysis further by implementing a product recommendation system and by looking at customers marginal contribution to revenues using a LASSO regression.</p>


	</div>
	<div id="analysis" style="display: none">
	<h2>Analysis</h2>
	
<?php
	// Most sold product pairs
	
	$query = "SELECT * from cigar.recommendation";
	$title = "REcom";
	query_and_print_table($query,$title);
	echo "";
?>
	<p>Below we show the top 20 product recommendation rules identified by the <b>Apriori algorithm</b>. The table can be read as follows: for each rule, the left-hand side shows a potential basket that the customer has put together, while the right-hand side shows the additional product that could be purchased to "complete that basket".</p>

	<p>For example, the first rule indicates that a customer that has already added dried applies and sild (herring) to her basket, would be recommended gorgonzola cheese <em>(note: it sounds disgusting but the customer is always right!)</em> The recommendations are based on the analysis of historical transaction already stored in the database.</p>

	<p>We build a log-log linear regression model of the revenues per product using the quantity purchased by the different customers as explanatory factors. We consider each different product as a new observation of the revenue generated. Other explanatory factors have been considered such as the price of the product, the quantity per product, the average expenses, or the mean product price. However, the quantity of each product purchased by the different customers gave the best interpretability of the results and provides the best matching to the recommendation system. Since most of the customers only bought a small fraction of the products our data matrix is sparse. We use the Lasso regression since it is optimal for sparse data, but also because it allows us to focus on the most relevant customers.</p>

	<p>The table below shows the coefficients of the LASSO Regression. We have used the results of this regression to rank customers according to their <b>percentage monetary contribution</b> to total revenues from buying an additional 1% of products. We believe that this analysis would help the sales team in two aspects:</p>

		<ul style="list-style-type:circle">

  			<li> Identify the most promising customers for their marketing activities to target. The customers with larger percentage monetary contribution are the most susceptible to increase their expenses either by increasing the quantity of the products they usually have in the basket or by purchasing products they have not tried yet. <a href="http://80y.mjt.lu/nl/80y/s6gjl.html#" target="_blank">(...and they may be amenable to suggestions like these...)</a></li>

			<li> Relax the potentially over-estimation of certain clients. For example, the client SAVEA is the client that generated more revenue for the firm <em>(see plot "Customers by Revenue")</em> up to now. However, according to the results of the LASSO analysis, it is not the client that will increase the most the firm's marginal revenue when buying "an average product". We strongly suggest doing this exercise (the LASSO Regression) before every new marketing campaign, to update the ranking of the "most interesting revenue generating customers"</li>
		
		</ul>



		</div>
<?php
	// Close connection
	mysql_close($link);
?>
