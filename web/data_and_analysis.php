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
    
   $query = "SELECT Dayname(Time) AS Days, TRUNCATE(AVG(Sales),2) AS Sales FROM
(SELECT ft.InvoiceDate AS Time , SUM(st.Sales) AS Sales FROM cigar.invoice ft INNER JOIN cigar.invoice_detail st ON ft.InvoiceNumber=st.InvoiceNumber GROUP BY InvoiceDate) Subtable
GROUP BY Days
ORDER BY Day(Time)";
   $title = "Average sales by weekdays (in USD)";
   query_and_print_graph($query,$title,"Dolars");
?>	

<p>Here we can see the average amount of revenue generated per day of the week. The fact that Monday is the highest grossing day on average could be explained by the wholesaling nature of a large portion of JR Cigar's activities - clients will tend to replenish stocks sold over the past week and weekend.</p>

<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, TRUNCATE(sum(i.Volume),0) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total desc limit 5";
	$title = "Best selling brands (by volume)";
	query_and_print_graph($query,$title,"Number of cigars");
?>

	<p>This chart indicates the brands of cigars that were the most in demand over the period of time covered by the dataset. The metric examined here is the total number of cigars sold and could provide cues as to which brands could be put on sale.</p>
<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, sum(i.Volume) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total asc limit 5";
	$title = "Least selling brands (by volume)";
	query_and_print_graph($query,$title,"Number of cigars");
?>
	
	<p>At the other end of the spectrum, this chart indicates the worst performing brands of cigars in terms of volume sold. Managers could either decide to remove them altogether from the catalogue, or come up with innovative marketing strategies specifically tailored to increasing the visibility and reputation of these brands.</p>
	
<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, truncate(sum(i.Sales),2) as Total	from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total desc limit 5";
	$title = "Most revenue generating brands (in USD) ";
	query_and_print_graph($query,$title,"Dollars");
?>

	<p>Here we display the cigar brands which generated the most revenue over the time period we evaluated. We notice that only two of the top 5 brands are also in the most sold brands by volume, highlighting the differences between products which result in the highest revnue and those which consumers are most drawn towards.</p>

<?php
	// Page body. Write here your queries
	
	$query = "SELECT p.Brand, truncate(sum(i.Sales),2) as Total from cigar.product p inner join cigar.invoice_detail i on p.BrandID=i.BrandID group by Brand order by Total asc limit 5";
	$title = "Least revenue generating brands (in USD)";
	query_and_print_graph($query,$title,"Dollars");
?>

	<p>Looking at the brands that generated the least revenue, there is a higher crossover with the least sold brands per volume. Once again, this gives us cues as to which brands could be cut entirely from the product portfolio, or re-branded entirely.</p>

	<p>Another important aspect of business intelligence is to map out where the customers are located. JR Cigars customers in North America appear to cluster on the East Coast, with a particularly large base in New York and Florida, as well as in California. Smaller clusters appear in Texas and in the Great Lakes Region.</p>
		
	<center><img src="Rplotfinal.svg" style="width: 80%"></center>

	<p>Such information could lead to targeted ad campaigns from the marketing department. Of additional interest could be a further analytical break down of the clusters of customers to better understand consumption drivers. Florida for example has a big customer base because of a large population of retirees, typically more fond of cigars than the rest of the population.</p>
	
<p>One of the most compact way of representing the 15 million entries in our dataset is through a time series. The following graph shows aggregate sales for the end of the year 2012, the whole of 2013, and the first half of 2014. The three "humps" represent seasonal highs that take place during summer, reflecting customers' preferences for outdoors smoking. The highest peak is in december 2014, most likely due to pre-Christmas purchases. In the Analysis section of our dashboard, we use this time series to build a predictive sales model via several regression techniques.</p>

<?php
	// Page body. Write here your queries
	
	$query = "Select * from cigar.sales";
	$title = "Aggregated sales per day (01/10/2012 - 30/06/14, in USD)";
	query_and_print_series($query,$title,"Aggregated daily sales");
?>

	</div>
	<div id="analysis" style="display: none">
	<h2>Analysis</h2>
	<h3>Recommendation system</h3>

<p>The recommendation system we used works via a User-Based Collaborative Filtering System (UBCF), which we modeled below.</p>

	<div>	<center><img src="recomsystem.png" style="width: 80%"></img></center></div>
	
<p>We started by building ratings for each customer based on their purchase history and implied preferences. The more a customer bought a specific brand in comparison to the others, the higher the rating will be (with 10 being the highest score). In order to then predict what brands a specific customer (here in red) will like, we used the ratings from customers with similar purchasing profiles - referred to as "nearest neighbours". The last step is to compute predicted ratings using the nearest neighbours' actual ratings. </p>

<p>As an output example, we display the top recommendation for five different customers identified by their Client ID, as well as the top brand they bought. Overall, this tool can be extremely useful to the marketing department to produce personalized advertising recommendations, proven to be much more effective than mass campaigns.</p>

<?php
	// Recommendation
	
	$query = "SELECT ClientID, Brand AS TopBrand, Recommendations as Recommendation from cigar.recommendation";
	$title = "Recommendation for top 5 clients";
	query_and_print_table($query,$title);
	echo "";
?>

<h3>Sales prediction</h3>

<p>The products in our database fit a hierarchical structure wherein each brand of cigar belongs to a brand family which itself belongs to a manufacturer and so on. When it came to predicting sales, we thus had the choice between the categories displayed below. One challenge to overcome was the amount sparsity associated with the inputs.</p>

	<div>	<center><img src="Tree.png" style="width: 80%"></img></center></div>	

<p>In order to build a good sales prediction model, we fitted three different models using the following techniques: Ridge Regression, Elastic Net and LASSO. The latter displayed the best fit, and provided an ideal framework by allowing for some "shrinkage" effect on brands which carry relatively less explanatory power with the sales predictions. As a baseline model we regressed total sales on the lagged total sales using OLS. The overall effect improves prediction accuracy significantly.</p>

<p>We used data from 2012 and 2013 to train our model and carried out predictions for the first half of the year 2014. The following plot shows the obtained results, with the actual observed sales appearing in blue and our predictions in grey.</p>

<?php
	// Page body. Write here your queries
	
	$query =  "Select time , sales from cigar.predictions";
	$queryn = "Select time,lasso from cigar.predictions";	
	$title =  "Prediction of sales (01/01/2014 - 30/06/2014, in USD)";
	$titlen = "";
	query_and_print_series2($query,$queryn,$title,$titlen,"Observed Sales","Predicted sales (Technique: Lasso-regression)");
?>

<p>Graphically, the fit looks quite satisfactory. We computed the Mean Absolute Percentage Error to confirm and obtained a result of 11.99%, compared to 18.16% for the Ordinary Least Square baseline model.</p>

<p>Overall, this predictive tool can be used at by the higher echelons of the company's management team in order to gain visibility on future sales and operations.</p>

<?php
	// MAPE
	
	$query = "SELECT TRUNCATE(Lasso,2) AS Lasso, 
			 TRUNCATE(Ridge,2) AS Ridge,
			 TRUNCATE(ElasticNet,2) AS ElasticNET,
			 TRUNCATE(OLS,2) AS OLS 
			 FROM cigar.accuracy";
	$title = "Mean absolute percentage error for fitted models";
	query_and_print_table($query,$title);
	echo "";
?>

		</div>
<?php
	// Close connection
	mysql_close($link);
?>
