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
	
	<p>In this section we carry out an initial analysis of past transactions, with the objective of gathering information about the categories, products and customers that tend to generate the highest revenues. The results shown in this page can provide insights to inform the activities of the sales team. This information, together with the recommendation system and customer analysis which we have implemented in the next page, can support the activities of the company's marketing team.</p>
	
	<p> The chart below shows the best selling products ranked according to the revenues they generate. Only the top 10 best selling products are shown.</p>

<?php
    // Total Revenue by product
    
    $query = "SELECT ProductName, Revenue FROM ecommerce.ProductsVsCustomers_Pivot ORDER BY Revenue DESC limit 10";
    $title = "Products by revenues";
    query_and_print_graph($query,$title,"Euros");
?>
	
	<p>The chart below shows the results of a similar analysis, this time to rank the customers that contribute the most to total revenues. Only the top 20 customers are shown below.</p>
	
<?php
	// Page body. Write here your queries
	
	$query = "SELECT b.CustomerID Customer, sum(a.Quantity*a.UnitPrice) Revenues from ecommerce.order_details a left join ecommerce.orders b on a.OrderID=b.OrderID group by CustomerID order by Revenues desc limit 20";
	$title = "Customers by revenues";
	query_and_print_graph($query,$title,"Euros");
?>

	<p>Once we have identified the best selling products and the top customers, we seek to improve our understanding of the relationships between them.</p>
	
	<p> We start from considering associations between product categories as observed in past transactions. Specifically, the chart below shown the links between pairs of categories according to the number of times they are bought together. The thicker the network edge connecting two categories, the more often those two categories are found together in the customers' baskets. The size of the circles is proportional to the total revenues that each product categories generates.</p>
	
	<center><img src="Rplotreddots.png" style="width: 40%"></center>

	<p>The information provided in the network graph above could be used to informed marketing campaigns that cover two or more product categories, so that the marketing team could deploy offers for products that belong to categories that "go together".
	
	<p> We then go one layer further to look at the associations between products. The following table shows a ranking of pairs of products that tend to be purchased together. The pairs of products are ranked according to the number of times each pair appears in a transaction. To focus on the most relevant information, we show only the product pairs that appear at least five times. While this information does not, on its own, provide a fully-fledge recommendation system, it can provide insight on customers behaviour that can be used in setting up marketing campaigns.</p>
	
<?php

	// Most sold product pairs
	
	$query = "SELECT
			  P1.ProductName as Product_1,
       		  P2.ProductName as Product_2,
       		  Count(DISTINCT O1.OrderID) as Number_of_occurrences
			  FROM ecommerce.products P1
       		  JOIN ecommerce.products P2
         	  ON P1.ProductID != P2.ProductID
       		  LEFT JOIN ecommerce.order_details O1
              INNER JOIN ecommerce.order_details O2
                ON O1.OrderID = O2.OrderID
         		ON O1.ProductID = P1.ProductId
            	AND O2.ProductID = P2.ProductID 
			  WHERE P1.ProductID > P2.ProductID
              GROUP BY P1.ProductID, P2.ProductID
              HAVING COUNT(DISTINCT O1.OrderID)>=5
			  ORDER BY Count(DISTINCT O1.OrderID) DESC";
	$title = "Pairs of products frequently purchased together";
	query_and_print_table($query,$title);
?>
	<p> In the next tab, we take this analysis further by implementing a product recommendation system and by looking at customers marginal contribution to revenues using a LASSO regression.</p>


	</div>
	<div id="analysis" style="display: none">
	<h2>Analysis</h2>
	
	<p>Below we show the top 20 product recommendation rules identified by the <b>Apriori algorithm</b>. The table can be read as follows: for each rule, the left-hand side shows a potential basket that the customer has put together, while the right-hand side shows the additional product that could be purchased to "complete that basket".</p>

	<p>For example, the first rule indicates that a customer that has already added dried applies and sild (herring) to her basket, would be recommended gorgonzola cheese <em>(note: it sounds disgusting but the customer is always right!)</em> The recommendations are based on the analysis of historical transaction already stored in the database.</p>
			
<?php

	$query = "SELECT * FROM ecommerce.apriori";
	$title = "Recommendation rules";
	query_and_print_table($query,$title);
	echo "";
?>

	<p>We build a log-log linear regression model of the revenues per product using the quantity purchased by the different customers as explanatory factors. We consider each different product as a new observation of the revenue generated. Other explanatory factors have been considered such as the price of the product, the quantity per product, the average expenses, or the mean product price. However, the quantity of each product purchased by the different customers gave the best interpretability of the results and provides the best matching to the recommendation system. Since most of the customers only bought a small fraction of the products our data matrix is sparse. We use the Lasso regression since it is optimal for sparse data, but also because it allows us to focus on the most relevant customers.</p>

	<p>The table below shows the coefficients of the LASSO Regression. We have used the results of this regression to rank customers according to their <b>percentage monetary contribution</b> to total revenues from buying an additional 1% of products. We believe that this analysis would help the sales team in two aspects:</p>

		<ul style="list-style-type:circle">

  			<li> Identify the most promising customers for their marketing activities to target. The customers with larger percentage monetary contribution are the most susceptible to increase their expenses either by increasing the quantity of the products they usually have in the basket or by purchasing products they have not tried yet. <a href="http://80y.mjt.lu/nl/80y/s6gjl.html#" target="_blank">(...and they may be amenable to suggestions like these...)</a></li>

			<li> Relax the potentially over-estimation of certain clients. For example, the client SAVEA is the client that generated more revenue for the firm <em>(see plot "Customers by Revenue")</em> up to now. However, according to the results of the LASSO analysis, it is not the client that will increase the most the firm's marginal revenue when buying "an average product". We strongly suggest doing this exercise (the LASSO Regression) before every new marketing campaign, to update the ranking of the "most interesting revenue generating customers"</li>
		
		</ul>

<?php

	$query = "SELECT * FROM ecommerce.top_customers";
	$title = "Top customers by marginal revenue contribution";
	query_and_print_table($query,$title);
	echo "";
?>

		</div>
<?php
	// Close connection
	mysql_close($link);
?>
