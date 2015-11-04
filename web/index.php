<?php ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html>
<head>
	<title>MyApp</title>    
	<link rel="stylesheet" type="text/css" href="style.css" />
</head>
<script>
/**
 * Given an element, or an element ID, blank its style's display
 * property (return it to default)
 */
function show(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = '';
    }
}

/**
 * Given an element, or an element ID, set its style's display property
 * to 'none'
 */
function hide(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = 'none';
    }
}

function show_content(optionsId) {
	var ids = new Array('home','data','analysis');
	show(optionsId);
	document.getElementById(optionsId + '_link').className = 'active';

	for (var i = 0; i < ids.length; i++)
	{
	    if (ids[i] == optionsId) continue;
	    hide(ids[i]);
	    document.getElementById(ids[i] + '_link').className = '';
	}
}
</script>
<body>
	<div id="header"><h1>group7,Felix Gutmann, Marco Fayet, Max van Esso</h1></div>

	<div id="menu">
		<a id="home_link" href="#" class="active" onclick="show_content('home'); return false;">Home</a> &middot;
		<a id="data_link" href="#" onclick="show_content('data'); update_data_charts(); return false;">Data</a> &middot;
		<a id="analysis_link" href="#" onclick="show_content('analysis'); return false;">Analysis</a> 
	</div>

	<div id="main">

		<div id="home">
			<h2>Home</h2>
			<h3>The challenge</h3>
			
			<p>Every sales team seeks to identify and exploit opportunities to increase sales. In addition to acquiring new customers, another way to increase sales is to target existing customers by providing them with relevant offers that might persuade them to purchase additional items. In order to be able to do that, a sales team would need to be able to build up offers that customers truly value and to identify those customers that are more likely to purchase new items.</p>
			
			<ul style="list-style-type:circle">
  				<li> The first objective can be achieved by developing a recommendation engine which, based on a customer's intended purchases can recommend additional items that are related to those already in the shopping basket. </li>
  				<li> The second objective can be achieved by analyzing the average contribution that existing customers make across the entire product line. This is based on the belief that customers that have historically generated the highest revenues are not necessarily the same ones with the highest marginal revenue potential.</li>
			</ul>
			
			<h3>The solution</h3>
						
			<p>We have addressed the above challenges in two steps:</p>
				<ul style="list-style-type:circle">
					<li> First we have implemented a simple recommendation system, based on the <b><a href="http://www.wikipedia.org/wiki/Apriori_algorithm" target="_blank">Apriori algorithm.</a></b>. This algorithm analyses existing transaction recorded in the database and develops associative rules between products, based on the frequency of them being purchased together. The output of the algorithm is a set of rules which link a set of two or more products (which are assumed to be already in the customer's basket) with an additional product that the customer is recommended to buy.</li>
					<li> To address the second part of the challenge, i.e. to identify the company's "most interesting revenue generating customers", we have carried out a LASSO Regression, on the subset of the 20 top clients measured by total revenue, regressing each customers purchases (measure in quantities) on the total revenues associated to each product. We have used the results of this regression to rank customers according to their <b>percentage monetary contribution</b> to total revenues from buying one additional product. We have used a <b>LASSO approach</b> to identify customers for personalised marketing campaigns, allowing the company to move away from a massive and unrestricted campaign to a more efficient use of marketing resources.</li>
				</ul>
						
		</div>	

                <?php include 'data_and_analysis.php' ?>
	
	</div>

	<div id="footer">Project team: Gaston Besanson, Stefano Costantini, Laura Cozma, Jordi Zamora Munt</div>

</body>
</html>
<?php ?>
