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
	<div id="header"><h1>Group7 JR Cigars Data (Felix Gutmann, Marco Fayet, Max van Esso)</h1></div>

	<div id="menu">
		<a id="home_link" href="#" class="active" onclick="show_content('home'); return false;">Home</a> &middot;
		<a id="data_link" href="#" onclick="show_content('data'); update_data_charts(); return false;">Data</a> &middot;
		<a id="analysis_link" href="#" onclick="show_content('analysis'); return false;">Analysis</a> 
	</div>

	<div id="main">

		<div id="home">
			<h2>Home</h2>
			<h3>The challenge</h3>
			
			<p>Introduction on the challenge this is something. And trying something else.</p>
			
			<ul style="list-style-type:circle">
  				<li> The first objective can be explained here. </li>
  				<li> Here we explain some interesting stuff.</li>
			</ul>
			
			<h3>The solution</h3>
						
			<p>We explain our work:</p>
				<ul style="list-style-type:circle">
					<li> Here we can also explain our recommendation system, check code for linking <b><a href="http://www.wikipedia.org/wiki/Apriori_algorithm" target="_blank">Apriori algorithm.</a></b>, maybe we can link it to the page where our recommendation system is explained .</li>
					<li> code to make bold words here <b>seems like doing latex</b>.</li>
				</ul>
						
		</div>	

                <?php include 'data_and_analysis.php' ?>
	
	</div>

	<div id="footer">Project team: Felix Gutmann, Marco Fayet, Max van Esso</div>

</body>
</html>
<?php ?>
