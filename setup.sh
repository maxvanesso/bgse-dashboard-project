#!/bin/bash

# installion script

cmd=$1

user=`grep dbuser service.conf | cut -f2 -d' '`
pswd=`grep dbpswd service.conf | cut -f2 -d' '`

target_dir='/var/www/html'
#target_dir=$HOME/public_html

case $cmd in

install)

	mysql -u $user -p$pswd < db/cigar.sql
	mysql -u $user -p$pswd < data/cigar-dump.sql
	#mysql -u $user -p$pswd < analysis/Customers_by_product.sql

	mkdir -p "$target_dir/MyApp"
	cp -rf web/* "$target_dir/MyApp"

	echo "done!"
	;;

uninstall)
	echo "Uninstalling"
	
	mysql -u $user -p$pswd -e "DROP DATABASE cigar;" 
	rm -rf "target_dir/MyApp"

	echo "done!"
	;;
	

run)
	echo "Running"
	R CMD BATCH analysis/analysis.R 
	cat analysis.Rout
	rm analysis.Rout
	cp web/Rplotfinal.svg "$target_dir/MyApp"

	;;

*)
	echo "Unknown Command!"



esac
