#!/bin/bash
# bbou@ac-toulouse;fr
# 25/11/2013

dbdelete=
if [ "$1" == "-d" ]; then
	dbdelete=true
	shift
fi
db=wordnetOccurrences #$1

playuser="playOccurrences"


dbtype=mysql
if [ -z "${db}" ]; then
	read -p "enter ${dbtype} database name: " db
	echo
	export db
fi
dbuser=root

modules="wn legacy vn bnc sumo xwn glf ilfwn logs"

function process()
{
	if [ ! -e "$1" ];then
		return
	fi
	echo "$2"
	mysql -u ${dbuser} --password=${MYSQLPASSWORD} $4 $3 < $1
}

function dbexists()
{
	mysql -u ${dbuser} --password=${MYSQLPASSWORD} -e "\q" ${db} > /dev/null 2> /dev/null
	return $? 
}

function deletedb()
{
	echo "delete ${db}"
	mysql -u ${dbuser} --password=${MYSQLPASSWORD} -e "DROP DATABASE ${db};"
}

function createdb()
{
	echo "create ${db}"
	mysql -u ${dbuser} --password=${MYSQLPASSWORD} -e "CREATE DATABASE ${db} DEFAULT CHARACTER SET UTF8;"
}

function getpassword()
{
	read -s -p "enter ${dbuser}'s password: " MYSQLPASSWORD
	echo
	export MYSQLPASSWORD
}

echo "restoring ${db}"
getpassword

#database
if [ ! -z "${dbdelete}" ]; then
	deletedb
fi
if ! dbexists; then
	createdb
fi

# module tables
for m in ${modules}; do
	sql=${dbtype}-${m}-schema.sql
	process ${sql} "schema ${m}" ${db}
done
for m in ${modules}; do
	sql=${dbtype}-${m}-data.sql
	process ${sql} "data ${m}" ${db}
done
for m in ${modules}; do
	sql=${dbtype}-${m}-unconstrain.sql
	process ${sql} "unconstrain ${m}" ${db} --force 2> /dev/null
	sql=${dbtype}-${m}-constrain.sql
	process ${sql} "constrain ${m}" ${db} --force
done
for m in ${modules}; do
	sql=${dbtype}-${m}-views.sql
	process ${sql} "views ${m}" ${db}
done

function sqlexecute() {
	mysql -u "$dbuser" --password="$MYSQLPASSWORD" -e "$1"
}

#creating the user for the server and giving it rights 
sqlexecutesarr=(
	"DROP USER '${playuser}'@'localhost';"
	"CREATE USER '${playuser}'@'localhost';" 
	"GRANT SELECT ON ${db}.* TO '${playuser}'@'%';"
	"GRANT SELECT ON ${db}.* TO '${playuser}'@'localhost';"
)

for cmd in "${sqlexecutesarr[@]}"; do
	sqlexecute "$cmd"
done
