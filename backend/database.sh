#!/bin/bash

if [ "$1" = "" ] ; then
	echo "Missing psql password as argument"
	exit
fi

if [ "$2" = "" ] ; then
	echo "Missing db-backup-?.sql file as argument"
	exit 
fi

PGPASSWORD=$1
DBBACKUP=$2

docker rm chordle-postgres || true
docker run -d --name chordle-postgres -e POSTGRES_PASSWORD=$PGPASSWORD -p 5432:5432 postgres:16

psql -h localhost -U postgres -c 'create database chordle'
psql -h localhost -U postgres chordle < $DBBACKUP

echo "Ready"