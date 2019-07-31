#!/usr/bin/env bash

if [ $# = 2 ]; then
	c_username=$1
	c_password=$2

	tar -xzf /tmp/cassandra_dump/snapshot.tgz -C /tmp/cassandra_dump

	until sstableloader --no-progress --nodes cassandra-0,cassandra-1.cassandra.bbp-nexus-dev.svc.cluster.local,cassandra-2.cassandra.bbp-nexus-dev.svc.cluster.local -u $c_username -pw $c_password /tmp/cassandra_dump/snapshot/kg/messages
	do
		echo "trying to restore kg messages table"
	done

	until sstableloader --no-progress --nodes cassandra-0,cassandra-1.cassandra.bbp-nexus-dev.svc.cluster.local,cassandra-2.cassandra.bbp-nexus-dev.svc.cluster.local -u $c_username -pw $c_password /tmp/cassandra_dump/snapshot/admin/messages
	do
		echo "trying to restore admin messages table"
	done

	until sstableloader --no-progress --nodes cassandra-0,cassandra-1.cassandra.bbp-nexus-dev.svc.cluster.local,cassandra-2.cassandra.bbp-nexus-dev.svc.cluster.local -u $c_username -pw $c_password /tmp/cassandra_dump/snapshot/iam/messages
	do
		echo "trying to restore iam messages table"
	done
else
	echo "usage: restore.sh cassandra_username cassandra_password"
	exit
fi