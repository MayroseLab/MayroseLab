for node in $(qmgr -c "p n @d" | grep -i itaym | awk '{print $3}' | sort | uniq)
do
	echo $node
	ssh $node "echo OK"
done
