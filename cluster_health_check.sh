for node in $(qmgr -c "p n @d" | grep -i itaym | awk '{print $3}' | sort | uniq)
do
	echo $node
	pingRes=$(ping $node -c1 | grep error)
	if [ -z "$pingRes" ]; then
		echo "OK"
	else
		echo "Can't reach node"
	fi
done
