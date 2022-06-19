for node in $(qmgr -c "p n @d" | grep -i itaym | awk '{print $3}' | sort | uniq)
do
        echo $node
        ssh $node "free -h" | head -2 | tail -1 | awk '{print $2}'
        ssh $node lscpu | grep '^CPU(s):' | awk '{print $2}'
done
