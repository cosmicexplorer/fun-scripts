#!/bin/zsh

used_and_free_mem=$(echo "$(free -m | grep buffers/cache | grep -o "[[:digit:]]*")")
used_mem=$(echo $used_and_free_mem | head -n1)
free_mem=$(echo $used_and_free_mem | tail -n1)
total_mem=$(echo "$used_mem + $free_mem" | bc)

echo -e "Used:\t$used_mem MB"
echo -e "Free:\t$free_mem MB"
echo -e "Total:\t$total_mem MB"
echo "$(echo "scale=3;$used_mem/$total_mem*100" | bc)% used."
