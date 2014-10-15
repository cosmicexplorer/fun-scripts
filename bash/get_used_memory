#!/bin/bash

# tr is annoying here, not sure why it's necessary

used_and_free_mem="$(free -m | grep "buffers/cache" | grep -o "[[:digit:]]*")"
used_mem="$(echo $used_and_free_mem | tr [[:space:]]* "\n" | head -n1)"
free_mem="$(echo $used_and_free_mem | tr [[:space:]]* "\n" | tail -n1)"
total_mem="$(echo "$used_mem + $free_mem" | bc)"
ratio="$(echo "scale=3; $used_mem / $total_mem * 100" | bc)"

echo -e "Used:\t$used_mem MB"
echo -e "Free:\t$free_mem MB"
echo -e "Total:\t$total_mem MB"
echo "$ratio% used."
