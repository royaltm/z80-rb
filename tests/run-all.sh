#!/bin/bash
# Parallel run benches (10 jobs at a time)
 
max_jobs=10  # Max parallel jobs
count=0

while IFS= read -r base_path; do
  base_name=$(basename $base_path)
  output="${base_path}.txt"
  zxrun bench.${base_name}.tap -- -m 48k -i resources/if1-2.rom -p >"${output}" 2>/dev/null &
  ((count++))
 
   if ((count % max_jobs == 0)); then
    wait
    echo "Completed batch of $max_jobs jobs. Starting next batch..."
  fi
done
 
wait
echo "All done!"
