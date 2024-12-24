#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu

# Check if the R script is provided as the first argument
if [ -z "$1" ]; then
  echo "Usage: $0 <full_or_relative_path_to_R_script>"
  exit 1
fi

# Get the R script from the first argument
R_SCRIPT="$1"

# Define the possible choices for each argument
ARG1_CHOICES=("gaussian" "gamma")
ARG2_CHOICES=("binary" "continuous")
ARG3_CHOICES=("1.33" "2" "2.66")
ARG4_CHOICES=("4000")
ARG5_CHOICES=("5")
ARG6_CHOICES=("500")
ARG7_CHOICES=("1")
ARG8_CHOICES=("0")

# Loop through all combinations of arg1, arg2, arg3, ...
for arg1 in "${ARG1_CHOICES[@]}"; do
  for arg2 in "${ARG2_CHOICES[@]}"; do
    for arg3 in "${ARG3_CHOICES[@]}"; do
      for arg4 in "${ARG4_CHOICES[@]}"; do
        for arg5 in "${ARG5_CHOICES[@]}"; do
          for arg6 in "${ARG6_CHOICES[@]}"; do
            for arg7 in "${ARG7_CHOICES[@]}"; do
              for arg8 in "${ARG8_CHOICES[@]}"; do
                # Run each combination in the background
                qsub -l m_mem_free=8G simulations/run_r_script_args.sh "$R_SCRIPT" "$arg1" "$arg2" "$arg3" "$arg4" "$arg5" "$arg6" "$arg7" "$arg8"
              done
            done
          done
        done
      done
    done
  done
done

# Wait for all background jobs to finish
wait

echo "All jobs submitted."
