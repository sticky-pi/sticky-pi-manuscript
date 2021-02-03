#!/bin/bash

echo Analysis using option $1

#source config.sh

module load cuda cudnn
source ~/sticky_ml/bin/activate
export S3BUCKET_NAME=sticky-ml-fly-ir

command="python analysis.py $1"
output=$(date +%F_%H-%M-%S)_%j_$1_$0.out

echo Running "$command. Logging to $output"

case "$1" in
        --pre)
            sbatch --job-name=$0$1 --cpus-per-task=1 --mem=4000 --time=0-05:00 --output=$output --wrap="$command"
            ;;
       --process)
             sbatch --job-name=$0$1 --gres=gpu:1  --cpus-per-task=4 --mem=20000 --time=0-08:00 --output=$output --wrap="$command"
            ;;
        --count)
             sbatch --job-name=$0$1 --cpus-per-task=12 --mem=20000 --time=0-01:00 --output=$output --wrap="$command"
            ;;
        --make-videos)
             sbatch --job-name=$0$1 --cpus-per-task=12 --mem=40000 --time=0-08:00 --output=$output --wrap="$command"
            ;;
        *)
            echo $"Wrong command"
            exit 1

esac

