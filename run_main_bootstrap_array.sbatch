#!/usr/bin/env bash
#SBATCH -J bootstrap-main
#SBATCH -p shared
#SBATCH -t 1:00:00                  # 8 iters × 5.6 min × 1.3 safety
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G                   # 8 × 3.9 GB ≈ 31 GB
#SBATCH --array=1-1250%300          # total 10 000 iters
#SBATCH -o logs/main/boot_%A_%a.out
#SBATCH -e logs/main/boot_%A_%a.err
#SBATCH --account=killewald_lab
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=ncricco@g.harvard.edu

module load gcc/9.5.0-fasrc01
module load R
export R_LIBS_USER=/n/home12/ncrico/R/x86_64-pc-linux-gnu-library/4.4

# map array index → iteration range
iters_per_chunk=8
c=${SLURM_ARRAY_TASK_ID}
start=$(( (c-1)*iters_per_chunk + 1 ))
end=$(( c*iters_per_chunk ))
outfile="bootstrap_estimates/main/main_bootstrap_${c}.rds"

mkdir -p bootstrap_estimates/main logs/main

echo "Task $c: iters $start-$end → $outfile"
srun --unbuffered Rscript jobs/4-estimate-bootstrap-main.R \
"$start" "$end" "$outfile"
