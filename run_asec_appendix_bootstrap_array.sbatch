#!/usr/bin/env bash
#SBATCH -J boot-appendix
#SBATCH -p shared
#SBATCH -t 04:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --array=1-50                  # 50 × 200 = 10 000 iters
#SBATCH -o logs/appendix_asec/boot_%A_%a.out
#SBATCH -e logs/appendix_asec/boot_%A_%a.err
#SBATCH --account=killewald_lab
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=ncricco@g.harvard.edu

module load gcc/9.5.0-fasrc01
module load R
export R_LIBS_USER=/n/home12/ncrico/R/x86_64-pc-linux-gnu-library/4.4

# map array index → iteration range
iters_per_chunk=200
c=${SLURM_ARRAY_TASK_ID}
start=$(( (c-1)*iters_per_chunk + 1 ))
end=$(( c*iters_per_chunk ))
outfile="bootstrap_estimates/appendix_asec/asec-appendix-bootstrap_${c}.rds"

mkdir -p bootstrap_estimates/appendix_asec logs/appendix_asec

echo "Task $c: iters $start-$end → $outfile"
srun --unbuffered Rscript jobs/4-estimate-bootstrap-asec-appendix.R \
      "$start" "$end" "$outfile"
