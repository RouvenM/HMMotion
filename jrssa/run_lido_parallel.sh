#!/bin/bash -l
#SBATCH --job-name=R_parallel_job
#SBATCH --output=/work/mroumich/R_parallel_%j.out
#SBATCH --partition=med           # oder ultralong bei großen Läufen
#SBATCH --time=08:00:00          # Laufzeitlimit (z.B. 2 Tage)
#SBATCH --mem=30G                 # Arbeitsspeicherbedarf (~150 GB)
#SBATCH --cpus-per-task=10          # Anzahl paralleler Kerne!

module load R/4.4.2-gcc141-base

cd /work/mroumich/          # Arbeitsverzeichnis setzen

Rscript zvm.R              # führt dein Skript aus