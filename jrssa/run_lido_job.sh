#!/bin/bash -l
#SBATCH --job-name=my_big_R_job
#SBATCH --partition=ultralong            # oder long / ultralong je nach Bedarf
#SBATCH --time=10-00:00:00              # Laufzeitlimit (2 Stunden Test)
#SBATCH --mem=200G                    # Arbeitsspeicherbedarf für Testlauf
#SBATCH --cpus-per-task=1            # Anzahl CPU-Kerne
#SBATCH --output=/work/mroumich/my_big_R_job.out.txt   # <--- hier liegt der Unterschied!

cd /work/mroumich                # <--- hier das Arbeitsverzeichnis setzen!

module load R/4.4.2-gcc141-base      # lädt R-Modul des Clusters

Rscript /work/mroumich/HMM3.R  # führt dein eigenes Skript aus