# Description

The scripts within this `hpc` directory are tailored to be used in a super computer infrastructure which uses [slurm](https://slurm.schedmd.com/documentation.html) as the backend and more specifically they were executed on the `batch` queue of the Aristotle University of Thessaloniki [HPC infrastructure](https://hpc.it.auth.gr/).

## Array vs single jobs

All scripts named with `_array` designate that they are designed to be run with in a batch array. That means the command line argument `--array=1-60%10` to `sbatch` is mandatory for correct results, where 1-60 designates the array index and `%10` is the number of concurrently submitted jobs at any given moment.

Single job scripts do not have the `_array` designation.
