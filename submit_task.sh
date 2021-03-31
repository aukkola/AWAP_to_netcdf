#!/bin/bash

#PBS -m ae
#PBS -P oq98
#PBS -q express
#PBS -l walltime=00:15:00
#PBS -l mem=6GB
#PBS -l ncpus=1
#PBS -j oe
#PBS -l wd
#PBS -l storage=gdata/w35+gdata/wd9+scratch/w35
#PBS -M a.ukkola@unsw.edu.au

./awap_to_netcdf
