#!/bin/bash

#PBS -m ae
#PBS -P w35
#PBS -q normal
#PBS -l walltime=1:30:00
#PBS -l mem=64GB
#PBS -l ncpus=1
#PBS -j oe
#PBS -l wd
#PBS -l storage=gdata/w35+gdata/wd9
#PBS -M mengyuan.mu@unsw.edu.au

./awap_to_netcdf
