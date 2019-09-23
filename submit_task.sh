#!/bin/bash

#PBS -m ae
#PBS -P w35
#PBS -q normalbw
#PBS -l walltime=1:40:00
#PBS -l mem=10GB
#PBS -l ncpus=1
#PBS -j oe
#PBS -l wd
#PBS -l other=gdata1

./awap_to_netcdf
