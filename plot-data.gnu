# gnuplot file used in generating reliability plots

set terminal png transparent nocrop enhanced font arial 8 size 620,320 
set output "plot-1.png"
set key inside right top vertical Right noreverse enhanced autotitles box linetype -1 linewidth 1.000
set xlabel "Cycles"
set ylabel "Sum of Distances"

plot "distances.dat" notitle
