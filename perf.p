# This is a Gnuplot script.
# Run "gnuplot perf.p" from the command line to execute this script.
# As input, it expects two CSV files, `list_perf.csv` and `tree_perf.csv`.  
#   Those are provided as examples.  It produces a GIF from each as output.
# You might want to play with the `set terminal` command to change
#   the size of the output.

set key outside
set autoscale fix
set datafile separator ","
set title "Set Performance"
set xlabel "number of elements"
set ylabel "running time (sec)"
set terminal png size 1000,500 enhanced
set output "list_perf.gif"
plot for [col=2:3] "list_perf.csv" using 1:col with lines title columnhead
set output "tree_perf.gif"
plot for [col=2:3] "tree_perf.csv" using 1:col with lines title columnhead
