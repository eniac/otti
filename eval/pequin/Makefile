results.png: plot.R results.csv
	Rscript plot.R

results.csv: run.zsh pequin_results.csv slow_results.csv
	zsh run.zsh

clean:
	rm -rf tmp*
