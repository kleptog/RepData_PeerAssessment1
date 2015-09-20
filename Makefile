all: PA1_template.html

PA1_template.html: PA1_template.Rmd
	Rscript -e "require(knitr); require(markdown); knit2html('PA1_template.Rmd', 'PA1_template.html');"
