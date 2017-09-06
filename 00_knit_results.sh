#!/bin/bash
R --vanilla --slave < 00_knit_results.R > 00_knit_results.txt
# tips:
# https://tex.stackexchange.com/questions/128668/building-tex-file-from-knitr-rnw-as-child-of-larger-parent-tex-file-to-use-r-obj
# Underscore in knitr:
# https://stackoverflow.com/questions/41933210/inline-code-with-underscore-in-knitr
