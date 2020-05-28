DSPG 2019 Broadband
================
Josh Goldstein, Teja Pristavec, Kateryna Savchyn
Summer 2019

# Project structure

The `DATA` directory is a symlink to data used in the analysis. The
“original” sub-folder is a read-only directory, and R should never
write files here. Put processed datasets in “Working” and “Final”
sub-folders as appropriate.

The `SRC` directory contains source code.

The `OUTPUT` directory contains simuation output, logs, and other
interim products.

The `DOC` directory contains any report-type output. Markdown, LaTeX,
Notebooks, etc. files go here.

The `FIGS` directory contains generated figures.
