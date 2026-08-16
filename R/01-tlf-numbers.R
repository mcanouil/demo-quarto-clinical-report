# Clinical Study Report - Output numbers for the numbering filter
# Writes the cross-reference identifier and ICH E3 number of every output, so
# `tlf-numbers.lua` can number each output with the number R holds rather than
# the sequence Quarto would give it.
#
# @license MIT
# @copyright 2026 Mickaël Canouil
# @author Mickaël Canouil

source("R/tlf-index.R")

path <- tlf_write_numbers()

message("Wrote ", nrow(tlf_index), " output numbers to ", path, ".")
