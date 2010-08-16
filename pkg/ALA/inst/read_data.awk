# read_data.awk --- Read data from data files for ALA
# Author: Sebastian P. Luque
# Created: Fri Aug 13 17:46:17 2010 (UTC)
# Last-Updated: Mon Aug 16 18:01:41 2010 (UTC)
#           By: Sebastian P. Luque
# copyright (c) 2010 Sebastian P. Luque
# -------------------------------------------------------------------------
# Commentary: 
#
# Data files for this book have description and meta-data at the beginning,
# so to extract only the data we need some filtering.  The structure of the
# files is as follows (inside boxed comment with leading space):
#
# ,-----
# | 
# | Data on count of leprosy bacilli pre- and post-treatment from a clinical trial 
# | of antibiotics for leprosy.
# | 
# | 
# | Source: Table 14.2.1 (page 422) in Snedecor, G.W. and Cochran, W.G. (1967). 
# | Statistical Methods, (6th edn). Ames, Iowa: Iowa State University Press
# | With permission of Iowa State University Press.
# | 
# | Reference: Snedecor, G.W. and Cochran, W.G. (1967). Statistical Methods, (6th edn). 
# | Ames, Iowa: Iowa State University Press
# | 
# | 
# | Description:
# | 
# | The dataset consists of count data from a placebo-controlled clinical
# | trial of 30 patients with leprosy at the Eversley Childs Sanitorium in the
# | Philippines. Participants in the study were randomized to either of two 
# | antibiotics (denoted treatment drug A and B) or to a placebo (denoted treatment 
# | drug C). Prior to receiving treatment, baseline data on the number of
# | leprosy bacilli at six sites of the body where the bacilli tend to
# | congregate were recorded for each patient. After several months of
# | treatment, the number of leprosy bacilli at six sites of the body
# | were recorded a second time. The outcome variable is the total
# | count of the number of leprosy bacilli at the six sites.
# | In this study, the question of main scientific interest is whether
# | treatment with antibiotics (drugs A and B) reduces the abundance
# | of leprosy bacilli at the six sites of the body when compared to
# | placebo (drug C). 
# | 
# | 
# | Variable List:
# | 
# | Drug, Pre-Treatment Bacilli Count, Post-Treatment Bacilli Count.
# | 
# | 
# | A      11     6
# | B       6     0
# | C      16    13
# | A       8     0
# | B       6     2
# | C      13    10
# | A       5     2
# | B       7     3
# | C      11    18
# | A      14     8
# | B       8     1
# | C       9     5
# | A      19    11
# | B      18    18
# | C      21    23
# | A       6     4
# | B       8     4
# | C      16    12
# | A      10    13
# | B      19    14
# | C      12     5
# | A       6     1
# | B       8     9
# | C      12    16
# | A      11     8
# | B       5     1
# | C       7     1
# | A       3     0
# | B      15     9
# | C      12    20
# `-----
#
# There may be leading space(s) in the data section, which may have any
# combination of alphanumeric characters, so this is tricky.
#
# The working strategy is to start from the end of the file, removing empty
# or white space lines, and search for the first empty or white space above
# it.
# -------------------------------------------------------------------------
# Code:

{ lines[++n]=$0 }

END {
    i=n
    #  Delete trailing whitespace
    while (lines[i] ~ /^[[:space:]]*$/) {
	delete lines[i]
	i--
    }
    # Find the index of last whitespace line in array
    while (lines[i] !~ /^[[:space:]]*$/) i--
    # Print from there to the last remaining line
    for (x=i + 1; x <= length(lines); x++) print lines[x]
}


# 
# read_data.awk ends here
