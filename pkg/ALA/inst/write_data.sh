# write_data.sh --- Extract and write data files from ALA book
# Author: Sebastian P. Luque
# Created: Fri Aug 13 18:41:41 2010 (UTC)
# Last-Updated: Mon Aug 16 18:02:00 2010 (UTC)
#           By: Sebastian P. Luque
# copyright (c) 2010 Sebastian P. Luque
# -------------------------------------------------------------------------
# Commentary: 
#
# Set READPROG properly.
# -------------------------------------------------------------------------
# Code:

READPROG=read_data.awk

for f in $@; do
    # Assume txt suffix
    f_data="${f%.txt}"
    awk -f ${READPROG} "$f" >> $f_data
done

exit 0


# 
# write_data.sh ends here
