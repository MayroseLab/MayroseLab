#!/bin/bash
###########################################################################
#
# Parallel blast - run a blast search in parallel
# using N CPUs. Works by splitting the input fasta,
# running blast on each chunk and combining all results.
# This is much faster than running blast with the -num_threads option
#
# Dependencies:
# - faSplit must be available to run from command line
#
# Usage: parallel_blast.sh <BLAST COMMAND>
# E.g: parallel_blast.sh tblastn -evalue 0.001 -num_threads 20
#            -query queries.fasta -db db.fasta -out out.blast7 -outfmt 7
# *** the -num_threads option must be included
#
###########################################################################
if ! command -v faSplit &> /dev/null
then
    echo "faSplit command is not available!"
    exit
fi

blast_command="$*"

in_fasta=`echo $blast_command | sed 's/.*-query \([^ ]*\).*/\1/'`
out_file=`echo $blast_command | sed 's/.*-out \([^ ]*\).*/\1/'`
ncpus=`echo $blast_command | sed 's/.*-num_threads \([^ ]*\).*/\1/'`

if [ -f "$FILE" ]; then
    echo "output file $FILE already exists!"
    exit
fi
out_path=`dirname $out_file`
tmp_path="$out_path/tmp_paral_blast"
rm -rf $tmp_path
mkdir $tmp_path

echo "Splitting input fasta..."
faSplit sequence $in_fasta $ncpus "$tmp_path/"
echo "Running $ncpus BLAST jobs in parallel..."
for f in $tmp_path/*.fa;
do
	new_com=`echo $blast_command | sed -e "s|\(.*-query \)[^ ]*\( .*\)|\1$f\2|" -e "s|\(.*-num_threads \)[^ ]*\( .*\)|\11\2|" -e "s|\(.*-out \)[^ ]*\( .*\)|\1$f\.blast\2|"`
	$new_com &
done
wait
echo "Combining outputs from parallel BLAST runs..."
cat $tmp_path/*.blast > $out_file

rm -rf $tmp_path
