#! /bin/bash
# https://stackoverflow.com/questions/8243864/finding-and-replacing-many-words/8481888

main_file=$1
bigram_file=$2
stopword_file=$3

new_file="${main_file%.txt}"_bigrammed.txt

cp "${main_file}" "$new_file"

echo "Replacing bigrams..."
while read -r bigram; do

  bi_gram=${bigram// /_} # replace *all* spaces in bigram

# -i means replace in place 
# -r is for "extended regex", which is needed for '{2,}' and the like.

  sed -i -r "s/\b${bigram}\b/ ${bi_gram} /g" "$new_file"

done < $bigram_file

echo "Removing stopwords..."
while read -r stopword; do

  sed -i -r "s/\b${stopword}\b/ /g" $new_file

done < $stopword_file

echo "Transforming any sequence of two or more whitespaces into one..."
sed -i -r "s/ {2,}/ /g" $new_file
sed -i -r "s/ $//g" $new_file
sed -i -r "s/\t /\t/g" $new_file


