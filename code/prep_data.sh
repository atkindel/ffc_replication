#!/bin/bash
cd ../data
cp -r leaderboard_nodupes submissions

cd submissions/

for zip in *.zip
do
  dirname=`echo $zip | sed 's/\.zip$//'`
  if mkdir "$dirname"
  then
    if cd "$dirname"
    then
      unzip ../"$zip"
      cd ..
      rm -f $zip
    else
      echo "Could not unpack $zip - cd failed"
    fi
  else
    echo "Could not unpack $zip - mkdir failed"
  fi
done

find . -name "__MACOSX" -delete
find submissions -name "*.zip" -delete

# Rename rpart duplicate chunk in err
# Unzip samantha_malte code dir
# find submissions -name "*.zip" -delete
