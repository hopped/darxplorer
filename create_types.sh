#! /bin/bash

if [ $# -eq 0 ]
  then 
    echo "-- Please let us know the location of your scripts."
    exit
fi

SOURCE="src/compiler/dxpl_types.dummy"
TYPES=(32 64)
#FILES=("$1/dxpl_types_32.ads" "$1/dxpl_types_64.ads")
for F in ${TYPES[@]}
do
  FILE="$1/dxpl_types_$F.ads"
  echo "with Interfaces;" > "$FILE"
  echo >> "$FILE"
  echo "----------------------------------------------------------------" >> "$FILE"
  echo "--!  Author   :: dennis.hoppe@uni-weimar.de" >> "$FILE"
  echo "--!  Created  :: This file was created automatically" >> "$FILE"
  echo >> "$FILE"
  echo "--!  Purpose:" >> "$FILE"
  echo "--!  This file was created automatically by means of" >> "$FILE"
  echo "--!  the source file 'src/compiler/dxpl_types.dummy'." >> "$FILE"
  echo "----------------------------------------------------------------" >> "$FILE"
  echo "package DXPL_Types_$F is" >> "$FILE"
  echo "  type Object is interface;" >> "$FILE"
  echo >> "$FILE"
  echo "  subtype Word is Interfaces.Unsigned_$F;" >> "$FILE"
  while read LINE
  do
    echo "  "$LINE >> "$FILE"
  done < $SOURCE
  echo "end DXPL_Types_$F;" >> "$FILE"
  echo "Created" $FILE "successfully."
done