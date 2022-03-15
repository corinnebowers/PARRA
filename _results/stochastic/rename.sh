mkdir -p checkpoints
mkdir -p logfiles

for i in $(cat id.txt) 
  do \
  mv checkpoints_$i/DV.Rdata checkpoints/DV_$i.Rdata 
  rm -r checkpoints_$i
  mv PARRA_$i.log logfiles
done
