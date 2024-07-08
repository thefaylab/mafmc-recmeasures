
for dir in {125..160} ; do
  #echo $(($dir + 100));
  echo "$dir"/ "$(($dir + 100))"/
  #cp -r "$dir"/ "$(($dir + 100))"/
  cp "../mse/sinatra.exe" "$(($dir + 100))"/"sinatra.exe"
  cp "../mse/calib_glm.rds" "$(($dir + 100))"/"calib_glm.rds"
done

#source_prefix=$1
#suffix=$2
#destination_prefix=$3
#
#for i in $(${source_prefix}..${suffix});do
#  echo $i; #{cp $i $i+${destination_prefix}}
#done