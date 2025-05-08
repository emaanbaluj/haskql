# DO NOT RUN OUTSIDE OF THE MAIN HASKQL DIRECTORY

# Prepare project zip

find haskql -mindepth 1 -delete
stack clean && rsync -av --exclude='*.cql' --exclude='*.csv' --exclude='haskql/' --exclude='.*' --exclude='tasks' --exclude='*.zip' ./ haskql/
rm project.zip
zip -r project.zip haskql
find haskql -mindepth 1 -delete

# Prepare tasks zip

find tasks -mindepth 1 -delete
rsync -av --include='*.cql' --exclude='*' --exclude='*.zip' ./src/ tasks/
rm tasks.zip
zip -r tasks.zip tasks
find tasks -mindepth 1 -delete

# Quick testing
unzip project.zip
unzip tasks.zip
cd haskql
cp ../*.csv .
mv ../tasks/*.cql .
stack build